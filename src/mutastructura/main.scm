;;; MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

;; Copyright © Josep Jesus Bigorra Algaba

;; mutastructura is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mutastructura is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with mutastructura. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The command line, and the only part of the program that talks to a *user*.
;;
;; It decides nothing about a database.  Which engine was asked for, what a
;; connection looks like and what a migration file is all come from the core;
;; what the ledger says and how a migration runs come from the runner the
;; engine's family selects.  So `--database mongodb' and `--database
;; postgresql' take the same path through this file, and adding an engine of a
;; family that already exists is a line in the registry rather than a branch
;; here.
;;
;; The five steps below are the whole program:
;;
;;   connect    a spec from `--connection' and `--database', handed to the
;;              engine family's runner, which opens whatever that engine
;;              needs -- a guile-dbi handle, a MongoDB connection.
;;
;;   read       the ledger, which `ensure-history!' has just created when it was
;;              missing, as entries the core can compare with files.
;;
;;   plan       the migration directory against the ledger: what is pending,
;;              what changed since it was applied, what is missing its file.
;;
;;   run        every pending migration inside the engine's transaction, then
;;              write the ledger row.  A failure rolls back where the engine can
;;              honour that, and is recorded as a failure where it cannot.
;;
;;   close      always, including when the run raised: a connection left open is
;;              a lock left held.

(define-module (mutastructura main)
  #:declarative? #t
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (srfi srfi-1)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence runners)
  #:export (main))

;; The four options the wrapper script translates and passes through.  Anything
;; else is a typo, and `getopt-long' says so rather than ignoring it.
(define option-spec
  '((resources (single-char #\r) (value #t))
    (migrations (single-char #\m) (value #t) (required? #t))
    (connection (single-char #\c) (value #t) (required? #t))
    (database (single-char #\d) (value #t) (required? #t))))

(define* (main #:key (args (command-line)))
  (log-banner)
  (let* ((options (getopt-long args option-spec))
         (migrations-path (option-ref options 'migrations #f))
         (connection (option-ref options 'connection #f))
         (resources (option-ref options 'resources #f))
         (engine (parse-engine (option-ref options 'database #f)))
         (runner (runner-for-engine engine))
         (spec (connection-spec-of connection engine))
         (session (open-session runner spec resources)))
    ;; The session is closed however the run ends.  A connection left open is a
    ;; lock left held, and on SQLite a file nothing else can write.
    (dynamic-wind
     (lambda () #t)
     (lambda ()
       (let* ((history ((runner-history runner) session))
              (files (scandir-for-migrations migrations-path #:engine engine))
              (plan (plan-migrations files history))
              (applicable (plan-applicable plan)))
         (log-history history)
         (log-problems plan)
         (if (null? applicable)
             (log-nothing-to-do)
             (begin
               (log-migrations-to-run applicable)
               (migrate! runner session applicable engine)))))
     (lambda () ((runner-close! runner) session)))))

;;; ---------------------------------------------------------------------------
;;; The connection
;;; ---------------------------------------------------------------------------

(define (connection-spec-of connection engine)
  "The connection spec for CONNECTION, reached as ENGINE.

A URL -- `postgresql://user:pass@host/db', `sqlite3:///var/lib/app.db',
`mongodb://host:27017/app' -- is parsed into its parts, and a URL for another
engine than the one asked for is refused rather than silently ignored.  Anything
else is taken at face value as the engine's own connection string, which is what
guile-dbi's `user:pass:db:tcp:host:port' is."
  (let ((spec (parse-connection-url connection #:engine engine)))
    (let ((url-engine (and (connection-spec-url spec)
                           (string-index connection #\:)
                           (find-engine (substring connection
                                                   0
                                                   (string-index connection #\:))))))
      (when (and url-engine (not (eq? (engine-id url-engine)
                                      (engine-id engine))))
        (raise-exception
         (make-exception-with-message
          (format #f "the URL names ~a but --database says ~a"
                  (engine-id url-engine)
                  (engine-id engine)))))
      spec)))

(define (open-session runner spec resources)
  "Open a session through RUNNER, with the ledger usable."
  (let* ((options (append (list (cons 'log log-message))
                          (if resources
                              (list (cons 'resources resources))
                              '())))
         (session ((runner-open runner) spec options)))
    ((runner-ensure-history! runner) session)
    session))

;;; ---------------------------------------------------------------------------
;;; Running the pending migrations
;;; ---------------------------------------------------------------------------

(define (migrate! runner session applicable engine)
  "Run APPLICABLE, recording each in the ledger as it goes.

The whole run is one transaction where the engine has transactional DDL, so a
migration that fails leaves the database as it was and nothing is recorded as
having happened.  Where the engine commits DDL as it goes -- MySQL -- the
statements that ran stand, and the message says so rather than pretending."
  ((runner-begin! runner) session)
  (catch #t
         (lambda ()
           (for-each (lambda (migration)
                       (run-one runner session migration))
                     applicable)
           ((runner-commit! runner) session))
         (lambda (key . args)
           ((runner-rollback! runner) session)
           (format #t "~%---------------------------------------------------------~%")
           (unless ((runner-transactional? runner) session)
             (format #t
                     "!! ~a commits DDL as it runs, so what ran before the failure stands.~%"
                     (engine-id engine)))
           (format #t "!! the run was rolled back.~%")
           (if (exception? key)
               (raise key)
               (apply throw key args))))
  (log-migrations-successful))

(define (run-one runner session migration)
  "Run MIGRATION in SESSION and record what happened in the ledger."
  (let* ((path (migration-path migration))
         (started (gettimeofday))
         (statements ((runner-apply! runner) session migration '()))
         (elapsed (- (car (gettimeofday)) (car started)))
         (checksum (or (assq-ref migration 'checksum)
                       (migration-checksum path))))
    ((runner-record! runner)
     session
     (list (cons 'name (migration-name migration))
           (cons 'checksum checksum)
           (cons 'description (migration-description migration))
           (cons 'kind (migration-kind migration))
           (cons 'execution-time elapsed)
           (cons 'success #t)))
    statements))

;;; ---------------------------------------------------------------------------
;;; CLI output
;;; ---------------------------------------------------------------------------

(define (log-message level message)
  "Print a LEVEL/MESSAGE pair from a runner."
  (format #t "  ~a~%" message))

(define (log-history history)
  (unless (null? history)
    (format #t "~%FOUND MIGRATION HISTORY:~%~%" )
    (for-each (lambda (entry)
                (format #t "  ~a (~a, ~a ms)~%"
                        (history-entry-name entry)
                        (history-entry-kind entry)
                        (history-entry-execution-time entry)))
              history)))

(define (log-problems plan)
  "Report what the plan found wrong, without refusing to run what is fine.

A migration whose file changed after it was applied, and a ledger entry whose
file has gone, are both things a person has to decide about: the plan holds
them, the run skips them, and the message is the reminder."
  (let ((problems (plan-problems plan)))
    (unless (null? problems)
      (format #t "~%!! ~a of the migrations in the plan need attention:~%"
              (length problems))
      (for-each (lambda (entry)
                  (format #t "   ~a: ~a~%"
                          (migration-name entry)
                          (assq-ref entry 'state)))
                problems))))

(define (log-migrations-to-run plan)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "MIGRATIONS TO RUN:~%~%")
  (for-each (lambda (entry)
              (format #t "  ~a (~a)~%"
                      (migration-name entry)
                      (migration-kind entry)))
            plan)
  (format #t "~%"))

(define (log-nothing-to-do)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "NOTHING TO DO!~%"))

(define (log-migrations-successful)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "MIGRATIONS COMPLETED SUCCESSFULLY~%"))

(define (log-banner)
  (display "
---------------------------------------------------------
MUTASTRUCTURA: Schema Migrations tooling powered by Lisp

 🏛️  M U T A S T R U C T U R A  🏛️

 _ _ V E N I • V I D I • L I S P _ _

"))
