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
;; engine's family selects.  So `--database mariadb' and `--database
;; postgresql' take the same path through this file, and adding an engine of a
;; family that already exists is a line in the registry rather than a branch
;; here.
;;
;; Three commands, all three of them the same plan read in different ways:
;;
;;   migrate    connect, read the ledger, plan the migration directory against
;;              it, and run what is pending.  The run is one transaction where
;;              the engine has transactional DDL, so a migration that fails
;;              leaves the database as it was and nothing is recorded as having
;;              happened.  Where the engine commits DDL as it goes -- MySQL --
;;              the statements that ran stand, and the message says so rather
;;              than pretending.
;;
;;   status     the plan reported and not run: every migration's state, what the
;;              plan adds up to, and what it found wrong.  It opens a session
;;              that writes nothing at all, which is what makes it safe to run
;;              against a database in production.
;;
;;   clean      drop everything the runner finds -- every table and view of the
;;              current schema, the ledger included.
;;
;; And three flags that reach further into a run:
;;
;;   --placeholder key=value   substitute `${key}' in every migration read.
;;                             Repeat the flag, or put the pairs in one
;;                             comma-separated value.
;;
;;   --dry-run                 report what would run, statement by statement,
;;                             and change nothing.
;;
;;   --verbose                 each statement in full, instead of the first line
;;                             of it.
;;
;; `status' and `clean' need no migration directory, and `migrate' and `status'
;; need one; the help text says which is which, and so does the message when one
;; is missing.

(define-module (mutastructura main)
  #:declarative? #t
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence runners)
  #:use-module (mutastructura persistence util)
  #:export (main))

;; The options the wrapper script translates and passes through.  Anything else
;; is a typo, and `getopt-long' says so rather than ignoring it.  None of them is
;; marked `required?': which ones a command needs depends on the command, and a
;; `clean' that demanded a migration directory would be asking for something it
;; never looks at.
(define option-spec
  '((command (value #t))
    (resources (single-char #\r) (value #t))
    (migrations (single-char #\m) (value #t))
    (connection (single-char #\c) (value #t))
    (database (single-char #\d) (value #t))
    (placeholder (value #t))
    (dry-run (value #f))
    (verbose (value #f))))

(define commands '(migrate status clean help))

;; Whether `--verbose' was asked for.  The runners report each statement at the
;; debug level and the one-line summary at the info level, so this is what
;; decides whether that level is shown.
(define verbose? #f)

(define* (main #:key (args (command-line)))
  "The whole program: read the options, and run the command they name.

A failure is reported the way a command line reports one -- the message, and an
exit status of 1 -- rather than as a Guile backtrace with the message somewhere
inside it.  A migration that failed, a connection that was refused and a URL
that named another engine are all things a person reads and acts on, and none of
them needs the evaluator's stack to go with it."
  (log-banner)
  (catch #t
         (lambda () (dispatch (getopt-long args option-spec)))
         report-error))

(define (report-error key . args)
  "Say what went wrong, or let an error that is not ours speak for itself.

The two shapes that are ours are the runner's migration errors and the command
layer's own complaints about options; everything else -- `getopt-long' quitting
over a typo, an error raised from deep inside a driver -- is re-raised, because
its own report is more truthful than a summary of ours."
  (let ((object (and (pair? args) (car args))))
    (cond
      ((or (migration-exception? object)
           (exception-with-message? object))
       (format (current-error-port) "~%[error] ~a~%" (error-message-of object))
       (exit 1))
      (else (apply throw key args)))))

(define (dispatch options)
  "Run the command OPTIONS name, opening one session for it and closing it."
  (let* ((resources (option-ref options 'resources #f))
         (command (parse-command (option-ref options 'command #f)))
         (migrations-path (option-ref options 'migrations #f))
         (placeholders (parse-placeholder-string
                        (option-ref options 'placeholder #f)))
         (dry-run? (and (option-ref options 'dry-run #f) #t))
         (engine (parse-engine (required-option options 'database
                                                "--database")))
         (connection (required-option options 'connection "--connection"))
         (runner (runner-for-engine engine))
         (spec (connection-spec-of connection engine)))
    (set! verbose? (and (option-ref options 'verbose #f) #t))
    (if (eq? command 'help)
        (show-help resources)
        (begin
          ;; `clean' never reads a migration directory; the other two always do,
          ;; and saying which option is missing beats a directory that is not
          ;; there being reported as a filesystem error.
          (when (and (memq command '(migrate status)) (not migrations-path))
            (raise-exception
             (make-exception-with-message
              (format #f "~a reads the migration files, so --migrations is required:~%   mutastructura ~a -m <directory> -c <connection> -d <engine>"
                      command command))))
          (let* ((options* (run-options resources command dry-run? placeholders))
                 (session (open-session runner spec options*)))
            ;; The session is closed however the run ends.  A connection left
            ;; open is a lock left held, and on SQLite a file nothing else can
            ;; write.
            (dynamic-wind
             (lambda () #t)
             (lambda ()
               (case command
                 ((migrate) (run-migrate runner session migrations-path engine
                                         dry-run? options*))
                 ((status) (run-status runner session migrations-path engine))
                 ((clean) (run-clean runner session dry-run?))))
             (lambda () ((runner-close! runner) session))))))))

(define (run-options resources command dry-run? placeholders)
  "What this run tells a runner, in the alist every runner reads.

Two of these are about writing, and they are the reason `status' and `clean' can
be pointed at a database that must not change: a session opened with
`ensure-history? #f' does not create the ledger, and one opened with `dry-run?'
does not run a statement."
  (append (list (cons 'log log-message)
                (cons 'placeholders placeholders)
                (cons 'verbose? verbose?)
                (cons 'dry-run? dry-run?)
                (cons 'ensure-history?
                      (not (or dry-run? (memq command '(status clean))))))
          (if resources
              (list (cons 'resources resources))
              '())))

(define (required-option options key flag)
  "OPTIONS' KEY, or an exception naming the FLAG that was left out."
  (or (option-ref options key #f)
      (raise-exception
       (make-exception-with-message
        (format #f "~a is required: run mutastructura --help for the options"
                flag)))))

(define (parse-command text)
  "The command named by TEXT, `migrate' when TEXT is #f."
  (let ((command (string->symbol (or text "migrate"))))
    (if (memq command commands)
        command
        (raise-exception
         (make-exception-with-message
          (format #f "unknown command ~s~%   known commands: ~a"
                  text
                  (string-join (map symbol->string commands) ", ")))))))

(define (show-help resources)
  "Print the help text bundled in RESOURCES, when it is there to print."
  (let ((file (and resources (format #f "~a/help.txt" resources))))
    (if (and file (file-exists? file))
        (display (call-with-input-file file get-string-all))
        (format #t "~%USAGE: mutastructura <command> [OPTIONS]~%   commands: ~a~%"
                (string-join (map symbol->string commands) ", ")))))

;;; ---------------------------------------------------------------------------
;;; The connection
;;; ---------------------------------------------------------------------------

(define (connection-spec-of connection engine)
  "The connection spec for CONNECTION, reached as ENGINE.

A URL -- `postgresql://user:pass@host/db', `sqlite3:///var/lib/app.db' -- is
parsed into its parts, and a URL for another engine than the one asked for is
refused rather than silently ignored.  Anything else is taken at face value as
the engine's own connection string, which is what guile-dbi's
`user:pass:db:tcp:host:port' is."
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

(define (open-session runner spec options)
  "Open a session through RUNNER, with the ledger created where OPTIONS say so."
  (let ((session ((runner-open runner) spec options)))
    (when (assq-ref options 'ensure-history?)
      ((runner-ensure-history! runner) session))
    session))

;;; ---------------------------------------------------------------------------
;;; migrate
;;; ---------------------------------------------------------------------------

(define (run-migrate runner session migrations-path engine dry-run? options)
  "Compare the migration directory with the ledger, and run what is pending."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history))
         (applicable (plan-applicable plan)))
    (log-history history)
    (log-problems plan)
    (cond
      ((null? applicable) (log-nothing-to-do))
      (dry-run?
       (log-migrations-to-run applicable)
       (log-dry-run runner applicable engine options)
       (log-dry-run-complete))
      (else
       (log-migrations-to-run applicable)
       (migrate! runner session applicable engine options)))))

(define (migrate! runner session applicable engine options)
  "Run APPLICABLE, recording each in the ledger as it goes."
  ((runner-begin! runner) session)
  (catch #t
         (lambda ()
           (for-each (lambda (migration)
                       (run-one runner session migration options))
                     applicable)
           ((runner-commit! runner) session))
         (lambda (key . args)
           ((runner-rollback! runner) session)
           (format #t "~%---------------------------------------------------------~%")
           ;; An engine without transactional DDL has nothing to roll back, so
           ;; saying that it was rolled back would be a second untruth on top of
           ;; the first: what ran stands, and the ledger will say it did not
           ;; finish.
           (if ((runner-transactional? runner) session)
               (format #t "!! the run was rolled back.~%")
               (format #t
                       "!! ~a commits DDL as it runs, so what ran before the failure stands;~%   nothing after it was run, and the ledger records no success for it.~%"
                       (engine-id engine)))
           (if (exception? key)
               (raise key)
               (apply throw key args))))
  (log-migrations-successful))

(define (run-one runner session migration options)
  "Run MIGRATION in SESSION and record what happened in the ledger."
  (let* ((path (migration-path migration))
         (started (gettimeofday))
         (statements ((runner-apply! runner) session migration options))
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
;;; status
;;; ---------------------------------------------------------------------------

(define (run-status runner session migrations-path engine)
  "Report the plan: every migration's state, and what it found wrong."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history)))
    (log-history history)
    (log-status plan)
    (log-problems plan)
    (unless (pair? (plan-problems plan))
      (format #t "~%NO PROBLEMS FOUND: nothing in the plan contradicts the ledger.~%"))))

(define (log-status plan)
  "Report each migration's state, and what the plan adds up to."
  (format #t "~%---------------------------------------------------------~%")
  (format #t "MIGRATION STATUS:~%~%")
  (if (null? plan)
      (format #t "  no migrations to report~%")
      (begin
        (format #t "  ~28a ~12a ~10a ~a~%" "MIGRATION" "STATE" "KIND" "APPLIED")
        (for-each (lambda (entry)
                    (format #t "  ~a~%" (status-line entry)))
                  plan)))
  (format #t "~%  ~a~%" (status-summary plan)))

(define (status-line entry)
  "One plan entry as a line of the status table.

The state is the plan's own word for what it found -- `pending', `applied',
`changed', `missing', `superseded', `beyond-target' and the repeatable
migration's own two -- and the applied column is blank for a migration the
ledger has never seen."
  (format #f "~28a ~12a ~10a ~a"
          (migration-name entry)
          (format #f "~a" (assq-ref entry 'state))
          (format #f "~a" (migration-kind entry))
          (let ((applied-on (assq-ref entry 'applied-on)))
            (if applied-on (format #f "~a" applied-on) ""))))

(define (status-summary plan)
  "The plan's states and how many migrations are in each."
  (let ((summary (plan-summary plan)))
    (if (null? summary)
        "nothing to report"
        (format #f "~a (~a migration~a in total)"
                (string-join (map (lambda (cell)
                                    (format #f "~a ~a" (cdr cell) (car cell)))
                                  summary)
                             ", ")
                (length plan)
                (if (= 1 (length plan)) "" "s")))))

;;; ---------------------------------------------------------------------------
;;; clean
;;; ---------------------------------------------------------------------------

(define (run-clean runner session dry-run?)
  "Drop what the runner finds, or -- with --dry-run -- name what it would drop.

Nothing is decided here: which objects there are, and what dropping one takes,
is the runner's business.  What this adds is the two lines that say what
happened, and the promise that a dry run reaches no further than a report."
  (format #t "~%---------------------------------------------------------~%")
  (if dry-run?
      (format #t "DRY RUN: nothing will be dropped.~%~%")
      (format #t "DROPPING:~%~%"))
  (let ((count ((runner-clean! runner) session)))
    (format #t "~%---------------------------------------------------------~%")
    (format #t "~a ~a object~a~%"
            (if dry-run? "WOULD DROP" "DROPPED")
            count
            (if (= count 1) "" "s"))
    (unless dry-run?
      (format #t "THE DATABASE IS CLEAN~%"))))

;;; ---------------------------------------------------------------------------
;;; CLI output
;;; ---------------------------------------------------------------------------

(define (log-message level message)
  "Print a LEVEL/MESSAGE pair from a runner.

The debug level is the detail behind the summary -- each migration's statement --
and is what `--verbose' turns on.  Everything else is printed always."
  (when (or (not (eq? level 'debug)) verbose?)
    (format #t "  ~a~%" message)))

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

(define (log-dry-run runner applicable engine options)
  "Report what APPLICABLE would run: its statements, placeholders substituted.

The statements are read through the runner, not guessed at here, so a dry run
shows the same statements -- multi-line bodies and delimiters included -- that a
real run would send."
  (format #t "DRY RUN: what would be run, statement by statement:~%")
  (for-each
   (lambda (migration)
     (let ((statements ((runner-statements runner) migration
                                                   (cons (cons 'engine engine)
                                                         options))))
       (format #t "~%  ~a (~a migration, ~a statement~a)~%"
               (migration-name migration)
               (migration-kind migration)
               (length statements)
               (if (= 1 (length statements)) "" "s"))
       (for-each (lambda (statement)
                   (let ((text (or (assq-ref statement 'sql) "")))
                     (format #t "      [~a] ~a~%"
                             (or (assq-ref statement 'line) 0)
                             ;; A SQL statement is recognised by its first line.
                             (if verbose?
                                 text
                                 (first-line text)))))
                 statements)))
   applicable))

(define (log-nothing-to-do)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "NOTHING TO DO!~%"))

(define (log-dry-run-complete)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "DRY RUN COMPLETE: nothing was applied.~%"))

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
