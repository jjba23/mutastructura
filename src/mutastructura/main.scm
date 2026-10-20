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
;; Six commands, each of them the same plan read in a different way:
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
;;   validate   the plan examined against the ledger, refusing when the two
;;              disagree: a migration edited after it was applied, or a ledger
;;              entry whose file has gone.  `status' reports those and exits 0;
;;              this one exits 1, which is what a CI pipeline wants.
;;
;;   rollback   undo applied migrations, newest first, with their *.undo.sql
;;              scripts.  Without an option it undoes one; --steps N undoes N,
;;              --to VERSION undoes everything above VERSION, and --all walks
;;              back as far as undo scripts allow.  A migration with no undo
;;              script, or one marked irreversible, blocks the rollback rather
;;              than being skipped, because undoing below it would leave a
;;              later migration applied on top of an earlier one that is not.
;;
;;   repair     bring the ledger back into agreement with the files: forget a
;;              row that records a failure, and realign the checksum of a file
;;              that changed after it was applied.  With --dry-run it names the
;;              rows it would touch and writes nothing.
;;
;;   clean      drop everything the runner finds -- every table and view of the
;;              current schema, the ledger included.
;;
;; And nine flags that reach further into a run:
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
;;   --target VERSION          run only what is at or below VERSION, exactly
;;                             like Flyway's `target'.
;;
;;   --out-of-order            apply a pending migration older than the newest
;;                             one already applied, which is held back by
;;                             default.
;;
;;   --to VERSION              roll back to just above VERSION (rollback only).
;;
;;   --steps N                 undo N migrations (rollback only).
;;
;;   --all                     undo as far back as undo scripts allow.
;;
;;   --allow-irreversible      roll back a migration that says not to.
;;
;; `clean' needs no migration directory; every other command needs one, and the
;; help text says which is which, and so does the message when one is missing.

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
    (target (value #t))
    (out-of-order (value #f))
    (to (value #t))
    (steps (value #t))
    (all (value #f))
    (allow-irreversible (value #f))
    (dry-run (value #f))
    (verbose (value #f))))

(define commands '(migrate status validate rollback repair clean help))

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
         (target (option-ref options 'target #f))
         (out-of-order? (and (option-ref options 'out-of-order #f) #t))
         (to-version (option-ref options 'to #f))
         (steps (parse-steps (option-ref options 'steps #f)))
         (all? (and (option-ref options 'all #f) #t))
         (allow-irreversible? (and (option-ref options 'allow-irreversible #f) #t))
         (dry-run? (and (option-ref options 'dry-run #f) #t)))
    (set! verbose? (and (option-ref options 'verbose #f) #t))
    ;; `help' needs neither a database nor a connection: it reads the bundled
    ;; text and nothing else.
    (if (eq? command 'help)
        (show-help resources)
        (let* ((engine (parse-engine (required-option options 'database
                                                      "--database")))
               (connection (required-option options 'connection
                                            "--connection"))
               (runner (runner-for-engine engine))
               (spec (connection-spec-of connection engine)))
          (begin
            ;; A flag that belongs to another command is a typo worth naming:
            ;; silently ignoring it would migrate when the user meant to undo.
            (when (and (not (eq? command 'rollback))
                       (or to-version steps all? allow-irreversible?))
              (raise-exception
               (make-exception-with-message
                "--to, --steps, --all and --allow-irreversible only apply to rollback")))
            (when (and (eq? command 'rollback) target)
              (raise-exception
               (make-exception-with-message
                "rollback takes --to, not --target")))
            ;; `clean' never reads a migration directory; the rest always do,
            ;; and saying which option is missing beats a directory that is not
            ;; there being reported as a filesystem error.
            (when (and (memq command '(migrate status validate rollback repair))
                       (not migrations-path))
              (raise-exception
               (make-exception-with-message
                (format #f "~a reads the migration files, so --migrations is required:~%   mutastructura ~a -m <directory> -c <connection> -d <engine>"
                        command command))))
            (let* ((options* (run-options resources command dry-run?
                                          placeholders))
                   (session (open-session runner spec options*)))
              ;; The session is closed however the run ends.  A connection left
              ;; open is a lock left held, and on SQLite a file nothing else can
              ;; write.
              (dynamic-wind
               (lambda () #t)
               (lambda ()
                 (case command
                   ((migrate) (run-migrate runner session migrations-path engine
                                           dry-run? target out-of-order? options*))
                   ((status) (run-status runner session migrations-path engine
                                         target out-of-order?))
                   ((validate) (run-validate runner session migrations-path engine
                                             target out-of-order?))
                   ((rollback) (run-rollback runner session migrations-path engine
                                             dry-run? to-version steps all?
                                             allow-irreversible? options*))
                   ((repair) (run-repair runner session migrations-path engine
                                         dry-run?))
                   ((clean) (run-clean runner session dry-run?))))
               (lambda () ((runner-close! runner) session)))))))))

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
                      (not (or dry-run?
                               (memq command
                                     '(status validate rollback repair clean))))))
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

(define (parse-steps text)
  "The positive whole number TEXT names, or #f when there is no TEXT."
  (if (not text)
      #f
      (let ((n (string->number text)))
        (if (and (exact-integer? n) (> n 0))
            n
            (raise-exception
             (make-exception-with-message
              (format #f "--steps wants a positive whole number, not ~s"
                      text)))))))

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

(define (run-migrate runner session migrations-path engine dry-run? target
                    out-of-order? options)
  "Compare the migration directory with the ledger, and run what is pending."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history #:target target
                                #:out-of-order? out-of-order?))
         (applicable (plan-applicable plan)))
    (log-history history)
    (log-target target)
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

(define (milliseconds-since started)
  "Whole milliseconds elapsed since STARTED, a `gettimeofday' pair."
  (let ((now (gettimeofday)))
    (+ (* 1000 (- (car now) (car started)))
       (quotient (- (cdr now) (cdr started)) 1000))))

(define (run-one runner session migration options)
  "Run MIGRATION in SESSION and record what happened in the ledger."
  (let* ((path (migration-path migration))
         (started (gettimeofday))
         (statements ((runner-apply! runner) session migration options))
         (elapsed (milliseconds-since started))
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

(define (run-status runner session migrations-path engine target
                    out-of-order?)
  "Report the plan: every migration's state, and what it found wrong."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history #:target target
                                #:out-of-order? out-of-order?)))
    (log-history history)
    (log-target target)
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
;;; validate
;;; ---------------------------------------------------------------------------

(define (run-validate runner session migrations-path engine target out-of-order?)
  "Refuse when the ledger and the migration files disagree.

This is `status' with an exit status: the same plan, the same report, and an
error instead of a reassuring line when a migration changed after it was
applied or a ledger entry's file has gone.  A CI pipeline can therefore run it
and believe the answer."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history #:target target
                                #:out-of-order? out-of-order?
                                #:validate? #t))
         (problems (plan-problems plan)))
    (log-history history)
    (log-target target)
    (log-status plan)
    (log-problems plan)
    (if (null? problems)
        (format #t "~%VALIDATION PASSED: the ledger and the migration files agree.~%")
        (raise-exception
         (make-exception-with-message
          (format #f "validation failed: ~a migration~a ~a attention"
                  (length problems)
                  (if (= 1 (length problems)) "" "s")
                  (if (= 1 (length problems)) "needs" "need")))))))

;;; ---------------------------------------------------------------------------
;;; rollback
;;; ---------------------------------------------------------------------------

(define (log-rollback-plan plan)
  "Report the rollback PLAN, newest first, and why each entry is there."
  (format #t "ROLLBACK PLAN (newest first):~%~%")
  (for-each
   (lambda (entry)
     (let ((action (assq-ref entry 'action)))
       (format #t "  ~28a ~10a ~a~%"
               (assq-ref entry 'name)
               (if (eq? action 'undo) "undo"
                   (if (eq? action 'boundary) "stop" "blocked"))
               (if (eq? action 'undo)
                   (format #f "~a" (migration-name (assq-ref entry 'undo)))
                   (assq-ref entry 'reason)))))
   plan))

(define (undo-migration entry)
  "ENTRY's undo script, presented as the migration the runner would apply."
  (append (assq-ref entry 'undo)
          (list (cons 'name (assq-ref entry 'name))
                (cons 'kind 'undo))))

(define (rollback! runner session undoable engine options)
  "Run the undo scripts in UNDOABLE, newest first, forgetting each ledger row."
  ((runner-begin! runner) session)
  (catch #t
         (lambda ()
           (for-each (lambda (entry)
                       (let ((undo (undo-migration entry)))
                         ((runner-apply! runner) session undo options)
                         ((runner-forget! runner) session
                          (assq-ref entry 'name))))
                     undoable)
           ((runner-commit! runner) session))
         (lambda (key . args)
           ((runner-rollback! runner) session)
           (format #t "~%---------------------------------------------------------~%")
           ;; Where the engine cannot undo DDL, the undo statements that ran
           ;; stand and the ledger row is still there: saying that is better
           ;; than a promise this program cannot keep.
           (if ((runner-transactional? runner) session)
               (format #t "!! the rollback was rolled back.~%")
               (format #t
                       "!! ~a commits DDL as it runs, so the undo statements before the failure stand;~%   nothing after it was run, and the ledger still records the migration as applied.~%"
                       (engine-id engine)))
           (if (exception? key)
               (raise key)
               (apply throw key args))))
  (format #t "~%---------------------------------------------------------~%")
  (format #t "ROLLED BACK ~a MIGRATION~a~%"
          (length undoable)
          (if (= 1 (length undoable)) "" "S")))

(define (log-rollback-dry-run runner undoable engine options)
  "Report the statements that would be run, and change nothing."
  (format #t "~%DRY RUN: nothing will be rolled back.~%")
  (log-statements runner (map undo-migration undoable) engine options)
  (format #t "~%---------------------------------------------------------~%")
  (format #t "WOULD ROLL BACK ~a MIGRATION~a~%"
          (length undoable)
          (if (= 1 (length undoable)) "" "S")))

(define (run-rollback runner session migrations-path engine dry-run?
                      to-version steps all? allow-irreversible? options)
  "Undo applied migrations, newest first, with their undo scripts.

Not every migration can be reversed, and pretending otherwise is how a database
ends up with a later migration applied on top of an earlier one that was undone.
So a requested number of steps, or a requested target, is refused in full when
any migration in the range is blocked -- no undo script, a recorded failure, or
an author's irreversible directive.  `--all' is the smart one: it walks back the
newest run of rollback-able migrations, names the one that stopped it, and exits
happily with as much undone as is safe."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (rollback-plan files history #:to-version to-version
                              #:steps steps #:all? all?
                              #:allow-irreversible? allow-irreversible?))
         (undoable (filter (lambda (entry)
                             (eq? (assq-ref entry 'action) 'undo))
                           plan))
         (blocked (filter (lambda (entry)
                            (eq? (assq-ref entry 'action) 'blocked))
                          plan))
         (boundary (find (lambda (entry)
                           (eq? (assq-ref entry 'action) 'boundary))
                         plan)))
    (format #t "~%---------------------------------------------------------~%")
    (cond
      ((null? plan)
       (if to-version
           (format #t "NOTHING TO ROLL BACK: nothing is applied above ~a.~%"
                   to-version)
           (format #t "NOTHING TO ROLL BACK: no versioned migration is applied.~%")))
      (else
       (log-rollback-plan plan)
       (when (pair? blocked)
         (raise-exception
          (make-exception-with-message
           (format #f "cannot roll back: ~a"
                   (string-join
                    (map (lambda (entry)
                           (format #f "~a (~a)"
                                   (assq-ref entry 'name)
                                   (assq-ref entry 'reason)))
                         blocked)
                    "; ")))))
       (if (null? undoable)
           ;; --all found its boundary immediately, so nothing is safe.
           (raise-exception
            (make-exception-with-message
             (format #f "nothing can be rolled back: ~a (~a)"
                     (assq-ref boundary 'name)
                     (assq-ref boundary 'reason))))
           (begin
             (if dry-run?
                 (log-rollback-dry-run runner undoable engine options)
                 (rollback! runner session undoable engine options))
             (when boundary
               (format #t "~%~a ~a: ~a.~%"
                       (if dry-run? "WOULD STOP BEFORE" "STOPPED BEFORE")
                       (assq-ref boundary 'name)
                       (assq-ref boundary 'reason)))))))))

;;; ---------------------------------------------------------------------------
;;; repair
;;; ---------------------------------------------------------------------------

(define (run-repair runner session migrations-path engine dry-run?)
  "Bring the ledger back into agreement with the files on disk.

Two disagreements are repairable, and each is named before it is touched: a
ledger row that records a failure is forgotten, so the migration is attempted
again; a migration whose file changed after it was applied has its checksum
realigned, which is knowingly accepting the file as it now stands.  Both are
deliberate acts, which is why `--dry-run' names them and writes nothing."
  (let* ((history ((runner-history runner) session))
         (files (scandir-for-migrations migrations-path #:engine engine))
         (plan (plan-migrations files history))
         (candidates (repair-candidates plan)))
    (format #t "~%---------------------------------------------------------~%")
    (if dry-run?
        (format #t "DRY RUN: nothing will be repaired.~%~%")
        (format #t "REPAIRING:~%~%"))
    (if (null? candidates)
        (format #t "  nothing to repair~%")
        (for-each
         (lambda (entry)
           (if (eq? (assq-ref entry 'state) 'changed)
               (begin
                 (format #t "  ~a: realigning the checksum with the file~%"
                         (migration-name entry))
                 (unless dry-run?
                   ((runner-record! runner) session entry)))
               (begin
                 (format #t "  ~a: forgetting the recorded failure~%"
                         (migration-name entry))
                 (unless dry-run?
                   ((runner-forget! runner) session (migration-name entry))))))
         candidates))
    (format #t "~%---------------------------------------------------------~%")
    (if (null? candidates)
        (format #t "NOTHING TO REPAIR~%")
        (format #t "~a ~a entr~a~%"
                (if dry-run? "WOULD REPAIR" "REPAIRED")
                (length candidates)
                (if (= 1 (length candidates)) "y" "ies")))))

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

(define (log-target target)
  "Say which target this run stops at, when it was asked for one."
  (when target
    (format #t "~%TARGET: ~a~%" target)))

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
  (log-statements runner applicable engine options))

(define (log-statements runner applicable engine options)
  "Report each migration in APPLICABLE and the statements it holds."
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
