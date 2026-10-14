;;; mutastructura - Relational Schema Migrations Powered by Lisp

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; mutastructura is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; mutastructura is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with mutastructura.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The SQL side of the runner vtable in `(mutastructura mutastructura)': a
;; session wraps one guile-dbi handle, the ledger's name, and the columns the
;; ledger is actually made of.
;;
;; Two decisions are worth spelling out.
;;
;; The ledger is checked through the catalogue rather than by selecting from it
;; and reading the error.  On a fresh PostgreSQL database the old approach worked
;; by accident: a failed statement is harmless outside a transaction, but the
;; first run inside one is poisoned and everything after it fails with "current
;; transaction is aborted" instead of "the ledger is not there yet".
;;
;; The ledger's columns are discovered, not assumed.  A deployment that has been
;; migrating since before this version has a ledger with three columns; one
;; created now has nine.  Writing only the columns that exist is what makes the
;; upgrade a non-event, and it is cheaper than a migration for the ledger itself.

(define-module (mutastructura persistence migration-runner)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (dbi dbi)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence util)
  #:use-module (mutastructura persistence dialect)
  #:use-module (mutastructura persistence sql-parser)
  ;; As with the document runner next door, the operations are exported rather
  ;; than reached through the vtable alone: a test wants to drive one of them
  ;; against a real database, and a caller that has a session already wants the
  ;; rest of the ledger's verbs.
  #:export (sql-runner
            sql-session?
            sql-session-engine
            sql-session-db
            sql-session-table
            sql-session-columns
            sql-session-options
            sql-open-session
            sql-close!
            sql-table-exists?
            sql-history-columns
            sql-history
            sql-entry-for
            sql-ensure-history!
            sql-record!
            sql-forget!
            sql-statements
            sql-apply!
            sql-begin!
            sql-commit!
            sql-rollback!
            sql-clean!
            sql-droppable-objects
            sql-entry->insert
            sql-entry->update
            sql-init-statements))

;;; ---------------------------------------------------------------------------
;;; Sessions
;;; ---------------------------------------------------------------------------

(define-record-type <sql-session>
  (make-sql-session engine spec db table columns options locked?)
  sql-session?
  (engine sql-session-engine)
  (spec sql-session-spec)
  (db sql-session-db)
  (table sql-session-table)
  (columns sql-session-columns)  ; the columns the ledger really has
  (options sql-session-options)
  (locked? sql-session-locked? set-sql-session-locked?!))

(define (sql-session-quoted-table session)
  (dialect-quote-identifier (sql-session-engine session)
                            (sql-session-table session)))

(define (session-log session level message)
  "Report MESSAGE at LEVEL through the session's logger, if it has one."
  (let ((log (assq-ref (sql-session-options session) 'log)))
    (when log (log level message))))

(define (session-option session key default)
  "SESSION's option KEY, or DEFAULT when the caller did not set it."
  (let ((pair (assq key (sql-session-options session))))
    (if pair (cdr pair) default)))

(define (session-placeholders session)
  (or (assq-ref (sql-session-options session) 'placeholders) '()))

(define (session-dry-run? session)
  "Whether the caller asked for a run that changes nothing."
  (and (session-option session 'dry-run? #f) #t))

(define (session-verbose? session)
  "Whether the caller asked for each statement, in full."
  (and (session-option session 'verbose? #f) #t))

;;; ---------------------------------------------------------------------------
;;; Verifying what the server did
;;; ---------------------------------------------------------------------------

(define (sql-verify! session context)
  "Ask SESSION's engine whether its last statement was accepted, where it can.

Only PostgreSQL (and everything reached through its driver) has to be asked: see
`dialect-verification-query'.  The question is worth asking only inside a
transaction, because outside one a refused statement leaves nothing behind to
find -- which is why the callers that work outside a transaction check for the
effect instead, as creating the ledger does."
  (let* ((engine (sql-session-engine session))
         (query (dialect-verification-query engine)))
    (when query
      ;; No `#:engine' here on purpose: what this asks about is the *rows*, and a
      ;; status that reads as a failure is how the server says `it was refused'
      ;; -- which the count below says better, in the caller's own words.
      (let ((rows (fetch-sql-rows (sql-session-db session) query
                                  #:context (format #f "verifying ~a" context))))
        (unless (= 1 (length rows))
          (raise-migration-error
           (format #f "~a: the server refused this statement.~%   guile-dbi's PostgreSQL driver reports only that a statement was sent, so the~%   server's own message is not carried back: run the statement by hand, or~%   read the server's log, for it.  Every statement after a refused one is~%   refused too, so the transaction is aborted and nothing this run did will~%   be kept."
                   context)))))))

(define (sql-run-verified! session sql context)
  "Run SQL in SESSION, and ask whether the server accepted it where that is knowable."
  (execute-sql! (sql-session-db session) sql #:context context)
  (sql-verify! session context))

;;; ---------------------------------------------------------------------------
;;; The ledger
;;; ---------------------------------------------------------------------------

(define (sql-table-exists? session)
  "Whether the ledger's table is already there."
  (let* ((engine (sql-session-engine session))
         (query (dialect-history-table-exists-query engine
                                                    (sql-session-table session))))
    (pair? (fetch-sql-rows (sql-session-db session) query
                           #:engine engine
                           #:context "asking the catalogue for the ledger"))))

(define (sql-history-columns session)
  "The column names the ledger really has, as symbols."
  (let* ((engine (sql-session-engine session))
         (query (dialect-history-columns-query engine
                                               (sql-session-table session))))
    (filter-map (lambda (row)
                  (let ((name (or (assq-ref row 'name)
                                  (assq-ref row 'column-name))))
                    (and name
                         (sql-column->symbol (format #f "~a" name)))))
                (fetch-sql-rows (sql-session-db session) query
                                #:engine engine
                                #:context "reading the ledger's columns"))))

(define (sql-init-statements engine table options)
  "The statements that create the ledger.

A `--resources' directory may carry its own definition of the ledger
(~application-migrations/<engine>/mutastructura-init.sql~), which is how a
deployment customises it.  It names the table `migrations_history', so it is used
only for that name; a `--history-table' of the user's own gets the generated
definition, which is built around their name."
  (let* ((resources (assq-ref options 'resources))
         (file (and resources
                    (format #f "~a/application-migrations/~a/mutastructura-init.sql"
                            resources
                            (engine-resource-subdirectory engine)))))
    (if (and file
             (file-exists? file)
             (string=? "migrations_history" table))
        (parse-sql-statements (read-migration-text file) #:engine engine)
        (list (list (cons 'index 0) (cons 'line 1)
                    (cons 'sql (dialect-history-ddl engine table)))))))

(define (sql-ensure-history! session)
  "Create the ledger when it is missing, and record its real columns."
  (unless (sql-table-exists? session)
    (session-log session 'debug
                 (format #f "creating the ledger ~a" (sql-session-table session)))
    (for-each (lambda (statement)
                (execute-sql! (sql-session-db session) (assq-ref statement 'sql)
                              #:context "creating the ledger"))
              (sql-init-statements (sql-session-engine session)
                                   (sql-session-table session)
                                   (sql-session-options session)))
    ;; The ledger is created outside a transaction, where a refused statement on
    ;; PostgreSQL cannot be asked about, so the proof has to be the ledger
    ;; itself: there, or not.  A run that carried on without one would report
    ;; every migration as applied and never read a single row of it back.
    (unless (sql-table-exists? session)
      (raise-migration-error
       (format #f "the ledger ~a is not there after creating it.~%   (guile-dbi's PostgreSQL driver does not carry the server's message back:~%   check the server's log, or run the CREATE TABLE by hand.)"
               (sql-session-table session)))))
  session)

(define (sql-select-columns session)
  "The ledger's columns, named for a SELECT the engine's driver can read back.

Named rather than asked for with `*': guile-dbi's PostgreSQL driver unpacks a
row from the result's own metadata, and `SELECT *' arrives there with none.  The
answer is no rows at all -- indistinguishable from an empty ledger -- which made
every PostgreSQL deployment re-run every migration on every run.  Naming them is
also what keeps a ledger made by an older mutastructura, with fewer columns,
readable, and what lets the one column the same driver cannot unpack be read as
text (`dialect-readable-column')."
  (let ((engine (sql-session-engine session)))
    (string-join (map (lambda (column)
                        (dialect-readable-column
                         engine (symbol->sql-column column)))
                      (or (sql-session-columns session)
                          (dialect-history-columns engine)))
                 ", ")))

(define (sql-history session)
  "Every row of the ledger, oldest first; empty when there is no ledger yet.

A database that has never been migrated has no ledger, and the honest answer to
`which migrations are applied here?' is then `none', not a failure."
  (if (not (sql-table-exists? session))
      '()
      (let ((rows (fetch-sql-rows (sql-session-db session)
                                  (format #f "SELECT ~a FROM ~a ORDER BY id"
                                          (sql-select-columns session)
                                          (sql-session-quoted-table session))
                                  #:engine (sql-session-engine session)
                                  #:context "reading the ledger")))
        (map (lambda (row)
               (map (lambda (pair)
                      (cons (if (symbol? (car pair))
                                (sql-column->symbol (symbol->string (car pair)))
                                (car pair))
                            (cdr pair)))
                    row))
             rows))))

(define (entry-value entry field)
  "FIELD's value in ENTRY, or #f."
  (let ((pair (assq field entry)))
    (and pair (cdr pair))))

(define (entry-column-value engine field value)
  "VALUE written as a literal for the ledger's FIELD."
  (cond
    ((not value) "NULL")
    ((memq field '(execution-time success))
     (dialect-literal engine (if (number? value) value (if (eq? value #t) 1 0))))
    (else (dialect-literal engine (format #f "~a" value)))))

(define (ledger-fields entry)
  "The `(column . value)' pairs of ENTRY that the ledger knows how to hold.

Fields the caller left out are filled with what the ledger's defaults would
have been anyway -- a migration is versioned unless it says otherwise, its
description is its name, and mutastructura is the one installing it.  Writing
them explicitly keeps the ledger readable, and keeps the NOT NULL columns
satisfied when a caller only passes a name and a checksum."
  (let* ((name (entry-value entry 'name))
         (kind (entry-value entry 'kind)))
    (list (cons 'name name)
          (cons 'checksum (or (entry-value entry 'checksum) ""))
          (cons 'description (or (entry-value entry 'description) name))
          (cons 'kind (if kind
                          (format #f "~a" kind)
                          "versioned"))
          (cons 'installed-on (entry-value entry 'installed-on))
          (cons 'installed-by (format #f "~a"
                                      (or (entry-value entry 'installed-by)
                                          "mutastructura")))
          (cons 'execution-time (or (entry-value entry 'execution-time) 0))
          (cons 'success (if (eq? (entry-value entry 'success) #f) 0
                             (or (entry-value entry 'success) 1))))))

(define (sql-entry->insert session entry)
  "An INSERT for ENTRY, over the columns the ledger actually has.

`installed-on' is only written when the caller asks for it: leaving it out lets
each engine fill in its own `CURRENT_TIMESTAMP', which is the one value that is
definitely in that engine's format."
  (let* ((engine (sql-session-engine session))
         (available (sql-session-columns session))
         (pairs (filter (lambda (pair)
                          (and (memq (car pair) available)
                               ;; An absent value is not the same as NULL: it
                               ;; means "let the column's default decide".
                               (or (not (memq (car pair)
                                              '(installed-on installed-by)))
                                   (cdr pair))))
                        (ledger-fields entry))))
    (format #f "INSERT INTO ~a (~a) VALUES (~a)"
            (sql-session-quoted-table session)
            (string-join (map (lambda (pair)
                                (symbol->sql-column (car pair)))
                              pairs)
                         ", ")
            (string-join (map (lambda (pair)
                                (entry-column-value engine (car pair)
                                                    (cdr pair)))
                              pairs)
                         ", "))))

(define (sql-entry->update session entry)
  "An UPDATE for ENTRY, addressed by its name."
  (let* ((engine (sql-session-engine session))
         (available (sql-session-columns session))
         (pairs (filter (lambda (pair)
                          (and (memq (car pair) available)
                               (not (eq? (car pair) 'name))
                               (or (not (memq (car pair) '(installed-on
                                                           installed-by)))
                                   (cdr pair))))
                        (ledger-fields entry))))
    (when (null? pairs)
      (raise-migration-error
       (format #f "the ledger ~a has none of the columns mutastructura writes;\n   create it with the current mutastructura, or drop it and let it be recreated"
               (sql-session-table session))))
    (format #f "UPDATE ~a SET ~a WHERE name = ~a"
            (sql-session-quoted-table session)
            (string-join (map (lambda (pair)
                                (format #f "~a = ~a"
                                        (symbol->sql-column (car pair))
                                        (entry-column-value engine
                                                            (car pair)
                                                            (cdr pair))))
                              pairs)
                         ", ")
            (dialect-quote-string engine (entry-value entry 'name)))))

(define (sql-entry-for session name)
  "The ledger row for NAME, or #f."
  (and (sql-table-exists? session)
       (find (lambda (row)
               (equal? (format #f "~a" (or (assq-ref row 'name) "")) name))
             (fetch-sql-rows (sql-session-db session)
                             (format #f "SELECT ~a FROM ~a WHERE name = ~a"
                                     (sql-select-columns session)
                                     (sql-session-quoted-table session)
                                     (dialect-quote-string
                                      (sql-session-engine session)
                                      name))
                             #:engine (sql-session-engine session)
                             #:context "looking up a migration in the ledger"))))

(define (sql-record! session entry)
  "Write ENTRY to the ledger, replacing the row for the same migration.

One row per migration, updated in place: that is what makes a repeatable
migration's re-run visible as a re-run rather than as a second migration, and it
is what the ledger's unique key on `name' enforces."
  (let* ((name (format #f "~a" (entry-value entry 'name)))
         (context (format #f "recording ~a" name)))
    (sql-run-verified! session
                       (if (sql-entry-for session name)
                           (sql-entry->update session entry)
                           (sql-entry->insert session entry))
                       context)))

(define (sql-forget! session name)
  "Remove NAME's row from the ledger."
  (sql-run-verified! session
                     (format #f "DELETE FROM ~a WHERE name = ~a"
                             (sql-session-quoted-table session)
                             (dialect-quote-string (sql-session-engine session)
                                                   name))
                     (format #f "forgetting ~a" name)))

;;; ---------------------------------------------------------------------------
;;; Running a migration
;;; ---------------------------------------------------------------------------

(define (sql-statements file options)
  "The statements of migration FILE, with placeholders substituted.

Nothing is sent anywhere: this is what `--dry-run' reports and what the tests
compare against."
  (let* ((engine (assq-ref options 'engine))
         (text (migration-contents (migration-path file)
                                   #:placeholders (or (assq-ref options
                                                                'placeholders)
                                                      '()))))
    (parse-sql-statements text #:engine engine)))

(define (sql-apply! session file options)
  "Run migration FILE inside SESSION, returning how many statements ran."
  (let* ((engine (sql-session-engine session))
         (db (sql-session-db session))
         (name (migration-name file))
         (dry-run? (assq-ref options 'dry-run?))
         (verbose? (assq-ref options 'verbose?))
         (statements (sql-statements file (append options
                                                   (list (cons 'engine engine)))))
         (total (length statements)))
    (session-log session 'info (format #f "~a: ~a statement~a"
                                       name total
                                       (if (= total 1) "" "s")))
    (unless dry-run?
      (let loop ((rest statements) (n 0))
        (unless (null? rest)
          (let* ((statement (car rest))
                 (sql (assq-ref statement 'sql))
                 (line (assq-ref statement 'line))
                 (context (format #f "~a, line ~a" name (or line 0))))
            ;; One line per statement, so a default run says what it did; the
            ;; whole statement, multi-line bodies included, is what --verbose
            ;; adds.
            (session-log session 'info
                         (format #f "  [~a/~a] ~a" (+ n 1) total
                                 (first-line sql)))
            (when verbose? (session-log session 'debug sql))
            (sql-run-verified! session sql context)
            (loop (cdr rest) (+ n 1))))))
    total))

;;; ---------------------------------------------------------------------------
;;; Transactions, locking and clean
;;; ---------------------------------------------------------------------------

(define (sql-begin! session)
  "Open a transaction where the engine can honour one, and take the migration lock.

The transaction is opened first on purpose.  On PostgreSQL a refused statement
is only visible from inside a transaction (see `dialect-verification-query'),
and the lock is taken there so that a lock statement the server refused aborts
the transaction instead of passing for a lock that was taken."
  (let ((engine (sql-session-engine session))
        (db (sql-session-db session)))
    (when (dialect-transactional-ddl? engine)
      (execute-sql! db (dialect-begin-statement engine)
                    #:context "begin transaction")
      (sql-verify! session "opening the transaction"))
    (sql-take-lock! session)
    (set-sql-session-locked?! session #t)))

(define (sql-take-lock! session)
  "Take the migration lock, or say why it could not be taken.

SQLite and CockroachDB have no lock statement -- on SQLite the transaction is
the lock -- and are done before anything is asked.  MySQL's `GET_LOCK' answers 1
when the lock is held, 0 when it waited ten seconds and gave up, and NULL when
it failed: a driver that reports only whether the statement was *sent* would
read all three as success, which is a lock that is not held at all.  PostgreSQL's
`pg_advisory_lock' blocks until it can grant the lock, so being accepted is the
whole of what there is to know about it -- and it is taken inside the
transaction, where being refused is visible."
  (let* ((engine (sql-session-engine session))
         (db (sql-session-db session))
         (statements (dialect-lock-statements engine (sql-session-table session))))
    (cond
      ((null? statements) #t)
      ((mysql-engine? engine)
       (let ((rows (fetch-sql-rows db (car statements)
                                   #:engine engine
                                   #:context "taking the migration lock")))
         (unless (lock-granted? rows)
           (raise-migration-error
            (format #f "could not take the migration lock ~a: another run is holding it (MySQL waits ten seconds and then answers 0)"
                    (dialect-lock-name (sql-session-table session)))))))
      (else
       (for-each (lambda (statement)
                   (execute-sql! db statement
                                 #:context "taking the migration lock"))
                 statements)
       (sql-verify! session "taking the migration lock")))))

(define (lock-granted? rows)
  "Whether MySQL answered `GET_LOCK' with the row that means it granted the lock.

ROWS is what `fetch-sql-rows' answers with -- rows, each an alist of column to
value -- so the answer is the first column of the first row, and not the rest of
that row's alist.  It is a 1 whatever the driver unpacked the column as:
guile-dbd-mysql hands back a number for a `LONGLONG' column and, with a type
mapping of its own, a string of one, and both mean the same thing."
  (let ((value (and (pair? rows)
                    (pair? (car rows))
                    (cdr (car (car rows))))))
    (or (equal? value 1) (equal? value "1"))))

(define (sql-commit! session)
  "Commit when there is a transaction to commit, then release the lock."
  (let ((engine (sql-session-engine session))
        (db (sql-session-db session)))
    (when (dialect-transactional-ddl? engine)
      ;; Everything the run did -- the migrations and the ledger rows -- is in
      ;; this transaction, so one question answers for all of it before the
      ;; commit that makes it permanent.
      (sql-verify! session "committing the run")
      (execute-sql! db (dialect-commit-statement engine)
                    #:context "commit transaction"))
    (release-lock! session)))

(define (sql-rollback! session)
  "Roll back when there is a transaction to roll back, then release the lock.

The lock is released even when the rollback itself fails: holding it would block
every later run, turning one bad migration into a stuck pipeline."
  (let ((engine (sql-session-engine session))
        (db (sql-session-db session)))
    (when (dialect-transactional-ddl? engine)
      (catch #t
             (lambda ()
               (execute-sql! db (dialect-rollback-statement engine)
                             #:context "rollback transaction"))
             (lambda _ #f)))
    (release-lock! session)))

(define (release-lock! session)
  "Release the migration lock, if this session took one."
  (when (sql-session-locked? session)
    (let ((engine (sql-session-engine session))
          (db (sql-session-db session)))
      (for-each (lambda (statement)
                  (catch #t
                         (lambda ()
                           (execute-sql! db statement
                                         #:context "releasing the migration lock"))
                         (lambda _ #f)))
                (dialect-unlock-statements engine (sql-session-table session)))
      (set-sql-session-locked?! session #f))))

(define (sql-droppable-objects session)
  "What the catalogue reports as droppable, as `(type . name)' pairs."
  (filter-map (lambda (row)
                (let ((type (or (assq-ref row 'object-type)
                                (assq-ref row 'table-type)))
                      (name (or (assq-ref row 'object-name)
                                (assq-ref row 'name))))
                  (and type name
                       (cons (format #f "~a" type) (format #f "~a" name)))))
              (fetch-sql-rows (sql-session-db session)
                              (dialect-clean-catalog-query
                               (sql-session-engine session))
                              #:engine (sql-session-engine session)
                              #:context "listing the objects to drop")))

(define (sql-clean! session)
  "Drop every object the catalogue reports, the ledger included.

`--dry-run' turns this into a report: every statement is named and nothing is
run.  Because the PostgreSQL driver cannot report a refused statement, what was
dropped is followed by the one question that can be asked about it -- is
anything left? -- rather than a promise that the database is now empty."
  (let* ((engine (sql-session-engine session))
         (db (sql-session-db session))
         (objects (sql-droppable-objects session))
         (statements (filter-map (lambda (object)
                                   (dialect-drop-statement engine
                                                           (car object)
                                                           (cdr object)))
                                 objects)))
    (for-each (lambda (statement)
                (session-log session 'info (format #f "  ~a" statement))
                (unless (session-dry-run? session)
                  (execute-sql! db statement #:context "dropping an object")))
              statements)
    (unless (session-dry-run? session)
      (let ((left (sql-droppable-objects session)))
        (unless (null? left)
          (session-log session 'warn
                       (format #f "~a object~a could not be dropped and ~a still there"
                               (length left)
                               (if (= 1 (length left)) "" "s")
                               (if (= 1 (length left)) "is" "are")))
          (for-each (lambda (object)
                      (session-log session 'warn
                                   (format #f "  ~a ~a" (car object) (cdr object))))
                    left))))
    (length statements)))

;;; ---------------------------------------------------------------------------
;;; Opening and closing
;;; ---------------------------------------------------------------------------

(define* (sql-open-session spec #:key (options '()))
  "Open a SQL session for SPEC, with the ledger created unless asked not to.

`(ensure-history? . #f)' is what `status', `clean' and `--dry-run' pass: they
must be able to look at a database without writing to it, and a ledger made for
a report is a ledger made for nothing.  The columns are discovered only when
there is a ledger to discover them in."
  (let* ((engine (connection-spec-engine spec))
         (options* (append options (list (cons 'engine engine))))
         (table (or (assq-ref options 'history-table)
                    (engine-default-history-name engine)))
         (db (open-database engine (connection-spec-connection spec))))
    (let ((session (make-sql-session engine spec db table '() options* #f)))
      (when (session-option session 'ensure-history? #t)
        (sql-ensure-history! session))
      (make-sql-session engine spec db table
                        (and (sql-table-exists? session)
                             (sql-history-columns session))
                        options* #f))))

(define (sql-close! session)
  (close-database (sql-session-db session)))

;;; ---------------------------------------------------------------------------
;;; The vtable
;;; ---------------------------------------------------------------------------

(define (sql-runner)
  "The runner for the relational engines."
  (make-runner
   ;; The vtable is family-wide: which engine a session is talking to comes from
   ;; the connection it was opened with, not from here.
   'relational
   "SQL over guile-dbi (SQLite3, PostgreSQL, MySQL/MariaDB)"
   (lambda (spec options)
     (sql-open-session spec #:options options))
   (lambda (session) (sql-close! session))
   (lambda (session) (sql-history session))
   (lambda (session) (sql-ensure-history! session))
   (lambda (session entry) (sql-record! session entry))
   (lambda (session name) (sql-forget! session name))
   (lambda (session file options) (sql-apply! session file options))
   (lambda (session) (sql-begin! session))
   (lambda (session) (sql-commit! session))
   (lambda (session) (sql-rollback! session))
   (lambda (session) (sql-clean! session))
   (lambda (file options) (sql-statements file options))
   (lambda (session) (dialect-transactional-ddl? (sql-session-engine session)))))
