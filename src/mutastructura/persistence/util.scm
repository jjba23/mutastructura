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
;; Everything that touches guile-dbi lives here, in one place, so the way a
;; driver's answer is read is decided once rather than at every call site.
;;
;; guile-dbi answers a statement with a pair: `(0 . "query ok")' when it went
;; through, and `(1 . "<the message the database gave>")' when it did not.  A
;; driver that cannot report on the handle at all answers with a message of its
;; own -- `invalid module handle', or `file not found' -- rather than with
;; anything the database said.  Those two are not the statement failing:
;; `dbi-query' is what runs a statement and what raises when the connection is
;; unusable, so the three readings are kept apart and only the third -- code 1
;; with a message the database itself wrote -- fails a statement.
;;
;; This used to be read with `(eq? 0 (car st))' and `(eq? '(0 . "query ok"))':
;; the second can never hold (`eq?' on a freshly made list is #f), and the fall
;; through to a raise turned every answer it did not recognise -- including a
;; driver that could not answer -- into a failed migration, which rolled back a
;; schema that had in fact been applied.

(define-module (mutastructura persistence util)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (dbi dbi)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence dialect)
  #:export (status-unreported?
            statement-status
            statement-failed?
            driver-message
            open-database
            database-open?
            close-database
            execute-sql!
            fetch-sql-rows
            run-stmt
            with-transaction
            get-all-rows
            sql-row->scheme-alist
            symbols->sql-columns-list
            symbol->sql-column
            sql-column->symbol
            alist?
            value->sql-literal
            pair->sql-assignment
            migration-exception
            migration-exception?
            migration-exception-message
            raise-migration-error
            error-message-of))

;;; ---------------------------------------------------------------------------
;;; Reading the driver's answer
;;; ---------------------------------------------------------------------------

(define (status-unreported? message)
  "Whether MESSAGE is guile-dbi's own, rather than the database's."
  (and (string? message)
       (or (string=? message "invalid module handle")
           (string=? message "file not found"))))

(define (statement-status status)
  "What STATUS -- the driver's reading of the last statement -- says happened.

The answer is `ok', `unreported' for a status the driver cannot give, or the
pair `(failed . MESSAGE)' with what the database said."
  (cond
    ((not (pair? status))
     (cons 'failed
           (format #f "the driver answered with ~s" status)))
    ((and (number? (car status))
          (zero? (car status)))
     'ok)
    ((equal? (cdr status) "not an error")
     'ok)
    ((status-unreported? (cdr status))
     'unreported)
    (else (cons 'failed
                (cdr status)))))

(define (statement-failed? reading)
  "Whether READING, from `statement-status', is a failure."
  (and (pair? reading)
       (eq? (car reading) 'failed)))

(define (driver-message reading)
  "The database's own words for READING, or a description of the answer."
  (if (pair? reading)
      (format #f "~a" (cdr reading))
      (format #f "~a" reading)))

;;; ---------------------------------------------------------------------------
;;; Errors
;;; ---------------------------------------------------------------------------

(define-record-type <migration-exception>
  (make-migration-exception message irritants)
  migration-exception?
  (message migration-exception-message)
  (irritants migration-exception-irritants))

(define (error-message-of ex)
  "The message inside EX, whether it is ours or a Guile condition.

`exception-message' rather than `condition-message': the latter is SRFI-35's and
is not what `(ice-9 exceptions)' binds, so calling it here was an unbound
variable waiting for the first exception that was not one of ours -- which is
exactly the exception the command line most needs to print."
  (cond
    ((migration-exception? ex)
     (migration-exception-message ex))
    ((exception-with-message? ex)
     (exception-message ex))
    (else
     (format #f "~a" (or (catch #t
                                 (lambda () (exception-message ex))
                                 (lambda _ #f))
                          ex)))))

(define (raise-migration-error message . irritants)
  "Raise a migration error carrying MESSAGE."
  (raise-exception (make-migration-exception message irritants)))

;;; ---------------------------------------------------------------------------
;;; Connections
;;; ---------------------------------------------------------------------------

(define* (open-database engine connection #:key (context #f))
  "Open a guile-dbi connection to ENGINE, or explain why not.

`dbi-open' reports a failed connection in the handle's status instead of
raising, so a run against an unreachable server used to continue and then fail
somewhere far away with `invalid module handle'.  Checking here means the
message names the host and the reason."
  (let ((db (dbi-open (parse-db-type (engine-id engine)) connection)))
    (let ((reading (statement-status (dbi-get_status db))))
      (cond
        ((eq? reading 'ok)
         db)
        ((eq? reading 'unreported)
         ;; A driver that cannot report is still usable; the caller is told once
         ;; by `database-open?', and `execute-sql!' will catch real failures.
         db)
        (else
         (catch #t
                (lambda () (close-database db))
                (lambda _ #f))
         (raise-migration-error
          (format #f "could not connect to ~a: ~a~a"
                  (engine-id engine)
                  (driver-message reading)
                  (or-empty (and context (format #f " (~a)" context))))))))))

(define (database-open? db)
  "Whether DB's driver is willing to report on the connection at all."
  (eq? (statement-status (dbi-get_status db)) 'ok))

;; Closed connections are kept here, rather than left to be collected.
;;
;; guile-dbi frees a database handle from the smob's own free function, and that
;; path calls the same C routine `dbi-close' does -- which asserts that its
;; argument is a smob, on a smob the collector has already moved on from.  The
;; result is an exception raised inside the garbage collector: harmless in a
;; program that exits after migrating, and fatal in one that does not, which is
;; how a test suite that opens a connection per case ends.  A finalizer cannot be
;; unregistered from Scheme, so the handle is held instead.  It costs one small
;; smob per closed connection, and it is the only thing that keeps a collected
;; connection from being the thing that ends the program.
(define closed-handles '())

(define (close-database db)
  "Close DB, doing nothing if it is already closed.

Closing twice is normal: the failure path closes and so does the exit path, and
a driver that answers `invalid module handle' to the second call must not turn a
successful migration into a failed run."
  (catch #t
         (lambda () (dbi-close db))
         (lambda _ #f))
  (set! closed-handles (cons db closed-handles)))

;;; ---------------------------------------------------------------------------
;;; Statements
;;; ---------------------------------------------------------------------------

(define* (execute-sql! db sql #:key (context #f))
  "Run SQL on DB, raising with CONTEXT's help if the database refused it."
  (catch #t
         (lambda () (dbi-query db sql))
         (lambda (key . args)
           (raise-migration-error
            (format #f "~athe driver raised ~a~a"
                    (or-empty (and context (format #f "~a: " context)))
                    key
                    (if (pair? args)
                        (catch #t
                               (lambda () (error-message-of (car args)))
                               (lambda _ (format #f " (~a)" args)))
                        "")))))
  (let ((reading (statement-status (dbi-get_status db))))
    (cond
      ((eq? reading 'ok) #t)
      ((eq? reading 'unreported) #t)
      (else
       (raise-migration-error
        (format #f "~a~a" (or-empty (and context (format #f "~a: " context)))
                (driver-message reading)))))))

(define* (fetch-sql-rows db sql #:key (context #f) (engine #f))
  "Run SQL on DB, returning every row as an alist.

A query that fails raises here rather than returning no rows: an empty answer
and a refusal are very different things, and the planner's decisions -- is the
ledger there? is the table there? -- depend on the difference.

ENGINE, when given, is what decides whether the status is read a second time,
after the rows.  A driver that can abandon a row it cannot unpack -- PostgreSQL's
does, for a column type it does not know -- ends the reading loop without a
word, and the answer looks like a table with nothing in it.  The check is asked
only of those engines (`dialect-may-abandon-rows?'): SQLite's driver ends a
finished read with a status of its own, and a check that took that for a failure
would break it."
  (execute-sql! db sql #:context context)
  (let ((rows (let loop ((acc '()))
                (let ((row (dbi-get_row db)))
                  (if (pair? row)
                      (loop (cons (sql-row->scheme-alist row) acc))
                      (reverse acc))))))
    (when (and engine (dialect-may-abandon-rows? engine))
      (let ((reading (statement-status (dbi-get_status db))))
        (when (statement-failed? reading)
          (raise-migration-error
           (format #f "~a~a" (or-empty (and context (format #f "~a: " context)))
                   (driver-message reading))))))
    rows))

(define (run-stmt db)
  "Return a procedure that runs one statement on DB."
  (lambda (stmt)
    (execute-sql! db stmt #:context "migration statement")
    (format #t "~a" stmt)))

(define* (with-transaction db thunk #:key (engine #f) (on-success #f) (on-error #f))
  "Run THUNK inside a transaction on DB.

The transaction's dialect comes from ENGINE: SQLite opens with `BEGIN IMMEDIATE'
so that the write lock is taken before the first read of the ledger, which is
what makes two concurrent runs safe rather than merely unlikely to collide."
  (let* ((begin (if engine (dialect-begin-statement engine) "BEGIN"))
         (commit (if engine (dialect-commit-statement engine) "COMMIT"))
         (rollback (if engine (dialect-rollback-statement engine) "ROLLBACK")))
    (execute-sql! db begin #:context "begin transaction")
    (with-exception-handler
     (lambda (ex)
       (catch #t
              (lambda () (execute-sql! db rollback #:context "rollback"))
              (lambda _ #f))
       (raise-exception ex))
     (lambda ()
       (let ((result (thunk)))
         (execute-sql! db commit #:context "commit transaction")
         (when on-success (on-success result))
         result)))))

(define (get-all-rows db query)
  "Every row of QUERY as an alist of symbol to value.

Kept for callers of the original API; new code should prefer `fetch-sql-rows',
which also reports a query that failed."
  (catch #t
         (lambda () (fetch-sql-rows db query))
         (lambda _ '())))

;;; ---------------------------------------------------------------------------
;;; Rows
;;; ---------------------------------------------------------------------------

(define (symbol->sql-column x)
  (string-replace-substring (symbol->string x) "-" "_"))

(define (sql-column->symbol x)
  (string->symbol (string-replace-substring x "_" "-")))

(define (symbols->sql-columns-list xs)
  (string-join (map symbol->sql-column xs) ", "))

(define (alist? obj)
  (cond
    ((null? obj)
     #t)
    ((not (pair? obj))
     #f)
    ((not (pair? (car obj)))
     #f)
    (else (alist? (cdr obj)))))

(define (sql-row->scheme-alist xs)
  "A driver row as an alist keyed on symbols.

Drivers disagree about the key type: sqlite3 hands back symbols, PostgreSQL and
MySQL strings.  Normalising here means the rest of the program has one shape to
read."
  (if (alist? xs)
      (map (lambda (pair)
             (cons (if (symbol? (car pair))
                       (sql-column->symbol (symbol->string (car pair)))
                       (sql-column->symbol (format #f "~a" (car pair))))
                   (cdr pair)))
           xs)
      xs))

;;; ---------------------------------------------------------------------------
;;; Literals
;;; ---------------------------------------------------------------------------

;; Kept because they are exported and used by callers of the library API; the
;; engine-aware `dialect-literal' is what the runner itself uses.
(define (escape-sql-string str)
  "A string with its single quotes doubled."
  (string-replace-substring str "'" "''"))

(define (pair->sql-assignment a)
  "An `column = value' assignment for a pair A."
  (cond
    ((number? (cdr a))
     (format #f "~a = ~a" (symbol->sql-column (car a)) (cdr a)))
    ((eq? #t (cdr a))
     (format #f "~a = 1" (symbol->sql-column (car a))))
    ((eq? #f (cdr a))
     (format #f "~a = 0" (symbol->sql-column (car a))))
    (else (format #f "~a = '~a'"
                  (symbol->sql-column (car a))
                  (escape-sql-string (format #f "~a" (cdr a)))))))

(define (value->sql-literal a)
  "A as a SQL literal, with strings escaped."
  (cond
    ((number? a)
     (number->string a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    ((null? a)
     "NULL")
    (else (format #f "'~a'"
                  (escape-sql-string (format #f "~a" a))))))
