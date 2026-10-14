;;; mutastructura - the CI/CD orchestrator written in Guile Scheme

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

(define-module (mutastructura persistence util)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (dbi dbi)
  #:use-module (mutastructura mutastructura)
  #:export (symbols->sql-columns-list sql-row->scheme-alist
                                      symbol->sql-column
                                      sql-column->symbol
                                      alist?
                                      get-all-rows
                                      value->sql-literal
                                      pair->sql-assignment
                                      statement-status
                                      status-unreported?
                                      run-stmt
                                      with-transaction))

;;; --- Reading the driver's answer ---

;; guile-dbi answers a statement with a pair: `(0 . "query ok")' when it went
;; through, and `(1 . "<the message SQLite gave>')' when it did not.  A driver
;; that cannot report on the handle at all answers with a message of guile-dbi's
;; own -- `invalid module handle', or `file not found' when it was opened against
;; something it did not bind -- rather than with anything SQLite said.  Those two
;; are not the statement failing: `dbi-query' is what runs a statement and what
;; raises when the connection is unusable, and the statements in a run like that
;; are applied all the same.  So the three readings are kept apart, and only the
;; third -- code 1 with a message the database itself wrote -- fails a migration.
;;
;; This used to be read with `(eq? 0 (car st))' and `(eq? '(0 . "query ok"))':
;; the second can never hold (`eq?' on a freshly made list is #f), and the fall
;; through to a raise turned every answer it did not recognise -- including a
;; driver that could not answer -- into a failed migration, which rolled back a
;; schema that had in fact been applied.
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

;;; --- DB Execution ---

(define (run-stmt db)
  (lambda (stmt)
    (format #t "\n\n~a" stmt)
    (dbi-query db stmt)
    (let* ((reading (statement-status (dbi-get_status db))))
      (cond
        ;; A driver that cannot report is said so once, where the connection is
        ;; opened, rather than once per statement; the statements themselves are
        ;; applied either way.
        ((or (eq? reading
                  'ok)
             (eq? reading
                  'unreported))
         #t)
        (else (raise-exception (make-exception-with-message (format #f "~a"
                                                                    (cdr
                                                                     reading)))))))))

(define (with-transaction db thunk)
  (let* ((on-success (lambda (db)
                       (dbi-query db "COMMIT")
                       (format #t "

[SUCCESS] COMMIT TRANSACTION SUCCESSFULLY
")
                       #t))
         (on-error (lambda (st)
                     (format #t "\n\n[ERROR] ~a\n"
                             (cdr st))
                     (raise-exception (make-exception-with-message (format #f
                                                                    "~a"
                                                                    (cdr st)))))))
    (with-exception-handler (lambda (ex)
                              (format #t
                               "
[ERROR] MIGRATION FAILED, ROLL BACK CHANGES...
~a
"
                               ex)
                              (dbi-query db "ROLLBACK")
                              (raise-exception ex))
                            (lambda ()
                              (dbi-query db "BEGIN")
                              (thunk)
                              (let* ((reading (statement-status (dbi-get_status
                                                                 db))))
                                (cond
                                  ((eq? reading
                                        'ok)
                                   (on-success db))
                                  ((eq? reading
                                        'unreported)
                                   (on-success db))
                                  (else (on-error reading))))))))

(define (get-all-rows db query)
  (dbi-query db query)
  (let loop
    ((acc '()))
    (let ((row (dbi-get_row db)))
      (if (pair? row) ;Safely checks for valid row data
          (loop (cons (sql-row->scheme-alist row) acc))
          (reverse acc)))))

;;; --- SQL Generation & Escaping ---

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
  (if (alist? xs)
      (map (lambda (pair)
             (cons (sql-column->symbol (car pair))
                   (cdr pair))) xs) xs))

(define (escape-sql-string str)
  "Helper to prevent SQL injection and syntax errors"
  (string-replace-substring str "'" "''"))

(define (pair->sql-assignment a)
  (cond
    ((number? (cdr a))
     (format #f "~a = ~a"
             (symbol->sql-column (car a))
             (cdr a)))
    ((eq? #t
          (cdr a))
     (format #f "~a = 1"
             (symbol->sql-column (car a))))
    ((eq? #f
          (cdr a))
     (format #f "~a = 0"
             (symbol->sql-column (car a))))
    (else (format #f "~a = '~a'"
                  (symbol->sql-column (car a))
                  (escape-sql-string (format #f "~a"
                                             (cdr a)))))))

(define (value->sql-literal a)
  (cond
    ((number? a)
     (number->string a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'"
                  (escape-sql-string (format #f "~a" a))))))

