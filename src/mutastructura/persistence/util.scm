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
                                      with-transaction
                                      run-stmt))

;;; --- Core DB Execution ---

(define (run-stmt db)
  (lambda (stmt)
    (format #t "\n\n~a" stmt)
    (dbi-query db stmt)
    (let* ((st (dbi-get_status db)))
      (format #t "\n\ndbi-get_status st: ~a\n" st)
      (cond
        ((eq? 0
              (car st))
         #t)
        ((eq? '(0 . "query ok"))
         #t)
        ((equal? '(1 . "not an error") st)
         #t)
        (else (raise-exception (make-exception-with-message (format #f "~a"
                                                                    (cdr st)))))))))

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
                              (let* ((st (dbi-get_status db)))
                                (cond
                                  
                                  ((eq? 0
                                        (car st))
                                   (on-success db))
                                  ((eq? '(0 . "query ok"))
                                   (on-success db))
                                  ((equal? '(1 . "not an error") st)
                                   (on-success db))
                                  (else (on-error st))))))))

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

