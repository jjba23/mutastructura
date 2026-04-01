;;; MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

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
                                      string-split-to-queries
                                      value->sql-literal
                                      pair->sql-assignment
                                      run-migration-file
                                      with-transaction
                                      run-stmt))

(define (run-stmt db)
  (lambda (stmt)
    (display (format #f "\n\n~a" stmt))
    (dbi-query db stmt)))

(define (with-transaction db thunk)
  (with-exception-handler (lambda (ex)
                            (display
                             "
[ERROR] MIGRATION FAILED, ROLL BACK CHANGES...
")
                            (display ex)
                            (newline)
                            (dbi-query db "ROLLBACK")
                            (raise-exception ex))
                          (lambda ()
                            (dbi-query db "BEGIN")
                            (thunk)
                            (let* ((st (dbi-get_status db)))
                              (cond
                                ((eq? 0
                                      (car st))
                                 (dbi-query db "COMMIT")
                                 (display
                                  "

[SUCCESS] COMMIT TRANSACTION SUCCESSFULLY")
                                 #t)
                                (else (display (format #f "\n\n[ERROR] ~a\n"
                                                       (cdr st)))
                                      (raise-exception (make-exception-with-message
                                                        (format #f "~a"
                                                                (cdr st))))))))))

(define (get-all-rows db query)
  (dbi-query db query)
  (let loop
    ((acc '()))
    (let ((row (dbi-get_row db)))
      (if (not (nil? row))
          (loop (cons (sql-row->scheme-alist row) acc))
          (reverse acc)))))

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
  (cond
    ((alist? xs)
     (map (lambda (pair)
            (cons (sql-column->symbol (car pair))
                  (cdr pair))) xs))
    (else xs)))

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
                  (cdr a)))))

(define (value->sql-literal a)
  (cond
    ((number? a)
     (format #f "~a" a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'" a))))

(define (string-split-to-queries sql-string)
  (let loop
    ((chars (string->list sql-string))
     (state 'normal) ;States: normal, string, identifier, line-comment, block-comment
     (current-acc '()) ;Accumulates characters for the current query
     (queries '())) ;Accumulates finished queries
    
    (if (null? chars)
        ;; Base case: End of string. Clean up and collect the final query.
        (let ((final-query (string-trim (list->string (reverse current-acc)))))
          (reverse (if (whitespace-only? final-query) queries
                       (cons final-query queries))))

        ;; Recursive case: Process the next character based on our current state.
        (let ((c (car chars))
              (rest (cdr chars)))
          (case state
            
            ;; NORMAL STATE
            ((normal)
             (cond
               ((char=? c #\')
                ;; Enter single-quote string literal
                (loop rest
                      'string
                      (cons c current-acc) queries))
               ((char=? c #\")
                ;; Enter double-quote identifier
                (loop rest
                      'identifier
                      (cons c current-acc) queries))
               ((char=? c #\;)
                ;; Query finished! (Because we are in 'normal' state)
                (let ((query (string-trim (list->string (reverse current-acc)))))
                  (loop rest
                        'normal
                        '()
                        (if (whitespace-only? query) queries
                            (cons query queries)))))
               ;; Check for start of line comment: --
               ((and (char=? c #\-)
                     (not (null? rest))
                     (char=? (car rest) #\-))
                (loop (cdr rest)
                      'line-comment
                      (cons #\-
                            (cons #\- current-acc)) queries))
               ;; Check for start of block comment: /*
               ((and (char=? c #\/)
                     (not (null? rest))
                     (char=? (car rest) #\*))
                (loop (cdr rest)
                      'block-comment
                      (cons #\*
                            (cons #\/ current-acc)) queries))
               (else (loop rest
                           'normal
                           (cons c current-acc) queries))))

            ;; INSIDE STRING LITERAL ('...')
            ((string)
             (if (char=? c #\')
                 (loop rest
                       'normal
                       (cons c current-acc) queries)
                 (loop rest
                       'string
                       (cons c current-acc) queries)))

            ;; INSIDE IDENTIFIER ("...")
            ((identifier)
             (if (char=? c #\")
                 (loop rest
                       'normal
                       (cons c current-acc) queries)
                 (loop rest
                       'identifier
                       (cons c current-acc) queries)))

            ;; INSIDE LINE COMMENT (-- ...)
            ((line-comment)
             (if (char=? c #\newline)
                 (loop rest
                       'normal
                       (cons c current-acc) queries)
                 (loop rest
                       'line-comment
                       (cons c current-acc) queries)))

            ;; INSIDE BLOCK COMMENT (/* ... */)
            ((block-comment)
             (if (and (char=? c #\*)
                      (not (null? rest))
                      (char=? (car rest) #\/))
                 (loop (cdr rest)
                       'normal
                       (cons #\/
                             (cons #\* current-acc)) queries)
                 (loop rest
                       'block-comment
                       (cons c current-acc) queries))))))))

(define (run-migration-file db)
  (lambda (path)
    (let* ((migration-sha (file-sha256 path))
           (contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (display (format #f "\n>>= MIGRATION: ~a" path))
      (display (format #f "\nSHA256: ~a" migration-sha))
      (for-each (run-stmt db) statements)
      ((run-stmt db)
       (format #f
        "INSERT INTO migrations_history (name, checksum) VALUES (~a, ~a)"
        (value->sql-literal (basename path))
        (value->sql-literal migration-sha))))))
