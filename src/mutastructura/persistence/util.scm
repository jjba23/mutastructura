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
                                      string-split-to-queries
                                      value->sql-literal
                                      pair->sql-assignment
                                      run-migration-file
                                      with-transaction
                                      run-stmt))

;;; --- Core DB Execution ---

(define (run-stmt db)
  (lambda (stmt)
    (format #t "\n\n~a" stmt)
    (dbi-query db stmt)
    (let* ((st (dbi-get_status db)))
      (cond
        ((eq? 0
              (car st))
         #t)
        (else (raise-exception (make-exception-with-message (format #f "~a"
                                                                    (cdr st)))))))))

(define (with-transaction db thunk)
  (with-exception-handler (lambda (ex)
                            (format #t
                             "
[ERROR] MIGRATION FAILED, ROLL BACK CHANGES...
~a
" ex)
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
                                 (format #t
                                  "

[SUCCESS] COMMIT TRANSACTION SUCCESSFULLY
")
                                 #t)
                                (else (format #t "\n\n[ERROR] ~a\n"
                                              (cdr st))
                                      (raise-exception (make-exception-with-message
                                                        (format #f "~a"
                                                                (cdr st))))))))))

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

;;; --- SQL Parser ---

(define (string-split-to-queries sql-string)
  (let loop
    ((chars (string->list sql-string))
     (state 'normal)
     (current-acc '())
     (queries '()))

    (if (null? chars)
        (let ((final-query (string-trim (list->string (reverse current-acc)))))
          (reverse (if (whitespace-only? final-query) queries
                       (cons final-query queries))))

        (let ((c (car chars))
              (rest (cdr chars)))
          (case state
            
            ((normal)
             (cond
               ((char=? c #\')
                (loop rest
                      'string
                      (cons c current-acc) queries))
               ((char=? c #\")
                (loop rest
                      'identifier
                      (cons c current-acc) queries))
               ((char=? c #\;)
                (let ((query (string-trim (list->string (reverse current-acc)))))
                  (loop rest
                        'normal
                        '()
                        (if (whitespace-only? query) queries
                            (cons query queries)))))
               ;; Check for --
               ((and (char=? c #\-)
                     (not (null? rest))
                     (char=? (car rest) #\-))
                (loop (cdr rest)
                      'line-comment
                      (cons #\-
                            (cons #\- current-acc)) queries))
               ;; Check for /*
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

            ((string)
             ;; Handle escaped quotes ('') inside strings!
             (if (and (char=? c #\')
                      (not (null? rest))
                      (char=? (car rest) #\'))
                 (loop (cdr rest)
                       'string
                       (cons #\'
                             (cons #\' current-acc)) queries)
                 (if (char=? c #\')
                     (loop rest
                           'normal
                           (cons c current-acc) queries)
                     (loop rest
                           'string
                           (cons c current-acc) queries))))

            ((identifier)
             ;; Handle escaped double quotes ("") inside identifiers!
             (if (and (char=? c #\")
                      (not (null? rest))
                      (char=? (car rest) #\"))
                 (loop (cdr rest)
                       'identifier
                       (cons #\"
                             (cons #\" current-acc)) queries)
                 (if (char=? c #\")
                     (loop rest
                           'normal
                           (cons c current-acc) queries)
                     (loop rest
                           'identifier
                           (cons c current-acc) queries))))

            ((line-comment)
             (if (char=? c #\newline)
                 (loop rest
                       'normal
                       (cons c current-acc) queries)
                 (loop rest
                       'line-comment
                       (cons c current-acc) queries)))

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

;;; --- Migration Runner ---

(define (run-migration-file db)
  (lambda (path)
    (let* ((is-repeatable (string-suffix? ".repeatable.sql" path))
           (migration-sha (file-sha256 path))
           (contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (format #t "\n>>= MIGRATION: ~a\nSHA256: ~a" path migration-sha)
      (for-each (run-stmt db) statements)
      (unless is-repeatable
        ((run-stmt db)
         (format #f
          "INSERT INTO migrations_history (name, checksum) VALUES (~a, ~a)"
          (value->sql-literal (basename path))
          (value->sql-literal migration-sha)))))))
