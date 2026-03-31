;;; Mutastructura

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

(define-module (mutastructura main)
  #:declarative? #f
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (dbi dbi)
  #:use-module (ice-9 ftw)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (main))

(define* (main #:key (args (command-line)))
  (display "
---------------------------------------------------------
Mutastructura: Migrations tool")

  (let* ((option-spec '((resources (single-char #\r)
                                   (value #t))
                        (migrations (single-char #\m)
                                    (value #t)
                                    (required? #t))
                        (connection (single-char #\c)
                                    (value #t)
                                    (required? #t))
                        (database (single-char #\d)
                                  (value #t)
                                  (required? #t))))
         (options (getopt-long args option-spec))
         (migrations-path (option-ref options
                                      'migrations #f))
         (connection (option-ref options
                                 'connection #f))
         (database (string->symbol (option-ref options
                                               'database #f)))
         (resources (option-ref options
                                'resources #f))
         (db-type (parse-db-type database))
         (db (dbi-open db-type connection))
         (migrations-history (get-migrations-history db))
         (_ (unless migrations-history
              ((run-migration-file db)
               "./resources/application-migrations/sqlite3/0-init.sql")))
         (migration-files (scandir-for-sql migrations-path)))
    (display (format #f "
    migrations-history: ~a
    loading migrations: ~a
" migrations-history
                     migration-files))
    (with-transaction db
                      (lambda ()
                        (for-each (run-migration-file db) migration-files)))
    (display "\nMigrations completed successfully")
    (display "\nClosing database...")
    (dbi-close db)))

(define (get-migrations-history db)
  (get-all-rows db "SELECT * FROM migrations_history"))

(define (with-transaction db thunk)
  (dbi-query db "BEGIN")
  (thunk)
  (dbi-query db "COMMIT"))

(define (run-migration-file db)
  (lambda (path)
    (let* ((migration-sha (file-sha256 path))
           (contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (display (format #f "\nmigration file: ~a" path))
      (display (format #f "\nmigration sha256: ~a" migration-sha))
      (for-each (run-statement-on-db db) statements))))

(define (run-statement-on-db db)
  (lambda (stmt)
    (display (format #f "\n\n~a" stmt))
    (dbi-query db stmt)
    (display (format #f "\n~a" db))))

(define (whitespace-only? str)
  (string-every char-whitespace? str))

(define (scandir-for-sql path)
  (map (lambda (file)
         (format #f "~a/~a" path file))
       (scandir path
                (lambda (file)
                  (string-suffix? ".sql" file)))))

(define (parse-db-type database)
  (match database
    ('postgresql "postgresql")
    ('mysql "mysql")
    ('sqlite3 "sqlite3")
    (_ (raise-exception (make-exception-with-message (format #f
                                                      "Unknown database type: ~a"
                                                      database))))))

(define (get-all-rows db query)
  (let* ((result #f))
    (dbi-query db query)
    (set! result
          (dbi-get_row db))
    (while (not (equal? result #f))
           (display result)
           (newline)
           (set! result
                 (dbi-get_row db))) result))

(define (file-sha256 path)
  "Computes the SHA-256 of a file and returns a lowercase hexadecimal string."
  (call-with-input-file path
    (lambda (port)
      (let ((hash-bytevector (port-sha256 port)))
        (string-downcase (bytevector->base16-string hash-bytevector))))))

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

