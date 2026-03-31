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
  #:use-module (dbi dbi)
  #:use-module (ice-9 ftw)
  #:export (main))

(define* (main #:key (args (command-line)))
  
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
         (migration-files (scandir-for-sql migrations-path)))
    (display (format #f
              "
---------------------------------------------------------
Mutastructura: Migrations
    loading migrations: ~a
---------------------------------------------------------
"
              migration-files))
    (with-transaction db
                      (lambda ()
                        (for-each (run-migration-file db) migration-files)))
    (display "\nMigrations completed successfully")
    (display "\nClosing database...")
    (dbi-close db)))

(define (with-transaction db thunk)
  (dbi-query db "BEGIN;")
  (thunk)
  (dbi-query db "COMMIT;"))

(define (run-migration-file db)
  (lambda (path)
    (let* ((contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (for-each (run-statement-on-db db) statements))))

(define (run-statement-on-db db)
  (lambda (stmt)
    (display (format #f "\n\n~a" stmt))
    (dbi-query db stmt)
    (display (format #f "\n~a" db))))

(define (whitespace-only? str)
  (string-every char-whitespace? str))

(define (is-not-whitespace-only? s)
  (not (whitespace-only? s)))

(define (string-split-to-queries s)
  (filter is-not-whitespace-only?
          (map string-trim
               (string-split s #\;))))

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
