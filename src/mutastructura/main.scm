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
  #:declarative? #t
  #:use-module (dbi dbi)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (mutastructura prelude)
  #:use-module (mutastructura persistence util)
  #:use-module (srfi srfi-1)
  #:export (main))

(define* (main #:key (args (command-line)))
  (display "
---------------------------------------------------------
MUTASTRUCTURA: Schema Migrations tooling powered by Lisp")

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
         (_ (begin
              (display "\nMIGRATION HISTORY:\n\n")
              (for-each (lambda (h)
                          (write h)
                          (newline)
                          (newline)) migrations-history)
              (newline)
              (newline)))
         (_ (when (nil? migrations-history)
              (with-transaction db
                                (lambda ()
                                  ((run-migration-file db)
                                   (format #f
                                    "~a/application-migrations/sqlite3/0-init.sql"
                                    resources))))))
         (migration-files (scandir-for-sql migrations-path))
         (file-checksums (map file-sha256 migration-files)))
    (display (format #f "
----------------------------
LOADING SCHEMA MIGRATIONS:

"))
    (write (zip migration-files file-checksums))
    (newline)
    (newline)

    (with-transaction db
                      (lambda ()
                        (for-each (run-migration-file db) migration-files)
                        (display "
---------------------------------
MIGRATIONS COMPLETED SUCCESSFULLY")))

    (display "\nCLOSING DATABASE...")
    (dbi-close db)))

(define (get-migrations-history db)
  (get-all-rows db "SELECT * FROM migrations_history"))

(define (run-migration-file db)
  (lambda (path)
    (let* ((migration-sha (file-sha256 path))
           (contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (display (format #f "\n>>= MIGRATION: ~a" path))
      (display (format #f "\nSHA256: ~a" migration-sha))
      (for-each (run-stmt db) statements))))

(define (parse-db-type database)
  (match database
    ('postgresql "postgresql")
    ('mysql "mysql")
    ('sqlite3 "sqlite3")
    (_ (raise-exception (make-exception-with-message (format #f
                                                      "UNKNOWN DATABASE TYPE: ~a"
                                                      database))))))
