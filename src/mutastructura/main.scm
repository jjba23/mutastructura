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
MUTASTRUCTURA: Schema Migrations tooling powered by Lisp

      🏛️  M U T A S T R U C T U R A  🏛️

     _ _ V E N I • V I D I • L I S P _ _

")

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
         (migrations-history (get-all-rows db
                                           "SELECT * FROM migrations_history"))
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
                                    "~a/application-migrations/~a/mutastructura-init.sql"
                                    resources db-type))))))
         (_ (set! migrations-history
                  (get-all-rows db "SELECT * FROM migrations_history")))
         (migration-files (scandir-for-sql migrations-path))
         (file-checksums (map file-sha256 migration-files))
         (migration-entries (zip migration-files file-checksums))
         (mws (flatten-1 (map (lambda (mh)
                              (map (lambda (me)
                                     (list me mh)) migration-entries))
                            migrations-history)))
         (_ (begin
              (newline)
              (write mws)))
         (xs (map (lambda (e)
                    (newline)
                    (write e)
                    (newline)
                    (display (format #f
                                     "\n\nentry name: ~a, entry checksum: ~a\n"
                                     (filename-from-path (car (car e)))
                                     (cadr (car e))))
                    (display (format #f "\n\nmh name: ~a, mh checksum: ~a\n"
                                     (assq-ref (cadr e)
                                               'name)
                                     (assq-ref (cadr e)
                                               'checksum)))
                    (and (equal? (filename-from-path (car (car e)))
                                 (assq-ref (cadr e)
                                           'name))
                         (equal? (cadr (car e))
                                 (assq-ref (cadr e)
                                           'checksum)))) mws))

         (_ (write xs))
         (to-run-migrations (map car xs)))
    (display (format #f "
----------------------------
LOADING SCHEMA MIGRATIONS:

"))
    (write to-run-migrations)
    (newline)
    (newline)

    (with-transaction db
                      (lambda ()
                        (for-each (run-migration-file db) to-run-migrations)
                        (display "
---------------------------------
MIGRATIONS COMPLETED SUCCESSFULLY")))

    (display "\nCLOSING DATABASE...")
    (dbi-close db)))

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
        (value->sql-literal (filename-from-path path))
        (value->sql-literal migration-sha))))))

(define (parse-db-type database)
  (match database
    ('postgresql "postgresql")
    ('mysql "mysql")
    ('sqlite3 "sqlite3")
    (_ (raise-exception (make-exception-with-message (format #f
                                                      "UNKNOWN DATABASE TYPE: ~a"
                                                      database))))))
