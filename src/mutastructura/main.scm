;;; MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

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
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence util)
  #:use-module (mutastructura persistence migration-runner)
  #:use-module (mutastructura persistence sql-parser)
  #:use-module (srfi srfi-1)
  #:export (main))

(define* (main #:key (args (command-line)))
  (log-banner)
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
         (db (dbi-open db-type connection)))
    
    ;; --- Initialization Check ---
    (let ((initial-history (get-all-rows db "SELECT * FROM migrations_history")))
      (when (nil? initial-history)
        (with-transaction db
                          (lambda ()
                            ((run-migration-file db)
                             (format #f
                              "~a/application-migrations/~a/mutastructura-init.sql"
                              resources db-type))))))

    ;; -- Fetch Migration History Data & Calculate Run List --
    (let* ((migrations-history (get-all-rows db
                                "SELECT * FROM migrations_history"))
           (migration-files (scandir-for-sql migrations-path))
           (repeatable-migrations (filter (lambda (mf)
                                            (string-suffix? ".repeatable.sql"
                                                            mf))
                                          migration-files))
           (non-repeatable-migrations (filter (lambda (mf)
                                                (not (string-suffix?
                                                      ".repeatable.sql" mf)))
                                              migration-files))
           (file-checksums (map file-sha256 non-repeatable-migrations))
           (migration-entries (zip migration-files file-checksums))
           (to-run-migrations (sort (append (get-unapplied-migrations
                                             migration-entries
                                             migrations-history)
                                            repeatable-migrations) string<?)))
      
      ;; -- Output History Logs --
      (unless (nil? migrations-history)
        (format #t "\nFOUND MIGRATION HISTORY:\n\n")
        (for-each (lambda (h)
                    (write h)
                    (format #t "\n\n")) migrations-history))

      (cond
        ((nil? to-run-migrations)
         (log-nothing-to-do))
        (else (log-migrations-to-run to-run-migrations)
              ;; -- Execute Unapplied Migrations --
              (with-transaction db
                                (lambda ()
                                  (for-each (run-migration-file db)
                                            to-run-migrations)
                                  (log-migrations-successful)))))

      (format #t "\nCLOSING DATABASE...\n\n")
      (dbi-close db))))

;;
;; --- CLI Output ---
;;
(define (log-migrations-to-run xs)
  (format #t "
---------------------------------------------------------
MIGRATIONS TO RUN:

")
  (write xs)
  (format #t "\n\n"))

(define (log-nothing-to-do)
  (format #t "
---------------------------------------------------------
NOTHING TO DO!
"))

(define (log-migrations-successful)
  (format #t "
---------------------------------------------------------
MIGRATIONS COMPLETED SUCCESSFULLY
"))

(define (log-banner)
  (display "
---------------------------------------------------------
MUTASTRUCTURA: Schema Migrations tooling powered by Lisp

      🏛️  M U T A S T R U C T U R A  🏛️

     _ _ V E N I • V I D I • L I S P Y _ _

"))
