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
  #:use-module (ice-9 match)
  #:use-module (dbi dbi)
  #:use-module (ice-9 ftw)
  #:export (main))

(define (whitespace-only? str)
  (string-every char-whitespace? str))

(define* (main #:key (args (command-line)))
  
  (let* ((option-spec '((resources (single-char #\r)
                                   (value #t))
                        (migrations (single-char #\m)
                                    (value #t)
                                    (required? #t))
                        (connection (single-char #\c)
                                    (value #t)
                                    (required? #t))))
         (options (getopt-long args option-spec))
         (migrations (option-ref options
                                 'migrations #f))
         (connection (option-ref options
                                 'connection #f))
         (resources (option-ref options
                                'resources #f))
         (raw-migration-files (scandir migrations
                                       (lambda (file)
                                         (string-suffix? ".sql" file))))
         (migration-files (map (lambda (rm)
                                 (format #f "~a/~a" migrations rm))
                               raw-migration-files))
         (db-obj (dbi-open "sqlite3" connection)))
    (display (format #f
              "
---------------------------------------------------------
Mutastructura: Migrations
    loading migrations: ~a
---------------------------------------------------------
"
              migration-files))
    (for-each (lambda (migration)
                (let* ((contents (call-with-input-file migration
                                   get-string-all))
                       (statements (map string-trim
                                        (string-split contents #\;)))
                       (qs (filter (lambda (s)
                                     (not (whitespace-only? s))) statements)))
                  (for-each (lambda (q)
                              (newline)
                              (newline)
                              (display q)
                              (dbi-query db-obj q)
                              (newline)
                              (display db-obj)) qs))) migration-files)
    (display "\nClosing database...")
    (dbi-close db-obj)

    ))

