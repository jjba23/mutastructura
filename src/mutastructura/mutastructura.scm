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

(define-module (mutastructura mutastructura)
  #:declarative? #t
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 ftw)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (get-unapplied-migrations file-sha256
                                     scandir-for-sql
                                     whitespace-only?
                                     parse-db-type
                                     flatten
                                     flatten-1))

(define (file-sha256 path)
  "Computes the SHA-256 of a file and returns a lowercase hexadecimal string."
  (call-with-input-file path
    (lambda (port)
      (string-downcase (bytevector->base16-string (port-sha256 port))))))

(define (scandir-for-sql path)
  (map (lambda (file)
         (format #f "~a/~a" path file))
       (scandir path
                (lambda (file)
                  (string-suffix? ".sql" file)) string<?)))

(define (whitespace-only? str)
  (string-every char-whitespace? str))

(define (flatten x)
  (cond
    ((null? x)
     '())
    ((pair? x)
     (append (flatten (car x))
             (flatten (cdr x))))
    (else (list x))))

(define (flatten-1 lst)
  (fold-right (lambda (x acc)
                (if (list? x)
                    (append x acc)
                    (cons x acc)))
              '() lst))

(define (get-unapplied-migrations migration-entries migrations-history)
  (filter-map (lambda (entry)
                (let* ((path (car entry))
                       (checksum (cadr entry))
                       (fname (basename path))
                       (history-entry (find (lambda (h)
                                              (equal? (assq-ref h
                                                                'name) fname))
                                            migrations-history)))
                  (if history-entry
                      (begin
                        (unless (equal? (assq-ref history-entry
                                                  'checksum) checksum)
                          (format #t
                           "
WARNING: Checksum mismatch for already applied migration ~a!
"
                           fname)) #f) ;Return #f so filter-map drops it
                      path))) ;Return path so filter-map keeps it
              migration-entries))

(define (parse-db-type database)
  (if (memq database
            '(postgresql mysql sqlite3))
      (symbol->string database)
      (raise-exception (make-exception-with-message (format #f
                                                     "UNKNOWN DATABASE TYPE: ~a"
                                                     database)))))
