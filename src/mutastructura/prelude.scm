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

(define-module (mutastructura prelude)
  #:declarative? #t
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 ftw)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (file-sha256 scandir-for-sql whitespace-only?))

(define (file-sha256 path)
  "Computes the SHA-256 of a file and returns a lowercase hexadecimal string."
  (call-with-input-file path
    (lambda (port)
      (let* ((hash-bytevector (port-sha256 port)))
        (string-downcase (bytevector->base16-string hash-bytevector))))))

(define (scandir-for-sql path)
  (map (lambda (file)
         (format #f "~a/~a" path file))
       (scandir path
                (lambda (file)
                  (string-suffix? ".sql" file)))))

(define (whitespace-only? str)
  (string-every char-whitespace? str))
