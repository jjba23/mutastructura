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

(define-module (mutastructura persistence sql-parser)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (dbi dbi)
  #:use-module (mutastructura mutastructura)
  #:export (run-migration-file))

;;; --- Migration Runner ---

(define (run-migration-file db)
  (lambda (path)
    (let* ((is-repeatable (string-suffix? ".repeatable.sql" path))
           (migration-sha (file-sha256 path))
           (contents (call-with-input-file path
                       get-string-all))
           (statements (string-split-to-queries contents)))
      (format #t "\n>>= MIGRATION: ~a\nSHA256: ~a" path migration-sha)
      (unless (nil? statements)
        (for-each (run-stmt db) statements))
      (unless is-repeatable
        ((run-stmt db)
         (format #f
          "INSERT INTO migrations_history (name, checksum) VALUES (~a, ~a)"
          (value->sql-literal (basename path))
          (value->sql-literal migration-sha)))))))
