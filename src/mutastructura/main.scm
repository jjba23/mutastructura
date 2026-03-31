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
  #:use-module (ice-9 match)
  #:export (main))

(define* (main #:key (args (command-line)))
  
  (let* ((option-spec '((resources (single-char #\r)
                                   (value #t))
                        (migrations (single-char #\m)
                                    (value #t)
                                    (required? #t))))
         (options (getopt-long args option-spec))
         (migrations (option-ref options
                                 'migrations #f))
         (resources (option-ref options
                                'resources #f)))
    (display (format #f
              "
---------------------------------------------------------
Mutastructura: Migrations
    loading migrations: ~a
---------------------------------------------------------
"
              migrations))

    ))

