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
  #:export (string-split-to-queries))

;;; --- SQL Parser ---

(define (string-split-to-queries sql-string)
  (let loop
    ((chars (string->list sql-string))
     (state 'normal)
     (current-acc '())
     (queries '()))

    (if (null? chars)
        (let ((final-query (string-trim (list->string (reverse current-acc)))))
          (reverse (if (whitespace-only? final-query) queries
                       (cons final-query queries))))

        (let ((c (car chars))
              (rest (cdr chars)))
          (case state
            
            ((normal)
             (cond
               ((char=? c #\')
                (loop rest
                      'string
                      (cons c current-acc) queries))
               ((char=? c #\")
                (loop rest
                      'identifier
                      (cons c current-acc) queries))
               ((char=? c #\;)
                (let ((query (string-trim (list->string (reverse current-acc)))))
                  (loop rest
                        'normal
                        '()
                        (if (whitespace-only? query) queries
                            (cons query queries)))))
               ;; Check for --
               ((and (char=? c #\-)
                     (not (null? rest))
                     (char=? (car rest) #\-))
                (loop (cdr rest)
                      'line-comment
                      (cons #\-
                            (cons #\- current-acc)) queries))
               ;; Check for /*
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

            ((string)
             ;; Handle escaped quotes ('') inside strings!
             (if (and (char=? c #\')
                      (not (null? rest))
                      (char=? (car rest) #\'))
                 (loop (cdr rest)
                       'string
                       (cons #\'
                             (cons #\' current-acc)) queries)
                 (if (char=? c #\')
                     (loop rest
                           'normal
                           (cons c current-acc) queries)
                     (loop rest
                           'string
                           (cons c current-acc) queries))))

            ((identifier)
             ;; Handle escaped double quotes ("") inside identifiers!
             (if (and (char=? c #\")
                      (not (null? rest))
                      (char=? (car rest) #\"))
                 (loop (cdr rest)
                       'identifier
                       (cons #\"
                             (cons #\" current-acc)) queries)
                 (if (char=? c #\")
                     (loop rest
                           'normal
                           (cons c current-acc) queries)
                     (loop rest
                           'identifier
                           (cons c current-acc) queries))))

            ((line-comment)
             (if (char=? c #\newline)
                 (loop rest
                       'normal
                       (cons c current-acc) queries)
                 (loop rest
                       'line-comment
                       (cons c current-acc) queries)))

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
