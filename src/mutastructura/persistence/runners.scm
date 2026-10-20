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
;; along with mutastructura.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The one place where an engine's *family* -- `relational' -- is turned into
;; the runner that speaks it.  Everything above this line knows only the vtable;
;; everything below knows only what a connection looks like.
;;
;; This is the whole of the dispatch: every engine reaches the same commands
;; because the commands ask for a runner and the engine decides which one they
;; get.  A new engine of a family already implemented is therefore a line in the
;; registry and nothing else, which is what makes `--database yugabytedb'
;; (PostgreSQL's protocol) or `--database mariadb' (MySQL's) cost nothing to add.

(define-module (mutastructura persistence runners)
  #:declarative? #t
  #:use-module (ice-9 exceptions)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence migration-runner)
  #:export (runner-for-engine
            runner-for-spec
            runner-families))

(define (runner-families)
  "The families a runner exists for, as symbols."
  '(relational))

(define (runner-for-engine engine)
  "The runner that speaks ENGINE's database, or an exception saying why not."
  (case (engine-family engine)
    ((relational) (sql-runner))
    (else
     (raise-exception
      (make-exception-with-message
       (format #f "no runner for the ~a family, which ~a belongs to\n   known families: ~a"
               (engine-family engine)
               (engine-id engine)
               (runner-families)))))))

(define (runner-for-spec spec)
  "The runner for the engine SPEC connects to."
  (runner-for-engine (connection-spec-engine spec)))
