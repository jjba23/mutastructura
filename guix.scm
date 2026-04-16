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

(use-modules (guix gexp)
             (guix packages)
             (guix git-download)
             ((guix licenses)
              #:prefix license:)
             (guix build-system guile)
             ((guix build utils)
              #:select (with-directory-excursion ))
             (gnu packages)
             (gnu packages gnupg)
             (gnu packages bash)
             (gnu packages guile)
             (gnu packages guile-xyz))

(define %source-dir
  (dirname (current-filename)))

(define-public mutastructura
  (package
    (name "mutastructura")
    (version "dev")
    (source
     (local-file %source-dir
                 #:recursive? #t
                 #:select? (git-predicate %source-dir)))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'install-program-files
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (share (string-append #$output "/share")))
                (copy-recursively "resources"
                                  (string-append share "/resources"))
                (install-file "scripts/mutastructura" bin)
                (install-file "scripts/log.bash"
                              (string-append share "/scripts/"))
                (chmod (string-append bin "/mutastructura") #o755)))))))
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-dbi
                             guile-dbd-mysql
                             guile-dbd-sqlite3
                             guile-dbd-postgresql
                             guile-gcrypt))
    (inputs (list guile-3.0 bash-minimal))
    (home-page "https://codeberg.org/jjba23/mutastructura")
    (synopsis "Relational database schema migrations powered by Guile Scheme")
    (description
     "Mutastructura provides a familiar, declarative and transactional
approach to managing database states. It exposes a user-friendly CLI
and has support for PostgreSQL, MySQL, and SQLite.

  Features include executing migrations transactionally and computing SHA256
checksums to track schema evolution in a @code{migrations_history} table.")
    (license license:agpl3+)))

mutastructura
