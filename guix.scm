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
    (arguments
     `(#:source-directory "src"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'install-program-files
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((bin (string-append (assoc-ref outputs "out")
                                                "/bin"))
                            (share (string-append (assoc-ref outputs "out")
                                                  "/share")))
                        (copy-recursively "resources"
                                      (string-append share "/resources"))
                        (install-file "scripts/mutastructura" bin)
                        (install-file "scripts/log.bash"
                                      (string-append share "/scripts/"))
                        (chmod (string-append bin "/mutastructura") #o755)))))))

    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (propagated-inputs (list guile-dbi guile-dbd-sqlite3 guile-gcrypt))
    (inputs (list guile-3.0 bash-minimal))
    (synopsis "")
    (description "")
    (home-page "https://codeberg.org/jjba23/mutastructura")
    (license license:agpl3+)))

mutastructura
