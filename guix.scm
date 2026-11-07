;;; MUTASTRUCTURA - Relational Schema Migrations Powered by Lisp

;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz)
             (gnu packages linux))

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
      #:modules '((guix build guile-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'install-program-files
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (share (string-append #$output "/share")))
                (copy-recursively "resources"
                                  (string-append share "/resources"))
                (install-file "scripts/mutastructura" bin)
                (chmod (string-append bin "/mutastructura") #o755))))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each (match-lambda
                          ((pattern program format-string)
                           (substitute* "scripts/mutastructura"
                             ((pattern)
                              (format #f format-string
                                      (search-input-file inputs
                                                         (string-append "bin/"
                                                          program)))))))
                        '(("readlink -f " "readlink" "~s -f ")
                          ("realpath " "realpath" "~s ")
                          ("dirname " "dirname" "~s ")
                          ("getopt " "getopt" "~s ")
                          ("exec guile " "guile" "exec ~s "))))))))
    (propagated-inputs (list guile-dbi guile-dbd-mysql guile-dbd-sqlite3
                             guile-dbd-postgresql guile-gcrypt))
    (inputs (list guile-3.0 bash-minimal coreutils util-linux))
    (home-page "https://github.com/jjba23/mutastructura")
    (synopsis "Relational database schema migrations powered by Guile Scheme")
    (description
     "Mutastructura provides a familiar, declarative and transactional
approach to managing database states. It exposes a user-friendly
@acronym{CLI, command-line interface} and has support for PostgreSQL, MySQL, and SQLite.
It also exposes library functions and can be imported and used directly from other Guile code.

Features include executing migrations transactionally and computing SHA256
checksums to track schema evolution in a @code{migrations_history} table.")
    (license license:expat)))

mutastructura
