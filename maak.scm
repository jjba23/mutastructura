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

(define-module (maak)
  #:use-module (maak maak)
  ;; The container helpers are imported under a prefix because the tasks
  ;; below are named the way maak exposes them on the command line.
  #:use-module ((maak container) #:prefix ct:)
  #:declarative? #t)

;; ====== Maak constants ======

(define some-test-db
  "./lucidplan-test.sqlite")

;; The path to the directory where the main project files will be deployed.
(define deploy-dir
  "/srv/http/jointhefreeworld.org")

;; The URL for the CSS stylesheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

(define project-name
  'mutastructura)

(define hot-reload-file-types
  '(scm))

;; ====== Maak tasks ======

(define (help)
  "Display program help screen."
  (time-machine-program-shell (list "mutastructura" "--help")))

(define (default)
  "Default task to run, call program's help."
  (list-tasks))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ;; The task named `run' below shadows the helper of the same name, so the
  ;; helper is reached through the module it comes from.
  ((@ (maak maak) run)
   (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
         "-exec" "guix" "style" "-f" "{}" ";")
   #:verbose? #t))

(define (test)
  "Run the unit test suite with Veritas."
  ;; `-L ./src' is the program, `-L ./test' is where the spec files live as
  ;; modules, and `-L .' is where the shared test helpers live.  The runner
  ;; scans ./ for test/veritas/unit by itself.  The Veritas variables go
  ;; through `#:env', which sets them for the command inside the shell.
  (manifest-shell (list "guile" "-L" "./src" "-L" "./test" "-L" "."
                        "-c" "'((@ (veritas runner) run-tests))'")
                  #:env '("VERITAS_TEST_TYPE=unit"
                          "VERITAS_RUN_BENCHMARKS=false")
                  #:verbose? #t))

(define (repl)
  "Development REPL."
  (manifest-shell (list "guile" "-L" "./src"
                        "-c" "'((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Maak."
  (delete-file-recursively "doc")
  ;; Documenta writes ./doc/api/index.texi and texi2any renders it; two calls,
  ;; so a failure names the step that failed.
  (time-machine-manifest-shell (list "documenta" "api"
                                     (~ "./src/~a" project-name)))
  (time-machine-manifest-shell
   (list "texi2any" "-v" "--html"
         (~ "--css-ref=~a" api-docs-css-url)
         "./doc/api/index.texi"))
  (mv "./index" "./doc/api-dist"))

(define (deploy)
  (docs)
  ;; Deploy API docs
  (remkdir-p (~ "~a/api-docs/~a" deploy-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-dir project-name)
      #:expand? #t)
  (delete-file-recursively "doc"))

(define (package-docker)
  "Build the container image for mutastructura."
  ;; `guix pack --format=docker' prints the name of the tarball it wrote, so
  ;; that name is read as a value and handed to the engine, rather than
  ;; round-tripped through a file and a shell pipeline.
  (let* ((pack-argv (list "guix" "time-machine" "--channels=channels.scm" "--"
                          "shell" "-m" "manifest.scm" "--"
                          "guix" "pack" "--file=./guix.scm" "--format=docker"
                          (~ "--image-tag=~a" project-name)
                          "-S" "/bin=bin"
                          "bash" "coreutils" "util-linux-with-udev" "guile"))
         (tarball (string-trim-both
                   (capture (string-join (map shell-quote pack-argv) " ")))))
    (if (string=? tarball "")
        ;; Dry run, where nothing was packed and no name came back.
        (log-info "No tarball was read, so nothing is loaded into the engine.")
        (ct:container (list "load" "--input" tarball) #:verbose? #t))))

(define (push-docker-latest)
  "Push the latest locally generated Docker image to Dockerhub."
  (ct:container (list "push"
                      (~ "localhost/~a:latest" project-name)
                      (~ "docker.io/jjba23/~a:latest" project-name))
                #:verbose? #t))

(define (push-docker-tag)
  "Push the locally generated Docker image to Dockerhub."
  (git-fetch "--all")
  (let ((latest-tag (git-describe-tag)))
    (ct:container (list "push"
                        (~ "localhost/~a:latest" project-name)
                        ;; A dry run reads no tag, so the line shows a
                        ;; placeholder rather than a dangling colon.
                        (~ "docker.io/jjba23/~a:~a" project-name
                           (if (string=? latest-tag "") "<tag>" latest-tag)))
                  #:verbose? #t)))

(define (dev-run)
  "Run mutastructura (dev)."
  ;; (delete-file-recursively some-test-db)
  (time-machine-manifest-shell
   (list "guile" "-L" "./src"
         "-c" "'((@(mutastructura main) main))'"
         "--connection" some-test-db
         "--database" "sqlite3"
         "--migrations" "./migrations/lucidplan"
         "--resources" "./resources")
   #:verbose? #t))

(define (run)
  "Run mutastructura."
  ;; (delete-file-recursively some-test-db)
  (time-machine-program-shell (list "mutastructura"
                                    "--database" "sqlite3"
                                    "--connection" some-test-db
                                    "--migrations" "./migrations/lucidplan")
                              #:verbose? #t))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for mutastructura."
  (ct:container-build "." #:tag "mutastructura:latest" #:verbose? #t))

(define (container-run)
  "Run the container image for mutastructura."
  (ct:container-run "mutastructura" #:interactive? #t #:tty? #t #:verbose? #t))
