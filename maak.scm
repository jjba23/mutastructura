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
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 popen)
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

;; ====== Maak abstractions ======

(define (syscall-with-output cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

;; ====== Maak tasks ======

(define (help)
  "Display program help screen."
  (time-machine-program-shell '("mutastructura --help")))

(define (default)
  "Default task to run, call program's help."
  (list-tasks))

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  ($ '("find . -maxdepth 8 -name '*.scm'" "-type f -exec guix style -f {} \\;")
     #:verbose? #t))

(define (test)
  "Run the unit test suite with Veritas."
  ;; `-L ./src' is the program, `-L ./test' is where the spec files live as
  ;; modules, and `-L .' is where the shared test helpers live.  The runner
  ;; scans ./ for test/veritas/unit by itself.
  ($ (list "VERITAS_TEST_TYPE=unit" "VERITAS_RUN_BENCHMARKS=false"
           "guix shell -m manifest.scm --"
           "guile -L ./src -L ./test -L ."
           "-c '((@ (veritas runner) run-tests))'")
     #:verbose? #t))

(define (repl)
  "Development REPL."
  (manifest-shell '("guile -L ./src"
                    "-c '((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Maak."
  (delete-file-recursively "doc")
  (time-machine-manifest-shell (list (~ "documenta api ./src/~a && "
                                        project-name)
                                     (~ "texi2any -v --html --css-ref=~a"
                                        api-docs-css-url)
                                     "./doc/api/index.texi"))
  (mv "./index" "./doc/api-dist"))

(define (deploy)
  (docs)
  ;; Deploy API docs
  (remkdir-p (~ "~a/api-docs/~a" deploy-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-dir project-name))
  (delete-file-recursively "doc"))

(define (package-docker)
  "Create a Docker image for Mutastructura."
  (time-machine-manifest-shell '("guix pack --file=./guix.scm"
                                 "--format=docker" "--image-tag=mutastructura"
                                 "-S /bin=bin"
                                 "bash coreutils util-linux-with-udev guile > .docker-tag")
                               #:verbose? #t)
  ($ '("cat .docker-tag | xargs podman load --input"))
  (delete-file-recursively ".docker-tag"))

(define (push-docker-latest)
  "Push the latest locally generated Docker image to Dockerhub."
  ($ (list (~ "podman push localhost/~a:latest" project-name)
           (~ "docker.io/jjba23/~a:latest" project-name))
     #:verbose? #t))

(define (push-docker-tag)
  "Push the locally generated Docker image to Dockerhub."
  ($ '("git fetch --all"))
  (let* ((latest-tag (syscall-with-output "git describe --tags --abbrev=0")))
    ($ (list (~ "podman push localhost/~a:latest" project-name)
             (~ "docker.io/jjba23/~a:~a" project-name latest-tag))
       #:verbose? #t)))

(define (dev-run)
  "Run mutastructura (dev)."
  ;; (delete-file-recursively some-test-db)
  (time-machine-manifest-shell (list
                                "guile -L ./src -c '((@(mutastructura main) main))'"
                                (~ "--connection ~a" some-test-db)
                                "--database sqlite3"
                                "--migrations ./migrations/lucidplan"
                                "--resources ./resources")
                               #:verbose? #t))

(define (run)
  "Run mutastructura."
  ;; (delete-file-recursively some-test-db)
  (time-machine-program-shell (list "mutastructura --database sqlite3"
                                    (~ "--connection ~a " some-test-db)
                                    "--migrations ./migrations/lucidplan")
                              #:verbose? #t))
