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
;; The core of the program, where nothing touches a database: the engines it
;; knows and the aliases people reach them by, the connection strings they are
;; opened with, what a migration file is and in what order the files run, and the
;; planning that compares a directory against a ledger.
;;
;; The cases here are the ones that are cheap to get wrong and expensive to
;; notice.  An alias that resolves to another engine's dialect, a connection
;; string that silently drops half a password, a version comparison that puts
;; `1.9' after `1.10', a repeatable migration counted as new on every run -- each
;; of those works fine on the machine of whoever wrote it.
;;
;; The ledger is where a planning test gets its history from, so the entries
;; below are written the way a driver answers: symbol keys, `success' as 1 or 0
;; or missing, `kind' sometimes a string.  Reading them is exactly what the
;; runner does, so a wrong reading here is a migration applied twice.

(define-module (veritas unit core)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (mutastructura mutastructura)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define (error-message thunk)
  "What THUNK raised, as a sentence, or #f when it returned normally."
  (catch #t
         (lambda () (thunk) #f)
         (lambda (key . args)
           (let ((condition (and (pair? args) (car args))))
             (catch #t
                    (lambda () (condition-message condition))
                    (lambda _ (format #f "~a: ~a" key args)))))))

(define (truth value) (and value #t))

(define (contains? haystack needle)
  "Whether HAYSTACK, a string, holds NEEDLE."
  (and haystack (string-contains haystack needle) #t))

(define (mentions? haystack needles)
  "Whether HAYSTACK holds every one of NEEDLES."
  (every (lambda (needle) (contains? haystack needle)) needles))

(define (id-of name) (engine-id (find-engine name)))

(define* (spec-of url #:optional (engine #f))
  "The connection spec URL parses to."
  (if engine
      (parse-connection-url url #:engine (find-engine engine))
      (parse-connection-url url)))

;; A migration directory that is in the repository, with two files in it: one
;; versioned and one repeatable.
(define examples "migrations/lucidplan")

(define init-file (string-append examples "/2026-03-31_20:24:00_init.sql"))
(define seed-file (string-append examples
                                  "/2026-04-01_21:54:00_test.repeatable.sql"))

(define (entry name checksum)
  "A ledger entry, as a driver answers with one."
  (list (cons 'name name)
        (cons 'checksum checksum)
        (cons 'description name)
        (cons 'kind 'versioned)
        (cons 'installed-on 1791981743651)
        (cons 'installed-by "mutastructura")
        (cons 'execution-time 12)
        (cons 'success 1)))

;;; ---------------------------------------------------------------------------
;;; The engines
;;; ---------------------------------------------------------------------------

(define engine-suite
  (suite "the engines and the names people call them"
         #:concurrent? #f
         (test "every alias resolves to the engine it names"
               #:shuffle? #f
               ;; An alias that resolves to another engine's dialect is a bug
               ;; only the person who typed `--database mariadb' ever sees.
               (assert-equal #:expect 'postgresql #:got (id-of 'pg)
                             #:name "pg is PostgreSQL")
               (assert-equal #:expect 'postgresql #:got (id-of 'psql)
                             #:name "and so is psql")
               (assert-equal #:expect 'postgresql #:got (id-of 'timescaledb)
                             #:name "a PostgreSQL-compatible database is reached as one")
               (assert-equal #:expect 'mysql #:got (id-of 'mariadb)
                             #:name "mariadb is MySQL's engine")
               (assert-equal #:expect 'mysql #:got (id-of 'percona)
                             #:name "and so is percona")
               (assert-equal #:expect 'cockroachdb #:got (id-of 'crdb)
                             #:name "crdb is CockroachDB"))
         (test "a name is matched without case"
               #:shuffle? #f
               (assert-equal #:expect 'mysql #:got (id-of 'MySQL)
                             #:name "MySQL and mysql are the same request")
               (assert-equal #:expect 'postgresql #:got (id-of 'POSTGRES)
                             #:name "and so are POSTGRES and postgres"))
         (test "an engine carries the port and the ledger it defaults to"
               #:shuffle? #f
               (assert-equal #:expect 5432
                             #:got (engine-default-port (find-engine 'postgres))
                             #:name "PostgreSQL's port")
               (assert-equal #:expect 3306
                             #:got (engine-default-port (find-engine 'mysql))
                             #:name "MySQL's port")
               (assert-equal #:expect 26257
                             #:got (engine-default-port (find-engine 'crdb))
                             #:name "CockroachDB's port")
               (assert-equal #:expect 0
                             #:got (engine-default-port (find-engine 'sqlite3))
                             #:name "and SQLite has none")
               (assert-equal #:expect "migrations_history"
                             #:got (engine-default-history-name
                                    (find-engine 'sqlite3))
                             #:name "the SQL ledger's default name"))
         (test "every engine is a relational one"
               #:shuffle? #f
               (assert-equal #:expect 'relational
                             #:got (engine-family (find-engine 'sqlite3))
                             #:name "SQLite is relational")
               (assert-equal #:expect 'relational
                             #:got (engine-family (find-engine 'postgres))
                             #:name "and so is PostgreSQL")
               (assert-equal #:expect #t
                             #:got (relational-engine? (find-engine 'sqlite3))
                             #:name "which is what the core asks"))
         (test "a database reached through another driver keeps its own identity"
               #:shuffle? #f
               ;; CockroachDB speaks PostgreSQL's protocol, and `info' has to say
               ;; which one it is *and* which driver opened it.
               (assert-equal #:expect 'postgresql
                             #:got (engine-driver (find-engine 'crdb))
                             #:name "CockroachDB is opened by the PostgreSQL driver")
               (assert-equal #:expect "postgresql"
                             #:got (parse-db-type 'crdb)
                             #:name "which is what guile-dbi is told")
               (assert-equal #:expect "mysql"
                             #:got (parse-db-type 'mariadb)
                             #:name "and MariaDB by the MySQL one"))
         (test "every engine takes SQL migration files"
               #:shuffle? #f
               (assert-equal #:expect '(".sql")
                             #:got (engine-migration-extensions
                                    (find-engine 'postgres))
                             #:name "SQL for PostgreSQL")
               (assert-equal #:expect '(".sql" ".sqlite.sql")
                             #:got (engine-migration-extensions
                                    (find-engine 'sqlite3))
                             #:name "and for SQLite, which also knows its own suffix"))
         (test "an unknown name is refused, naming the ones that exist"
               #:shuffle? #f
               ;; A typo in a command line is the commonest failure this tool
               ;; sees, so the message is worth asserting.
               (assert-equal #:expect #f #:got (find-engine 'nope)
                             #:name "there is no such engine")
               (assert-equal #:expect #t
                             #:got (mentions? (error-message
                                               (lambda () (parse-engine 'nope)))
                                              (list "UNKNOWN DATABASE TYPE: nope"
                                                    "postgresql"
                                                    "sqlite3"))
                             #:name "and the error says what there is instead"))
         (test "the engines are listed once each"
               #:shuffle? #f
               (assert-equal #:expect '("sqlite3" "postgresql" "cockroachdb"
                                        "mysql")
                             #:got (supported-engine-ids)
                             #:name "in the order help lists them"))))

;;; ---------------------------------------------------------------------------
;;; Connection URLs
;;; ---------------------------------------------------------------------------

(define url-suite
  (suite "connection URLs"
         #:concurrent? #f
         (test "a PostgreSQL URL is split into its parts"
               #:shuffle? #f
               (let ((spec (spec-of "postgresql://alice:secret@db.example.net:5432/appdb")))
                 (assert-equal #:expect 'postgresql
                               #:got (engine-id (connection-spec-engine spec))
                               #:name "the engine")
                 (assert-equal #:expect "db.example.net"
                               #:got (connection-spec-host spec)
                               #:name "the host")
                 (assert-equal #:expect 5432
                               #:got (connection-spec-port spec)
                               #:name "the port")
                 (assert-equal #:expect "alice"
                               #:got (connection-spec-user spec)
                               #:name "the user")
                 (assert-equal #:expect "appdb"
                               #:got (connection-spec-database spec)
                               #:name "and the database")))
         (test "a URL becomes the driver's own connection string"
               #:shuffle? #f
               ;; guile-dbi splits this on `:' and has no other form, so the
               ;; shape of the string is part of the interface.
               (assert-equal #:expect "alice:secret:appdb:tcp:db.example.net:5432"
                             #:got (connection-spec-connection
                                    (spec-of "postgresql://alice:secret@db.example.net:5432/appdb"))
                             #:name "PostgreSQL's colon-delimited form")
               (assert-equal #:expect "alice:secret:appdb:tcp:127.0.0.1:3306"
                             #:got (connection-spec-connection
                                    (spec-of "mysql://alice:secret@127.0.0.1:3306/appdb"))
                             #:name "and MySQL's"))
         (test "a port left out is the engine's own"
               #:shuffle? #f
               (assert-equal #:expect "alice:secret:appdb:tcp:db.example.net:5432"
                             #:got (connection-spec-connection
                                    (spec-of "postgresql://alice:secret@db.example.net/appdb"))
                             #:name "PostgreSQL defaults to 5432")
               (assert-equal #:expect "alice:secret:appdb:tcp:localhost:3306"
                             #:got (connection-spec-connection
                                    (spec-of "mysql://alice:secret@/appdb"))
                             #:name "and MySQL to its own port on localhost"))
         (test "a password with a colon is refused rather than truncated"
               #:shuffle? #f
               ;; Half a password connects as the wrong user, or does not
               ;; connect at all, and either is worse than being told.
               (assert-equal #:expect #t
                             #:got (mentions? (error-message
                                               (lambda ()
                                                 (spec-of "postgresql://alice:se:cret@host/app")))
                                              (list "contains a ':'" "--connection"))
                             #:name "the reason and the way out are both named"))
         (test "a SQLite URL is a path, taken verbatim"
               #:shuffle? #f
               (let ((spec (spec-of "sqlite3:///var/lib/app.db")))
                 (assert-equal #:expect 'sqlite3
                               #:got (engine-id (connection-spec-engine spec))
                               #:name "the engine")
                 (assert-equal #:expect "/var/lib/app.db"
                               #:got (connection-spec-connection spec)
                               #:name "the absolute path is kept whole")
                 (assert-equal #:expect "/var/lib/app.db"
                               #:got (connection-spec-database spec)
                               #:name "and is the database"))
               (assert-equal #:expect "./local.db"
                             #:got (connection-spec-connection
                                    (spec-of "sqlite3://./local.db"))
                             #:name "a relative path stays relative"))
         (test "a parameter is percent-decoded"
               #:shuffle? #f
               (assert-equal #:expect "a b"
                             #:got (assoc-ref (connection-spec-options
                                               (spec-of "postgresql://host/app?application_name=a%20b"))
                                              "application_name")
                             #:name "a space written as %20"))
         (test "the description of a connection never holds the password"
               #:shuffle? #f
               ;; It is printed by `info' and written to every log line.
               (let ((described (describe-connection-spec
                                 (spec-of "postgresql://alice:secret@db.example.net:5432/appdb"))))
                 (assert-equal #:expect #t
                               #:got (mentions? described
                                                (list "postgresql" "alice"
                                                      "db.example.net" "5432" "appdb"))
                               #:name "it says where it is going and as whom")
                 (assert-equal #:expect #f
                               #:got (contains? described "secret")
                               #:name "and not the password"))
               (assert-equal #:expect #t
                             #:got (contains? (describe-connection-spec
                                               (spec-of "cockroachdb://127.0.0.1:26257/app"))
                                              "via postgresql")
                             #:name "and it names the driver when that is the interesting part"))
         (test "something that is not a URL is a connection string"
               #:shuffle? #f
               ;; guile-dbi's own strings look like `user:pass:db:tcp:host:port'
               ;; and must not be mistaken for a URL whose scheme is the user.
               (let ((spec (spec-of "alice:secret:appdb:tcp:host:5432" 'postgresql)))
                 (assert-equal #:expect "alice:secret:appdb:tcp:host:5432"
                               #:got (connection-spec-connection spec)
                               #:name "the string is passed through")
                 (assert-equal #:expect 'postgresql
                               #:got (engine-id (connection-spec-engine spec))
                               #:name "with the engine it was told")))))

;;; ---------------------------------------------------------------------------
;;; What a migration file is
;;; ---------------------------------------------------------------------------

(define migration-suite
  (suite "a migration file and its order"
         #:concurrent? #f
         (test "a name is read into a version and a description"
               #:shuffle? #f
               (let ((file (migration-file init-file)))
                 (assert-equal #:expect "2026-03-31_20:24:00_init.sql"
                               #:got (migration-name file)
                               #:name "the file it is")
                 (assert-equal #:expect "2026-03-31_20:24:00_init"
                               #:got (migration-version file)
                               #:name "the version --target compares against")
                 (assert-equal #:expect 'versioned
                               #:got (migration-kind file)
                               #:name "and that it is versioned")))
         (test "a `.repeatable.sql' name is a repeatable migration"
               #:shuffle? #f
               ;; It runs again whenever its checksum changes, which is what lets
               ;; a view or a function be edited in place.
               (let ((file (migration-file seed-file)))
                 (assert-equal #:expect 'repeatable
                               #:got (migration-kind file)
                               #:name "even without Flyway's `R__' prefix")
                 (assert-equal #:expect "2026-04-01_21:54:00_test"
                               #:got (migration-version file)
                               #:name "with the marker stripped from the version")))
         (test "Flyway's names are read the way Flyway reads them"
               #:shuffle? #f
               (let ((file (migration-file "V1.2__add-user.sql")))
                 (assert-equal #:expect "1.2" #:got (migration-version file)
                               #:name "the version is the number")
                 (assert-equal #:expect "add-user"
                               #:got (migration-description file)
                               #:name "and the description what follows the underscores")
                 (assert-equal #:expect 'versioned #:got (migration-kind file)
                               #:name "a V file is versioned"))
               (assert-equal #:expect 'repeatable
                             #:got (migration-kind (migration-file "R__views.sql"))
                             #:name "an R file is repeatable")
               (assert-equal #:expect 'undo
                             #:got (migration-kind (migration-file "U1.2__back.sql"))
                             #:name "and a U file is a rollback"))
         (test "a dotted version sorts by number, not by text"
               #:shuffle? #f
               ;; Plain string comparison puts `1.10' before `1.9', which is the
               ;; wrong order to run migrations in.
               (assert-equal #:expect #t
                             #:got (truth (string>? (numeric-version-key "1.2.10")
                                                    (numeric-version-key "1.2.9")))
                             #:name "1.2.10 comes after 1.2.9")
               (assert-equal #:expect #t
                             #:got (truth (string>? (migration-order-key
                                                     (migration-file "V1.10__ten.sql"))
                                                    (migration-order-key
                                                     (migration-file "V1.9__nine.sql"))))
                             #:name "and the order key of the files agrees")
               (assert-equal #:expect #f #:got (numeric-version-key "views")
                             #:name "a version that is not numeric has no such key"))
         (test "which files are migrations is the engine's answer"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (truth (migration-file?
                                           (find-engine 'postgres) "001_init.sql"))
                             #:name "SQL for PostgreSQL")
               (assert-equal #:expect #f
                             #:got (truth (migration-file?
                                           (find-engine 'postgres) "001_init.json"))
                             #:name "and not JSON")
               (assert-equal #:expect #f
                             #:got (truth (migration-file?
                                           (find-engine 'sqlite3) "notes.txt"))
                             #:name "nor does an engine take a stray note"))
         (test "a file's checksum is its content's"
               #:shuffle? #f
               ;; What makes an edited migration that was already applied a
               ;; difference worth reporting.
               (let ((first (file-sha256 init-file)))
                 (assert-equal #:expect 64 #:got (string-length first)
                               #:name "a SHA-256 is 64 hexadecimal characters")
                 (assert-equal #:expect first #:got (file-sha256 init-file)
                               #:name "and it is the same on the next read")
                 (assert-equal #:expect #f
                               #:got (equal? first (file-sha256 seed-file))
                               #:name "while another file has another")))
         (test "a file is read with its byte order mark removed"
               #:shuffle? #f
               (assert-equal #:expect "SELECT 1"
                             #:got (strip-bom (string-append (string #\xfeff)
                                                             "SELECT 1"))
                             #:name "the BOM an editor wrote")
               (assert-equal #:expect "SELECT 1" #:got (strip-bom "SELECT 1")
                             #:name "and a file that never had one"))
         (test "the discovery of a directory is by extension, and sorted"
               #:shuffle? #f
               ;; A stray note in a migrations directory is ignored rather than
               ;; becoming a failed migration.
               (let ((found (scandir-for-migrations
                             examples
                             #:engine (find-engine 'sqlite3))))
                 (assert-equal #:expect '("2026-03-31_20:24:00_init.sql"
                                          "2026-04-01_21:54:00_test.repeatable.sql")
                               #:got (map migration-name found)
                               #:name "the SQL directory, in order")))
         (test "a directory that is not there is refused"
               #:shuffle? #f
               ;; Rather than silently finding nothing and reporting success.
               (let ((message (error-message
                               (lambda ()
                                 (scandir-for-migrations
                                  "no/such/dir"
                                  #:engine (find-engine 'sqlite3))))))
                 (assert-equal #:expect #t
                               #:got (truth message)
                               #:name "the directory is refused")))))

;;; ---------------------------------------------------------------------------
;;; Placeholders
;;; ---------------------------------------------------------------------------

(define placeholder-suite
  (suite "placeholders"
         #:concurrent? #f
         (test "a migration's `${name}' becomes what it was given"
               #:shuffle? #f
               (assert-equal #:expect "SELECT * FROM app.users WHERE id = 7"
                             #:got (apply-placeholders
                                    "SELECT * FROM ${schema}.users WHERE id = ${id}"
                                    '(("schema" . "app") ("id" . 7)))
                             #:name "both placeholders substituted"))
         (test "a placeholder nobody defined is left exactly as it was"
               #:shuffle? #f
               ;; A migration may hold `${...}' syntax that belongs to somebody
               ;; else -- a shell script in a comment, a template table.
               (assert-equal #:expect "SELECT '${not-ours}'"
                             #:got (apply-placeholders "SELECT '${not-ours}'" '())
                             #:name "an empty set of placeholders changes nothing")
               (assert-equal #:expect "SELECT ${other}"
                             #:got (apply-placeholders "SELECT ${other}"
                                                       '(("known" . "value")))
                             #:name "and neither does an incomplete one"))
         (test "a `key=value' list becomes an alist"
               #:shuffle? #f
               (assert-equal #:expect '(("user" . "ada") ("schema" . "app"))
                             #:got (parse-placeholder-specs '("user=ada"
                                                              " schema = app "))
                             #:name "spaces around the parts are not part of them")
               (assert-equal #:expect '(("user" . "ada"))
                             #:got (parse-placeholder-string "user=ada")
                             #:name "a comma-separated string is the same thing")
               (assert-equal #:expect '()
                             #:got (parse-placeholder-string "  ")
                             #:name "an empty one defines nothing"))
         (test "a spec with no `=' is refused"
               #:shuffle? #f
               ;; A silently ignored placeholder is a migration that runs with
               ;; the wrong schema name in it.
               (assert-equal #:expect #t
                             #:got (mentions? (error-message
                                               (lambda ()
                                                 (parse-placeholder-specs '("user"))))
                                              (list "user" "key=value"))
                             #:name "and the form it wants is named"))
         (test "the question of whether there is a placeholder at all"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (truth (placeholder-syntax? "SELECT ${a}"))
                             #:name "a `${' and a `}' together are one")
               (assert-equal #:expect #f
                             #:got (truth (placeholder-syntax? "SELECT $1"))
                             #:name "a positional parameter is not"))))

;;; ---------------------------------------------------------------------------
;;; The ledger, as the core reads it
;;; ---------------------------------------------------------------------------

(define ledger-suite
  (suite "the ledger entry the core reads"
         #:concurrent? #f
         (test "a row is recognised as an entry"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (truth (history-entry? (entry "a.sql" "abc")))
                             #:name "an alist with a name is one")
               (assert-equal #:expect #f
                             #:got (truth (history-entry? "a.sql"))
                             #:name "a string is not")
               (assert-equal #:expect #f
                             #:got (truth (history-entry? '((checksum . "abc"))))
                             #:name "and neither is an alist without a name"))
         (test "a kind is read whether it is a symbol, a string or missing"
               #:shuffle? #f
               ;; The ledger stores a string, but a driver may answer with the
               ;; symbol this program wrote.
               (assert-equal #:expect 'versioned
                             #:got (history-entry-kind (entry "a.sql" "abc"))
                             #:name "the symbol the runner writes")
               (assert-equal #:expect 'repeatable
                             #:got (history-entry-kind
                                    (list (cons 'name "a.sql")
                                          (cons 'kind "repeatable")))
                             #:name "the string a driver answers with")
               (assert-equal #:expect 'repeatable
                             #:got (history-entry-kind
                                    (list (cons 'name "a.repeatable.sql")))
                             #:name "and a name that says so when the field is gone"))
         (test "a success is read whether it is a boolean or 1 and 0"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (history-entry-success (entry "a.sql" "abc"))
                             #:name "1 is a success")
               (assert-equal #:expect #f
                             #:got (history-entry-success
                                    (list (cons 'name "a.sql") (cons 'success 0)))
                             #:name "0 is a failure")
               (assert-equal #:expect #t
                             #:got (history-entry-success
                                    (list (cons 'name "a.sql")))
                             #:name "and an entry that does not mention it succeeded")
               (assert-equal #:expect #f
                             #:got (history-entry-success
                                    (list (cons 'name "a.sql") (cons 'success #f)))
                             #:name "an explicit #f is a failure"))
         (test "a time is read from either column name"
               #:shuffle? #f
               ;; The ledger stamps `installed_on', and an older ledger may
               ;; call the same field `applied_at'.
               (assert-equal #:expect 1791981743651
                             #:got (history-entry-installed-on
                                    (entry "a.sql" "abc"))
                             #:name "the server's own stamp")
               (assert-equal #:expect 5
                             #:got (history-entry-installed-on
                                    (list (cons 'name "a.sql")
                                          (cons 'applied-at 5)))
                             #:name "and the older spelling"))
         (test "an entry becomes a migration file for reporting"
               #:shuffle? #f
               (let ((file (history-entry->migration-file
                            (entry "2026-04-01_21:54:00_test.repeatable.sql"
                                   "abc"))))
                 (assert-equal #:expect "2026-04-01_21:54:00_test"
                               #:got (migration-version file)
                               #:name "with its version")
                 ;; The helper's entry records the kind the runner writes, and
                 ;; the recorded kind is what a ledger entry is read as.
                 (assert-equal #:expect 'versioned #:got (migration-kind file)
                               #:name "and its kind")
                 (assert-equal #:expect #t #:got (assq-ref file 'applied?)
                               #:name "and known to be applied"))
               (assert-equal #:expect 0
                             #:got (history-entry-execution-time
                                    (list (cons 'name "a.sql")))
                             #:name "an entry with no time took none"))
         (test "the newest applied version is the newest versioned one"
               #:shuffle? #f
               ;; A repeatable migration has no version, so it must not become
               ;; the answer: `--target' would then hold back real migrations.
               (assert-equal #:expect "2026-03-31_20:24:00_init"
                             #:got (newest-applied-version
                                    (list (entry "2026-02-01_10:00:00_a.sql" "a")
                                          (entry "2026-03-31_20:24:00_init.sql" "b")
                                          (acons 'kind 'repeatable
                                                 (entry "2026-04-01_21:54:00_v.repeatable.sql"
                                                        "c"))))
                             #:name "the newest versioned migration")
               (assert-equal #:expect #f #:got (newest-applied-version '())
                             #:name "and an empty ledger has none"))))

;;; ---------------------------------------------------------------------------
;;; Planning
;;; ---------------------------------------------------------------------------

(define planning-suite
  (suite "comparing a directory with a ledger"
         #:concurrent? #f
         (test "an empty ledger makes every file pending"
               #:shuffle? #f
               (let* ((files (scandir-for-migrations examples
                                                      #:engine (find-engine 'sqlite3)))
                      (plan (plan-migrations files '())))
                 (assert-length #:expect 2 #:got plan
                                #:name "both files are in the plan")
                 (assert-equal #:expect '(pending repeatable-pending)
                               #:got (map (lambda (e) (assq-ref e 'state)) plan)
                               #:name "and both are pending")
                 (assert-length #:expect 2 #:got (plan-applicable plan)
                                #:name "so both would run")
                 ;; A repeatable migration pends in its own way: it is not a
                 ;; version waiting to be reached, it is a file whose content
                 ;; decides whether it runs again.
                 (assert-equal #:expect '((pending . 1) (repeatable-pending . 1))
                               #:got (plan-summary plan)
                               #:name "and the summary says so")))
         (test "a file the ledger knows about is skipped"
               #:shuffle? #f
               (let* ((files (list (migration-file init-file)))
                      (history (list (entry "2026-03-31_20:24:00_init.sql"
                                            (file-sha256 init-file))))
                      (plan (plan-migrations files history)))
                 (assert-equal #:expect 'applied
                               #:got (assq-ref (car plan) 'state)
                               #:name "it is applied")
                 (assert-equal #:expect 'skip
                               #:got (assq-ref (car plan) 'action)
                               #:name "and nothing is done")
                 (assert-length #:expect 0 #:got (plan-applicable plan)
                                #:name "so there is nothing to run")
                 (assert-length #:expect 0 #:got (plan-problems plan)
                                #:name "and nothing to complain about")))
         (test "a file edited after it was applied is a change"
               #:shuffle? #f
               ;; The whole reason a checksum is in the ledger.
               (let* ((files (list (migration-file init-file)))
                      (history (list (entry "2026-03-31_20:24:00_init.sql"
                                            "not-the-checksum-it-has")))
                      (plan (plan-migrations files history)))
                 (assert-equal #:expect 'changed
                               #:got (assq-ref (car plan) 'state)
                               #:name "the state is changed")
                 (assert-equal #:expect 'warn #:got (assq-ref (car plan) 'action)
                               #:name "which is a warning by default")
                 (assert-length #:expect 1 #:got (plan-problems plan)
                                #:name "and a problem")
                 (assert-equal #:expect 'fail
                               #:got (assq-ref (car (plan-migrations
                                                     files history
                                                     #:validate? #t))
                                               'action)
                               #:name "which validate turns into a refusal"))
               (assert-length
                #:expect 1
                #:got (checksum-mismatches
                       (list (migration-file init-file))
                       (list (entry "2026-03-31_20:24:00_init.sql"
                                    "not-the-checksum-it-has")))
                #:name "the same question asked on its own"))
         (test "a ledger entry whose file has gone is missing"
               #:shuffle? #f
               (let* ((files (list (migration-file init-file)))
                      (history (list (entry "2026-03-31_20:24:00_init.sql"
                                            (file-sha256 init-file))
                                     (entry "2026-01-01_00:00:00_gone.sql" "dead")))
                      (plan (plan-migrations files history)))
                 (assert-length #:expect 2 #:got plan
                                #:name "the file and the entry that has none")
                 (assert-equal #:expect 'missing
                               #:got (assq-ref (cadr plan) 'state)
                               #:name "the entry is missing its file")
                 (assert-equal #:expect 'warn #:got (assq-ref (cadr plan) 'action)
                               #:name "and is reported rather than run")
                 (assert-length #:expect 1
                                #:got (history-orphans files history)
                                #:name "the orphans are found on their own too")))
         (test "a target holds back what is above it"
               #:shuffle? #f
               ;; Exactly like Flyway's `target': the migration whose version is
               ;; the target runs, anything above it does not.  A date may be a
               ;; prefix of a version, so `2026-03-31' is a whole day.
               (let* ((files (scandir-for-migrations examples
                                                      #:engine (find-engine 'sqlite3)))
                      (plan (plan-migrations files '() #:target "2026-03-31")))
                 (assert-equal #:expect 'pending
                               #:got (assq-ref (car plan) 'state)
                               #:name "the migration at the target runs")
                 (assert-equal #:expect 'beyond-target
                               #:got (assq-ref (cadr plan) 'state)
                               #:name "and the one after it does not")
                 (assert-length #:expect 1 #:got (plan-applicable plan)
                                #:name "so one of the two would run")
                 (assert-length #:expect 0 #:got (plan-problems plan)
                                #:name "and holding one back is not a problem")))
         (test "a pending migration older than the newest one is held back"
               #:shuffle? #f
               ;; Unless it is asked for: applying an old migration on top of a
               ;; newer schema is how a deploy breaks in a subtle way.
               (let* ((files (list (migration-file init-file)))
                      (history (list (entry "2026-04-01_21:54:00_later.sql" "x")))
                      (plan (plan-migrations files history)))
                 (assert-equal #:expect 'superseded
                               #:got (assq-ref (car plan) 'state)
                               #:name "it is superseded by what is applied")
                 (assert-equal #:expect 'apply
                               #:got (assq-ref (car (plan-migrations
                                                     files history
                                                     #:out-of-order? #t))
                                               'action)
                               #:name "unless out-of-order runs are allowed")))
         (test "repair names the recorded failures and the changed files"
               #:shuffle? #f
               ;; What repair acts on, and only that: a healthy applied
               ;; migration is not a candidate, and the two kinds of repair are
               ;; told apart by the state the plan already reports.
               (let* ((file (migration-file init-file))
                      (name (migration-name file))
                      (checksum (file-sha256 init-file))
                      (healthy (plan-migrations (list file)
                                                (list (entry name checksum))))
                      (changed (plan-migrations (list file)
                                                (list (entry name "stale"))))
                      (failed (plan-migrations
                               (list file)
                               (list (list (cons 'name name)
                                           (cons 'checksum checksum)
                                           (cons 'success 0))))))
                 (assert-length #:expect 0 #:got (repair-candidates healthy)
                                #:name "nothing to repair when the ledger agrees")
                 (assert-length #:expect 1 #:got (repair-candidates changed)
                                #:name "a changed file is a candidate")
                 (assert-equal #:expect 'changed
                               #:got (assq-ref (car (repair-candidates changed))
                                               'state)
                               #:name "which repair realigns")
                 (assert-length #:expect 1 #:got (repair-candidates failed)
                                #:name "and so is a recorded failure")
                 (assert-equal #:expect #f
                               #:got (assq-ref (car (repair-candidates failed))
                                               'success)
                               #:name "which repair forgets")))))

;;; ---------------------------------------------------------------------------
;;; Rolling back
;;; ---------------------------------------------------------------------------

;; The files are never read: the planner is handed a `read-file' that answers
;; with the directive text a case is about, so the planning can be tested
;; without a migration directory on disk.
(define rollback-files
  (list (migration-file "/m/2026-01-01_a.sql")
        (migration-file "/m/2026-01-02_b.sql")
        (migration-file "/m/2026-01-02_b.undo.sql")
        (migration-file "/m/2026-01-03_c.sql")
        (migration-file "/m/2026-01-03_c.undo.sql")))

(define (no-directive path) "")

(define (rollback-history)
  (list (entry "2026-01-01_a.sql" "a")
        (entry "2026-01-02_b.sql" "b")
        (entry "2026-01-03_c.sql" "c")))

(define rollback-suite
  (suite "rolling back what can be rolled back"
         #:concurrent? #f
         (test "one migration is undone by default, and it is the newest"
               #:shuffle? #f
               (let ((plan (rollback-plan rollback-files (rollback-history)
                                          #:read-file no-directive)))
                 (assert-length #:expect 1 #:got plan
                                #:name "the safe default is one step")
                 (assert-equal #:expect "2026-01-03_c.sql"
                               #:got (assq-ref (car plan) 'name)
                               #:name "the newest applied migration")
                 (assert-equal #:expect 'undo
                               #:got (assq-ref (car plan) 'action)
                               #:name "with the undo script beside it")
                 (assert-equal #:expect "2026-01-03_c.undo.sql"
                               #:got (migration-name (assq-ref (car plan) 'undo))
                               #:name "which is the file the runner would apply")))
         (test "a requested number of steps reports what blocks it"
               #:shuffle? #f
               ;; `a' has no undo script, so a request for three is not silently
               ;; turned into two: the caller refuses before anything runs.
               (let ((plan (rollback-plan rollback-files (rollback-history)
                                          #:steps 3 #:read-file no-directive)))
                 (assert-equal #:expect '(undo undo blocked)
                               #:got (map (lambda (e) (assq-ref e 'action)) plan)
                               #:name "the two that can, and the one that cannot")
                 (assert-equal #:expect "no undo script"
                               #:got (assq-ref (caddr plan) 'reason)
                               #:name "and why the last one is blocked")))
         (test "--all stops at the first migration it cannot undo"
               #:shuffle? #f
               ;; The smart boundary: do the reversible suffix, name the wall.
               (let ((plan (rollback-plan rollback-files (rollback-history)
                                          #:all? #t #:read-file no-directive)))
                 (assert-equal #:expect '(undo undo boundary)
                               #:got (map (lambda (e) (assq-ref e 'action)) plan)
                               #:name "newest first, then the boundary")
                 (assert-equal #:expect "2026-01-01_a.sql"
                               #:got (assq-ref (caddr plan) 'name)
                               #:name "which is the migration without a script")))
         (test "--all with nothing reversible is a boundary alone"
               #:shuffle? #f
               (let ((plan (rollback-plan rollback-files
                                          (list (entry "2026-01-01_a.sql" "a"))
                                          #:all? #t #:read-file no-directive)))
                 (assert-length #:expect 1 #:got plan)
                 (assert-equal #:expect 'boundary
                               #:got (assq-ref (car plan) 'action)
                               #:name "the newest migration is the wall")))
         (test "--to leaves the target applied and undoes what is above it"
               #:shuffle? #f
               (let ((plan (rollback-plan rollback-files (rollback-history)
                                          #:to-version "2026-01-01_a"
                                          #:read-file no-directive)))
                 (assert-equal #:expect '("2026-01-03_c.sql" "2026-01-02_b.sql")
                               #:got (map (lambda (e) (assq-ref e 'name)) plan)
                               #:name "newest first, target excluded"))
               (assert-length
                #:expect 0
                #:got (rollback-plan rollback-files (rollback-history)
                                     #:to-version "2026-01-03_c"
                                     #:read-file no-directive)
                #:name "a target that is already the newest undoes nothing")
               (assert-equal
                #:expect #t
                #:got (mentions? (error-message
                                  (lambda ()
                                    (rollback-plan rollback-files
                                                   (rollback-history)
                                                   #:to-version "1999-01-01"
                                                   #:read-file no-directive)))
                                 (list "--to 1999-01-01" "not an applied"))
                #:name "and a target that was never applied is refused"))
         (test "more steps than there are migrations is refused"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (mentions? (error-message
                                               (lambda ()
                                                 (rollback-plan
                                                  rollback-files (rollback-history)
                                                  #:steps 9
                                                  #:read-file no-directive)))
                                              (list "--steps 9" "more migrations"))
                             #:name "rather than quietly undoing fewer"))
         (test "a file marked irreversible is blocked, until it is not"
               #:shuffle? #f
               (let* ((files (list (migration-file "/m/2026-01-03_c.sql")
                                   (migration-file "/m/2026-01-03_c.undo.sql")))
                      (history (list (entry "2026-01-03_c.sql" "c")))
                      (marked (lambda (path)
                                (if (string-contains path "2026-01-03_c.sql")
                                    "-- mutastructura: irreversible\nCREATE TABLE c (id INTEGER);\n"
                                    "")))
                      (blocked (rollback-plan files history #:read-file marked))
                      (allowed (rollback-plan files history
                                              #:allow-irreversible? #t
                                              #:read-file marked)))
                 (assert-equal #:expect 'blocked
                               #:got (assq-ref (car blocked) 'action)
                               #:name "the directive stops the default")
                 (assert-equal #:expect "marked irreversible"
                               #:got (assq-ref (car blocked) 'reason)
                               #:name "and says why")
                 (assert-equal #:expect 'undo
                               #:got (assq-ref (car allowed) 'action)
                               #:name "while the explicit override runs it")))
         (test "a recorded failure blocks its own rollback"
               #:shuffle? #f
               ;; The undo script may be fine, but the ledger is not: repair is
               ;; the command for that state, not rollback.
               (let* ((history (list (list (cons 'name "2026-01-03_c.sql")
                                           (cons 'checksum "c")
                                           (cons 'kind 'versioned)
                                           (cons 'success 0))))
                      (plan (rollback-plan rollback-files history
                                           #:read-file no-directive)))
                 (assert-equal #:expect 'blocked
                               #:got (assq-ref (car plan) 'action))
                 (assert-equal #:expect "records a failure; run repair first"
                               #:got (assq-ref (car plan) 'reason))))
         (test "repeatable migrations are not rollback candidates"
               #:shuffle? #f
               ;; They are re-run by `migrate' when their contents change, so
               ;; rolling back a database that holds only repeatables is a no-op.
               (let ((history (list (list (cons 'name
                                                "2026-01-01_seed.repeatable.sql")
                                          (cons 'checksum "x")
                                          (cons 'kind 'repeatable)
                                          (cons 'success 1)))))
                 (assert-length #:expect 0
                                #:got (rollback-plan rollback-files history
                                                     #:read-file no-directive)
                                #:name "a repeatable is left for migrate to re-run")
                 (assert-length #:expect 0
                                #:got (rollback-plan rollback-files '()
                                                     #:read-file no-directive)
                                #:name "and an empty database has nothing to undo")))
         (test "--all is not combined with --steps or --to"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (mentions? (error-message
                                               (lambda ()
                                                 (rollback-plan
                                                  rollback-files (rollback-history)
                                                  #:all? #t #:steps 1
                                                  #:read-file no-directive)))
                                              (list "--all" "cannot be combined"))
                             #:name "the alternatives are named"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               engine-suite
               url-suite
               migration-suite
               placeholder-suite
               ledger-suite
               planning-suite
               rollback-suite))
