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
;; This is the functional core of mutastructura: everything here is a pure
;; function of its arguments and touches neither the filesystem nor a database
;; connection -- reading a file is passed in as `read-file' where it matters, so
;; the planning logic can be tested without a single I/O call.
;;
;; The three ideas it implements are:
;;
;;   1. an *engine registry* (which databases we speak to, and what a
;;      connection to them looks like),
;;   2. *discovery* (which files in a directory are migrations, what version
;;      each one carries and in what order they must run),
;;   3. *planning* (given the ledger and the files, what should happen next).
;;
;; Planning is deliberately separated from execution: `info', `validate',
;; `migrate', `baseline' and `undo' are all the same plan read in different
;; ways, which is why one planner serves the whole command set.

(define-module (mutastructura mutastructura)
  #:declarative? #t
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 ftw)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt base16)
  #:export (;; --- engines ---
            engines
            find-engine
            parse-engine
            parse-db-type
            engine?
            engine-id
            engine-family
            engine-driver
            engine-aliases
            engine-default-port
            engine-default-history-name
            engine-migration-extensions
            engine-resource-subdirectory
            engine-description
            relational-engine?
            supported-engine-ids
            ;; --- connection strings and URLs ---
            connection-spec?
            connection-spec-engine
            connection-spec-connection
            connection-spec-database
            connection-spec-host
            connection-spec-port
            connection-spec-user
            connection-spec-password
            connection-spec-params
            connection-spec-url
            connection-spec-options
            describe-connection-spec
            or-empty
            parse-connection-url
            dbi-connection-string
            ;; --- runners ---
            runner?
            make-runner
            runner-family
            runner-description
            runner-open
            runner-close!
            runner-history
            runner-ensure-history!
            runner-record!
            runner-forget!
            runner-apply!
            runner-begin!
            runner-commit!
            runner-rollback!
            runner-clean!
            runner-statements
            runner-transactional?
            ;; --- discovery ---
            file-sha256
            scandir-for-sql
            scandir-for-migrations
            migration-file?
            migration-file
            migration-files
            migration-name
            migration-path
            migration-version
            migration-description
            migration-kind
            migration-order-key
            migration-checksum
            migration-contents
            read-migration-text
            strip-bom
            basename-without-extension
            trim-whitespace
            numeric-version-key
            history-orphans
            newest-applied-version
            within-target?
            ;; --- placeholders ---
            parse-placeholder-specs
            parse-placeholder-string
            apply-placeholders
            placeholder-syntax?
            ;; --- hashing and helpers ---
            whitespace-only?
            first-line
            flatten
            flatten-1
            nil?
            string-split-once
            percent-decode
            ;; --- ledger and planning ---
            history-entry?
            history-entry-name
            history-entry-checksum
            history-entry-description
            history-entry-kind
            history-entry-installed-on
            history-entry-installed-by
            history-entry-execution-time
            history-entry-success
            history-entry->migration-file
            get-unapplied-migrations
            plan-migrations
            plan-problems
            plan-applicable
            repair-candidates
            rollback-candidates
            rollback-plan
            migration-irreversible?
            plan-summary
            unapplied-migrations
            pending-files
            checksum-mismatches))

;;; ============================================================================
;;; 1. Engines
;;; ============================================================================

;; A database we know how to talk to.  `family' is what the orchestration layer
;; switches on to pick a runner -- `relational' goes through guile-dbi and speaks
;; SQL -- while `id' is what a user types.
;;
;; `driver' is separate from `id' on purpose, and it is what makes a database we
;; do not ship a driver for usable anyway.  CockroachDB, YugabyteDB and
;; TimescaleDB all speak the PostgreSQL wire protocol, so guile-dbi's `postgresql'
;; driver connects to them; only the dialect differs, and only in small ways
;; (CockroachDB has no advisory locks, for instance).  An engine is therefore a
;; *dialect* plus a driver, and `--database cockroachdb' can mean something
;; without a new C library anywhere.
;;
;; `history-name' is the ledger's default name.  It is a per-engine default
;; rather than a constant because every engine has its own idea of what a table
;; may look like, so a name that suits one need not suit the next.
(define-record-type <engine>
  (make-engine id family driver aliases migration-extensions default-port
               default-history-name resource-subdirectory description)
  engine?
  (id engine-id)
  (family engine-family)
  ;; The name of the guile-dbi module that speaks to this engine.
  (driver engine-driver)
  (aliases engine-aliases)
  (migration-extensions engine-migration-extensions)
  (default-port engine-default-port)
  (default-history-name engine-default-history-name)
  (resource-subdirectory engine-resource-subdirectory)
  (description engine-description))

(define (relational-engine? engine)
  "Whether ENGINE is spoken to with SQL over guile-dbi."
  (eq? (engine-family engine) 'relational))

;; A runner is the whole of what the command layer knows about a database: the
;; operations it needs, as procedures, so that `migrate', `info', `validate',
;; `baseline', `repair' and `clean' can be written once instead of once per
;; engine.  Every operation takes the session the runner's own `open' returned,
;; and the session is opaque to everybody else -- which is what lets a
;; guile-dbi handle live behind the same commands.
(define-record-type <runner>
  (make-runner family description open close! history ensure-history!
               record! forget! apply! begin! commit! rollback! clean!
               statements transactional?)
  runner?
  (family runner-family)
  (description runner-description)
  ;; (connection-spec options) -> session
  (open runner-open)
  (close! runner-close!)
  ;; session -> list of ledger entries
  (history runner-history)
  (ensure-history! runner-ensure-history!)
  ;; (session entry) -> void; an entry is an alist of the ledger's fields
  (record! runner-record!)
  (forget! runner-forget!)
  ;; (session migration options) -> number of statements that ran
  (apply! runner-apply!)
  (begin! runner-begin!)
  (commit! runner-commit!)
  (rollback! runner-rollback!)
  (clean! runner-clean!)
  ;; (migration options) -> list of statements, without touching a database
  (statements runner-statements)
  ;; whether a failed migration's changes can be undone by the engine
  (transactional? runner-transactional?))

;; Every engine mutastructura can connect to, in the order help lists them.
;;
;; The aliases are the names people actually type, which is why they are worth
;; listing: `--database mariadb' and `--database percona' are the same request as
;; `--database mysql', and a PostgreSQL-compatible database such as YugabyteDB or
;; TimescaleDB is reached through the PostgreSQL driver rather than through a
;; driver of its own.
(define engines
  (list (make-engine 'sqlite3
                     'relational
                     'sqlite3
                     '(sqlite sqlite-3 sqlite3-embedded)
                     '(".sql" ".sqlite.sql")
                     0
                     "migrations_history"
                     "sqlite3"
                     "Embedded, serverless. The connection is the database file.")
        (make-engine 'postgresql
                     'relational
                     'postgresql
                     '(postgres pg psql pgsql yugabyte yugabytedb timescaledb
                       citus aurorapg aurora-postgresql)
                     '(".sql")
                     5432
                     "migrations_history"
                     "postgresql"
                     "PostgreSQL. Transactional DDL: a failed migration rolls back.")
        (make-engine 'cockroachdb
                     'relational
                     'postgresql
                     '(cockroach crdb)
                     '(".sql")
                     26257
                     "migrations_history"
                     "postgresql"
                     "CockroachDB, through the PostgreSQL driver. Serializable, no advisory locks.")
        (make-engine 'mysql
                     'relational
                     'mysql
                     '(mariadb mysql-mariadb percona auroramysql aurora-mysql)
                     '(".sql")
                     3306
                     "migrations_history"
                     "mysql"
                     "MySQL and MariaDB. Note: DDL here commits implicitly.")))

(define (find-engine name)
  "Return the engine NAME denotes, or #f.

NAME is matched (case-insensitively) against every engine's id and aliases, and
against its resource subdirectory, so `--database postgres', `--database PG'
and `--database postgresql' are the same request."
  (let* ((needle (string-downcase (if (symbol? name)
                                      (symbol->string name)
                                      (format #f "~a" name)))))
    (find (lambda (engine)
            (or (string=? needle (symbol->string (engine-id engine)))
                (string=? needle (engine-resource-subdirectory engine))
                (any (lambda (alias)
                       (string=? needle (symbol->string alias)))
                     (engine-aliases engine))))
          engines)))

(define (parse-engine database)
  "Resolve DATABASE, a symbol or string, to an <engine>.

Raises a helpful exception when DATABASE is not an engine we know, naming the
ones we do know, because a typo in a command line is the single most common
failure this tool sees."
  (or (find-engine database)
      (raise-exception
       (make-exception-with-message
        (format #f "UNKNOWN DATABASE TYPE: ~a
   known engines: ~a"
                database
                (string-join (map (lambda (e)
                                    (string-join (map symbol->string
                                                      (cons (engine-id e)
                                                            (engine-aliases
                                                             e)))
                                                  "/"))
                                  engines)
                             ", "))))))

(define (parse-db-type database)
  "Resolve DATABASE to the name guile-dbi's `dbi-open' wants.

That is the engine's *driver*, not its id, which is the whole point of keeping
both: `--database timescaledb' is opened with the `postgresql' driver, and
`--database mariadb' with the `mysql' one."
  (symbol->string (engine-driver (parse-engine database))))

(define (supported-engine-ids)
  "The canonical ids of every engine, as a list of strings."
  (map (lambda (engine)
         (symbol->string (engine-id engine)))
       engines))

;;; ============================================================================
;;; 2. Connection strings and URLs
;;; ============================================================================

;; A parsed connection: where to connect, as what, and which database.  Keeping
;; this as a record rather than a plist means `--url' and a bare `--connection'
;; produce the same shape, and the runners never see the difference.
(define-record-type <connection-spec>
  (make-connection-spec engine connection database host port user password
                        params url)
  connection-spec?
  (engine connection-spec-engine)
  (connection connection-spec-connection)
  (database connection-spec-database)
  (host connection-spec-host)
  (port connection-spec-port)
  (user connection-spec-user)
  (password connection-spec-password)
  (params connection-spec-params)
  (url connection-spec-url))

(define (or-empty text)
  "TEXT when there is a TEXT, and the empty string when there is not.

Guile's `format' has no `~@[' -- the conditional that keeps its own argument --
and `~:[~;...~]' needs a boolean and reads badly next to a sentence.  One helper
says 'print this when there is something to print' at every call site that wants
it, which is what an optional part of an error message is."
  (if (or (not text) (eq? text #nil) (equal? text ""))
      ""
      (format #f "~a" text)))

(define (describe-connection-spec spec)
  "A one-line, password-free description of SPEC, for logs and `info' output."
  (string-append
   (format #f "~a" (or (connection-spec-engine spec) 'unknown))
   ;; The engine id alone is ambiguous for a database reached through another
   ;; driver -- CockroachDB through the PostgreSQL one -- so the driver is named
   ;; when it differs.
   (let ((engine (connection-spec-engine spec)))
     (if (and engine (not (eq? (engine-id engine) (engine-driver engine))))
         (format #f "(via ~a)" (engine-driver engine))
         ""))
   (or-empty (and (connection-spec-user spec)
                  (format #f " //~a" (connection-spec-user spec))))
   (or-empty (and (connection-spec-host spec)
                  (format #f " @~a" (connection-spec-host spec))))
   (or-empty (and (connection-spec-port spec)
                  (format #f " :~a" (connection-spec-port spec))))
   (or-empty (and (connection-spec-database spec)
                  (format #f " /~a" (connection-spec-database spec))))))

(define (connection-spec-options spec)
  "The query-string parameters of SPEC as an alist of string to string."
  (or (connection-spec-params spec) '()))

(define (scheme-of url)
  "The scheme of URL in lowercase, or #f when it has none."
  (let ((colon (string-index url #\:)))
    (and colon
         ;; A Windows path such as `C:\db.sqlite' has a colon too, but nothing
         ;; after it looks like a URL, so require `:' to be followed by `//' or
         ;; by a scheme-shaped remainder (`sqlite:/tmp/x.db' is legal).
         (> colon 0)
         (let ((scheme (substring url 0 colon)))
           (and (string-every (lambda (c)
                                (or (char-alphabetic? c)
                                    (char-numeric? c)
                                    (memv c '(#\+ #\- #\.))))
                              scheme)
                (string-downcase scheme))))))

(define (percent-decode s)
  "Decode the %XX escapes in S, treating them as the UTF-8 bytes they are."
  (let* ((bytes (string->utf8 s))
         (n (bytevector-length bytes)))
    (define (hex-digit b)
      (cond
        ((and (>= b 48) (<= b 57)) (- b 48))
        ((and (>= b 97) (<= b 102)) (+ 10 (- b 97)))
        ((and (>= b 65) (<= b 70)) (+ 10 (- b 65)))
        (else #f)))
    (let loop ((i 0) (acc '()))
      (if (>= i n)
          (utf8->string (u8-list->bytevector (reverse acc)))
          (let ((b (bytevector-u8-ref bytes i)))
            (if (and (= b 37) (<= (+ i 2) (- n 1)))
                (let ((hi (hex-digit (bytevector-u8-ref bytes (+ i 1))))
                      (lo (hex-digit (bytevector-u8-ref bytes (+ i 2)))))
                  (if (and hi lo)
                      (loop (+ i 3) (cons (+ (* 16 hi) lo) acc))
                      (loop (+ i 1) (cons b acc))))
                (loop (+ i 1) (cons b acc))))))))

(define (string-split-once s ch)
  "Split S at the first CH into a pair, or #f when CH is absent."
  (let ((i (string-index s ch)))
    (and i
         (cons (substring s 0 i) (substring s (+ i 1))))))

(define (trim-slash s)
  "S with every leading and trailing `/` removed."
  (let loop ((s s))
    (cond
      ((and (> (string-length s) 0)
            (char=? (string-ref s 0) #\/))
       (loop (substring s 1)))
      ((and (> (string-length s) 0)
            (char=? (string-ref s (- (string-length s) 1)) #\/))
       (loop (substring s 0 (- (string-length s) 1))))
      (else s))))

(define (parse-query-params query)
  "Turn a URL query string into an alist of string to string."
  (if (or (not query) (string=? query ""))
      '()
      (filter-map (lambda (pair)
                    (let ((split (string-split-once pair #\=)))
                      (if split
                          (cons (percent-decode (car split))
                                (percent-decode (cdr split)))
                          (cons (percent-decode pair) ""))))
                  (delete "" (string-split query #\&)))))

(define (split-authority authority)
  "Split `user:pass@host:port' into user, password, host and port."
  (let* ((at (string-rindex authority #\@))
         (userinfo (and at (substring authority 0 at)))
         (hostport (if at (substring authority (+ at 1)) authority))
         (user #f)
         (password #f))
    (when userinfo
      (let ((split (string-split-once userinfo #\:)))
        (if split
            (begin
              (set! user (percent-decode (car split)))
              (set! password (percent-decode (cdr split))))
            (set! user (percent-decode userinfo)))))
    ;; An IPv6 literal is bracketed, so its colons are not port separators.
    (if (and (> (string-length hostport) 0)
             (char=? (string-ref hostport 0) #\[))
        (let* ((close (string-index hostport #\]))
               (host (and close (substring hostport 1 close)))
               (rest (and close (substring hostport (+ close 1)))))
          (values user
                  password
                  host
                  (and rest
                       (string-prefix? ":" rest)
                       (substring rest 1))))
        (let ((split (string-split-once hostport #\:)))
          (if split
              (values user password (car split) (cdr split))
              (values user password hostport #f))))))

(define (require-sql-credential-safe! field value)
  "Refuse a VALUE for FIELD that guile-dbi's colon-delimited form cannot carry.

The PostgreSQL and MySQL drivers split the connection string on `:' with no
escaping whatsoever, so a password containing a colon cannot be expressed
through `--url'.  Saying so is far better than silently connecting as the wrong
user with half a password."
  (when (and value (string-index value #\:))
    (raise-exception
     (make-exception-with-message
      (format #f "~a contains a ':' which guile-dbi's connection string cannot escape.
   Pass --connection with the driver's own colon-delimited form instead of --url."
              field)))))

(define (dbi-connection-string engine user password database host port socket)
  "Build the connection string guile-dbi's ENGINE driver expects.

SQLite3 takes a path; PostgreSQL and MySQL take
`user:pass:db:connection-type:host-or-path[:port]'."
  ;; The *driver* decides the connection string's shape, because that is whose
  ;; parser reads it: a CockroachDB URL is a PostgreSQL connection string with a
  ;; different default port, nothing more.
  (let ((id (engine-driver engine)))
    (cond
      ((eq? id 'sqlite3)
       ;; The database *is* the path for SQLite; a URL path is always absolute
       ;; (`sqlite3:///var/lib/app.db'), and the leading slash is preserved.
       (or database (raise-exception
                     (make-exception-with-message
                      "a sqlite3 connection needs a database file path"))))
      ((memq id '(postgresql mysql))
       (require-sql-credential-safe! "the user name" user)
       (require-sql-credential-safe! "the password" password)
       (require-sql-credential-safe! "the database name" database)
       ;; An IPv6 literal is a perfectly good host and cannot be written here
       ;; either: the driver would split on its colons.
       (require-sql-credential-safe! "the host name"
                                     (and host (string-index host #\:) host))
       (let ((type (or (and socket 'socket) 'tcp))
             (location (or socket host "localhost"))
             (port* (or port (and (not socket) (engine-default-port engine)))))
         (string-join (filter-map (lambda (x) (and x (format #f "~a" x)))
                                  (if (eq? type 'socket)
                                      (list (or user "") (or password "") database
                                            "socket" location)
                                      (list (or user "") (or password "") database
                                            "tcp" location port*)))
                      ":")))
      (else (raise-exception
             (make-exception-with-message
              (format #f "~a has no guile-dbi connection string form"
                      (engine-id engine))))))))


(define* (parse-connection-url url #:key (engine #f))
  "Parse URL into a <connection-spec>.

The accepted shapes are the ones a user already knows from other migration
tools:

@example
sqlite3:///var/lib/app.db
sqlite3://./local.db
postgresql://alice:secret@db.example.net:5432/appdb
mysql://alice:secret@127.0.0.1:3306/appdb
@end example

A string whose scheme is not a known engine is taken at face value: the result
carries ENGINE (or #f) and the string as the connection, exactly what
@code{--connection} gives you.  That is deliberate, because guile-dbi's own
connection strings look like `alice:secret:appdb:tcp:host:5432' and must not be
mistaken for a URL with the scheme `alice'.

A MySQL or PostgreSQL URL is *translated* into guile-dbi's colon-delimited form.
That form cannot escape a colon, so a user, password or database name containing
one is rejected with an explanation rather than silently truncated."
  (let ((scheme (scheme-of url)))
    (cond
      ((and scheme (find-engine scheme))
       (let* ((rest (substring url (+ (string-length scheme) 1)))
              ;; Strip the `//' of an authority-based URL; tolerate its absence
              ;; because `sqlite3:/tmp/x.db' and `sqlite3:///tmp/x.db' are both
              ;; spellings people write.
              (rest (if (string-prefix? "//" rest) (substring rest 2) rest))
              (query-split (or (string-split-once rest #\?)
                               (cons rest #f)))
              (before-query (car query-split))
              (query (cdr query-split))
              (params (parse-query-params query))
              (engine* (find-engine scheme)))
         (if (eq? (engine-id engine*) 'sqlite3)
             ;; SQLite has no authority: everything after the scheme is the
             ;; path, verbatim, so `sqlite3:///abs.db' is absolute and
             ;; `sqlite3://./rel.db' is relative to the working directory.
             (make-connection-spec engine* rest rest #f #f #f #f params url)
             (let* ((path-split (or (string-split-once before-query #\/)
                                    (cons before-query #f)))
                    (authority (car path-split))
                    (path (cdr path-split))
                    (database (and path (trim-slash path))))
               (receive (user password host port)
                   (split-authority authority)
                 (let* ((with-socket (assoc-ref params "socket"))
                        (with-host (assoc-ref params "host"))
                        (host* (if (or (not host) (string=? host ""))
                                   with-host host))
                        (port* (and port (string->number port)))
                        (connection (dbi-connection-string engine*
                                                           user password
                                                           database
                                                           host* port*
                                                           with-socket)))
                   (make-connection-spec engine* connection database host*
                                         port* user password params
                                         url)))))))
      (else (make-connection-spec engine url #f #f #f #f #f '() url)))))

;;; ============================================================================
;;; 3. Discovery: what is a migration file, and in what order do they run
;;; ============================================================================

(define (file-sha256 path)
  "Compute the SHA-256 of a file and return a lowercase hexadecimal string."
  (call-with-input-file path
    (lambda (port)
      (string-downcase (bytevector->base16-string (port-sha256 port))))))

(define (strip-bom text)
  "Drop a leading UTF-8 byte-order mark from TEXT, if there is one.

Editors on Windows love to write one, and a `\\ufeff' in front of `CREATE' is
syntax SQL itself cannot forgive."
  (if (and (> (string-length text) 0)
           (char=? (string-ref text 0) #\xfeff))
      (substring text 1)
      text))

;; Longest first: `.repeatable.sql' has to be tried before `.sql'.
(define migration-suffixes
  '(".repeatable.sql" ".undo.sql" ".sql"))

(define (basename-without-extension name)
  "NAME with its migration extension and repeatable/undo marker removed.

So `2026-04-01_21:54:00_views.repeatable.sql' becomes
`2026-04-01_21:54:00_views', which is the version `--target' compares against and
the name `info' prints."
  (let* ((bare (basename name))
         (match (find (lambda (suffix)
                        (string-suffix? suffix bare))
                      migration-suffixes)))
    (if match
        (substring bare 0 (- (string-length bare) (string-length match)))
        bare)))

(define (basename-without-extension* bare)
  "BARE without its extension, when the kebab markers are already gone."
  (basename-without-extension bare))

(define (numeric-version-key version)
  "A sortable zero-padded key for a dotted numeric VERSION, or #f.

`1.2.10' must sort after `1.2.9', which plain string comparison gets wrong, so
each component is padded before comparison."
  (and (string-every (lambda (c)
                       (or (char-numeric? c) (char=? c #\.)))
                     version)
       (> (string-length version) 0)
       (string-join (map (lambda (part)
                           (if (= (string-length part) 0)
                               part
                               (string-append (make-string (max 0 (- 10
                                                                   (string-length
                                                                    part)))
                                                           #\0)
                                              part)))
                         (string-split version #\.))
                    ".")))

(define (migration-file path)
  "Describe the migration file at PATH as an alist.

The alist is the unit every later stage works with:

@example
((name . \"2026-04-01_21:54:00_add-user.sql\")
 (path . \"./migrations/app/2026-04-01_21:54:00_add-user.sql\")
 (version . \"2026-04-01_21:54:00_add-user\")
 (description . \"2026-04-01_21:54:00_add-user\")
 (kind . versioned)
 (order-key . \"2026-04-01_21:54:00_add-user\"))
@end example

Two naming conventions are recognised.  Mutastructura's own
`YYYY-MM-DD_HH:MM:SS_description.sql' (ordered by file name), and Flyway's
`V1.2__description.sql' / `R__description.sql' / `U1.2__description.sql'
(ordered numerically).  A name ending in `.repeatable.sql' is repeatable even
when it does not use the `R__' prefix, and `.undo.sql' marks a rollback script."
  (let* ((name (basename path))
         (repeatable? (repeatable-name? name))
         (undo? (undo-name? name))
         (flyway (or (string-match "^V([0-9][0-9._-]*)__(.+)$" name)
                     (string-match "^R__(.+)$" name)
                     (string-match "^U([0-9][0-9._-]*)__(.+)$" name)))
         (kind (cond
                 (undo? 'undo)
                 (repeatable? 'repeatable)
                 ((string-prefix? "R__" name) 'repeatable)
                 ((string-prefix? "U" name) 'undo)
                 ((string-prefix? "V" name) 'versioned)
                 (else 'versioned))))
    (cond
      ;; Flyway's `V1.2__add user.sql': the version is the number, and the
      ;; description is what follows the double underscore.
      ((and flyway (string-prefix? "V" name))
       (let* ((version (string-replace-substring (match:substring flyway 1)
                                                 "_" "."))
              (description (basename-without-extension*
                            (match:substring flyway 2))))
         (list (cons 'name name)
               (cons 'path path)
               (cons 'version version)
               (cons 'description description)
               (cons 'kind kind)
               (cons 'order-key (or (numeric-version-key version) version)))))
      ((and flyway (string-prefix? "U" name))
       (let* ((version (string-replace-substring (match:substring flyway 1)
                                                 "_" "."))
              (description (basename-without-extension*
                            (match:substring flyway 2))))
         (list (cons 'name name)
               (cons 'path path)
               (cons 'version version)
               (cons 'description description)
               (cons 'kind 'undo)
               (cons 'order-key (or (numeric-version-key version) version)))))
      ((and flyway (string-prefix? "R__" name))
       (let ((description (basename-without-extension*
                           (match:substring flyway 1))))
         (list (cons 'name name)
               (cons 'path path)
               (cons 'version name)
               (cons 'description description)
               (cons 'kind 'repeatable)
               (cons 'order-key name))))
      (else
       (let ((version (basename-without-extension name)))
         (list (cons 'name name)
               (cons 'path path)
               (cons 'version version)
               (cons 'description version)
               (cons 'kind kind)
               (cons 'order-key version)))))))

(define (repeatable-name? name)
  "Whether NAME marks a repeatable migration."
  (string-suffix? ".repeatable.sql" name))

(define (undo-name? name)
  "Whether NAME marks an undo (rollback) script."
  (string-suffix? ".undo.sql" name))

(define (migration-name file) (assq-ref file 'name))
(define (migration-path file) (assq-ref file 'path))
(define (migration-version file) (assq-ref file 'version))
(define (migration-description file) (assq-ref file 'description))
(define (migration-kind file) (assq-ref file 'kind))
(define (migration-order-key file) (assq-ref file 'order-key))

(define (migration-file? engine name)
  "Whether NAME, a file in a migrations directory, is a migration for ENGINE."
  (any (lambda (extension)
         (string-suffix? extension name))
       (engine-migration-extensions engine)))

(define* (scandir-for-migrations path #:key (engine #f) (extensions #f))
  "Return the migration files in PATH, sorted by their order key.

Files are discovered by extension (`--engine' decides which extensions count),
which means a stray note in the directory is ignored instead of becoming a
failed migration."
  (let* ((extensions* (or extensions
                          (and engine (engine-migration-extensions engine))
                          '(".sql")))
         ;; `scandir' takes a comparison *procedure* as its third argument, not
         ;; a flag: `#f' there is a call to `#f' and an exception.  Name order
         ;; is the pre-sort because the real order -- the migration's own -- is
         ;; decided below.
         (names (scandir path
                         (lambda (name)
                           (any (lambda (extension)
                                  (string-suffix? extension name))
                                extensions*))
                         string<?)))
    (when (not names)
      (raise-exception
       (make-exception-with-message
        (format #f "no migrations directory at ~a" path))))
    (sort (map (lambda (name)
                 (migration-file (format #f "~a/~a" path name)))
               names)
          (lambda (a b)
            (string<? (migration-order-key a) (migration-order-key b))))))

(define (scandir-for-sql path)
  "Every `*.sql' file in PATH, sorted by name.

Kept for compatibility; new code wants `scandir-for-migrations'."
  (map (lambda (name)
         (format #f "~a/~a" path name))
       (scandir path
                (lambda (name)
                  (string-suffix? ".sql" name))
                string<?)))

(define (migration-checksum path)
  "The SHA-256 of the file at PATH."
  (file-sha256 path))

(define* (migration-contents path #:key (placeholders '()) (read-file #f))
  "Read the migration at PATH and substitute PLACEHOLDERS into it.

READ-FILE, when given, replaces `call-with-input-file'/`get-string-all', which
is how the planning tests avoid touching the disk."
  (let ((text (if read-file
                  (read-file path)
                  (call-with-input-file path get-string-all))))
    (apply-placeholders (strip-bom text) placeholders)))

(define (read-migration-text path)
  "The text of the migration at PATH, BOM removed."
  (strip-bom (call-with-input-file path get-string-all)))

;;; ============================================================================
;;; 4. Placeholders
;;; ============================================================================

(define (placeholder-syntax? text)
  "Whether TEXT contains a `${name}' placeholder at all."
  (and (string? text)
       (string-contains text "${")
       (string-contains text "}")))

(define (parse-placeholder-specs specs)
  "Turn a list of `key=value' strings into an alist of string to string.

Raises when a spec has no `=', because a silently ignored placeholder is a
migration that runs with the wrong schema name in it."
  (map (lambda (spec)
         (let ((split (string-split-once spec #\=)))
           (if split
               (cons (trim-whitespace (car split))
                     (trim-whitespace (cdr split)))
               (raise-exception
                (make-exception-with-message
                 (format #f "placeholder ~s is not of the form key=value" spec))))))
       specs))

(define (parse-placeholder-string spec)
  "Turn a comma-separated `key=value,key=value' string into an alist."
  (if (or (not spec) (string=? (trim-whitespace spec) ""))
      '()
      (parse-placeholder-specs (string-split spec #\,))))

(define* (apply-placeholders text placeholders)
  "Substitute every `${key}' in TEXT with its value from PLACEHOLDERS.

Unknown placeholders are left exactly as they were, so a migration may contain
`${...}' syntax that belongs to somebody else (a shell script inside a
comment, a template table) without mutastructura mangling it."
  (if (or (null? placeholders) (not (placeholder-syntax? text)))
      text
      (fold (lambda (pair acc)
              (let ((key (car pair)))
                (if (or (not (string? key)) (string=? key ""))
                    acc
                    (%replace-placeholder acc key (format #f "~a" (cdr pair))))))
            text
            placeholders)))

(define (%replace-placeholder text key value)
  "Replace every `${KEY}' in TEXT with VALUE."
  (let ((needle (string-append "${" key "}")))
    (let loop ((start 0) (acc '()))
      (let ((hit (string-contains text needle start)))
        (if (not hit)
            (apply string-append (reverse (cons (substring text start) acc)))
            (loop (+ hit (string-length needle))
                  (cons value
                        (cons (substring text start hit) acc))))))))

;;; ============================================================================
;;; 5. Small helpers
;;; ============================================================================

(define (whitespace-only? str)
  (string-every char-whitespace? str))

(define (trim-whitespace s)
  "S with whitespace removed from both ends.

Guile's own `string-trim' is a *left* trim and `string-trim-right' a right one,
so trimming the two ends of a statement takes both.  This matters more than it
sounds: a trailing newline left on a line is the difference between recognising
`DELIMITER $$' and not."
  (if (string? s)
      (string-trim-right (string-trim s))
      s))

(define (first-line text)
  "TEXT's first line worth reading, for a one-line report of something long.

The long thing here is a statement: a migration's `CREATE FUNCTION' is forty
lines, and the line that says what it does is somewhere near the top.  Blank
lines and `--' comments are skipped on the way there, because a statement that
opens with a comment would otherwise be reported as its comment; anything past
90 characters is elided, because a log line that wraps is read by nobody."
  (let loop ((lines (string-split (or (and (string? text) text) "") #\newline)))
    (if (null? lines)
        ""
        (let ((line (trim-whitespace (car lines))))
          (if (or (string=? line "")
                  (string-prefix? "--" line)
                  (string-prefix? "//" line))
              (loop (cdr lines))
              (if (> (string-length line) 90)
                  (string-append (substring line 0 87) "...")
                  line))))))

(define (nil? x)
  "Whether X is null or the empty list."
  (or (null? x) (eq? x #nil)))

(define (flatten x)
  (cond
    ((null? x)
     '())
    ((pair? x)
     (append (flatten (car x))
             (flatten (cdr x))))
    (else (list x))))

(define (flatten-1 lst)
  (fold-right (lambda (x acc)
                (if (list? x)
                    (append x acc)
                    (cons x acc)))
              '() lst))

;;; ============================================================================
;;; 6. The ledger
;;; ============================================================================

;; A row of the history table, normalised across engines: however a driver
;; answered, the rest of the program sees these keys.
(define (history-entry? x)
  (and (list? x)
       (assq 'name x)))

(define (history-entry-name entry) (assq-ref entry 'name))
(define (history-entry-checksum entry) (or (assq-ref entry 'checksum) ""))
(define (history-entry-description entry)
  (or (assq-ref entry 'description) (assq-ref entry 'name)))
(define (history-entry-kind entry)
  (let ((kind (assq-ref entry 'kind)))
    (cond
      ((symbol? kind) kind)
      ((string? kind) (string->symbol kind))
      ((and (string? (history-entry-name entry))
            (repeatable-name? (history-entry-name entry)))
       'repeatable)
      (else 'versioned))))
(define (history-entry-installed-on entry)
  (or (assq-ref entry 'installed-on) (assq-ref entry 'applied-at)))
(define (history-entry-installed-by entry)
  (or (assq-ref entry 'installed-by) "mutastructura"))
(define (history-entry-execution-time entry)
  (or (assq-ref entry 'execution-time) 0))
(define (history-entry-success entry)
  ;; A ledger written before the `success' column existed has no such field, and
  ;; a migration that is *in* the ledger is one that ran: reading the absent
  ;; field as a failure would report every old deployment as broken and hold its
  ;; migrations back.  Only an explicit #f -- or a SQL ledger's 0 -- is one.
  (let ((success (assq 'success entry)))
    (cond
      ((not success) #t)
      ((eq? (cdr success) #f) #f)
      ((number? (cdr success)) (not (zero? (cdr success))))
      (else #t))))

(define (history-entry->migration-file entry)
  "Present a ledger ENTRY as a migration file alist, for reporting."
  (let ((version (basename-without-extension (history-entry-name entry))))
    (list (cons 'name (history-entry-name entry))
          (cons 'path #f)
          (cons 'version version)
          (cons 'description (history-entry-description entry))
          (cons 'kind (history-entry-kind entry))
          (cons 'order-key version)
          (cons 'checksum (history-entry-checksum entry))
          (cons 'applied? #t))))

(define (history-by-name history)
  "Index HISTORY by migration name."
  (map (lambda (entry)
         (cons (history-entry-name entry) entry))
       history))

;;; ============================================================================
;;; 7. Planning
;;; ============================================================================

;; A state is what the plan says about one migration:
;;
;;   pending              on disk, not in the ledger                  -> apply
;;   applied              in both, checksums agree                    -> skip
;;   changed              in both, checksums differ                   -> warn/validate
;;   repeatable-pending   repeatable, never applied or changed        -> re-apply
;;   repeatable-unchanged repeatable, checksum matches the ledger     -> skip
;;   missing              in the ledger, not on disk                  -> warn/validate
;;   superseded           pending, but older than one already applied -> apply only with
;;                                                                      --out-of-order
;;   undo                 an undo script; never run by `migrate'
(define (migration-state file history-index)
  "The state of FILE against HISTORY-INDEX, an alist of name to ledger entry."
  (let* ((kind (migration-kind file))
         (name (migration-name file))
         (entry (assoc-ref history-index name)))
    (cond
      ((eq? kind 'undo) 'undo)
      ((eq? kind 'repeatable)
       (cond
         ((not entry) 'repeatable-pending)
         ((equal? (history-entry-checksum entry) (migration-checksum
                                                  (migration-path file)))
          'repeatable-unchanged)
         (else 'repeatable-pending)))
      ((and entry (not (history-entry-success entry)))
       ;; The ledger holds a failure for this migration: an interrupted run on an
       ;; engine without transactional DDL.  It has to be attempted again, so it
       ;; is pending -- `info' shows the recorded failure, and `repair' forgets
       ;; it, which is what Flyway's repair does with the same situation.
       'pending)
      (entry
       (if (equal? (history-entry-checksum entry) (migration-checksum
                                                   (migration-path file)))
           'applied
           'changed))
      (else 'pending))))

(define (history-orphans files history)
  "Ledger entries whose file is no longer in FILES."
  (let ((names (map migration-name files)))
    (filter (lambda (entry)
              (not (member (history-entry-name entry) names)))
            history)))

(define (newest-applied-version history)
  "The order key of the newest versioned migration in the ledger, or #f."
  (let ((keys (filter-map (lambda (entry)
                            (and (eq? (history-entry-kind entry) 'versioned)
                                 (let ((key (basename-without-extension
                                             (history-entry-name entry))))
                                   key)))
                          history)))
    (and (pair? keys) (car (sort keys string>?)))))

(define* (plan-migrations files history #:key (target #f) (out-of-order? #f)
                          (validate? #f))
  "Compare the migration FILES with the ledger HISTORY and say what should happen.

The answer is one alist per migration on disk, plus one per ledger entry whose
file has disappeared, each carrying a `state' drawn from the list in the
commentary above, plus the `action' that state implies:

@example
((name . \"V2__add-user.sql\") (state . pending) (action . apply) ...)
((name . \"V1__init.sql\")     (state . applied) (action . skip)  ...)
@end example

TARGET limits the plan to versions at or below it, exactly like Flyway's
`target': the migration whose version is TARGET runs, anything above it does
not.  OUT-OF-ORDER? decides whether a pending migration older than the newest
one already applied is applied (`apply') or held back (`superseded').

Nothing here reads a ledger or a database.  It does hash the files it is
handed, because a checksum is what decides whether an applied migration still
matches its file; pass files rather than paths and nothing touches the disk."
  (let* ((history-index (history-by-name history))
         (newest (newest-applied-version history)))
    (append
     (map (lambda (file)
            (let* ((state (migration-state file history-index))
                   (entry (assoc-ref history-index (migration-name file)))
                   (within (or (not target) (within-target? file target)))
                   (superseded (and (eq? state 'pending)
                                    newest
                                    (string<? (migration-order-key file)
                                              newest)))
                   (state* (cond
                             ((and (eq? state 'pending) superseded
                                   (not out-of-order?))
                              'superseded)
                             ((and (memq state '(pending repeatable-pending))
                                   (not within))
                              'beyond-target)
                             (else state)))
                   (action (case state*
                             ((pending repeatable-pending) 'apply)
                             ((changed) (if validate? 'fail 'warn))
                             (else 'skip))))
              (append file
                      (list (cons 'checksum (migration-checksum
                                             (migration-path file)))
                            (cons 'state state*)
                            (cons 'action action)
                            (cons 'applied-on (and entry
                                                   (history-entry-installed-on
                                                    entry)))
                            (cons 'execution-time (and entry
                                                       (history-entry-execution-time
                                                        entry)))
                            (cons 'success (if entry
                                               (history-entry-success entry)
                                               #t))))))
          files)
     (map (lambda (entry)
            (append (history-entry->migration-file entry)
                    (list (cons 'state 'missing)
                          (cons 'action (if validate? 'fail 'warn))
                          (cons 'applied-on (history-entry-installed-on entry))
                          (cons 'execution-time (history-entry-execution-time
                                                 entry))
                          (cons 'success (history-entry-success entry)))))
          (history-orphans files history)))))

(define (plan-problems plan)
  "The plan entries that `validate' must refuse to accept.

Two things count as a problem: a migration whose content changed after it was
applied, and a migration the ledger knows whose file is gone.  A held-back
out-of-order migration and a recorded failure are differences in progress, not
in truth, and are reported as such rather than as validation failures."
  (filter (lambda (entry)
            (memq (assq-ref entry 'state)
                  '(changed missing)))
          plan))

(define (within-target? file target)
  "Whether FILE's version is at or below TARGET.

Two naming conventions need two comparisons.  Flyway-style versions are dotted
numbers, compared as zero-padded keys -- plain string comparison would put
`1.10' before `1.9'.  Timestamp-style versions, and versions that are not
numeric at all, are compared lexicographically, with TARGET also allowed to be a
*prefix* of the version so `--target 2026-04-01' covers every migration recorded
on that day."
  (let* ((version (migration-version file))
         (file-key (migration-order-key file))
         (target-key (or (numeric-version-key target) target)))
    (if (and (numeric-version-key version) (numeric-version-key target))
        (string<=? file-key target-key)
        (or (string<=? version target)
            (string-prefix? target version)))))

(define (plan-applicable plan)
  "The plan entries `migrate' would run, in order."
  (filter (lambda (entry)
            (eq? (assq-ref entry 'action) 'apply))
          plan))

(define (repair-candidates plan)
  "The plan entries `repair' would act on.

Two things bring a ledger and its files back into agreement: a row that
records a failure (so the migration is attempted again rather than silently
skipped), and a row whose checksum no longer matches the file it names (which
is knowingly accepting the file as it now stands)."
  (filter (lambda (entry)
            (or (not (assq-ref entry 'success))
                (eq? (assq-ref entry 'state) 'changed)))
          plan))

;;; ---------------------------------------------------------------------------
;;; Rolling back
;;; ---------------------------------------------------------------------------

;; A rollback is a forward migration's undo script, run in reverse order.  What
;; makes it smart is what it does about the migrations that cannot be reversed:
;; a requested number of steps or a requested target is refused outright rather
;; than leaving a later migration applied on top of an undone earlier one, and
;; `--all' walks back only as far as the newest run of rollback-able migrations,
;; naming the one that stopped it.

(define* (migration-irreversible? path #:optional (read-file read-migration-text))
  "Whether the migration at PATH says, in a comment, not to be rolled back.

The directive is a line beginning `-- mutastructura: irreversible' (or
`-- +mutastructura irreversible').  A data migration may have perfectly good
undo SQL and still be a change nobody wants reversed automatically; this is how
its author says so."
  (and path
       (catch #t
         (lambda ()
           (any (lambda (line)
                  (let ((line (string-downcase (trim-whitespace line))))
                    (or (string-prefix? "-- mutastructura: irreversible" line)
                        (string-prefix? "-- +mutastructura irreversible" line))))
                (string-split (read-file path) #\newline)))
         (lambda _ #f))))

(define* (rollback-candidates files history #:key (read-file read-migration-text))
  "The applied versioned migrations, newest first, with the files that undo them.

Each entry carries its `name', its `version', the forward `file' when that is
still on disk, the `undo' file when there is one, whether the file is marked
`irreversible', and what the ledger recorded about it.  Repeatable migrations
are not candidates: `migrate' re-runs them when their contents change, so a
rollback leaves them alone."
  (reverse
   (filter-map
    (lambda (entry)
      (and (eq? (history-entry-kind entry) 'versioned)
           (let* ((name (history-entry-name entry))
                  (version (basename-without-extension name))
                  (forward (find (lambda (file)
                                   (and (not (eq? (migration-kind file) 'undo))
                                        (equal? (migration-version file) version)))
                                 files))
                  (undo (find (lambda (file)
                                (and (eq? (migration-kind file) 'undo)
                                     (equal? (migration-version file) version)))
                              files)))
             (list (cons 'name name)
                   (cons 'version version)
                   (cons 'file forward)
                   (cons 'undo undo)
                   (cons 'irreversible
                         (and forward
                              (migration-irreversible?
                               (migration-path forward) read-file)
                              #t))
                   (cons 'success (history-entry-success entry))
                   (cons 'applied-on (history-entry-installed-on entry))))))
    history)))

(define (rollback-entry-state entry allow-irreversible?)
  "ENTRY with the action a rollback would take on it, and the reason.

`blocked' is a migration that cannot be reversed: it records a failure, its
author marked it irreversible, or there is no undo script beside it.  `undo' is
one that can.  A blocked entry is reported before anything runs, never skipped."
  (append entry
          (cond
            ((not (assq-ref entry 'success))
             (list (cons 'action 'blocked)
                   (cons 'reason "records a failure; run repair first")))
            ((and (assq-ref entry 'irreversible) (not allow-irreversible?))
             (list (cons 'action 'blocked)
                   (cons 'reason "marked irreversible")))
            ((not (assq-ref entry 'undo))
             (list (cons 'action 'blocked)
                   (cons 'reason "no undo script")))
            (else (list (cons 'action 'undo)
                        (cons 'reason "undo script"))))))

(define* (rollback-plan files history #:key (steps #f) (to-version #f)
                        (all? #f) (allow-irreversible? #f)
                        (read-file read-migration-text))
  "The rollback a command would run, newest first.

Three selections are possible, and they are alternatives:

@example
--steps N      undo the N newest applied migrations
--to VERSION   undo everything applied after VERSION, leaving it applied
--all          undo as far back as undo scripts allow
@end example

Without one of them, one migration is undone, which is the safe default.
`--all' stops at the first migration that cannot be reversed and returns it as a
`boundary' entry; the other selections return their entries as `undo' or
`blocked', and the caller refuses the whole rollback when anything is blocked.
An empty list means the target is already the newest applied migration."
  (when (and all? (or steps to-version))
    (raise-exception
     (make-exception-with-message
      "--all cannot be combined with --steps or --to")))
  (when (and steps to-version)
    (raise-exception
     (make-exception-with-message
      "--steps and --to are alternatives; give one of them")))
  (let* ((applied (rollback-candidates files history #:read-file read-file)))
    (define (select n)
      (map (lambda (entry)
             (rollback-entry-state entry allow-irreversible?))
           (take applied n)))
    (cond
      ((null? applied) '())
      (to-version
       (let loop ((rest applied) (n 0))
         (cond
           ((null? rest)
            (raise-exception
             (make-exception-with-message
              (format #f "--to ~a is not an applied versioned migration"
                      to-version))))
           ((equal? (assq-ref (car rest) 'version) to-version) (select n))
           (else (loop (cdr rest) (+ n 1))))))
      (all?
       (let loop ((rest applied) (acc '()))
         (if (null? rest)
             (reverse acc)
             (let ((entry (rollback-entry-state (car rest)
                                                allow-irreversible?)))
               (if (eq? (assq-ref entry 'action) 'undo)
                   (loop (cdr rest) (cons entry acc))
                   (reverse
                    (cons (cons (cons 'action 'boundary)
                                (filter (lambda (pair)
                                          (not (eq? (car pair) 'action)))
                                        entry))
                          acc)))))))
      (else
       (let ((n (or steps 1)))
         (when (> n (length applied))
           (raise-exception
            (make-exception-with-message
             (format #f "--steps ~a asks for more migrations than are applied (~a)"
                     n (length applied)))))
         (select n))))))

(define (plan-summary plan)
  "Count the plan by state, as an alist, for the summary line."
  (fold (lambda (entry acc)
          (let* ((state (assq-ref entry 'state))
                 (cell (assoc state acc)))
            (if cell
                (begin
                  (set-cdr! cell (+ 1 (cdr cell)))
                  acc)
                (append acc (list (cons state 1))))))
        '()
        plan))

(define (get-unapplied-migrations migration-entries migrations-history)
  "The paths in MIGRATION-ENTRIES that MIGRATIONS-HISTORY has never seen.

Kept for compatibility: MIGRATION-ENTRIES is a list of `(path checksum)' pairs,
which is what the first version of this program passed around."
  (filter-map (lambda (entry)
                (let* ((path (car entry))
                       (checksum (cadr entry))
                       (fname (basename path))
                       (history-entry (find (lambda (h)
                                              (equal? (assq-ref h 'name) fname))
                                            migrations-history)))
                  (if history-entry
                      (begin
                        (unless (equal? (assq-ref history-entry 'checksum)
                                        checksum)
                          (format (current-error-port)
                                  "
WARNING: Checksum mismatch for already applied migration ~a!
" fname))
                        #f)
                      path)))
              migration-entries))

(define* (checksum-mismatches files history)
  "The migrations that changed after they were applied."
  (let ((history-index (history-by-name history)))
    (filter-map (lambda (file)
                  (let ((entry (assoc-ref history-index (migration-name file))))
                    (and entry
                         (eq? (migration-kind file) 'versioned)
                         (not (equal? (history-entry-checksum entry)
                                      (migration-checksum
                                       (migration-path file))))
                         file)))
                files)))

(define* (pending-files files history)
  "The migration files still to run, in order."
  (filter-map (lambda (entry)
                (and (eq? (assq-ref entry 'action) 'apply)
                     (assq-ref entry 'path)))
              (plan-migrations files history)))

(define* (unapplied-migrations files history)
  "The pending versioned migration files, in order."
  (filter (lambda (file)
            (eq? (migration-kind file) 'versioned))
          (pending-files files history)))
