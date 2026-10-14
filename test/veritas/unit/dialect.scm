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
;; The SQL mutastructura writes for itself, asserted per engine rather than
;; against a database: quoting, the ledger's DDL, the catalogue probes and
;; `clean' are what this module exists for, and a wrong string here is a
;; migration tool that works on one engine and fails on the next.
;;
;; The engines are reached through `find-engine' with the names people actually
;; type -- `postgres', `mariadb', `crdb' -- because an alias that resolves to a
;; dialect other than the one it names is exactly the kind of bug a `--database'
;; flag hides.  `mariadb' is the interesting one: it must behave like MySQL,
;; backslash escaping and all, because that is what the server does.

(define-module (veritas unit dialect)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 format)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence dialect)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define sqlite (find-engine 'sqlite3))
(define postgres (find-engine 'postgres))
(define cockroach (find-engine 'crdb))
(define mysql (find-engine 'mysql))
(define mariadb (find-engine 'mariadb))

(define (contains? haystack needle)
  "Whether HAYSTACK, a string, holds NEEDLE."
  (and haystack (string-contains haystack needle) #t))

(define (mentions haystack needles)
  "How many of NEEDLES HAYSTACK holds, as a number, for naming what was missing."
  (length (filter (lambda (needle) (contains? haystack needle)) needles)))

(define (mentions-all? haystack needles)
  "Whether HAYSTACK holds every one of NEEDLES."
  (= (mentions haystack needles) (length needles)))

(define (quoted engine name)
  (dialect-quote-identifier engine name))

;;; ---------------------------------------------------------------------------
;;; Quoting
;;; ---------------------------------------------------------------------------

(define quoting-suite
  (suite "quoting, per engine"
         #:concurrent? #f
         (test "an identifier is quoted the way the engine quotes them"
               #:shuffle? #f
               (assert-equal #:expect "\"users\""
                             #:got (quoted sqlite "users")
                             #:name "SQLite doubles the double quote")
               (assert-equal #:expect "\"users\""
                             #:got (quoted postgres "users")
                             #:name "PostgreSQL does the same")
               (assert-equal #:expect "`users`"
                             #:got (quoted mysql "users")
                             #:name "MySQL uses backticks")
               (assert-equal #:expect "`users`"
                             #:got (quoted mariadb "users")
                             #:name "and so does MariaDB, through the same alias"))
         (test "an identifier that holds a quote is still one identifier"
               #:shuffle? #f
               ;; The reason to quote at all: a name with the engine's own
               ;; quote character in it must not end the identifier early.
               (assert-equal #:expect "\"we\"\"ird\""
                             #:got (quoted sqlite "we\"ird")
                             #:name "SQLite doubles the inner double quote")
               (assert-equal #:expect "`we``ird`"
                             #:got (quoted mysql "we`ird")
                             #:name "MySQL doubles the inner backtick"))
         (test "a string is quoted with doubled single quotes"
               #:shuffle? #f
               (assert-equal #:expect "'users'"
                             #:got (dialect-quote-string sqlite "users")
                             #:name "an ordinary value")
               (assert-equal #:expect "'user''s'"
                             #:got (dialect-quote-string sqlite "user's")
                             #:name "an apostrophe does not end the literal"))
         (test "MySQL doubles a backslash and SQLite does not"
               #:shuffle? #f
               ;; MySQL gives a backslash meaning inside a string by default, so
               ;; a Windows path would otherwise end the literal it sits in.
               (assert-equal #:expect "'C:\\temp'"
                             #:got (dialect-quote-string sqlite "C:\\temp")
                             #:name "SQLite leaves a backslash alone")
               (assert-equal #:expect "'C:\\\\temp'"
                             #:got (dialect-quote-string mysql "C:\\temp")
                             #:name "MySQL doubles it")
               (assert-equal #:expect "'C:\\\\temp'"
                             #:got (dialect-quote-string mariadb "C:\\temp")
                             #:name "and MariaDB doubles it too"))
         (test "a literal is written as its SQL type"
               #:shuffle? #f
               (assert-equal #:expect "36" #:got (dialect-literal sqlite 36)
                             #:name "a number as itself")
               (assert-equal #:expect "1" #:got (dialect-literal sqlite #t)
                             #:name "true as 1")
               (assert-equal #:expect "0" #:got (dialect-literal sqlite #f)
                             #:name "false as 0")
               (assert-equal #:expect "NULL" #:got (dialect-literal sqlite #nil)
                             #:name "the null value as NULL")
               (assert-equal #:expect "'versioned'"
                             #:got (dialect-literal sqlite 'versioned)
                             #:name "a symbol as a quoted string"))))

;;; ---------------------------------------------------------------------------
;;; Transactions and locking
;;; ---------------------------------------------------------------------------

(define locking-suite
  (suite "transactions and locking"
         #:concurrent? #f
         (test "SQLite takes its write lock with the transaction"
               #:shuffle? #f
               ;; `BEGIN IMMEDIATE' is the migration lock: there is no second
               ;; statement to run and nothing to release.
               (assert-equal #:expect "BEGIN IMMEDIATE"
                             #:got (dialect-begin-statement sqlite)
                             #:name "the write lock comes with BEGIN")
               (assert-equal #:expect '()
                             #:got (dialect-lock-statements sqlite "h")
                             #:name "and no lock statement is needed"))
         (test "every other engine opens a plain transaction"
               #:shuffle? #f
               (assert-equal #:expect "BEGIN"
                             #:got (dialect-begin-statement postgres)
                             #:name "PostgreSQL")
               (assert-equal #:expect "BEGIN"
                             #:got (dialect-begin-statement mysql)
                             #:name "MySQL"))
         (test "PostgreSQL takes one advisory lock, named for the ledger"
               #:shuffle? #f
               (let ((statements (dialect-lock-statements postgres "migrations_history")))
                 (assert-length #:expect 1 #:got statements
                                #:name "one statement")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements)
                                                "SELECT pg_advisory_lock(")
                               #:name "and it is the advisory lock"))
               ;; A lock nobody can find again is worse than no lock, so the key
               ;; is the same in the next run and different for another ledger.
               (assert-equal #:expect (dialect-lock-statements postgres "h")
                             #:got (dialect-lock-statements postgres "h")
                             #:name "the same ledger gets the same key")
               (assert-equal #:expect #f
                             #:got (equal? (dialect-lock-statements postgres "h")
                                           (dialect-lock-statements postgres "other"))
                             #:name "and another ledger a different one"))
         (test "CockroachDB takes no advisory lock at all"
               #:shuffle? #f
               ;; `pg_advisory_lock' there is a function that does not exist;
               ;; what keeps two runs apart is serializable isolation and the
               ;; ledger's own unique key.
               (assert-equal #:expect '()
                             #:got (dialect-lock-statements cockroach "h")
                             #:name "no statement to take a lock that is not there")
               (assert-equal #:expect '()
                             #:got (dialect-unlock-statements cockroach "h")
                             #:name "and none to release it")
               (assert-equal #:expect "BEGIN"
                             #:got (dialect-begin-statement cockroach)
                             #:name "and it speaks PostgreSQL's SQL otherwise")
               (assert-equal #:expect "#t"
                             #:got (format #f "~s"
                                           (dialect-transactional-ddl? cockroach))
                             #:name "with transactional DDL"))
         (test "MySQL asks for a lock and gives up after ten seconds"
               #:shuffle? #f
               ;; An indefinite wait in the middle of a deploy is worse than a
               ;; clear failure.
               (assert-equal #:expect #t
                             #:got (mentions-all? (car (dialect-lock-statements
                                                        mysql "migrations_history"))
                                                  (list "SELECT GET_LOCK('mutastructura:migrations_history', 10)"))
                             #:name "the lock is named and timed"))
         (test "a lock is released by the engine that took it"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (contains? (car (dialect-unlock-statements
                                                    postgres "h"))
                                              "SELECT pg_advisory_unlock(")
                             #:name "PostgreSQL unlocks its own advisory lock")
               (assert-equal #:expect #t
                             #:got (contains? (car (dialect-unlock-statements
                                                    mysql "h"))
                                              "SELECT RELEASE_LOCK(")
                             #:name "MySQL releases the named lock"))
         (test "only MySQL cannot undo a failed migration's DDL"
               #:shuffle? #f
               ;; `CREATE TABLE' there commits implicitly, which is a property
               ;; of the server and not something a client can work around.
               (assert-equal #:expect #f
                             #:got (dialect-transactional-ddl? mysql)
                             #:name "MySQL commits DDL as it runs")
               (assert-equal #:expect #t
                             #:got (dialect-transactional-ddl? sqlite)
                             #:name "SQLite rolls it back")
               (assert-equal #:expect #t
                             #:got (dialect-transactional-ddl? postgres)
                             #:name "and so does PostgreSQL"))))

;;; ---------------------------------------------------------------------------
;;; The ledger
;;; ---------------------------------------------------------------------------

(define ledger-suite
  (suite "the ledger's own SQL"
         #:concurrent? #f
         (test "the ledger has the same columns on every engine"
               #:shuffle? #f
               (assert-equal #:expect '(name checksum description kind installed-on
                                             installed-by execution-time success)
                             #:got (dialect-history-columns sqlite)
                             #:name "what the runner reads and writes")
               (assert-equal #:expect (dialect-history-columns sqlite)
                             #:got (dialect-history-columns mysql)
                             #:name "MySQL agrees")
               (assert-equal #:expect (dialect-history-columns sqlite)
                             #:got (dialect-history-columns postgres)
                             #:name "and PostgreSQL"))
         (test "each engine gets a ledger it can create"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-ddl sqlite "migrations_history")
                                                  (list "CREATE TABLE IF NOT EXISTS \"migrations_history\""
                                                        "AUTOINCREMENT"
                                                        "name VARCHAR(255) NOT NULL UNIQUE"))
                             #:name "SQLite's own autoincrement")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-ddl postgres "migrations_history")
                                                  (list "CREATE TABLE IF NOT EXISTS \"migrations_history\""
                                                        "SERIAL PRIMARY KEY"
                                                        "TIMESTAMP WITH TIME ZONE"))
                             #:name "PostgreSQL's serial and timestamps")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-ddl mysql "migrations_history")
                                                  (list "CREATE TABLE IF NOT EXISTS `migrations_history`"
                                                        "INT AUTO_INCREMENT PRIMARY KEY"
                                                        "UNIQUE KEY `migrations_history_name_idx` (name)"
                                                        "ENGINE=InnoDB"))
                             #:name "MySQL's unique key and engine"))
         (test "creating the ledger twice is not an error"
               #:shuffle? #f
               ;; `IF NOT EXISTS' is what removes the question of who wins a
               ;; race to create it.
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-ddl mysql "h")
                                                  (list "CREATE TABLE IF NOT EXISTS"))
                             #:name "MySQL")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-ddl postgres "h")
                                                  (list "CREATE TABLE IF NOT EXISTS"))
                             #:name "PostgreSQL"))
         (test "the catalogue is asked, not the table"
               #:shuffle? #f
               ;; Asking for the table and reading the error poisons the
               ;; surrounding transaction on PostgreSQL, and fills the log with
               ;; noise about a table that is merely not there yet.
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-table-exists-query
                                                   sqlite "migrations_history")
                                                  (list "sqlite_master" "'table'"
                                                        "'migrations_history'"))
                             #:name "SQLite asks sqlite_master")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-table-exists-query
                                                   postgres "migrations_history")
                                                  (list "information_schema.tables"
                                                        "current_schema()"
                                                        "'migrations_history'"))
                             #:name "PostgreSQL asks information_schema")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-table-exists-query
                                                   mysql "migrations_history")
                                                  (list "information_schema.tables"
                                                        "DATABASE()"
                                                        "'migrations_history'"))
                             #:name "MySQL asks information_schema too"))
         (test "the columns are read from the catalogue as well"
               #:shuffle? #f
               ;; This is what lets a ledger created by an older mutastructura
               ;; keep working: the runner writes the columns that are there.
               (assert-equal #:expect #t
                             #:got (contains? (dialect-history-columns-query
                                               sqlite "migrations_history")
                                              "PRAGMA table_info(\"migrations_history\")")
                             #:name "SQLite gets the pragma")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-history-columns-query
                                                   postgres "migrations_history")
                                                  (list "information_schema.columns"
                                                        "column_name"
                                                        "'migrations_history'"))
                             #:name "PostgreSQL gets the column names"))))

;;; ---------------------------------------------------------------------------
;;; Clean
;;; ---------------------------------------------------------------------------

(define reading-suite
  (suite "reading a ledger column"
         #:concurrent? #f
         (test "the timestamp column is read as text, where the driver needs it"
               #:shuffle? #f
               ;; Not a preference: guile-dbi's PostgreSQL driver abandons a row
               ;; whose column type it does not know, and guile-dbd-mysql answers
               ;; a DATETIME with seconds since the epoch.  Read as text, both
               ;; say when the migration was applied.
               (assert-equal #:expect "\"installed_on\"::text AS \"installed_on\""
                             #:got (dialect-readable-column postgres "installed_on")
                             #:name "PostgreSQL, whose driver cannot unpack a TIMESTAMPTZ")
               (assert-equal #:expect "DATE_FORMAT(`installed_on`, '%Y-%m-%d %H:%i:%s') AS `installed_on`"
                             #:got (dialect-readable-column mysql "installed_on")
                             #:name "MySQL, whose driver answers with a number instead")
               (assert-equal #:expect "DATE_FORMAT(`installed_on`, '%Y-%m-%d %H:%i:%s') AS `installed_on`"
                             #:got (dialect-readable-column mariadb "installed_on")
                             #:name "and MariaDB through the same alias")
               (assert-equal #:expect "\"installed_on\""
                             #:got (dialect-readable-column sqlite "installed_on")
                             #:name "SQLite, which keeps its timestamps as text already")
               (assert-equal #:expect "\"installed_on\"::text AS \"installed_on\""
                             #:got (dialect-readable-column cockroach "installed_on")
                             #:name "and CockroachDB, which is reached as PostgreSQL")
               (assert-equal #:expect "\"applied_at\"::text AS \"applied_at\""
                             #:got (dialect-readable-column postgres "applied_at")
                             #:name "including the name the first ledger called it"))
         (test "every other column is read as it is named"
               #:shuffle? #f
               (assert-equal #:expect "\"name\""
                             #:got (dialect-readable-column postgres "name")
                             #:name "a ledger column comes back quoted")
               (assert-equal #:expect "`success`"
                             #:got (dialect-readable-column mysql "success")
                             #:name "with the engine's own quoting"))))

(define clean-suite
  (suite "dropping what clean found"
         #:concurrent? #f
         (test "each engine lists what it can drop"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-clean-catalog-query sqlite)
                                                  (list "sqlite_master" "AS object_type"
                                                        "AS object_name"
                                                        "NOT LIKE 'sqlite_%'"))
                             #:name "SQLite, without its own internal tables")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-clean-catalog-query postgres)
                                                  (list "pg_tables" "pg_views"
                                                        "AS object_type" "AS object_name"))
                             #:name "PostgreSQL, tables and views")
               (assert-equal #:expect #t
                             #:got (mentions-all? (dialect-clean-catalog-query mysql)
                                                  (list "information_schema.tables" "DATABASE()"))
                             #:name "MySQL, through information_schema"))
         (test "an object type is normalised to one of four"
               #:shuffle? #f
               ;; Every engine spells these differently: `BASE TABLE' and
               ;; `SYSTEM TABLE' are both tables to MySQL.
               (assert-equal #:expect 'table
                             #:got (dialect-normalize-object-type mysql "BASE TABLE")
                             #:name "a base table is a table")
               (assert-equal #:expect 'table
                             #:got (dialect-normalize-object-type mysql "SYSTEM TABLE")
                             #:name "and so is a system table")
               (assert-equal #:expect 'view
                             #:got (dialect-normalize-object-type postgres "VIEW")
                             #:name "a view is a view")
               (assert-equal #:expect 'trigger
                             #:got (dialect-normalize-object-type sqlite "trigger")
                             #:name "a trigger is a trigger")
               (assert-equal #:expect 'index
                             #:got (dialect-normalize-object-type mysql "INDEX")
                             #:name "an index is an index"))
         (test "only PostgreSQL drops with CASCADE"
               #:shuffle? #f
               ;; CASCADE is what makes clean finish on a schema whose views
               ;; depend on its tables.
               (assert-equal #:expect "DROP TABLE IF EXISTS \"users\" CASCADE"
                             #:got (dialect-drop-statement postgres "table" "users")
                             #:name "PostgreSQL cascades")
               (assert-equal #:expect "DROP TABLE IF EXISTS \"users\""
                             #:got (dialect-drop-statement sqlite "table" "users")
                             #:name "SQLite has nothing to cascade")
               (assert-equal #:expect "DROP TABLE IF EXISTS `users`"
                             #:got (dialect-drop-statement mysql "table" "users")
                             #:name "and neither has MySQL"))
         (test "a view, a trigger and an index are dropped as what they are"
               #:shuffle? #f
               (assert-equal #:expect "DROP VIEW IF EXISTS \"active_users\" CASCADE"
                             #:got (dialect-drop-statement postgres "view"
                                                          "active_users")
                             #:name "a view is dropped as a view")
               (assert-equal #:expect "DROP TRIGGER IF EXISTS \"touch_users\""
                             #:got (dialect-drop-statement sqlite "TRIGGER"
                                                          "touch_users")
                             #:name "a trigger as a trigger, whatever the case")
               (assert-equal #:expect "DROP INDEX IF EXISTS `email_1`"
                             #:got (dialect-drop-statement mysql "index" "email_1")
                             #:name "an index as an index"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               quoting-suite
               locking-suite
               ledger-suite
               reading-suite
               clean-suite))
