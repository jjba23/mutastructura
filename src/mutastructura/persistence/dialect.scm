;;; mutastructura - Relational Schema Migrations Powered by Lisp

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

;;; Commentary:
;;
;; Everything mutastructura writes for itself -- the ledger, the lock, the
;; catalogue probes, `clean' -- lives behind this module, so the rest of the
;; program never contains a string that only one database would accept.
;;
;; The differences that matter, and why they are what they are:
;;
;;   quoting        PostgreSQL and SQLite quote identifiers with `"', MySQL
;;                  with backticks.  Strings are escaped with `''` everywhere
;;                  (MySQL also treats a backslash as an escape by default, so
;;                  a backslash in a value is doubled for it).
;;
;;   the ledger     one row per migration, keyed on the migration's name, with
;;                  the same columns on every engine.  An older deployment's
;;                  ledger has fewer columns; the runner asks the catalogue
;;                  which ones are there and writes to those.  That is what
;;                  makes upgrading mutastructura a non-event.
;;
;;   locking        SQLite locks the whole file, so a `BEGIN IMMEDIATE'
;;                  transaction *is* the migration lock.  PostgreSQL and MySQL
;;                  need an explicit advisory lock, because they have no such
;;                  thing.
;;
;;   transactions   SQLite and PostgreSQL have transactional DDL, so a failed
;;                  migration rolls back completely.  MySQL commits DDL
;;                  implicitly -- `CREATE TABLE' cannot be undone -- and saying
;;                  so plainly is better than pretending otherwise.
;;
;;   refusals       SQLite's and MySQL's drivers report a refused statement in
;;                  their status.  PostgreSQL's does not: it sends the query and
;;                  the send is all it reports on, so a syntax error there looks
;;                  exactly like a statement that worked.  What makes the error
;;                  visible is asking the server a question it cannot answer
;;                  wrongly -- see `dialect-verification-query'.
;;
;; The switches below ask about *dialect*, not about an engine id, wherever the
;; two differ: CockroachDB speaks PostgreSQL's SQL and has no advisory locks, and
;; the PostgreSQL-compatibles (YugabyteDB, TimescaleDB, Citus) are reached
;; through the PostgreSQL driver by aliasing.  `sqlite-engine?',
;; `postgres-engine?' and `mysql-engine?' are the three questions the rest of
;; this file actually asks.

(define-module (mutastructura persistence dialect)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 string-fun)
  #:use-module (mutastructura mutastructura)
  #:export (mysql-engine?
            postgres-engine?
            sqlite-engine?
            dialect-quote-identifier
            dialect-quote-string
            dialect-literal
            dialect-begin-statement
            dialect-commit-statement
            dialect-rollback-statement
            dialect-lock-statements
            dialect-unlock-statements
            dialect-lock-name
            dialect-transactional-ddl?
            dialect-verification-query
            dialect-may-abandon-rows?
            dialect-readable-column
            dialect-history-ddl
            dialect-history-columns
            dialect-history-table-exists-query
            dialect-history-columns-query
            dialect-clean-catalog-query
            dialect-drop-statement
            dialect-normalize-object-type))

;;; ---------------------------------------------------------------------------
;;; Quoting and literals
;;; ---------------------------------------------------------------------------

(define (mysql-engine? engine)
  "Whether ENGINE is MySQL or MariaDB, which quote identifiers with backticks."
  (memq (engine-id engine) '(mysql mariadb)))

(define (postgres-engine? engine)
  "Whether ENGINE speaks PostgreSQL's SQL.

True for PostgreSQL itself and for the databases that implement its protocol and
its catalogue -- CockroachDB among them -- because everything this module asks
of them, from `information_schema' to `DROP TABLE ... CASCADE', is the same."
  (memq (engine-id engine) '(postgresql cockroachdb)))

(define (sqlite-engine? engine)
  "Whether ENGINE is SQLite, whose catalogue and locking are its own thing."
  (eq? (engine-id engine) 'sqlite3))

(define (dialect-quote-identifier engine name)
  "NAME quoted the way ENGINE expects an identifier to be quoted."
  (let ((text (if (symbol? name) (symbol->string name) (format #f "~a" name))))
    (if (mysql-engine? engine)
        (string-append "`" (string-replace-substring text "`" "``") "`")
        (string-append "\"" (string-replace-substring text "\"" "\"\"") "\""))))

(define (dialect-quote-string engine text)
  "TEXT as a SQL string literal for ENGINE.

Single quotes are doubled, which every engine accepts.  MySQL also gives a
backslash meaning inside a string by default, so there a backslash has to be
doubled as well or `C:\\temp' could end the literal it sits in."
  (let* ((text (if (string? text) text (format #f "~a" text)))
         (escaped (string-replace-substring text "'" "''")))
    (format #f "'~a'"
            (if (mysql-engine? engine)
                (string-replace-substring escaped "\\" "\\\\")
                escaped))))

(define (dialect-literal engine value)
  "VALUE written as a SQL literal for ENGINE."
  (cond
    ((number? value)
     (number->string value))
    ((eq? #t value)
     "1")
    ((eq? #f value)
     "0")
    ((eq? value #nil)
     "NULL")
    ((null? value)
     "NULL")
    ((symbol? value)
     (dialect-quote-string engine (symbol->string value)))
    ((string? value)
     (dialect-quote-string engine value))
    (else (dialect-quote-string engine (format #f "~a" value)))))

;;; ---------------------------------------------------------------------------
;;; Transactions and locking
;;; ---------------------------------------------------------------------------

(define (dialect-begin-statement engine)
  "How to open a transaction on ENGINE.

SQLite's `BEGIN IMMEDIATE' takes the write lock at once rather than at the
first write, which is what stops two concurrent runs from both deciding to
apply the same migration and then colliding halfway through."
  (if (sqlite-engine? engine)
      "BEGIN IMMEDIATE"
      "BEGIN"))

(define (dialect-commit-statement engine) "COMMIT")
(define (dialect-rollback-statement engine) "ROLLBACK")

(define (dialect-lock-name history-name)
  "The name of the advisory lock that guards HISTORY-NAME."
  (format #f "mutastructura:~a" history-name))

(define (dialect-lock-statements engine history-name)
  "Statements that take the migration lock, or '() when the transaction is one.

SQLite gets no statement: its write lock comes with `BEGIN IMMEDIATE', so a
second run waits (or fails) exactly where it would otherwise have collided.
CockroachDB gets none either, and for a different reason: it has no advisory
locks at all -- `pg_advisory_lock' there is a function that does not exist.  What
keeps two concurrent runs apart there is its serializable isolation plus the
ledger's own unique key on the migration name: the second transaction to write
its ledger row is the one that fails, and it fails before anything else it did
can be seen."
  (cond
    ((postgres-engine? engine)
     (if (eq? (engine-id engine) 'cockroachdb)
         '()
         ;; One advisory lock for the whole run.  The number is a hash of the
         ;; ledger's name, so two different ledgers in one database do not block
         ;; each other, while two runs on the same ledger do.
         (list (format #f "SELECT pg_advisory_lock(~a)"
                       (lock-key (dialect-lock-name history-name))))))
    ((mysql-engine? engine)
     ;; Ten seconds of patience, then a clear failure rather than an
     ;; indefinite wait in the middle of a deploy.
     (list (format #f "SELECT GET_LOCK(~a, 10)"
                   (dialect-quote-string engine
                                         (dialect-lock-name history-name)))))
    (else '())))

(define (dialect-unlock-statements engine history-name)
  "Statements that release the migration lock taken by `dialect-lock-statements'."
  (cond
    ((eq? (engine-id engine) 'postgresql)
     (list (format #f "SELECT pg_advisory_unlock(~a)"
                   (lock-key (dialect-lock-name history-name)))))
    ((mysql-engine? engine)
     (list (format #f "SELECT RELEASE_LOCK(~a)"
                   (dialect-quote-string engine
                                         (dialect-lock-name history-name)))))
    (else '())))

(define (lock-key name)
  "A stable 63-bit number derived from NAME, for `pg_advisory_lock'.

Stable across runs and machines, because a lock nobody can find again is worse
than no lock at all.  FNV-1a, because it is five lines and good enough."
  (let loop ((chars (string->list name)) (hash 1469598103934665603))
    (if (null? chars)
        (modulo hash 9223372036854775807)
        (loop (cdr chars)
              (modulo (* (logxor hash (char->integer (car chars)))
                         1099511628211)
                      9223372036854775807)))))

(define (dialect-transactional-ddl? engine)
  "Whether ENGINE can undo a failed migration's DDL.

MySQL cannot: `CREATE TABLE', `ALTER TABLE' and friends commit implicitly, which
is a property of the server and not something a client can work around."
  (not (mysql-engine? engine)))

(define (dialect-verification-query engine)
  "A query whose answer proves ENGINE's last statement was accepted, or #f.

guile-dbi's PostgreSQL driver has no way to report a refused statement: it sends
the query, answers `query ok' when the *send* worked, and only unpacks a result
when somebody asks for a row.  A migration with a syntax error in it therefore
read as a success, and the ledger recorded a migration that never ran.

What makes it visible is asking the server something that must come back with
one row.  Inside a transaction that is proof: PostgreSQL refuses *every*
statement after a failed one until the transaction ends, so a query that always
has an answer coming back with none means the statement before it was refused.
The question is asked only where the answer means something -- inside a
transaction -- which is why the runner asks it there and why SQLite and MySQL,
whose drivers do report a failure, are not asked anything at all."
  (if (postgres-engine? engine) "SELECT 1 AS mutastructura_probe" #f))

;;; ---------------------------------------------------------------------------
;;; The ledger
;;; ---------------------------------------------------------------------------

(define (dialect-may-abandon-rows? engine)
  "Whether ENGINE's driver can stop reading a row and say nothing about it.

guile-dbi's PostgreSQL driver unpacks a row from the table of types it knows and
abandons the row for anything else -- `TIMESTAMP WITH TIME ZONE' among them.  The
caller is then handed no rows, which is exactly what an empty table looks like,
so a read there has to be checked after the fact.  The other drivers report a
refusal and say so in the same breath, and SQLite's goes further: it ends even a
*successful* read with a status of its own (`no more rows to get'), so asking it
the same question would mistake a finished read for a broken one."
  (postgres-engine? engine))

(define (dialect-readable-column engine column)
  "COLUMN -- a SQL column name -- as a SELECT expression ENGINE can unpack.

The ledger's `installed_on' is the one column every engine has trouble with, and
for a different reason each:

guile-dbi's PostgreSQL driver unpacks the column types it knows and abandons the
row for the rest, without raising anything.  `TIMESTAMP WITH TIME ZONE' (oid 1184)
is one of the rest, and the ledger is made of it -- so the ledger read as empty on
every PostgreSQL deployment, and an empty ledger means every migration is applied
again, every run.

guile-dbd-mysql hands a `DATETIME' back as the seconds since the epoch, which is
true and unreadable.  A status table that says `1791987754' where it means a date
is not reporting what the ledger says.

Read as text, both arrive whole: the string a timestamp comes back as is the one
the server itself would print, which is all the ledger is asked for -- when a
migration was applied.  SQLite, which stores its timestamps as text already, is
asked nothing."
  (let* ((name (if (symbol? column) (symbol->string column) (format #f "~a" column)))
         (quoted (dialect-quote-identifier engine name))
         (timestamp? (member name '("installed_on" "applied_at"))))
    (cond
      ((and timestamp? (postgres-engine? engine))
       (format #f "~a::text AS ~a" quoted quoted))
      ((and timestamp? (mysql-engine? engine))
       (format #f "DATE_FORMAT(~a, '%Y-%m-%d %H:%i:%s') AS ~a" quoted quoted))
      (else quoted))))

;; The columns the ledger has when mutastructura creates it.
(define history-columns
  '(name checksum description kind installed-on installed-by execution-time
    success))

(define (dialect-history-columns engine)
  "The ledger's column names, in the order mutastructura writes them."
  history-columns)

(define (dialect-history-ddl engine table)
  "The `CREATE TABLE' statement for the ledger TABLE, for ENGINE.

`IF NOT EXISTS' is supported by every version we target, and it removes the
question of who wins a race to create the ledger."
  (let ((t (dialect-quote-identifier engine table)))
    (cond
      ((sqlite-engine? engine)
       (format #f "CREATE TABLE IF NOT EXISTS ~a (\n    id INTEGER PRIMARY KEY AUTOINCREMENT,\n    name VARCHAR(255) NOT NULL UNIQUE,\n    checksum VARCHAR(64) NOT NULL,\n    description VARCHAR(512),\n    kind VARCHAR(32) NOT NULL DEFAULT 'versioned',\n    installed_on DATETIME DEFAULT CURRENT_TIMESTAMP,\n    installed_by VARCHAR(255),\n    execution_time INTEGER DEFAULT 0,\n    success INTEGER NOT NULL DEFAULT 1\n)" t))
      ((postgres-engine? engine)
       (format #f "CREATE TABLE IF NOT EXISTS ~a (\n    id SERIAL PRIMARY KEY,\n    name VARCHAR(255) NOT NULL UNIQUE,\n    checksum VARCHAR(64) NOT NULL,\n    description VARCHAR(512),\n    kind VARCHAR(32) NOT NULL DEFAULT 'versioned',\n    installed_on TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,\n    installed_by VARCHAR(255),\n    execution_time INTEGER DEFAULT 0,\n    success INTEGER NOT NULL DEFAULT 1\n)" t))
      ((mysql-engine? engine)
       (format #f "CREATE TABLE IF NOT EXISTS ~a (\n    id INT AUTO_INCREMENT PRIMARY KEY,\n    name VARCHAR(255) NOT NULL,\n    checksum VARCHAR(64) NOT NULL,\n    description VARCHAR(512),\n    kind VARCHAR(32) NOT NULL DEFAULT 'versioned',\n    installed_on DATETIME DEFAULT CURRENT_TIMESTAMP,\n    installed_by VARCHAR(255),\n    execution_time INTEGER DEFAULT 0,\n    success TINYINT NOT NULL DEFAULT 1,\n    UNIQUE KEY ~a (name)\n) ENGINE=InnoDB" t (dialect-quote-identifier engine
                                                   (format #f "~a_name_idx" table))))
      (else (error "no ledger definition for" (engine-id engine))))))

(define (dialect-history-table-exists-query engine table)
  "A query that answers with one row when TABLE exists on ENGINE.

Asking the catalogue beats asking for the table and interpreting the error: on
PostgreSQL a failed statement poisons the surrounding transaction, and on every
engine it fills the log with noise about a table that is merely not there yet."
  (cond
    ((sqlite-engine? engine)
     (format #f "SELECT name FROM sqlite_master WHERE type = 'table' AND name = ~a"
             (dialect-quote-string engine table)))
    ((postgres-engine? engine)
     (format #f "SELECT table_name FROM information_schema.tables WHERE table_name = ~a AND table_schema = current_schema()"
             (dialect-quote-string engine table)))
    ((mysql-engine? engine)
     (format #f "SELECT table_name FROM information_schema.tables WHERE table_name = ~a AND table_schema = DATABASE()"
             (dialect-quote-string engine table)))
    (else (error "no catalogue probe for" (engine-id engine)))))

(define (dialect-history-columns-query engine table)
  "A query listing the column names TABLE actually has on ENGINE.

This is what lets a ledger created by an older mutastructura keep working: the
runner only writes the columns that are really there."
  (cond
    ((sqlite-engine? engine)
     (format #f "PRAGMA table_info(~a)" (dialect-quote-identifier engine table)))
    ((postgres-engine? engine)
     (format #f "SELECT column_name FROM information_schema.columns WHERE table_name = ~a AND table_schema = current_schema()"
             (dialect-quote-string engine table)))
    ((mysql-engine? engine)
     (format #f "SELECT column_name FROM information_schema.columns WHERE table_name = ~a AND table_schema = DATABASE()"
             (dialect-quote-string engine table)))
    (else (error "no column probe for" (engine-id engine)))))

;;; ---------------------------------------------------------------------------
;;; Clean
;;; ---------------------------------------------------------------------------

(define (dialect-clean-catalog-query engine)
  "A query listing the droppable objects of the current schema on ENGINE.

The answer must have two columns: `object_type' and `object_name'."
  (cond
    ((sqlite-engine? engine)
     "SELECT type AS object_type, name AS object_name FROM sqlite_master WHERE type IN ('table', 'view', 'trigger', 'index') AND name NOT LIKE 'sqlite_%'")
    ((postgres-engine? engine)
     "SELECT 'table' AS object_type, tablename AS object_name FROM pg_tables WHERE schemaname = current_schema() UNION ALL SELECT 'view' AS object_type, viewname AS object_name FROM pg_views WHERE schemaname = current_schema()")
    ((mysql-engine? engine)
     "SELECT table_type AS object_type, table_name AS object_name FROM information_schema.tables WHERE table_schema = DATABASE()")
    (else (error "no clean catalogue for" (engine-id engine)))))

(define (dialect-normalize-object-type engine type)
  "OBJECT_TYPE as one of `table', `view', `trigger' or `index'."
  (let ((text (string-downcase (format #f "~a" type))))
    (cond
      ((string-contains text "view") 'view)
      ((string-contains text "trigger") 'trigger)
      ((string-contains text "index") 'index)
      (else 'table))))

(define (dialect-drop-statement engine object-type object-name)
  "The statement that drops OBJECT-NAME of OBJECT-TYPE on ENGINE."
  (let ((kind (dialect-normalize-object-type engine object-type))
        (name (dialect-quote-identifier engine object-name)))
    (case kind
      ((table) (format #f "DROP TABLE IF EXISTS ~a~a" name
                       (if (postgres-engine? engine) " CASCADE" "")))
      ((view) (format #f "DROP VIEW IF EXISTS ~a~a" name
                      (if (postgres-engine? engine) " CASCADE" "")))
      ((trigger) (format #f "DROP TRIGGER IF EXISTS ~a" name))
      ((index) (format #f "DROP INDEX IF EXISTS ~a" name))
      (else (format #f "DROP TABLE IF EXISTS ~a" name)))))
