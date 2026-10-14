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
;; The SQL runner, against a real database: SQLite over a file in /tmp, because
;; the half of this program that talks to a server is worth testing against a
;; server's answers rather than against strings.  What the cases below are for is
;; the class of bug the strings cannot catch -- a driver that answers a query
;; with no rows when it meant to report an error, a ledger that reads back empty
;; when it is full, a `GET_LOCK' whose 0 is taken for a lock that was taken --
;; every one of which a deployment of this program has actually had.
;;
;; The database file is deleted before each session is opened, so every case
;; starts from a database nobody has migrated, which is the state that hides the
;; most mistakes.

(define-module (veritas unit sql-runner)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 format)
  #:use-module (ice-9 exceptions)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence runners)
  #:use-module (mutastructura persistence util)
  #:use-module (mutastructura persistence migration-runner)
  #:use-module (dbi dbi)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define db-file "/tmp/mutastructura-sql-runner-spec.db")

(define (fresh-file!)
  "A database file nobody has migrated, so a case starts from nothing."
  (when (file-exists? db-file)
    (delete-file db-file))
  db-file)

(define (spec-of)
  (parse-connection-url (format #f "sqlite3://~a" db-file)
                        #:engine (parse-engine "sqlite3")))

(define* (on-session thunk #:key (options '()) (fresh? #f))
  "Run THUNK with a session, on a fresh database when FRESH? is set.

A case that needs more than one session -- one to fill the database, one to
report on it -- asks for the file not to be emptied between them."
  (when fresh? (fresh-file!))
  (let ((session (sql-open-session (spec-of) #:options options)))
    (dynamic-wind
     (lambda () #t)
     (lambda () (thunk session))
     (lambda () (sql-close! session)))))

(define* (with-session thunk #:key (options '()))
  "Run THUNK with a session on a fresh database, and answer what it collected."
  (on-session thunk #:options options #:fresh? #t))

(define (passed? result)
  (and (pair? result)
       (eq? #t (assoc-ref result 'assertion-successful))))

(define (verdict vc results)
  "The result a case ends on, given the RESULTS of its assertions."
  (let ((failed (filter (lambda (result) (not (passed? result))) results)))
    (if (null? failed)
        (car (reverse results))
        ((assert-true #f #:name "an earlier assertion in this case failed") vc))))

;; A ledger entry, as the command layer hands one to a runner.
(define entry
  '((name . "2026-01-01_10:00:00_init.sql")
    (checksum . "d0f1")
    (description . "init")
    (kind . versioned)
    (execution-time . 12)
    (success . #t)))

(define (entry-with field value)
  "ENTRY's fields with FIELD set to VALUE, sharing nothing mutable with ENTRY.

The cases below run one after another and, through the runner, at the same time:
a case that changed ENTRY in place would be changing it under the others."
  (cons (cons field value)
        (filter (lambda (pair) (not (eq? (car pair) field))) entry)))

(define (table-names session)
  "The tables the database has, by name, ignoring SQLite's own bookkeeping.

`sqlite_sequence' comes with the ledger's AUTOINCREMENT and belongs to nobody's
migration; `clean' ignores it for the same reason, and a case that asked for an
empty database would otherwise never see one."
  (map (lambda (row) (format #f "~a" (cdr (car row))))
       (fetch-sql-rows (sql-session-db session)
                       "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%' ORDER BY name")))

;;; ---------------------------------------------------------------------------
;;; The ledger
;;; ---------------------------------------------------------------------------

(define (ledger-is-created-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (list ((assert-true (sql-table-exists? session)
                          #:name "opening a session creates the ledger")
             vc)
            ((assert-true (and (member "migrations_history" (table-names session))
                               #t)
                          #:name "and the catalogue agrees it is there")
             vc)
            ((assert-empty #:got (sql-history session)
                           #:name "and it holds nothing yet")
             vc)
            ;; The nine the ledger is made of: the eight mutastructura writes,
            ;; and the id the database gives each row.
            ((assert-length #:expect 9 #:got (sql-session-columns session)
                            #:name "with the columns the ledger is made of")
             vc))))))

(define (recorded-migration-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (sql-record! session entry)
      (let ((history (sql-history session)))
        (list ((assert-length #:expect 1 #:got history
                              #:name "one migration is one row")
               vc)
              ((assert-equal #:expect "2026-01-01_10:00:00_init.sql"
                             #:got (history-entry-name (car history))
                             #:name "with the name it was recorded under")
               vc)
              ((assert-equal #:expect "d0f1"
                             #:got (history-entry-checksum (car history))
                             #:name "and its checksum")
               vc)
              ((assert-equal #:expect 'versioned
                             #:got (history-entry-kind (car history))
                             #:name "and its kind, which only this ledger version has")
               vc)
              ((assert-equal #:expect 12
                             #:got (history-entry-execution-time (car history))
                             #:name "and how long it took")
               vc)
              ((assert-true (history-entry-success (car history))
                            #:name "and a success the core can read")
               vc)))))))

(define (re-recorded-migration-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (sql-record! session entry)
      (sql-record! session (entry-with 'execution-time 99))
      (let ((history (sql-history session)))
        (list ((assert-length #:expect 1 #:got history
                              #:name "recording the same migration again leaves one row")
               vc)
              ((assert-equal #:expect 99
                             #:got (history-entry-execution-time (car history))
                             #:name "with what the second recording said")
               vc)))))))

(define (forgotten-migration-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (sql-record! session entry)
      (sql-forget! session "2026-01-01_10:00:00_init.sql")
      (list ((assert-empty #:got (sql-history session)
                           #:name "forgetting a migration removes its row")
             vc)
            ((assert-false (sql-entry-for session "2026-01-01_10:00:00_init.sql")
                           #:name "and looking it up answers nothing")
             vc))))))

(define (closed-session-is-collectable-case vc)
  (verdict
   vc
   (let ((session (sql-open-session (spec-of))))
     (sql-close! session)
     (sql-close! session)
     ;; guile-dbi frees a connection from the smob's own free function, where the
     ;; same routine `dbi-close' runs asserts on a smob the collector has moved
     ;; on from -- an exception inside the collector, which ends the program.  A
     ;; closed connection is held for that reason, so collecting here must be as
     ;; uneventful as collecting anything else.
     (set! session #f)
     (gc)
     (gc)
     (list ((assert-true #t
                         #:name "closing a session twice, and collecting it, is not an error")
            vc)))))

(define (read-only-session-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (list ((assert-false (sql-table-exists? session)
                           #:name "a session that may not create the ledger does not")
             vc)
            ((assert-empty #:got (sql-history session)
                           #:name "and a database with no ledger has no history to report")
             vc)
            ((assert-equal #:expect '() #:got (table-names session)
                           #:name "and nothing was written to the database at all")
             vc)))
    #:options '((ensure-history? . #f)))))

;;; ---------------------------------------------------------------------------
;;; Reading rows
;;; ---------------------------------------------------------------------------

(define (rows-are-read-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (let ((db (sql-session-db session))
            (engine (sql-session-engine session)))
        (execute-sql! db "CREATE TABLE t (a INTEGER, b VARCHAR(16))")
        (execute-sql! db "INSERT INTO t VALUES (1, 'x')")
        (execute-sql! db "INSERT INTO t VALUES (2, 'y')")
        (list ((assert-length
                #:expect 2
                #:got (fetch-sql-rows db "SELECT a, b FROM t ORDER BY a"
                                      #:engine engine)
                #:name "every row of a query comes back")
               vc)
              ((assert-equal #:expect 1
                             #:got (cdr (car (car (fetch-sql-rows
                                                   db "SELECT a FROM t ORDER BY a"
                                                   #:engine engine))))
                             #:name "with the values the database has")
               vc)))))))

(define (empty-answer-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      ;; SQLite's driver ends even a *successful* read by setting the status to
      ;; `no more rows to get', so a check that took a status after the rows for
      ;; a failure would call every empty answer a broken query.
      (list ((assert-empty
              #:got (fetch-sql-rows (sql-session-db session)
                                    "SELECT name FROM sqlite_master WHERE name = 'nothing'"
                                    #:engine (sql-session-engine session))
              #:name "a query with no rows answers with no rows, and no error")
             vc))))))

(define (refused-query-raises? session)
  "Whether a query the database cannot answer raises with the database's words."
  (catch #t
         (lambda ()
           (fetch-sql-rows (sql-session-db session)
                           "SELECT nope FROM nosuchtable"
                           #:engine (sql-session-engine session))
           #f)
         (lambda (key . args)
           (let ((message (error-message-of (car args))))
             (and (string? message)
                  (string-contains message "nosuchtable")
                  #t)))))

(define (refused-query-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (list ((assert-true (refused-query-raises? session)
                          #:name "a refused query raises the database's own message")
             vc))))))

;;; ---------------------------------------------------------------------------
;;; Running a migration
;;; ---------------------------------------------------------------------------

;; A migration to run, with a placeholder in it: the one file the runner cases
;; below hand to `apply'.
(define migration-file* "/tmp/mutastructura-sql-runner-spec-migration.sql")

;; The `${other}' in the first statement's comment is nobody's placeholder: a
;; migration may carry template syntax of its own, and mutastructura leaves it
;; exactly as it found it.
(define (write-migration!)
  (call-with-output-file migration-file*
    (lambda (port)
      (display "-- ${other} is somebody else's\n" port)
      (display "CREATE TABLE ${prefix}users (id INTEGER PRIMARY KEY);\n" port)
      (display "CREATE INDEX users_idx ON ${prefix}users (id);\n" port)))
  (migration-file migration-file*))

(define (statements-are-substituted-case vc)
  (verdict
   vc
   (let* ((file (write-migration!))
          (statements (sql-statements file
                                      (list (cons 'engine (parse-engine "sqlite3"))
                                            (cons 'placeholders
                                                  '(("prefix" . "app_")))))))
     (list
      ((assert-length #:expect 2 #:got statements
                      #:name "a file is read as its statements")
       vc)
      ((assert-true (and (string-contains (assq-ref (car statements) 'sql)
                                          "app_users")
                         #t)
                    #:name "with the placeholder substituted in the first")
       vc)
      ((assert-true (and (string-contains (assq-ref (cadr statements) 'sql)
                                          "app_users")
                         #t)
                    #:name "and in the second")
       vc)
      ((assert-true (and (string-contains (assq-ref (car statements) 'sql)
                                          "${other}")
                         #t)
                    #:name "and a placeholder nobody defined is left alone")
       vc)))))

(define (dry-run-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (let ((ran (sql-apply! session (write-migration!)
                             '((dry-run? . #t)
                               (placeholders . (("prefix" . "app_")))))))
        (list ((assert-equal #:expect 2 #:got ran
                             #:name "a dry run reports the statements it would run")
               vc)
              ((assert-false (member "app_users" (table-names session))
                             #:name "and runs none of them")
               vc)))))))

(define (apply-then-clean-case vc)
  (verdict
   vc
   (with-session
    (lambda (session)
      (sql-apply! session (write-migration!)
                  '((placeholders . (("prefix" . "app_")))))
      (sql-record! session entry)
      (let ((after-apply (table-names session)))
        (list
         ((assert-true (and (member "app_users" after-apply)
                            (member "migrations_history" after-apply)
                            #t)
                       #:name "applying a migration creates its table")
          vc)
         ;; `clean' is told what to do through the session it is given, so a dry
         ;; run is a session opened for one, and the same database is asked
         ;; afterwards what it still has.
         ((assert-equal
           #:expect 3
           #:got (on-session (lambda (session) (sql-clean! session))
                             #:options '((dry-run? . #t)))
           #:name "a dry run names what clean would drop")
          vc)
         ((assert-true (and (member "app_users"
                                    (on-session table-names #:fresh? #f))
                            #t)
                       #:name "and drops nothing")
          vc)
         ((assert-equal
           #:expect 3
           #:got (on-session (lambda (session) (sql-clean! session))
                             #:fresh? #f)
           #:name "clean drops what the catalogue reports")
          vc)
         ;; Read with `ensure-history? #f': a session that may create the ledger
         ;; would create it here, and the case is about what clean left behind.
         ((assert-empty
           #:got (on-session table-names #:fresh? #f
                             #:options '((ensure-history? . #f)))
           #:name "and the database, ledger included, is empty")
          vc)))))))

;;; ---------------------------------------------------------------------------
;;; The vtable
;;; ---------------------------------------------------------------------------

(define (vtable-case vc)
  (verdict
   vc
   (let ((runner (sql-runner)))
     (list ((assert-equal #:expect 'relational #:got (runner-family runner)
                          #:name "the SQL runner is the relational family")
            vc)
           ((assert-true (and (string-contains (runner-description runner) "SQL")
                              #t)
                         #:name "and it says what it speaks")
            vc)
           ((assert-equal #:expect 'relational
                          #:got (runner-family
                                 (runner-for-engine (parse-engine "sqlite3")))
                          #:name "sqlite3 is spoken to by it")
            vc)
           ((assert-equal #:expect 'relational
                          #:got (runner-family
                                 (runner-for-engine (parse-engine "mariadb")))
                          #:name "and so is an engine reached through MySQL's driver")
            vc)))))

(define ledger-suite
  (suite "the ledger, on a real database"
         #:concurrent? #f
         (test "opening a session creates the ledger"
               #:shuffle? #f
               ledger-is-created-case)
         (test "a recorded migration reads back in full"
               #:shuffle? #f
               recorded-migration-case)
         (test "recording the same migration again replaces its row"
               #:shuffle? #f
               re-recorded-migration-case)
         (test "forgetting a migration removes its row"
               #:shuffle? #f
               forgotten-migration-case)
         (test "a session that may not write writes nothing"
               #:shuffle? #f
               read-only-session-case)
         (test "a closed session can be collected without an error"
               #:shuffle? #f
               closed-session-is-collectable-case)))

(define read-suite
  (suite "reading rows"
         #:concurrent? #f
         (test "every row of a query comes back"
               #:shuffle? #f
               rows-are-read-case)
         (test "an empty answer is not a failure"
               #:shuffle? #f
               empty-answer-case)
         (test "a refused query raises"
               #:shuffle? #f
               refused-query-case)))

(define apply-suite
  (suite "running a migration"
         #:concurrent? #f
         (test "a file becomes its statements, placeholders and all"
               #:shuffle? #f
               statements-are-substituted-case)
         (test "a dry run reports and changes nothing"
               #:shuffle? #f
               dry-run-case)
         (test "applying then cleaning leaves an empty database"
               #:shuffle? #f
               apply-then-clean-case)))

(define dispatch-suite
  (suite "the vtable"
         #:concurrent? #f
         (test "the SQL runner and the engines it answers for"
               #:shuffle? #f
               vtable-case)))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               ledger-suite
               read-suite
               apply-suite
               dispatch-suite))
