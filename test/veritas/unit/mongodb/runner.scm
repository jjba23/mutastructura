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
;; The MongoDB runner, which is the half of the tool that is not SQL: a ledger
;; kept as documents in a collection, a migration that is a file of JSON
;; commands, and the dispatch that decides an engine of the `document' family
;; gets this runner and not the SQL one.
;;
;; The cases that need a server use the mock next door, which keeps what it is
;; told: a ledger test that cannot read the ledger back proves nothing, and a
;; migration test that cannot see the collection it created proves the same.
;; Anything that touches the wire lives in a procedure of one argument -- called
;; with the run's `vc' when the test runs, not while this file is loaded, because
;; a socket opened during a load hangs the load -- and each test is then a single
;; shallow form naming that procedure.

(define-module (veritas unit mongodb runner)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 format)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence json)
  #:use-module (mutastructura persistence mongodb bson)
  #:use-module (mutastructura persistence mongodb runner)
  #:use-module (mutastructura persistence runners)
  #:use-module (test helpers mongodb-mock)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define ledger-name "_mutastructura_history")

(define (url port)
  (format #f "mongodb://127.0.0.1:~a/app" port))

(define (open-session port)
  (mongo-open-session (parse-connection-url (url port))))

(define (with-ledger thunk)
  "Run THUNK with a session on a mock server, and answer what it collected.

The session is closed before the server is: a client that is still connected
when the listener goes is a mock that waits out a timeout it does not need."
  (with-mock-server
   (lambda (port)
     (let ((session (open-session port)))
       (let ((results (thunk session)))
         (mongo-close! session)
         results)))))

(define (passed? result)
  (and (pair? result)
       (eq? #t (assoc-ref result 'assertion-successful))))

(define (verdict vc results)
  "The result a case ends on, given the RESULTS of its assertions."
  (let ((failed (filter (lambda (result) (not (passed? result))) results)))
    (if (null? failed)
        (car (reverse results))
        ((assert-true #f #:name "an earlier assertion in this case failed") vc))))

(define (migration path)
  (migration-file path))

;; A ledger entry, as the command layer hands one to a runner.
(define entry
  '((name . "2026-01-02_10:00:00_users")
    (checksum . "d0f1")
    (description . "users")
    (kind . versioned)
    (execution-time . 12)))

;; A migration that creates a collection and then indexes it: two commands whose
;; order is the point.
(define users-migration
  "test/fixtures/mongodb/2026-01-02_10:00:00_users.json")

;; A migration that inserts what it is given, with a placeholder in it.
(define seed-migration
  "test/fixtures/mongodb/2026-01-01_09:00:00_seed.repeatable.json")

(define (statements-of path options)
  (mongo-statements (migration path) options))

(define (first-document-name path options)
  "The `name' of the first document of the first command of PATH's migration."
  (let* ((statement (car (statements-of path options)))
         (documents (assoc-ref (assq-ref statement 'document) "documents")))
    (assoc-ref (vector-ref documents 0) "name")))

;;; ---------------------------------------------------------------------------
;;; An entry and its document
;;; ---------------------------------------------------------------------------

(define document-suite
  (suite "an entry and its document"
         #:concurrent? #f
         (test "an entry becomes a document, with its defaults filled in"
               #:shuffle? #f
               (let ((document (mongo-entry->document entry)))
                 (assert-equal #:expect "2026-01-02_10:00:00_users"
                               #:got (assoc-ref document "name")
                               #:name "the name is the key")
                 (assert-equal #:expect "d0f1"
                               #:got (assoc-ref document "checksum")
                               #:name "the checksum is kept")
                 (assert-equal #:expect "versioned"
                               #:got (assoc-ref document "kind")
                               #:name "a migration is versioned unless it says otherwise")
                 (assert-equal #:expect "mutastructura"
                               #:got (assoc-ref document "installed_by")
                               #:name "and mutastructura installed it")
                 (assert-equal #:expect #t
                               #:got (assoc-ref document "success")
                               #:name "success is a boolean, not 1 or 0")
                 (assert-true (number? (assoc-ref document "installed_on"))
                              #:name "the time is filled in when the caller has none")))
         (test "a document reads back as an entry"
               #:shuffle? #f
               (let* ((document '(("name" . "2026-01-02_10:00:00_users")
                                  ("checksum" . "d0f1")
                                  ("description" . "users")
                                  ("kind" . "versioned")
                                  ("installed_on" . 1700000000000)
                                  ("installed_by" . "mutastructura")
                                  ("execution_time" . 12)
                                  ("success" . #t)))
                      (back (mongo-document->entry document)))
                 (assert-equal #:expect "2026-01-02_10:00:00_users"
                               #:got (history-entry-name back)
                               #:name "the core reads the name it expects")
                 (assert-equal #:expect 'versioned
                               #:got (history-entry-kind back)
                               #:name "and the kind as a symbol")
                 (assert-equal #:expect 12
                               #:got (history-entry-execution-time back)
                               #:name "and the execution time")
                 (assert-true (history-entry-success back)
                              #:name "and a success that is a boolean")))
         (test "a document survives the trip through BSON"
               #:shuffle? #f
               (let* ((document (mongo-entry->document entry))
                      (back (mongo-document->entry
                             (bson-decode (bson-encode document)))))
                 (assert-equal #:expect (history-entry-name entry)
                               #:got (history-entry-name back)
                               #:name "the name comes back")
                 (assert-equal #:expect (history-entry-checksum entry)
                               #:got (history-entry-checksum back)
                               #:name "the checksum comes back")
                 (assert-true (history-entry-success back)
                              #:name "and so does the success")))
         (test "the ledger's unique key is the migration's name"
               #:shuffle? #f
               (let* ((command (mongo-history-index-command ledger-name))
                      (index (vector-ref (assoc-ref command "indexes") 0)))
                 (assert-equal #:expect "createIndexes"
                               #:got (json-document->command-name command)
                               #:name "an index is created")
                 (assert-equal #:expect #t
                               #:got (assoc-ref index "unique")
                               #:name "and it is unique, which keeps two runs apart")
                 (assert-equal #:expect 1
                               #:got (assoc-ref (assoc-ref index "key") "name")
                               #:name "on the migration's name")))))

;;; ---------------------------------------------------------------------------
;;; The commands a ledger is written with
;;; ---------------------------------------------------------------------------

(define command-suite
  (suite "the commands the ledger is written with"
         #:concurrent? #f
         (test "recording a migration is an upsert on its name"
               #:shuffle? #f
               (let* ((command (mongo-entry->upsert ledger-name entry))
                      (update (vector-ref (assoc-ref command "updates") 0)))
                 (assert-equal #:expect ledger-name
                               #:got (assoc-ref command "update")
                               #:name "the ledger collection is named")
                 (assert-equal #:expect #t
                               #:got (assoc-ref update "upsert")
                               #:name "a first write inserts")
                 (assert-equal #:expect "2026-01-02_10:00:00_users"
                               #:got (assoc-ref (assoc-ref update "q") "name")
                               #:name "the document is picked by name")
                 (assert-equal #:expect "d0f1"
                               #:got (assoc-ref (assoc-ref (assoc-ref update "u")
                                                           "$set")
                                                "checksum")
                               #:name "and replaced field by field")))
         (test "forgetting a migration deletes one document"
               #:shuffle? #f
               (let* ((command (mongo-entry->delete ledger-name
                                                    "2026-01-02_10:00:00_users"))
                      (delete (vector-ref (assoc-ref command "deletes") 0)))
                 (assert-equal #:expect ledger-name
                               #:got (assoc-ref command "delete")
                               #:name "the ledger collection is named")
                 (assert-equal #:expect "2026-01-02_10:00:00_users"
                               #:got (assoc-ref (assoc-ref delete "q") "name")
                               #:name "the document is picked by name")
                 (assert-equal #:expect 1
                               #:got (assoc-ref delete "limit")
                               #:name "and only one is removed")))))

;;; ---------------------------------------------------------------------------
;;; Reading a migration
;;; ---------------------------------------------------------------------------

(define statements-suite
  (suite "a migration's commands"
         #:concurrent? #f
         (test "the commands are read in the order they were written"
               #:shuffle? #f
               (let ((statements (statements-of users-migration '())))
                 (assert-length #:expect 2 #:got statements
                                #:name "both commands are there")
                 (assert-equal #:expect '("create" "createIndexes")
                               #:got (map (lambda (statement)
                                            (assq-ref statement 'command))
                                          statements)
                               #:name "in the order of the file")
                 (assert-equal #:expect '(1 2)
                               #:got (map (lambda (statement)
                                            (assq-ref statement 'position))
                                          statements)
                               #:name "each carries its position")))
         (test "a document's JSON is available for reporting"
               #:shuffle? #f
               (let ((first (car (statements-of users-migration '()))))
                 (assert-contains #:expect (or (assq-ref first 'json) "")
                                  #:got "\"create\""
                                  #:name "the command is echoed as JSON")))
         (test "a placeholder is substituted"
               #:shuffle? #f
               (assert-equal #:expect "ada"
                             #:got (first-document-name
                                    seed-migration
                                    '((placeholders . (("user" . "ada")))))
                             #:name "${user} became the value it was given"))
         (test "an unknown placeholder is left alone"
               #:shuffle? #f
               (assert-equal #:expect "${user}"
                             #:got (first-document-name seed-migration '())
                             #:name "a placeholder nobody defined is not mangled"))
         (test "a bare array and a `commands' header read the same"
               #:shuffle? #f
               ;; Two other spellings a migration file may use, both read by the
               ;; JSON reader the runner reads files with.
               (assert-length
                #:expect 1
                #:got (read-json-documents
                       "[{\"insert\": \"users\", \"documents\": []}]")
                #:name "a bare array")
               (assert-length
                #:expect 1
                #:got (read-json-documents
                       "{\"commands\": [{\"insert\": \"users\"}]}")
                #:name "a file with a header"))))

;;; ---------------------------------------------------------------------------
;;; The ledger, against the mock server
;;; ---------------------------------------------------------------------------

(define (ledger-is-created-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (list ((assert-true (and (member (mongo-session-collection session)
                                       (mongo-collections session))
                               #t)
                          #:name "the ledger's collection exists")
             vc)
            ((assert-empty #:got (mongo-history session)
                           #:name "and holds nothing yet")
             vc))))))

(define (recorded-migration-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (mongo-record! session entry)
      (let ((history (mongo-history session)))
        (list ((assert-length #:expect 1 #:got history
                              #:name "one migration is one document")
               vc)
              ((assert-equal #:expect "2026-01-02_10:00:00_users"
                             #:got (history-entry-name (car history))
                             #:name "with the name it was recorded under")
               vc)
              ((assert-equal #:expect "d0f1"
                             #:got (history-entry-checksum (car history))
                             #:name "and its checksum")
               vc)
              ((assert-true (history-entry-success (car history))
                            #:name "and a success the core can read")
               vc)
              ((assert-true (number? (history-entry-installed-on (car history)))
                            #:name "and a time the server stamped")
               vc)))))))

(define (re-recorded-migration-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (mongo-record! session entry)
      (mongo-record! session (acons 'checksum "beef" entry))
      (let ((history (mongo-history session)))
        (list ((assert-length #:expect 1 #:got history
                              #:name "a re-run is not a second migration")
               vc)
              ((assert-equal #:expect "beef"
                             #:got (history-entry-checksum (car history))
                             #:name "and the newer checksum is the one kept")
               vc)))))))

(define (forgotten-migration-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (mongo-record! session entry)
      (mongo-forget! session "2026-01-02_10:00:00_users")
      (list ((assert-empty #:got (mongo-history session)
                           #:name "the ledger is empty again")
             vc))))))

(define (vtable-case vc)
  (let ((runner (mongodb-runner)))
    (verdict
     vc
     (with-ledger
      (lambda (session)
        (list ((assert-equal #:expect 'document
                             #:got (runner-family runner)
                             #:name "a document-family runner")
               vc)
              ((assert-false ((runner-transactional? runner) session)
                             #:name "with no transaction to roll back")
               vc)
              ((assert-equal #:expect ledger-name
                             #:got (mongo-session-collection session)
                             #:name "and the engine's default ledger name")
               vc)
              ((assert-empty #:got ((runner-history runner) session)
                             #:name "the vtable reads the same ledger")
               vc)
              ((assert-no-error
                (lambda ()
                  ((runner-record! runner) session entry)
                  ((runner-ensure-history! runner) session))
                #:name "recording through the vtable works")
               vc)
              ((assert-length #:expect 1
                              #:got ((runner-history runner) session)
                              #:name "and the entry is there")
               vc)))))))

(define ledger-suite
  (suite "the ledger, on a server"
         #:concurrent? #f
         (test "opening a session creates the ledger"
               #:shuffle? #f
               ledger-is-created-case)
         (test "a recorded migration is in the ledger"
               #:shuffle? #f
               recorded-migration-case)
         (test "recording the same migration again replaces its document"
               #:shuffle? #f
               re-recorded-migration-case)
         (test "forgetting a migration removes its document"
               #:shuffle? #f
               forgotten-migration-case)
         (test "the vtable is what the command layer sees"
               #:shuffle? #f
               vtable-case)))

;;; ---------------------------------------------------------------------------
;;; Running a migration, against the mock server
;;; ---------------------------------------------------------------------------

(define (apply-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (list ((assert-equal #:expect 2
                           #:got (mongo-apply! session
                                               (migration users-migration)
                                               '())
                           #:name "both commands ran")
             vc)
            ((assert-true (and (member "users" (mongo-collections session)) #t)
                          #:name "and the collection it created is there")
             vc))))))

(define (dry-run-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (list ((assert-equal #:expect 2
                           #:got (mongo-apply! session
                                               (migration users-migration)
                                               '((dry-run? . #t)))
                           #:name "the commands are counted")
             vc)
            ((assert-false (member "users" (mongo-collections session))
                           #:name "and none of them was sent")
             vc))))))

(define (clean-case vc)
  (verdict
   vc
   (with-ledger
    (lambda (session)
      (mongo-apply! session (migration users-migration) '())
      (let ((dropped (mongo-clean! session)))
        (list ((assert-equal #:expect 2 #:got dropped
                             #:name "the ledger and the migration's collection")
               vc)
              ((assert-empty #:got (mongo-collections session)
                             #:name "and the database is empty again")
               vc)))))))

(define apply-suite
  (suite "running a migration, on a server"
         #:concurrent? #f
         (test "a migration's commands are run, in order"
               #:shuffle? #f
               apply-case)
         (test "a dry run reports without running"
               #:shuffle? #f
               dry-run-case)
         (test "clean drops every collection"
               #:shuffle? #f
               clean-case)))

;;; ---------------------------------------------------------------------------
;;; Which runner an engine gets
;;; ---------------------------------------------------------------------------

(define dispatch-suite
  (suite "which runner an engine gets"
         #:concurrent? #f
         (test "an engine's family decides its runner"
               #:shuffle? #f
               (assert-equal #:expect 'document
                             #:got (runner-family
                                    (runner-for-engine (find-engine 'mongodb)))
                             #:name "mongodb is spoken to by the document runner")
               (assert-equal #:expect 'relational
                             #:got (runner-family
                                    (runner-for-engine (find-engine 'postgresql)))
                             #:name "postgresql by the SQL one")
               (assert-equal #:expect 'relational
                             #:got (runner-family
                                    (runner-for-engine (find-engine 'cockroachdb)))
                             #:name "and an engine reached through a driver we ship")
               (assert-equal #:expect 'document
                             #:got (runner-family
                                    (runner-for-engine (find-engine 'mongo)))
                             #:name "an alias picks the same runner"))
         (test "the runner follows the connection, not the call"
               #:shuffle? #f
               (assert-equal
                #:expect 'document
                #:got (runner-family
                       (runner-for-spec
                        (parse-connection-url "mongodb://127.0.0.1:27017/app")))
                #:name "a connection spec is enough to pick one")
               (assert-equal
                #:expect 'relational
                #:got (runner-family
                       (runner-for-spec
                        (parse-connection-url "postgresql://127.0.0.1:5432/app")))
                #:name "and so is a PostgreSQL URL"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               document-suite
               command-suite
               statements-suite
               ledger-suite
               apply-suite
               dispatch-suite))
