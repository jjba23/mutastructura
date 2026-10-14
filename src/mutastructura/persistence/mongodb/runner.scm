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
;; The document side of the runner vtable in `(mutastructura mutastructura)': a
;; session wraps one MongoDB connection, the ledger's collection, and the
;; options the command layer passed down.
;;
;; A migration here is a file of JSON command documents -- `{"create":
;; "users"}' , `{"createIndexes": ...}', `{"insert": ...}' -- run in the order
;; they were written, because that order is usually the point.  It is the
;; document-database equivalent of a `.sql' file, and it is read by the same
;; discovery code: an engine's `migration-extensions' decides what a migration
;; file looks like.
;;
;; Three decisions are worth spelling out.
;;
;;   the ledger    one *document* per migration in a collection, keyed on the
;;                 migration's name, with the same fields the SQL ledger has as
;;                 columns.  Writing one document per migration, replaced in
;;                 place, is what makes a repeatable migration's re-run visible
;;                 as a re-run rather than as a second migration, and a unique
;;                 index on `name' is what enforces it.
;;
;;   transactions  MongoDB's multi-document transactions need a replica set and
;;                 a session id per command, and this client sends neither.  A
;;                 failed migration's changes therefore *stand*, which is worth
;;                 saying plainly rather than discovering later -- the same
;;                 admission MySQL's implicit DDL commits force.
;;
;;   concurrency   there is no advisory lock to take, so what keeps two runs
;;                 from both applying the same migration is the ledger's unique
;;                 index: the second writer to insert its row is refused, and it
;;                 is refused before anything else it did is recorded.

(define-module (mutastructura persistence mongodb runner)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence json)
  #:use-module (mutastructura persistence mongodb bson)
  #:use-module ((mutastructura persistence mongodb client)
                #:prefix wire:)
  #:export (mongodb-runner
            mongo-session?
            mongo-session-engine
            mongo-session-spec
            mongo-session-database
            mongo-session-collection
            mongo-session-options
            mongo-open-session
            mongo-close!
            mongo-collections
            mongo-history
            mongo-ensure-history!
            mongo-record!
            mongo-forget!
            mongo-statements
            mongo-apply!
            mongo-begin!
            mongo-commit!
            mongo-rollback!
            mongo-clean!
            mongo-entry->document
            mongo-document->entry
            mongo-entry->upsert
            mongo-entry->delete
            mongo-history-index-command))

;;; ---------------------------------------------------------------------------
;;; The ledger's fields
;;; ---------------------------------------------------------------------------

;; The document's field names, and the entry keys the rest of the program reads
;; them as.  Kept in one place so the two cannot drift: a ledger written by this
;; version and read by the next one is the whole point of a ledger.
(define ledger-fields
  '(("name" . name)
    ("checksum" . checksum)
    ("description" . description)
    ("kind" . kind)
    ("installed_on" . installed-on)
    ("installed_by" . installed-by)
    ("execution_time" . execution-time)
    ("success" . success)))

(define (entry-value entry field)
  "FIELD's value in ENTRY, or #f."
  (let ((pair (assq field entry)))
    (and pair (cdr pair))))

(define (field-name field)
  "The document field an entry key FIELD is stored in."
  (or (car (find (lambda (pair) (eq? (cdr pair) field)) ledger-fields))
      (format #f "~a" field)))

(define (field-key name)
  "The entry key a document field NAME is read as."
  (or (cdr (assoc name ledger-fields))
      (string->symbol name)))

;;; ---------------------------------------------------------------------------
;;; Sessions
;;; ---------------------------------------------------------------------------

(define-record-type <mongo-session>
  (make-mongo-session engine spec client database collection options)
  mongo-session?
  (engine mongo-session-engine)
  (spec mongo-session-spec)
  (client mongo-session-client)
  (database mongo-session-database)
  (collection mongo-session-collection)
  (options mongo-session-options))

(define (session-log session level message)
  "Report MESSAGE at LEVEL through the session's logger, if it has one."
  (let ((log (assq-ref (mongo-session-options session) 'log)))
    (when log (log level message))))

(define (session-placeholders session)
  (or (assq-ref (mongo-session-options session) 'placeholders) '()))

;;; ---------------------------------------------------------------------------
;;; Reading the ledger
;;; ---------------------------------------------------------------------------

(define (mongo-document->entry document)
  "DOCUMENT, the ledger's own shape, as the entry the rest of the program reads.

The ledger is written with underscores because that is what a SQL column looks
like next door; everything above this line uses the hyphenated keys, so the two
back ends are interchangeable."
  (filter-map (lambda (pair)
                (let* ((name (format #f "~a" (car pair)))
                       (key (field-key name)))
                  (and (member name (map car ledger-fields))
                       (cons key (cdr pair)))))
              document))

(define (mongo-entry->document entry)
  "ENTRY as the document the ledger's collection holds.

Fields the caller left out are filled with what the ledger's defaults would have
been anyway -- a migration is versioned unless it says otherwise, its description
is its name, mutastructura is the one installing it -- which keeps a document
readable on its own."
  (let ((name (entry-value entry 'name))
        (kind (entry-value entry 'kind)))
    (list (cons "name" (format #f "~a" name))
          (cons "checksum" (format #f "~a" (or (entry-value entry 'checksum) "")))
          (cons "description" (format #f "~a" (or (entry-value entry 'description)
                                                  name)))
          (cons "kind" (if kind (format #f "~a" kind) "versioned"))
          (cons "installed_on" (or (entry-value entry 'installed-on)
                                   (bson-date-now-millis)))
          (cons "installed_by" (format #f "~a" (or (entry-value entry 'installed-by)
                                                   "mutastructura")))
          (cons "execution_time" (or (entry-value entry 'execution-time) 0))
          ;; A boolean rather than the SQL ledger's 1 or 0: BSON has one, and a
          ;; document that says true is a document somebody can read.  An entry
          ;; that does not mention success succeeded -- it is being recorded
          ;; because it did -- so only an explicit #f (or a SQL ledger's 0, which
          ;; a caller may hand over here) is a failure.
          (cons "success" (let ((success (assq 'success entry)))
                            (cond
                              ((not success) #t)
                              ((number? (cdr success)) (not (zero? (cdr success))))
                              (else (and (cdr success) #t))))))))

(define (mongo-collections session)
  "The collection names the server reports for the session's database."
  (map (lambda (document) (assoc-ref document "name"))
       (wire:mongo-command-cursor (mongo-session-client session)
                             '(("listCollections" . 1)))))

(define (mongo-history session)
  "Every document of the ledger, oldest first."
  (map mongo-document->entry
       (wire:mongo-command-cursor
        (mongo-session-client session)
        (list (cons "find" (mongo-session-collection session))
              (cons "sort" (list (cons "_id" 1)))))))

(define (mongo-history-index-command collection)
  "The command that gives the ledger COLLECTION its unique key on `name'."
  (list (cons "createIndexes" collection)
        (cons "indexes" (vector (list (cons "key" (list (cons "name" 1)))
                                      (cons "name" "name_1")
                                      (cons "unique" #t))))))

(define (mongo-ensure-history! session)
  "Create the ledger's collection and its unique key when they are missing.

Creating them when they are already there is not an error, which is what makes
this safe to run on every connection: MongoDB answers `create' on an existing
collection with an error only when it was asked for something different, and
`createIndexes' with an index that already exists is a no-op."
  (let ((collection (mongo-session-collection session))
        (client (mongo-session-client session)))
    (unless (member collection (mongo-collections session))
      (session-log session 'debug
                   (format #f "creating the ledger collection ~a" collection))
      (wire:mongo-command client (list (cons "create" collection))))
    (wire:mongo-command client (mongo-history-index-command collection))
    session))

;;; ---------------------------------------------------------------------------
;;; Writing the ledger
;;; ---------------------------------------------------------------------------

(define (mongo-entry->upsert collection entry)
  "The command that writes ENTRY to the ledger collection COLLECTION,
replacing the same migration's document when there is one."
  (list (cons "update" collection)
        (cons "updates"
              (vector (list (cons "q" (list (cons "name"
                                                  (format #f "~a"
                                                          (entry-value entry
                                                                       'name)))))
                            (cons "u" (list (cons "$set"
                                                  (mongo-entry->document entry))))
                            (cons "upsert" #t))))))

(define (mongo-entry->delete collection name)
  "The command that removes NAME's document from the ledger collection
COLLECTION."
  (list (cons "delete" collection)
        (cons "deletes"
              (vector (list (cons "q" (list (cons "name" (format #f "~a" name))))
                            (cons "limit" 1))))))

(define (mongo-record! session entry)
  "Write ENTRY to the ledger, replacing the document for the same migration."
  (wire:mongo-command (mongo-session-client session)
                 (mongo-entry->upsert (mongo-session-collection session)
                                      entry)))

(define (mongo-forget! session name)
  "Remove NAME's document from the ledger."
  (wire:mongo-command (mongo-session-client session)
                 (mongo-entry->delete (mongo-session-collection session)
                                      name)))

;;; ---------------------------------------------------------------------------
;;; Running a migration
;;; ---------------------------------------------------------------------------

(define (mongo-statements file options)
  "The command documents of migration FILE, with placeholders substituted.

Nothing is sent anywhere: this is what `--dry-run' reports and what the tests
compare against.  A statement's `position' is where it sits in the file, because
a JSON document has no separator to count lines by -- the position is what the
error messages name."
  (let* ((path (migration-path file))
         (text (migration-contents path
                                   #:placeholders (or (assq-ref options
                                                                'placeholders)
                                                      '())))
         (documents (read-json-documents text #:source (format #f "~a" path))))
    (map (lambda (document position)
           (list (cons 'index position)
                 (cons 'position position)
                 (cons 'command (json-document->command-name document))
                 (cons 'document document)
                 (cons 'json (value->json document #:pretty #t))))
         documents
         (iota (length documents) 1))))

(define (mongo-apply! session file options)
  "Run migration FILE's commands in order, returning how many ran."
  (let* ((name (migration-name file))
         (dry-run? (assq-ref options 'dry-run?))
         (verbose? (assq-ref options 'verbose?))
         (statements (mongo-statements file (append options
                                                    (list (cons 'engine
                                                                (mongo-session-engine
                                                                 session))))))
         (total (length statements)))
    (session-log session 'info (format #f "~a: ~a command~a"
                                       name total
                                       (if (= total 1) "" "s")))
    (unless dry-run?
      (for-each (lambda (statement)
                  (if verbose?
                      (session-log session 'debug (assq-ref statement 'json))
                      (session-log session 'debug
                                   (format #f "  [~a/~a] ~a"
                                           (assq-ref statement 'position) total
                                           (or (assq-ref statement 'command)
                                               "command"))))
                  (wire:mongo-command
                   (mongo-session-client session)
                   (assq-ref statement 'document)))
                statements))
    total))

;;; ---------------------------------------------------------------------------
;;; Transactions, locking and clean
;;; ---------------------------------------------------------------------------

(define (mongo-begin! session)
  "Nothing to open: a MongoDB migration is not transactional here.

Saying so plainly is better than pretending: the commands of a failed migration
have already been applied, and the ledger is what records that it did not
finish."
  (session-log session 'debug "no transaction to open (MongoDB, no session id)")
  session)

(define (mongo-commit! session) session)
(define (mongo-rollback! session) session)

(define (mongo-clean! session)
  "Drop every collection in the database, the ledger included."
  (let ((names (mongo-collections session)))
    (for-each (lambda (name)
                (session-log session 'debug (format #f "dropping ~a" name))
                (wire:mongo-command (mongo-session-client session)
                               (list (cons "drop" name))))
              names)
    (length names)))

;;; ---------------------------------------------------------------------------
;;; Opening and closing
;;; ---------------------------------------------------------------------------

(define* (mongo-open-session spec #:key (options '()))
  "Open a MongoDB session for SPEC and make sure the ledger is usable."
  (let* ((engine (connection-spec-engine spec))
         (options* (append options (list (cons 'engine engine))))
         (collection (or (assq-ref options 'history-table)
                         (engine-default-history-name engine)))
         (client (wire:mongo-connect spec)))
    (let ((session (make-mongo-session engine spec client
                                       (or (connection-spec-database spec) "admin")
                                       collection options*)))
      (mongo-ensure-history! session))))

(define (mongo-close! session)
  "Close SESSION's connection."
  (wire:mongo-close! (mongo-session-client session)))

;;; ---------------------------------------------------------------------------
;;; The vtable
;;; ---------------------------------------------------------------------------

(define (mongodb-runner)
  "The runner for the document engines."
  (make-runner
   ;; As with SQL, the vtable is family-wide: which engine a session talks to
   ;; comes from the connection it was opened with.
   'document
   "MongoDB over the wire protocol (JSON command documents)"
   (lambda (spec options) (mongo-open-session spec #:options options))
   (lambda (session) (mongo-close! session))
   (lambda (session) (mongo-history session))
   (lambda (session) (mongo-ensure-history! session))
   (lambda (session entry) (mongo-record! session entry))
   (lambda (session name) (mongo-forget! session name))
   (lambda (session file options) (mongo-apply! session file options))
   (lambda (session) (mongo-begin! session))
   (lambda (session) (mongo-commit! session))
   (lambda (session) (mongo-rollback! session))
   (lambda (session) (mongo-clean! session))
   (lambda (file options) (mongo-statements file options))
   ;; A failed migration's commands stand: see the commentary above.
   (lambda (session) #f)))
