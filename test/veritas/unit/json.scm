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
;; The JSON reader a MongoDB migration file is read with, and the two decisions
;; this program makes on top of guile-json: `null' is `#nil' rather than the
;; symbol `null', and a migration file may be one command, an array of them, or a
;; document with a `commands' header.
;;
;; The cases worth writing down are the ones a `.sql' file cannot have.  A JSON
;; object and a JSON array are both ``a group of things'' to most readers and are
;; not to this one -- MongoDB accepts `"filter": {}' and refuses `"filter": []'
;; -- so the empty object and the empty array are asserted to come back as what
;; they are.  A `null' read as the symbol `null' would reach the server as the
;; *string* "null", which is a different document, so that too is asserted rather
;; than assumed.

(define-module (veritas unit json)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (mutastructura persistence json)
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

(define (truth value)
  "VALUE as a boolean, so an assertion can compare it with #t.

`assert-true' asks for #t itself, and most of the predicates worth asserting are
happy to answer with something merely true -- `member' hands back the tail it
found, `string-contains' the index it found -- so the conversion happens here
once rather than at each call."
  (and value #t))

;; A migration file with two commands, whose order is the point.
(define users-migration
  "test/fixtures/mongodb/2026-01-02_10:00:00_users.json")

;;; ---------------------------------------------------------------------------
;;; Reading
;;; ---------------------------------------------------------------------------

(define reading-suite
  (suite "reading JSON"
         #:concurrent? #f
         (test "an object is an alist with string keys"
               #:shuffle? #f
               (let ((value (read-json "{\"name\": \"users\", \"age\": 36}")))
                 (assert-equal #:expect "users"
                               #:got (assoc-ref value "name")
                               #:name "a string is a string")
                 (assert-equal #:expect 36
                               #:got (assoc-ref value "age")
                               #:name "and a number is a number")))
         (test "the order of a document's keys is kept"
               #:shuffle? #f
               ;; A migration file is read by people, so a command that lists
               ;; `key' before `name' should still list them that way when it is
               ;; echoed back or written to a log.
               (assert-equal #:expect '("b" "a")
                             #:got (map car (read-json "{\"b\": 1, \"a\": 2}"))
                             #:name "the keys come back as they were written"))
         (test "an array is a vector and an object is an alist"
               #:shuffle? #f
               ;; The reason documents and arrays cannot share a
               ;; representation.
               (let ((value (read-json "{\"documents\": [], \"filter\": {}}")))
                 (assert-equal #:expect #t
                               #:got (truth (vector? (assoc-ref value
                                                                "documents")))
                               #:name "an empty array is a vector")
                 (assert-equal #:expect #t
                               #:got (truth (null? (assoc-ref value "filter")))
                               #:name "an empty object is an empty alist")))
         (test "null is #nil, not the symbol `null'"
               #:shuffle? #f
               (let ((value (read-json "{\"success\": null, \"deleted\": null}")))
                 (assert-equal #:expect #t
                               #:got (truth (json-null? (assoc-ref value
                                                                   "success")))
                               #:name "the reader answers with the null value")
                 (assert-equal #:expect #nil
                               #:got (assoc-ref value "deleted")
                               #:name "and every null is that same value")))
         (test "a nested document is read as one, not as a string"
               #:shuffle? #f
               (let* ((value (read-json "{\"key\": {\"email\": 1}}"))
                      (key (assoc-ref value "key")))
                 (assert-equal #:expect 1
                               #:got (assoc-ref key "email")
                               #:name "the inner object has its own field")))
         (test "a number with a fraction that is integral reads as an integer"
               #:shuffle? #f
               ;; guile-json decides this, deliberately, and it decides which
               ;; BSON type the number is written as: `1.0' and `1' are the same
               ;; number to its reader.
               (assert-equal #:expect #t
                             #:got (truth (exact? (read-json "1.0")))
                             #:name "1.0 comes back exact"))
         (test "broken JSON is refused, naming where it broke"
               #:shuffle? #f
               ;; ``file not valid JSON'', with no file named, is not a useful
               ;; thing to read in a CI log.
               (let* ((message (error-message
                                (lambda ()
                                  (read-json "{\"name\": "
                                             #:source "migrations/bad.json"))))
                      (mentions? (lambda (needle)
                                   (truth (and message
                                               (string-contains message
                                                                needle))))))
                 (assert-equal #:expect #t
                               #:got (mentions? "migrations/bad.json")
                               #:name "the source is named")
                 (assert-equal #:expect #t
                               #:got (mentions? "not valid JSON")
                               #:name "and so is what was wrong with it")))))

;;; ---------------------------------------------------------------------------
;;; What a migration file may look like
;;; ---------------------------------------------------------------------------

(define documents-suite
  (suite "what a migration file may look like"
         #:concurrent? #f
         (test "one command is a migration of one command"
               #:shuffle? #f
               (let ((documents (read-json-documents
                                 "{\"insert\": \"users\"}")))
                 (assert-length #:expect 1 #:got documents
                                #:name "the file is one document")
                 (assert-equal #:expect "insert"
                               #:got (json-document->command-name
                                      (car documents))
                               #:name "and it is the command that was written")))
         (test "an array of commands is read in order"
               #:shuffle? #f
               (assert-equal
                #:expect '("create" "createIndexes")
                #:got (map json-document->command-name
                           (read-json-documents
                            "[{\"create\": \"users\"},\n        {\"createIndexes\": \"users\"}]"))
                #:name "the commands come back in the order of the file"))
         (test "a `commands' header is the same migration as a bare array"
               #:shuffle? #f
               ;; The header is what makes a file self-describing: it can carry
               ;; a version or a comment next to the commands.
               (assert-equal
                #:expect '("insert")
                #:got (map json-document->command-name
                           (read-json-documents
                            "{\"version\": 3, \"commands\": [{\"insert\": \"users\"}]}"))
                #:name "the commands are under the header"))
         (test "a migration file is read from disk"
               #:shuffle? #f
               (let ((documents (read-json-file users-migration)))
                 (assert-length #:expect 2 #:got documents
                                #:name "both commands are there")
                 (assert-equal #:expect '("create" "createIndexes")
                               #:got (map json-document->command-name documents)
                               #:name "in the order they were written")
                 (assert-equal #:expect "users"
                               #:got (assoc-ref (car documents) "create")
                               #:name "with the collection named")))))

;;; ---------------------------------------------------------------------------
;;; The command a document names
;;; ---------------------------------------------------------------------------

(define command-name-suite
  (suite "the command a document names"
         #:concurrent? #f
         (test "the first field that is not a modifier is the command"
               #:shuffle? #f
               ;; A MongoDB command is a document with one name at the top, next
               ;; to its modifiers.
               (assert-equal #:expect "find"
                             #:got (json-document->command-name
                                    '(("find" . "users") ("$db" . "app")))
                             #:name "the command is named"))
         (test "`$' fields and lsid are skipped"
               #:shuffle? #f
               (assert-equal #:expect "insert"
                             #:got (json-document->command-name
                                    '(("$db" . "app")
                                      ("lsid" . (("id" . "x")))
                                      ("insert" . "users")))
                             #:name "the modifiers come first and are not it"))
         (test "a document with no command has none"
               #:shuffle? #f
               (assert-equal #:expect #f
                             #:got (json-document->command-name '())
                             #:name "an empty document names nothing"))))

;;; ---------------------------------------------------------------------------
;;; Writing
;;; ---------------------------------------------------------------------------

(define writing-suite
  (suite "writing JSON back"
         #:concurrent? #f
         (test "a document comes back as it went in"
               #:shuffle? #f
               (let ((document '(("name" . "users")
                                 ("success" . #nil)
                                 ("documents" . #())
                                 ("size" . 8192))))
                 (assert-equal #:expect document
                               #:got (read-json (value->json document))
                               #:name "the round trip keeps every type")))
         (test "null is written as null"
               #:shuffle? #f
               (assert-contains #:expect (value->json '(("deleted_at" . #nil)))
                                #:got "null"
                                #:name "a null is written as the JSON null"))
         (test "pretty printing is available for reporting"
               #:shuffle? #f
               ;; What `--verbose' shows a migration's command as.
               (assert-contains #:expect (value->json '(("create" . "users"))
                                                     #:pretty #t)
                                #:got "\n"
                                #:name "a pretty document has lines"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               reading-suite
               documents-suite
               command-name-suite
               writing-suite))
