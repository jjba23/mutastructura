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
;; JSON, by way of guile-json rather than by hand.  guile-json already maps
;; JSON onto Scheme the way a migration file wants it read:
;;
;;   object -> alist of string to value      array -> vector
;;   string -> string                        number -> number
;;   true/false -> #t / #f
;;
;; Two things it cannot know, and which this module decides once for the whole
;; program:
;;
;;   null   `#nil', not the symbol `null'.  BSON has a null too, MongoDB is
;;          written in BSON, and a MongoDB migration document that says
;;          `{"success": null}' has to reach the server as a BSON null rather
;;          than as the string "null".  guile-json takes the value to use for
;;          null as a keyword argument, so this is a one-word difference from
;;          its default, applied at every entry point.
;;
;;   shape  what a migration file is allowed to look like.  A `.sql' file holds
;;          statements separated by `;'; the JSON equivalent has to accept the
;;          same three spellings people write, which is what
;;          `read-json-documents' is for.
;;
;; Note the pair/vector distinction, because it is the reason JSON documents and
;; JSON arrays cannot share a representation: MongoDB rejects `{"find": "c",
;; "filter": []}' and accepts `{"find": "c", "filter": {}}'.  guile-json keeps an
;; empty object an empty alist and an empty array an empty vector, as does the
;; BSON codec next door, so the distinction survives the whole round trip.
;;
;; One guile-json behaviour is worth knowing when writing a migration: a number
;; that is written with a fraction but *is* integral -- `1.0', `1e3' -- reads as
;; an exact integer, and so becomes a BSON int32 or int64 rather than a double.
;; That is guile-json's reader deciding, deliberately: `1.0' and `1' are the same
;; number.  A MongoDB query matches either against a field of either type, so
;; this only matters to a command that cares about the stored type, and there a
;; `NumberDouble` has to be written as something that is not an integer.

(define-module (mutastructura persistence json)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:export (read-json
            read-json-documents
            read-json-file
            value->json
            write-json
            json-document->command-name
            json-null?))

;; JSON's null, which is the value BSON's null reads as too.  Named so that the
;; places that need to say "this is a null" do not each spell `#nil' and hope.
(define json-null #nil)

(define (json-null? value)
  "Whether VALUE is JSON's null."
  (eq? value json-null))

(define (json-error-message condition)
  "A readable description of a guile-json failure CONDITION."
  (catch #t
         (lambda ()
           (let ((port (if (exception-with-irritants? condition)
                           (car (exception-irritants condition))
                           #f)))
             (if (port? port)
                 (format #f "not valid JSON (at line ~a, column ~a)"
                         (+ 1 (or (port-line port) 0))
                         (+ 1 (or (port-column port) 0)))
                 "not valid JSON")))
         (lambda _ "not valid JSON")))

(define* (read-json text #:key (source "JSON"))
  "Parse TEXT, a JSON document, into Scheme values.

Objects become alists with string keys, arrays vectors, `null' `#nil', and
numbers themselves -- an integer written without a fraction stays exact, so a
`1' reaches the server as a BSON int32 and a `1.0' as a double.

The order of a document's keys is kept, because a migration file is read by
people: a `createIndexes' command that lists `key' before `name' should still
list `key' before `name' when it is echoed back or written to a log.

SOURCE names the document in the error message when there is a syntax error,
because \"file not valid JSON\" with no file named is not a useful thing to read
in a CI log."
  (catch 'json-invalid
         (lambda ()
           (json-string->scm text #:null json-null #:ordered #t))
         (lambda (key . args)
           (raise-exception
            (make-exception-with-message
             (format #f "~a is ~a" source
                     (json-error-message
                      (make-exception-with-irritants args))))))))

(define* (read-json-file path)
  "Every JSON value the file at PATH holds, as the documents of one migration."
  (read-json-documents (call-with-input-file path get-string-all)
                       #:source path))

(define* (read-json-documents text #:key (source "JSON"))
  "The migration documents TEXT holds.

Three spellings are accepted, and they mean the same thing:

@example
{\"insert\": \"user\", \"documents\": [...]}          one command
[{\"insert\": ...}, {\"createIndexes\": ...}]         an array of commands
{\"commands\": [{\"insert\": ...}, ...]}              a file with a header
@end example

An object with a `commands' key is the one that makes a migration file
self-describing: it can carry a comment or a version next to the commands."
  (let* ((value (read-json text #:source source))
         (commands (if (and (list? value)
                            (pair? value)
                            (assoc-ref value "commands"))
                       (assoc-ref value "commands")
                       #f)))
    (cond
      (commands (if (vector? commands)
                    (vector->list commands)
                    (list commands)))
      ((vector? value) (vector->list value))
      (else (list value)))))

(define (json-document->command-name document)
  "The command a MongoDB DOCUMENT names, or #f.

A MongoDB command is a document with one name at the top, next to its modifiers:
`{\"insert\": \"user\", \"documents\": [...]}'.  Fields starting with `$' -- `$db',
`$readPreference' -- and `lsid' are modifiers, so they are skipped.  The answer is
used for reporting, never for deciding what to run."
  (and (list? document)
       (pair? document)
       (let loop ((pairs document))
         (cond
           ((null? pairs) #f)
           ((string-prefix? "$" (format #f "~a" (car (car pairs))))
            (loop (cdr pairs)))
           ((equal? "lsid" (format #f "~a" (car (car pairs))))
            (loop (cdr pairs)))
           (else (format #f "~a" (car (car pairs))))))))

(define* (write-json value #:optional (port (current-output-port))
                     #:key (pretty #t))
  "Write VALUE to PORT as JSON, with `#nil' written as null."
  (scm->json value port #:null json-null #:pretty pretty))

(define* (value->json value #:key (pretty #f))
  "VALUE written as a JSON string."
  (scm->json-string value #:null json-null #:pretty pretty))
