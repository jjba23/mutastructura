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
;; A MongoDB server that is only as real as the tests need: it speaks `OP_MSG',
;; does the server's half of the SCRAM-SHA-256 handshake, checks the client's
;; proof, and answers application commands from a table the test supplies.
;;
;; The dialogs a client is likely to get wrong are the ones it reproduces:
;;
;;   it answers `find' with the batch in a *document sequence* (a section of
;;   kind 1), which is what a real server does and what a client that reads only
;;   kind 0 silently sees as an empty batch;
;;
;;   it holds a cursor open across a `getMore', so a client that reads one batch
;;   and stops loses documents;
;;
;;   it refuses a wrong password, so the failure path of the handshake is
;;   exercised rather than assumed.
;;
;; It is a test double and not a server: no TLS, no compression, one connection,
;; and the commands it knows are the ones in `default-handlers'.  What it does
;; with bytes comes from the code under test's own BSON and frame encoders, which
;; are pinned to the specification by the known-answer vectors in the unit
;; tests, so sharing them here does not make the test circular.

(define-module (test helpers mongodb-mock)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 threads)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt base64)
  #:use-module (mutastructura persistence json)
  #:use-module (mutastructura persistence mongodb bson)
  #:use-module (mutastructura persistence mongodb client)
  #:export (with-mock-server
            mock-reply
            mock-error
            default-handlers
            mock-state-collections
            set-mock-state-collections!))

;;; ---------------------------------------------------------------------------
;;; Messages
;;; ---------------------------------------------------------------------------

(define* (mock-reply document #:key (response-to 0) (batch #f)
                     (batch-class "firstBatch"))
  "A whole `OP_MSG' reply, with DOCUMENT and, optionally, BATCH as a sequence.

BATCH is sent the way a server sends a cursor's documents: as a section of kind 1
rather than as an array inside the reply document."
  (let* ((kind-0 (concatenate-bytes (list (u8-bytes 0)
                                          (bson-encode-document document))))
         (kind-1 (if batch
                     (mock-sequence batch batch-class)
                     (make-bytevector 0 0)))
         (payload (concatenate-bytes (list (int32-bytes 0) kind-0 kind-1)))
         (header (concatenate-bytes (list (int32-bytes (+ 16
                                                         (bytevector-length
                                                          payload)))
                                          (int32-bytes 1)
                                          (int32-bytes response-to)
                                          (int32-bytes 2013)))))
    (concatenate-bytes (list header payload))))

(define (mock-sequence documents class)
  "An identifier and its DOCUMENTS, as an `OP_MSG' section of kind 1."
  (let* ((encoded (map bson-encode-document documents))
         (body (concatenate-bytes
                (cons (string->utf8 (string-append class
                                                   (string (integer->char 0))))
                      encoded))))
    (concatenate-bytes (list (u8-bytes 1)
                             (int32-bytes (+ 4 (bytevector-length body)))
                             body))))

(define* (mock-error message #:key (code 123) (code-name "Deliberate"))
  "A reply that fails the way a server fails: `ok: 0' and a sentence."
  (mock-reply (list (cons "ok" 0)
                    (cons "errmsg" message)
                    (cons "code" code)
                    (cons "codeName" code-name))))

;;; ---------------------------------------------------------------------------
;;; The state one connection keeps
;;; ---------------------------------------------------------------------------

(define-record-type <mock-state>
  (make-mock-state version wire collections store cursors nonce client-first
                   mechanism)
  mock-state?
  (version mock-state-version)
  (wire mock-state-wire)
  ;; The collections `listCollections' reports, so a test can watch a migration
  ;; create one.
  (collections mock-state-collections set-mock-state-collections!)
  ;; What is actually in them: an alist of collection name to the documents it
  ;; holds, oldest first.  A double that forgets what it was told cannot check a
  ;; ledger, and checking the ledger is most of what a migration runner does.
  (store mock-state-store set-mock-state-store!)
  ;; The cursors a `find' left open: an alist of id to the documents the next
  ;; `getMore' on it answers with.  A real server pages only what does not fit;
  ;; this one pages *every* result set, because the bug worth catching is the
  ;; client that reads the first batch and stops.
  (cursors mock-state-cursors set-mock-state-cursors!)
  (nonce mock-state-nonce set-mock-state-nonce!)
  (client-first mock-state-client-first set-mock-state-client-first!)
  (mechanism mock-state-mechanism))

;; What is already in the database when a connection opens: a couple of
;; documents for a `find' to page through.  No collection is *reported* until a
;; `create' says so -- a fresh database lists nothing, and the dry-run case leans
;; on that -- so this is the data of a database nobody has asked about yet.
(define seed-store
  (list (cons "users" (list (list (cons "name" "joe") (cons "age" 36))
                            (list (cons "name" "amy") (cons "age" 41))))))

(define (new-state)
  (make-mock-state "7.0.0" 17 #() seed-store '() #f #f "SCRAM-SHA-256"))

;;; ---------------------------------------------------------------------------
;;; Cursors
;;; ---------------------------------------------------------------------------

(define (mock-cursor-documents state id)
  "The documents the cursor ID still owes, or none."
  (or (assoc-ref (mock-state-cursors state) id) '()))

(define (open-mock-cursor! state documents)
  "Keep DOCUMENTS for a fresh cursor, answering with the id it was given."
  (let ((id (+ 1 (length (mock-state-cursors state)))))
    (set-mock-state-cursors! state
                             (cons (cons id documents)
                                   (mock-state-cursors state)))
    id))

(define (close-mock-cursor! state id)
  "Forget the cursor ID: its batch has been answered with."
  (set-mock-state-cursors! state
                           (filter (lambda (pair) (not (equal? (car pair) id)))
                                   (mock-state-cursors state))))

(define (mock-store-documents state collection)
  "The documents STATE holds for COLLECTION."
  (or (assoc-ref (mock-state-store state) collection) '()))

(define (set-mock-store-documents! state collection documents)
  "Keep DOCUMENTS as the contents of COLLECTION."
  (set-mock-state-store!
   state
   (cons (cons collection documents)
         (filter (lambda (pair) (not (equal? (car pair) collection)))
                 (mock-state-store state)))))

(define (add-mock-collection! state collection)
  "Add COLLECTION to what the server reports, keeping the list unique."
  (unless (member collection (vector->list (mock-state-collections state)))
    (set-mock-state-collections!
     state
     (list->vector (cons collection
                         (vector->list (mock-state-collections state)))))))

(define (document-matches? document query)
  "Whether DOCUMENT has every field QUERY asks for, with exactly that value.

A test's queries are equality on one or two fields, so equality is the whole of
what a matcher needs to be."
  (every (lambda (pair)
           (equal? (assoc-ref document (car pair)) (cdr pair)))
         query))

(define (matching-documents documents query)
  "The documents of DOCUMENTS that QUERY selects."
  (if (and (list? query) (pair? query))
      (filter (lambda (document) (document-matches? document query)) documents)
      documents))

;;; ---------------------------------------------------------------------------
;;; What it answers
;;; ---------------------------------------------------------------------------

(define (default-handlers)
  "The commands the mock server knows, as an alist of name to a procedure.

Each procedure is given the request and the connection's state, and answers with
the reply to send.  The ones that write -- `insert', `update', `delete', `create'
and `drop' -- keep what they were told in the state's store, because a ledger and
a migration are both things a test has to be able to look at afterwards."

  `(("hello" . ,(lambda (request state)
                  (mock-reply (list (cons "ok" 1)
                                    (cons "maxWireVersion"
                                          (mock-state-wire state))
                                    (cons "minWireVersion" 0)
                                    (cons "version" (mock-state-version state))
                                    (cons "saslSupportedMechs"
                                          #("SCRAM-SHA-256"))))))
;; `ping' echoes the database the client said it was talking to, which is
    ;; how a test can see that `$db' really travels with every command.
    ("ping" . ,(lambda (request state)
                 (mock-reply (list (cons "ok" 1)
                                   (cons "$db" (assoc-ref request "$db"))))))
    ("insert" . ,(lambda (request state)
                   (let* ((collection (format #f "~a" (assoc-ref request "insert")))
                          (documents (assoc-ref request "documents"))
                          (new (if (vector? documents)
                                   (vector->list documents)
                                   '())))
                     (add-mock-collection! state collection)
                     (set-mock-store-documents! state collection
                                                (append (mock-store-documents
                                                         state collection)
                                                        new))
                     (mock-reply (list (cons "ok" 1)
                                       (cons "n" (length new)))))))
    ;; `update' is what a ledger is written with: `q' picks the migration,
    ;; `u/$set' is what it becomes, and `upsert' is what makes the first write
    ;; an insert.
    ("update" . ,(lambda (request state)
                   (let* ((collection (format #f "~a" (assoc-ref request "update")))
                          (updates (assoc-ref request "updates"))
                          (entries (if (vector? updates)
                                       (vector->list updates)
                                       '())))
                     (add-mock-collection! state collection)
                     (for-each (lambda (entry)
                                 (let* ((query (assoc-ref entry "q"))
                                        (set (assoc-ref (assoc-ref entry "u")
                                                        "$set"))
                                        (existing (mock-store-documents
                                                   state collection)))
                                   (if (null? (matching-documents existing query))
                                       (when (assoc-ref entry "upsert")
                                         (set-mock-store-documents!
                                          state collection
                                          (cons set existing)))
                                       (set-mock-store-documents!
                                        state collection
                                        (map (lambda (document)
                                               (if (document-matches? document
                                                                      query)
                                                   (merge-document document set)
                                                   document))
                                             existing)))))
                               entries)
                     (mock-reply (list (cons "ok" 1)
                                       (cons "n" (length entries))
                                       (cons "nModified" (length entries)))))))
    ("delete" . ,(lambda (request state)
                   (let* ((collection (format #f "~a" (assoc-ref request "delete")))
                          (deletes (assoc-ref request "deletes"))
                          (entries (if (vector? deletes)
                                       (vector->list deletes)
                                       '()))
                          (queries (map (lambda (entry) (assoc-ref entry "q"))
                                        entries))
                          (before (mock-store-documents state collection))
                          (after (filter (lambda (document)
                                           (not (any (lambda (query)
                                                       (and (list? query)
                                                            (pair? query)
                                                            (document-matches?
                                                             document query)))
                                                     queries)))
                                         before)))
                     (set-mock-store-documents! state collection after)
                     (mock-reply (list (cons "ok" 1)
                                       (cons "n" (- (length before)
                                                    (length after))))))))
    ("create" . ,(lambda (request state)
                   (add-mock-collection! state
                                         (format #f "~a"
                                                 (assoc-ref request "create")))
                   (mock-reply '((ok . 1)))))
    ("createIndexes" . ,(lambda (request state) (mock-reply '((ok . 1)))))
    ("drop" . ,(lambda (request state)
                 (let ((collection (format #f "~a" (assoc-ref request "drop"))))
                   (set-mock-store-documents! state collection '())
                   (set-mock-state-collections!
                    state
                    (list->vector
                     (remove (lambda (name) (equal? name collection))
                             (vector->list (mock-state-collections state)))))
                   (mock-reply '((ok . 1))))))
    ("listCollections" . ,(lambda (request state)
                            (mock-reply
                             (list (cons "ok" 1)
                                   (cons "cursor"
                                         (list (cons "id" 0)
                                               (cons "ns"
                                                     "$cmd.listCollections"))))
                             #:batch (map (lambda (name)
                                            (list (cons "name" name)
                                                  (cons "type" "collection")))
                                          (vector->list
                                           (mock-state-collections state))))))
    ("dropDatabase" . ,(lambda (request state) (mock-reply '((ok . 1)))))
    ("find" . ,(lambda (request state)
                 (let* ((collection (format #f "~a" (assoc-ref request "find")))
                        (documents (matching-documents
                                    (mock-store-documents state collection)
                                    (assoc-ref request "filter")))
                        ;; One document in the reply's batch, the rest behind a
                        ;; cursor: the second one only arrives if the client asks
                        ;; for it.
                        (batch (if (null? documents) '() (list (car documents))))
                        (rest (if (null? documents) '() (cdr documents))))
                   (mock-reply
                    (list (cons "ok" 1)
                          (cons "cursor"
                                (list (cons "id" (if (null? rest)
                                                     0
                                                     (open-mock-cursor! state
                                                                        rest)))
                                      (cons "ns"
                                            (format #f "app.~a" collection)))))
                    #:batch batch))))
    ("getMore" . ,(lambda (request state)
                    (let* ((id (assoc-ref request "getMore"))
                           (collection (format #f "~a"
                                               (or (assoc-ref request
                                                              "collection")
                                                   "users")))
                           (documents (mock-cursor-documents state id)))
                      (close-mock-cursor! state id)
                      (mock-reply
                       (list (cons "ok" 1)
                             (cons "cursor" (list (cons "id" 0)
                                                  (cons "ns"
                                                        (format #f "app.~a"
                                                                collection)))))
                       #:batch documents
                       #:batch-class "nextBatch"))))
    ("count" . ,(lambda (request state) (mock-reply '((ok . 1) (n . 0)))))))

(define (merge-document document updates)
  "DOCUMENT with UPDATES' fields replaced, keeping DOCUMENT's own order."
  (let* ((keys (map car document))
         (new (filter (lambda (pair) (not (member (car pair) keys))) updates)))
    (append (map (lambda (pair)
                   (let ((replacement (assoc (car pair) updates)))
                     (if replacement replacement pair)))
                 document)
            new)))

(define (collection-count request)
  "How many documents the request says it is writing."
  (let ((documents (assoc-ref request "documents"))
        (deletes (assoc-ref request "deletes"))
        (updates (assoc-ref request "updates")))
    (cond
      ((vector? documents) (vector-length documents))
      ((vector? updates) (vector-length updates))
      ((vector? deletes) (vector-length deletes))
      (else 1))))

;;; ---------------------------------------------------------------------------
;;; SCRAM, the server's half
;;; ---------------------------------------------------------------------------

;; The salt and the iteration count are constants because a test that cannot
;; predict them cannot check the proof the client sends back.
(define mock-salt "saltysalt")
(define mock-iterations 4096)

(define (server-first nonce)
  "The server-first message for a client's NONCE."
  (format #f "r=~a%server,s=~a,i=~a" nonce
          (base64-encode (string->utf8 mock-salt)) mock-iterations))

(define (payload-fields payload)
  "PAYLOAD's `key=value' fields, as an alist of character keys."
  (map (lambda (part)
         (let ((split (string-index part #\=)))
           (if split
               (cons (string-ref part 0) (substring part (+ split 1)))
               (cons (string-ref part 0) ""))))
       (string-split (if (string? payload) payload (utf8->string payload)) #\,)))

(define (payload-field fields key)
  "KEY's value among FIELDS."
  (let ((pair (assoc key fields)))
    (and pair (cdr pair))))

(define (answer-scram name request state password)
  "The server's half of SCRAM, for a REQUEST the client sent.

The proof is checked against the password the server holds rather than against
anything the client sent: that is what makes a wrong password fail here, and
what makes the check meaningful rather than circular."
  (let* ((payload (bson-binary-bytes (assoc-ref request "payload")))
         (text (utf8->string payload))
         (mechanism (mock-state-mechanism state)))
    (if (equal? name "saslStart")
        (let* ((at (string-contains text ",r="))
               (client-first (substring text 3))
               (nonce (and at (substring text (+ at 3)))))
          (set-mock-state-nonce! state nonce)
          (set-mock-state-client-first! state client-first)
          (mock-reply (list (cons "ok" 1)
                            (cons "conversationId" 1)
                            (cons "done" #f)
                            (cons "payload"
                                  (bson-binary
                                   0
                                   (string->utf8 (server-first nonce)))))))
        (let* ((fields (payload-fields text))
               (proof (payload-field fields #\p))
               (combined (payload-field fields #\r))
               (client-first (mock-state-client-first state))
               (auth-message (format #f "~a,~a,c=biws,r=~a" client-first
                                     (server-first (mock-state-nonce state))
                                     combined))
               (salted (pbkdf2 (or password "") (string->utf8 mock-salt)
                               mock-iterations #:mechanism mechanism))
               (client-key (scram-hmac mechanism salted "Client Key"))
               (stored-key (scram-hash mechanism client-key))
               (signature (scram-hmac mechanism stored-key auth-message))
               (expected (bytevector-xor client-key signature))
               (server-signature (scram-hmac
                                  mechanism
                                  (scram-hmac mechanism salted "Server Key")
                                  auth-message)))
          (if (and proof (equal? (base64-decode proof) expected))
              (mock-reply (list (cons "ok" 1)
                                (cons "conversationId" 1)
                                (cons "done" #t)
                                (cons "payload"
                                      (bson-binary
                                       0
                                       (string->utf8
                                        (format #f "v=~a"
                                                (base64-encode
                                                 server-signature)))))))
              (mock-error "Authentication failed."
                          #:code 18
                          #:code-name "AuthenticationFailed"))))))

;;; ---------------------------------------------------------------------------
;;; Reading requests and serving one connection
;;; ---------------------------------------------------------------------------

(define (read-exactly port count)
  "COUNT bytes from PORT, or #f at the end of the connection."
  (let loop ((acc '()) (remaining count))
    (if (zero? remaining)
        (concatenate-bytes (reverse acc))
        (let ((chunk (get-bytevector-n port remaining)))
          (if (or (eof-object? chunk) (zero? (bytevector-length chunk)))
              #f
              (loop (cons chunk acc) (- remaining (bytevector-length chunk))))))))

(define (read-request port)
  "The next command the client sent, or #f when it has gone."
  (let ((header (read-exactly port 16)))
    (and header
         (let* ((length (bytevector-u32-ref header 0 (endianness little)))
                (body (read-exactly port (- length 16))))
           (and body (op-msg-decode (concatenate-bytes (list header body))))))))

(define (answer request handlers state user password)
  "The reply to REQUEST: the handshake, or whatever HANDLERS says."
  (let ((name (json-document->command-name request)))
    (if (member name '("saslStart" "saslContinue"))
        (answer-scram name request state password)
        (let ((handler (assoc-ref handlers name)))
          (if handler
              (handler request state)
              (mock-error (format #f "no such command: ~a" name)
                          #:code 59 #:code-name "CommandNotFound"))))))

(define (serve connection handlers state user password)
  "Answer requests on CONNECTION until the client stops asking."
  (let ((request (read-request connection)))
    (when request
      (put-bytevector connection (answer request handlers state user password))
      (force-output connection)
      (serve connection handlers state user password))))

(define (mock-listener)
  "A listening socket on an ephemeral loopback port.

It is bound here, in the thread the test runs in, rather than inside the server:
a port that is known before the server starts is a port a test can connect to
without waiting for a thread to be scheduled, and a wait loop is one more thing
that can time out for a reason nobody predicts."
  (let ((listener (socket AF_INET SOCK_STREAM 0)))
    (setsockopt listener SOL_SOCKET SO_REUSEADDR 1)
    (bind listener (make-socket-address AF_INET (inet-pton AF_INET "127.0.0.1")
                                        0))
    (listen listener 4)
    listener))

(define (serve-listener listener handlers user password)
  "Answer one client on LISTENER, and return once that client has gone.

`accept' answers with a pair whose first half is the connection and whose second
is the peer's address; it is the connection that is spoken to.  A listener that
was closed while this thread waited for a client fails here, which is how a test
that never connected ends the server rather than leaving it behind."
  (catch #t
         (lambda ()
           (let ((connection (car (accept listener))))
             ;; A socket port carries bytes; `OP_MSG' framing and BSON are read
             ;; and written with the binary port operations, which ignore any
             ;; transcoder.
             (dynamic-wind
              (lambda () #t)
              (lambda () (serve connection handlers (new-state) user password))
              (lambda ()
                (catch #t (lambda () (close-port connection)) (lambda _ #f))))))
         (lambda (key . args)
           ;; Worth saying out loud: a server that cannot accept is a broken
           ;; test double, and silence would look like a client that never
           ;; connected.
           (format (current-error-port)
                   "~%the mock server stopped: ~a ~s~%" key args))))

;;; ---------------------------------------------------------------------------
;;; Starting one
;;; ---------------------------------------------------------------------------

(define* (with-mock-server thunk #:key (user #f) (password #f) (handlers #f))
  "Run THUNK with a mock server's port, and stop the server afterwards.

THUNK is given the port the server is bound to.  The listener is closed before
this returns, so a test that connected leaves no thread behind, and a test that
never connected does not leave the suite waiting on one: the join that follows is
bounded, because a client that is not coming is not a reason to hang."
  (let* ((handlers* (or handlers (default-handlers)))
         (listener (mock-listener))
         (port (sockaddr:port (getsockname listener)))
         (thread (call-with-new-thread
                  (lambda () (serve-listener listener handlers* user password)))))
    (dynamic-wind
      (lambda () #t)
      (lambda () (thunk port))
      (lambda ()
        (catch #t (lambda () (close-port listener)) (lambda _ #f))
        (catch #t
               (lambda () (join-thread thread 1 'timed-out))
               (lambda _ #f))))))
