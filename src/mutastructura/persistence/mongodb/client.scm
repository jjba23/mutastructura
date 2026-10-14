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
;; A MongoDB client, written here rather than linked from somewhere, for the
;; same reason the SQL side uses guile-dbi and nothing more: a migration tool
;; that needs a driver and another language runtime installed before it will run
;; is a migration tool that does not get run.  Nothing here but Guile, sockets
;; and libgcrypt (through guile-gcrypt, which is already a dependency because of
;; the SHA-256 checksums).
;;
;; The protocol is small once it is written down.  A message is a sixteen byte
;; header -- length, request id, what it answers, opcode -- followed by a body.
;; Since MongoDB 3.6 the body of a command and of its reply is `OP_MSG' (2013):
;; four bytes of flags, then sections.  Section kind 0 is the command document;
;; section kind 1 is a *document sequence*, the same documents sent as a run
;; rather than as an array inside the command, which is how a server sends a
;; cursor's batch.  Both are read here, because a `find' reply that arrives as a
;; sequence is the normal case and a client that only reads kind 0 would see an
;; empty batch.
;;
;; Authentication is SASL, and the mechanisms are SCRAM: SHA-256 first, with
;; SHA-1 kept for users created before MongoDB 4.0.  SCRAM is a four-message
;; dance whose whole point is that the password never crosses the wire, and it
;; is built from libgcrypt's HMAC and SHA rather than by hand: PBKDF2 is an HMAC
;; in a loop, the client proof is the exclusive-or of an HMAC and a key, and the
;; server's own proof is checked before the connection is trusted.  A wrong
;; password fails here, with the server's reason, rather than at the first
;; command.
;;
;; Two things are deliberately not supported, and both are refused by name
;; rather than attempted:
;;
;;   TLS        an SRV lookup and a trust store are a different program's worth
;;              of policy.  `mongodb+srv://' is refused while parsing the URL; a
;;              `tls=true' parameter is refused here, in words that say what to
;;              do instead.
;;
;;   OP_QUERY   the pre-3.6 handshake, which MongoDB 6 removed from the server.
;;              A server older than wire version 6 is refused with the version
;;              it actually reported.
;;
;; The handshake's answer is kept on the session: `maxWireVersion' decides what
;; may be hoped for, and the server's version string is what `info' prints.

(define-module (mutastructura persistence mongodb client)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt base64)
  #:use-module (gcrypt hash)
  #:use-module (gcrypt mac)
  #:use-module (gcrypt random)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence json)
  #:use-module (mutastructura persistence mongodb bson)
  #:export (mongo-connect
            mongo-close!
            mongo-command
            mongo-command-cursor
            mongo-commands
            mongo-session?
            mongo-session-database
            mongo-session-server-info
            mongo-server-version
            mongo-max-wire-version
            mongo-error?
            mongo-error-message
            mongo-error-code
            op-msg-encode
            op-msg-decode
            mongo-wire-version-minimum
            scram-hmac
            scram-hash
            scram-nonce
            pbkdf2
            bytevector-xor))

;;; ---------------------------------------------------------------------------
;;; The oldest server this client speaks to
;;; ---------------------------------------------------------------------------

;; Wire version 6 is MongoDB 3.6, the release that introduced `OP_MSG' and so
;; the oldest server an `OP_MSG' handshake can talk to.  Refusing by number, and
;; printing the number the server reported, is what turns "the connection went
;; quiet" into "upgrade the server".
(define mongo-wire-version-minimum 6)

(define op-msg 2013)

;;; ---------------------------------------------------------------------------
;;; Errors
;;; ---------------------------------------------------------------------------

(define-record-type <mongo-error>
  (make-mongo-error message code code-name)
  mongo-error?
  (message mongo-error-message)
  (code mongo-error-code)
  (code-name mongo-error-code-name))

(define (raise-mongo-error format-string . args)
  "Raise a MongoDB error whose message is FORMAT-STRING applied to ARGS."
  (raise-exception
   (make-mongo-error (apply format #f format-string args) #f #f)))

(define (raise-server-error! response context)
  "Raise the server's own error from RESPONSE, naming CONTEXT.

A failed command is answered with `ok: 0' and a sentence worth reading --
`not authorized on app to execute command', `Index must have unique name' -- so
the sentence is passed on rather than replaced by a number."
  (let ((code (assoc-ref response "code"))
        (code-name (assoc-ref response "codeName"))
        (message (or (assoc-ref response "errmsg") "no message")))
    (raise-exception
     (make-mongo-error
      (format #f "~a~a~a"
              (or-empty (and context (format #f "~a: " context)))
              message
              (cond ((and code-name code)
                     (format #f " (~a, code ~a)" code-name code))
                    (code-name (format #f " (~a)" code-name))
                    (code (format #f " (code ~a)" code))
                    (else "")))
      code code-name))))

;;; ---------------------------------------------------------------------------
;;; Binary helpers
;;; ---------------------------------------------------------------------------

(define (bytevector-xor a b)
  "A bytevector of A's bytes exclusive-ored with B's, which must be as long."
  (unless (= (bytevector-length a) (bytevector-length b))
    (raise-mongo-error "cannot exclusive-or bytevectors of ~a and ~a bytes"
                       (bytevector-length a) (bytevector-length b)))
  (let ((out (make-bytevector (bytevector-length a) 0)))
    (let loop ((i 0))
      (unless (= i (bytevector-length a))
        (bytevector-u8-set! out i (logxor (bytevector-u8-ref a i)
                                          (bytevector-u8-ref b i)))
        (loop (+ i 1))))
    out))

(define (utf8-bytes text)
  "TEXT's bytes, treating a non-string as its written form."
  (string->utf8 (if (string? text) text (format #f "~a" text))))

(define (bytes-utf8 bytes)
  "BYTES as a string, assuming UTF-8."
  (utf8->string bytes))

(define (base64-of value)
  "VALUE, a string or a bytevector, encoded as base64."
  (base64-encode (if (bytevector? value) value (utf8-bytes value))))

(define (read-exactly port count)
  "COUNT bytes from PORT, or #f when the connection ended first.

`get-bytevector-n' answers with fewer bytes than asked for when the port has
some but not enough, so the loop is what turns a stream into a message."
  (let loop ((acc '()) (remaining count))
    (if (zero? remaining)
        (concatenate-bytes (reverse acc))
        (let ((chunk (get-bytevector-n port remaining)))
          (if (or (eof-object? chunk) (zero? (bytevector-length chunk)))
              #f
              (loop (cons chunk acc) (- remaining (bytevector-length chunk))))))))

(define (cstring-at bytes offset)
  "The NUL-terminated name at OFFSET, as bytes."
  (let loop ((at offset) (acc '()))
    (if (>= at (bytevector-length bytes))
        (raise-mongo-error "a section name runs past the end of the message")
        (if (zero? (bytevector-u8-ref bytes at))
            (u8-list->bytevector (reverse acc))
            (loop (+ at 1) (cons (bytevector-u8-ref bytes at) acc))))))

(define (read-documents bytes start stop)
  "The documents of BYTES from START to STOP, as a vector."
  (let loop ((at start) (acc '()))
    (if (>= at stop)
        (list->vector (reverse acc))
        (receive (value next) (bson-decode-at bytes at)
          (loop next (cons value acc))))))

;;; ---------------------------------------------------------------------------
;;; OP_MSG
;;; ---------------------------------------------------------------------------

(define* (op-msg-encode request-id document #:key (response-to 0))
  "REQUEST-ID's `OP_MSG' message holding DOCUMENT, answering to RESPONSE-TO."
  (let* ((payload (concatenate-bytes (list (int32-bytes 0)        ; flags
                                           (u8-bytes 0)          ; section 0
                                           (bson-encode-document document))))
         (header (concatenate-bytes (list (int32-bytes
                                           (+ 16 (bytevector-length payload)))
                                          (int32-bytes request-id)
                                          (int32-bytes response-to)
                                          (int32-bytes op-msg)))))
    (concatenate-bytes (list header payload))))

(define (op-msg-decode bytes)
  "The command document an `OP_MSG' reply BYTES holds.

A reply may carry its documents as a kind 0 section, as kind 1 sequences, or as
both -- a cursor's batch arrives as a sequence as often as not -- and the two are
merged into the one document the rest of this module works with."
  (let ((length (bytevector-u32-ref bytes 0 (endianness little)))
        (opcode (bytevector-s32-ref bytes 12 (endianness little))))
    (unless (= opcode op-msg)
      (raise-mongo-error
       "the server answered with opcode ~a, and only OP_MSG (~a) is understood"
       opcode op-msg))
    (let* ((flags (bytevector-u32-ref bytes 16 (endianness little)))
           ;; Bit 0 says a CRC-32C follows the sections.  It is only set when
           ;; the client asked for one, which this client does not, but reading
           ;; the length correctly costs a line and makes a surprising answer a
           ;; non-event rather than a truncated document.
           (end (- length (if (odd? flags) 4 0))))
      (let loop ((at 20) (document '()) (sequences '()))
        (if (>= at end)
            ;; `fold' hands each element and the accumulator to the procedure,
            ;; so the sequence is the first argument and the reply the second.
            (fold place-sequence document (reverse sequences))
            (let ((kind (bytevector-u8-ref bytes at)))
              (case kind
                ((0)
                 (receive (value next) (bson-decode-at bytes (+ at 1))
                   (loop next (append document value) sequences)))
                ((1)
                 (let* ((size (bytevector-u32-ref bytes (+ at 1)
                                                  (endianness little)))
                        (class (cstring-at bytes (+ at 5)))
                        (start (+ at 5 (bytevector-length class) 1))
                        (stop (+ at 1 size)))
                   (loop stop document
                         (cons (cons (bytes-utf8 class)
                                     (read-documents bytes start stop))
                               sequences))))
                (else
                 (raise-mongo-error "the server sent section kind ~a" kind)))))))))

;; A document sequence arrives *beside* the command's own fields, and its
;; identifier names what it is: `firstBatch', `nextBatch', `documents'.  The
;; field it belongs to is a document that was sent without it -- for a cursor
;; reply, `cursor' -- so the sequence goes into the first document-valued field
;; that does not already have that key.  That is exactly what the official
;; drivers do, and it is why a `find' reply whose batch arrived as a sequence
;; still reads here as `cursor' holding `firstBatch'.
(define (place-sequence sequence document)
  "DOCUMENT with SEQUENCE's documents placed in the field they belong to."
  (let ((class (car sequence))
        (documents (cdr sequence)))
    (if (assoc class document)
        (map (lambda (pair)
               (if (equal? (car pair) class)
                   (cons class documents)
                   pair))
             document)
        (map (lambda (pair)
               (let ((value (cdr pair)))
                 (if (and (list? value)
                          (pair? value)
                          (pair? (car value))
                          (not (assoc class value)))
                     (cons (car pair)
                           (append value (list (cons class documents))))
                     pair)))
             document))))

;;; ---------------------------------------------------------------------------
;;; Sockets
;;; ---------------------------------------------------------------------------

(define (resolve-address host port)
  "A socket address for HOST, a name or a literal address, at PORT."
  (let ((literal (catch #t (lambda () (inet-pton AF_INET host))
                        (lambda _ #f))))
    (if literal
        (make-socket-address AF_INET literal port)
        (let ((entry (catch #t (lambda () (gethost host)) (lambda _ #f))))
          (if (not entry)
              (raise-mongo-error "could not resolve the host ~a" host)
              (let ((addresses (hostent:addr-list entry)))
                (if (or (not addresses) (null? addresses))
                    (raise-mongo-error "the host ~a has no address" host)
                    (make-socket-address
                     AF_INET
                     (inet-pton AF_INET (inet-ntop AF_INET (car addresses)))
                     port))))))))

(define (tcp-connect host port)
  "A connected socket to HOST:PORT, or an exception worth reading."
  (let ((socket (socket AF_INET SOCK_STREAM 0)))
    (catch #t
           (lambda ()
             (connect socket (resolve-address host port))
             ;; A socket port that decodes text would corrupt a BSON document
             ;; whose bytes are not UTF-8, so the port stays binary.
             (catch #t (lambda () (set-port-encoding! socket #f))
                    (lambda _ #f))
             socket)
           (lambda (key . args)
             (catch #t (lambda () (close-port socket)) (lambda _ #f))
             (raise-mongo-error "could not connect to ~a:~a (~a)" host port
                                (if (pair? args) (car args) key))))))

;;; ---------------------------------------------------------------------------
;;; SCRAM
;;; ---------------------------------------------------------------------------

;; SCRAM-SHA-1 hashes with SHA-1 and is given an already digested password:
;; `MD5(user ":" "mongo" ":" password)' in hexadecimal.  SCRAM-SHA-256 takes the
;; password itself.  Both are here because a user created before MongoDB 4.0 has
;; only the first.
(define (scram-hash mechanism text)
  "TEXT hashed with MECHANISM's hash function."
  (let ((bytes (if (bytevector? text) text (utf8-bytes text))))
    (if (string=? mechanism "SCRAM-SHA-256") (sha256 bytes) (sha1 bytes))))

(define (scram-hmac mechanism key data)
  "KEY's HMAC over DATA, with MECHANISM's hash function.

The two calls are written out rather than computed, because guile-gcrypt's
`mac-algorithm' is a macro: it turns an algorithm's *name* into its libgcrypt
number while the code is being expanded, so the name has to be there to read."
  (let ((key (if (bytevector? key) key (utf8-bytes key)))
        (data (if (bytevector? data) data (utf8-bytes data))))
    (if (string=? mechanism "SCRAM-SHA-256")
        (sign-data key data #:algorithm (mac-algorithm hmac-sha256))
        (sign-data key data #:algorithm (mac-algorithm hmac-sha1)))))

(define (hash-length mechanism)
  "The number of bytes MECHANISM's hash produces."
  (if (string=? mechanism "SCRAM-SHA-256") 32 20))

(define* (pbkdf2 password salt iterations
                 #:key (mechanism "SCRAM-SHA-256") (length #f))
  "PBKDF2-HMAC-SHA of PASSWORD over SALT, ITERATIONS times, LENGTH bytes.

PBKDF2 is not in the interface guile-gcrypt exposes, and it is ten lines once
the definition is read: each block is the salt and a big-endian block number
HMACed with the password, then iterated with the previous answer as the key, and
the blocks are concatenated."
  (let* ((password (if (bytevector? password) password (utf8-bytes password)))
         (salt (if (bytevector? salt) salt (utf8-bytes salt)))
         (size (or length (hash-length mechanism)))
         (blocks (quotient (+ size (hash-length mechanism) -1)
                           (hash-length mechanism))))
    (let loop ((block 1) (acc '()))
      (if (> block blocks)
          (bytevector-slice (concatenate-bytes (reverse acc)) 0 size)
          (let* ((index (make-bytevector 4 0)))
            (bytevector-u32-set! index 0 block (endianness big))
            (let ((first (scram-hmac mechanism password
                                     (concatenate-bytes (list salt index)))))
              (let iterate ((remaining (- iterations 1))
                            (previous first)
                            (mixed first))
                (if (zero? remaining)
                    (loop (+ block 1) (cons mixed acc))
                    (let ((next (scram-hmac mechanism password previous)))
                      (iterate (- remaining 1) next (bytevector-xor mixed
                                                                    next)))))))))))

(define (scram-nonce)
  "A fresh client nonce: 24 random bytes, base64, which is what drivers send."
  (base64-of (gen-random-bv 24)))

(define (sasl-name text)
  "TEXT with the two characters SCRAM reserves escaped, as SCRAM wants them."
  (with-output-to-string
    (lambda ()
      (string-for-each (lambda (c)
                         (case c
                           ((#\=) (display "=3D"))
                           ((#\,) (display "=2C"))
                           (else (display c))))
                       text))))

(define (mongo-password-digest user password)
  "The hexadecimal MD5 digest that SCRAM-SHA-1 is given instead of a password."
  (let ((digest (md5 (utf8-bytes (format #f "~a:mongo:~a" user password)))))
    (string-concatenate
     (map (lambda (b) (format #f "~2,'0x" b))
          (bytevector->u8-list digest)))))

(define (scram-fields payload)
  "PAYLOAD's comma-separated `key=value' fields, as an alist of character keys."
  (let ((text (if (string? payload) payload (bytes-utf8 payload))))
    (map (lambda (part)
           (let ((split (string-index part #\=)))
             (if split
                 (cons (string-ref part 0) (substring part (+ split 1)))
                 (cons (string-ref part 0) ""))))
         (string-split text #\,))))

(define (scram-field fields key)
  "KEY's value among FIELDS."
  (let ((pair (assoc key fields)))
    (and pair (cdr pair))))

(define (payload-text payload)
  "PAYLOAD as text, whether the server sent a BSON binary or a string."
  (if (bson-binary? payload)
      (bytes-utf8 (bson-binary-bytes payload))
      (format #f "~a" payload)))

(define* (authenticate! session user password source
                        #:key (mechanisms '("SCRAM-SHA-256" "SCRAM-SHA-1")))
  "Authenticate SESSION as USER against SOURCE, with the first mechanism that works.

The mechanisms are tried in order, strongest first; a server that does not know
SCRAM-SHA-256 answers the first message with an error, and the answer is only
raised once every mechanism has been refused.  The server's final signature is
checked before the connection is trusted: without that check a client will
happily talk to whoever is listening on the port."
  (let loop ((remaining mechanisms) (last-error #f))
    (cond
      ((null? remaining)
       (raise-exception
        (if last-error
            (make-mongo-error (format #f "could not authenticate ~a: ~a"
                                      user (mongo-error-message last-error))
                              #f #f)
            (raise-mongo-error "no SCRAM mechanism was offered"))))
      (else
       (let ((mechanism (car remaining)))
         (catch #t
                (lambda () (scram-authenticate! session user password source
                                                mechanism))
                (lambda (key . args)
                  (if (and (pair? args) (mongo-error? (car args)))
                      (loop (cdr remaining) (car args))
                      (throw key args)))))))))

(define (scram-authenticate! session user password source mechanism)
  "Run one SCRAM exchange for MECHANISM, or raise what the server said."
  (let* ((secret (if (string=? mechanism "SCRAM-SHA-256")
                     password
                     (mongo-password-digest user password)))
         (nonce (scram-nonce))
         (gs2 "n,,")
         (without-proof (format #f "n=~a,r=~a" (sasl-name user) nonce))
         (start (mongo-command
                 session
                 (list (cons "saslStart" 1)
                       (cons "mechanism" mechanism)
                       (cons "payload"
                             (bson-binary 0 (utf8-bytes
                                             (string-append gs2 without-proof))))
                       (cons "autoAuthorize" 1))
                 #:database source))
         (conversation (assoc-ref start "conversationId"))
         (first-text (payload-text (assoc-ref start "payload")))
         (fields (scram-fields first-text))
         (combined (scram-field fields #\r))
         (salt (base64-decode (or (scram-field fields #\s) "")))
         (iterations (or (string->number (or (scram-field fields #\i) "4096"))
                         4096))
         (salted (pbkdf2 secret salt iterations #:mechanism mechanism))
         (client-key (scram-hmac mechanism salted "Client Key"))
         (stored-key (scram-hash mechanism client-key))
         (final-without-proof (format #f "c=~a,r=~a" (base64-of gs2) combined))
         (auth-message (format #f "~a,~a,~a" without-proof first-text
                               final-without-proof))
         (client-signature (scram-hmac mechanism stored-key auth-message))
         (proof (bytevector-xor client-key client-signature))
         (server-signature (scram-hmac mechanism
                                       (scram-hmac mechanism salted "Server Key")
                                       auth-message)))
    (unless (and combined (string-prefix? nonce combined))
      (raise-mongo-error
       "the server's SCRAM nonce does not begin with the one this client sent"))
    (let loop ((reply (mongo-command
                       session
                       (list (cons "saslContinue" 1)
                             (cons "conversationId" conversation)
                             (cons "payload"
                                   (bson-binary 0 (utf8-bytes
                                                   (format #f "~a,p=~a"
                                                           final-without-proof
                                                           (base64-of proof))))))
                       #:database source))
               (steps 0)
               (verifier #f))
      (let* ((text (payload-text (assoc-ref reply "payload")))
             (fields* (scram-fields text))
             (server-proof (scram-field fields* #\v))
             (done (assoc-ref reply "done")))
        (cond
          ;; The server proves it knows the password in the same message.
          ((and (not verifier) server-proof)
           (unless (equal? (base64-decode server-proof) server-signature)
             (raise-mongo-error
              "the server's SCRAM signature does not match, so the password may be wrong"))
           (if done
               mechanism
               (loop (mongo-command
                      session
                      (list (cons "saslContinue" 1)
                            (cons "conversationId" conversation)
                            (cons "payload" (bson-binary 0 (make-bytevector 0 0))))
                      #:database source)
                     (+ steps 1) server-proof)))
          ((and verifier done) mechanism)
          ;; A server that has not finished is told so, but only twice: a
          ;; conversation that never ends is a bug, not a server's opinion.
          ((< steps 1)
           (loop (mongo-command
                  session
                  (list (cons "saslContinue" 1)
                        (cons "conversationId" conversation)
                        (cons "payload" (bson-binary 0 (make-bytevector 0 0))))
                  #:database source)
                 (+ steps 1) verifier))
          (else
           (raise-mongo-error
            "the server did not finish the SCRAM exchange after ~a more messages"
            steps)))))))

;;; ---------------------------------------------------------------------------
;;; Sessions
;;; ---------------------------------------------------------------------------

(define-record-type <mongo-session>
  (make-mongo-session spec socket database server-info request-id)
  mongo-session?
  (spec mongo-session-spec)
  (socket mongo-session-socket)
  (database mongo-session-database)
  ;; The handshake's answer, filled in once it has been read.
  (server-info mongo-session-server-info set-mongo-session-server-info!)
  (request-id mongo-session-request-id set-mongo-session-request-id!))

(define (mongo-server-version session)
  "The server's version string, as it reported it during the handshake."
  (assoc-ref (mongo-session-server-info session) "version"))

(define (mongo-max-wire-version session)
  "The highest wire version the server understands."
  (assoc-ref (mongo-session-server-info session) "maxWireVersion"))

(define (session-next-id! session)
  "A fresh request id for SESSION."
  (let ((id (mongo-session-request-id session)))
    (set-mongo-session-request-id! session (+ id 1))
    id))

(define (send-message session document)
  "Write DOCUMENT as a command message, answering with its request id."
  (let* ((id (session-next-id! session))
         (bytes (op-msg-encode id document))
         (socket (mongo-session-socket session)))
    (catch #t
           (lambda ()
             (put-bytevector socket bytes)
             (force-output socket)
             id)
           (lambda (key . args)
             (raise-mongo-error "could not send a command to the server (~a)"
                                (if (pair? args) (car args) key))))))

(define (receive-message session)
  "Read one reply from SESSION, as a document."
  (let* ((socket (mongo-session-socket session))
         (header (read-exactly socket 16)))
    (unless header
      (raise-mongo-error "the server closed the connection"))
    (let* ((length (bytevector-u32-ref header 0 (endianness little)))
           (body (read-exactly socket (- length 16))))
      (unless body
        (raise-mongo-error "the server closed the connection mid-message"))
      (op-msg-decode (concatenate-bytes (list header body))))))

(define (command-document session document database)
  "DOCUMENT with the database it runs against, which `OP_MSG' insists on."
  (append document
          (list (cons "$db" (or (assoc-ref document "$db")
                                database
                                (mongo-session-database session))))))

(define* (mongo-command session document #:key (database #f))
  "Run DOCUMENT on SESSION and answer with the reply, or raise what failed.

The reply's own `ok' is checked before it is returned: every command answers
with `ok: 1' or with an error worth reading, and a caller that has to remember
to check is a caller that will forget."
  (let* ((wire (command-document session document database))
         (_ (send-message session wire))
         (response (receive-message session)))
    (unless (equal? (assoc-ref response "ok") 1)
      (raise-server-error! response
                           (let ((name (json-document->command-name document)))
                             (and name (format #f "~a failed" name)))))
    response))

(define (mongo-commands session documents)
  "Run DOCUMENTS in order, answering with the replies.

A migration file is a list of commands and they run in the order they were
written, because that order is usually the point: a collection is created
before something indexes it."
  (map (lambda (document) (mongo-command session document)) documents))

(define* (mongo-command-cursor session document
                               #:key (database #f) (batch-limit 10000))
  "Run DOCUMENT and read every batch of its cursor, answering with the documents.

`find' and `listCollections' answer with a cursor holding a first batch and,
when there is more, an id to ask about with `getMore'.  Reading to the end here
means callers never think about batches, and BATCH-LIMIT is what stops a server
that keeps answering with an id from looping forever."
  (let* ((response (mongo-command session document #:database database))
         (cursor (assoc-ref response "cursor"))
         (namespace (and cursor (assoc-ref cursor "ns"))))
    (unless cursor
      (raise-mongo-error "the command did not answer with a cursor"))
    (let* ((first (assoc-ref cursor "firstBatch"))
           (collection (and namespace
                            (let ((dot (string-rindex namespace #\.)))
                              (and dot (substring namespace (+ dot 1)))))))
      (let loop ((id (assoc-ref cursor "id"))
                 (documents (if (vector? first) (vector->list first) '()))
                 (batches 1))
        (if (or (not id) (equal? id 0))
            (reverse documents)
            (begin
              (when (or (>= batches batch-limit) (not collection))
                (raise-mongo-error "the cursor on ~a never finished" namespace))
              (let* ((more (mongo-command
                            session
                            (list (cons "getMore" id)
                                  (cons "collection" collection))
                            #:database database))
                     (cursor* (assoc-ref more "cursor"))
                     (batch (and cursor* (assoc-ref cursor* "nextBatch"))))
                (loop (and cursor* (assoc-ref cursor* "id"))
                      (append (reverse (if (vector? batch)
                                           (vector->list batch)
                                           '()))
                              documents)
                      (+ batches 1)))))))))

;;; ---------------------------------------------------------------------------
;;; Connecting
;;; ---------------------------------------------------------------------------

(define (connection-host spec)
  (or (connection-spec-host spec) "127.0.0.1"))

(define (connection-port spec engine)
  (or (connection-spec-port spec) (engine-default-port engine)))

(define (auth-source spec)
  "Where a user is looked up: the URL's `authSource', else the database."
  (or (assoc-ref (connection-spec-options spec) "authSource")
      (connection-spec-database spec)
      "admin"))

(define (option-enabled? spec key)
  "Whether SPEC's connection parameters turn KEY on."
  (let ((value (assoc-ref (connection-spec-options spec) key)))
    (and value (member (string-downcase (format #f "~a" value))
                       '("true" "yes" "1" "")))))

(define* (mongo-connect spec #:key (options '()))
  "Open a MongoDB session for SPEC.

The handshake comes first, so a server that is too old, a name that does not
resolve, and a password that is wrong are each reported in those words before
any migration is attempted."
  (when (or (option-enabled? spec "tls") (option-enabled? spec "ssl"))
    (raise-mongo-error
     "a TLS connection was asked for, and this client does not speak TLS.
   Terminate TLS in front of the server, or connect over a private network."))
  (let* ((engine (connection-spec-engine spec))
         (host (connection-host spec))
         (port (connection-port spec engine))
         (database (or (connection-spec-database spec) "admin"))
         (user (connection-spec-user spec))
         (socket (tcp-connect host port)))
    (catch #t
           (lambda ()
             (let* ((hello (append (list (cons "hello" 1)
                                         (cons "helloOk" #t)
                                         (cons "client"
                                               (list (cons "application"
                                                           (list (cons "name"
                                                                       "mutastructura")))
                                                     (cons "driver"
                                                           (list (cons "name"
                                                                       "mutastructura")
                                                                 (cons "version"
                                                                       "dev"))))))
                                   (if user
                                       (list (cons "saslSupportedMechs"
                                                   (format #f "~a.~a"
                                                           (auth-source spec)
                                                           user)))
                                       '())))
                    ;; One session is made before the handshake and kept: the
                    ;; request ids on a connection have to be unique, and the
                    ;; answer to `hello' belongs to the connection, not to a
                    ;; session made for it.
                    (session (make-mongo-session spec socket database '() 1))
                    (answer (mongo-command session hello #:database "admin"))
                    (wire (assoc-ref answer "maxWireVersion")))
               (begin
                 (set-mongo-session-server-info! session answer)
                 (when (and (number? wire) (< wire mongo-wire-version-minimum))
                   (raise-mongo-error
                    "the server reports wire version ~a, and MongoDB 3.6 (wire version ~a)
   is the oldest this client speaks; upgrade the server"
                    wire mongo-wire-version-minimum))
                 (when user
                   (let* ((listed (assoc-ref answer "saslSupportedMechs"))
                          (mechanisms (if (and (vector? listed)
                                               (pair? (vector->list listed)))
                                          (vector->list listed)
                                          '("SCRAM-SHA-256" "SCRAM-SHA-1"))))
                     (authenticate! session user
                                    (connection-spec-password spec)
                                    (auth-source spec)
                                    #:mechanisms mechanisms)))
                 session)))
           (lambda (key . args)
             (catch #t (lambda () (close-port socket)) (lambda _ #f))
             (raise-exception
              (if (and (pair? args) (mongo-error? (car args)))
                  (car args)
                  (make-mongo-error
                   (format #f "could not talk to the MongoDB server at ~a:~a (~a)"
                           host port (if (pair? args) (car args) key))
                   #f #f)))))))

(define (mongo-close! session)
  "Close SESSION's socket, saying nothing if it is already closed.

There is no `goodbye' to send: `endSessions' is for a pooled client, and a
migration tool has one connection it is about to drop anyway."
  (catch #t
         (lambda () (close-port (mongo-session-socket session)))
         (lambda _ #f)))
