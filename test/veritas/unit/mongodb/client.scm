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
;; The MongoDB client, against the mock server next door rather than against a
;; MongoDB that happens to be running: the dialogs a client gets wrong -- a batch
;; that arrives as a document sequence, a cursor that needs a `getMore', a
;; password that is refused -- are the ones the mock reproduces on purpose.
;;
;; A case that touches the wire is written as a procedure of one argument, called
;; with the run's `vc', rather than as a bare assertion.  Two reasons, and both
;; bite:
;;
;;   an assertion's arguments are evaluated when the suite is built, which is
;;   while this file is loaded, and starting a server or opening a socket there
;;   hangs the load rather than failing the test;
;;
;;   a procedure case is *asked* for its verdict -- the runner reads the value
;;   it returns -- so a case that has work to do after its assertions (closing a
;;   session, say) ends on `verdict' below, which passes the case when every
;;   assertion in it passed.
;;
;; Nothing here runs concurrently either: these are sockets and threads, and a
;; suite that interleaves them fails for reasons that have nothing to do with the
;; client.

(define-module (veritas unit mongodb client)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence mongodb bson)
  #:use-module (mutastructura persistence mongodb client)
  #:use-module (test helpers mongodb-mock)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define* (url port #:optional user password)
  "The connection URL for the mock server listening on PORT."
  (if user
      (format #f "mongodb://~a:~a@127.0.0.1:~a/app" user password port)
      (format #f "mongodb://127.0.0.1:~a/app" port)))

(define* (connect port #:optional user password)
  (mongo-connect (parse-connection-url (url port user password))))

(define (error-message thunk)
  "What THUNK raised, as a sentence, or #f when it returned normally."
  (catch #t
         (lambda () (thunk) #f)
         (lambda (key . args)
           (let ((condition (and (pair? args) (car args))))
             (if (mongo-error? condition)
                 (mongo-error-message condition)
                 (format #f "~a: ~a" key args))))))

(define (handlers-with-hello answer)
  "The mock's handlers, with `hello' replaced by ANSWER."
  (cons (cons "hello" (lambda (request state) (answer)))
        (filter (lambda (pair) (not (equal? (car pair) "hello")))
                (default-handlers))))

(define (passed? result)
  "Whether RESULT, an assertion result, says the assertion passed."
  (and (pair? result)
       (eq? #t (assoc-ref result 'assertion-successful))))

(define (verdict vc results)
  "The result a case ends on, given the RESULTS of its assertions.

The runner reads the value a case returns, so a case whose last act is to close
a connection has nothing of its own to report.  This hands back the last
assertion's result when every one of them passed, and a failing result when one
did not -- which is also what makes a failed assertion fail the run rather than
merely printing a line."
  (let ((failed (filter (lambda (result) (not (passed? result))) results)))
    (if (null? failed)
        (car (reverse results))
        ((assert-true #f #:name "an earlier assertion in this case failed") vc))))

;;; ---------------------------------------------------------------------------
;;; The handshake and the wire
;;; ---------------------------------------------------------------------------

(define handshake-suite
  (suite "the handshake"
         #:concurrent? #f
         (test "a connection starts with `hello'"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           (cases
                            (list
                             ((assert-equal #:expect "7.0.0"
                                            #:got (mongo-server-version session)
                                            #:name "the server's version is kept")
                              vc)
                             ((assert-equal #:expect 17
                                            #:got (mongo-max-wire-version session)
                                            #:name "and its wire version")
                              vc)
                             ((assert-equal #:expect "app"
                                            #:got (mongo-session-database session)
                                            #:name "the session keeps its database")
                              vc)
                             ((assert-equal #:expect 1
                                            #:got (assoc-ref
                                                   (mongo-command session
                                                                  '(("ping" . 1)))
                                                   "ok")
                                            #:name "a command is answered")
                              vc)
                             ((assert-equal #:expect "app"
                                            #:got (assoc-ref
                                                   (mongo-command session
                                                                  '(("ping" . 1)))
                                                   "$db")
                                            #:name "the database goes with every command")
                              vc)
                             ((assert-equal #:expect "other"
                                            #:got (assoc-ref
                                                   (mongo-command session
                                                                  '(("ping" . 1))
                                                                  #:database "other")
                                                   "$db")
                                            #:name "#:database overrides it per command")
                              vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))
         (test "a message is a header and a document"
               #:shuffle? #f
               (let* ((bytes (op-msg-encode 7 '(("ping" . 1))))
                      (length (bytevector-u32-ref bytes 0 (endianness little)))
                      (request-id (bytevector-u32-ref bytes 4 (endianness little)))
                      (opcode (bytevector-s32-ref bytes 12 (endianness little))))
                 (assert-equal #:expect (bytevector-length bytes) #:got length
                               #:name "the header's length counts the whole message")
                 (assert-equal #:expect 7 #:got request-id
                               #:name "the request id is in the header")
                 (assert-equal #:expect 2013 #:got opcode
                               #:name "the opcode is OP_MSG")
                 (assert-equal #:expect '(("ping" . 1))
                               #:got (op-msg-decode bytes)
                               #:name "the document round-trips")))
         (test "an answer that is not OP_MSG is refused"
               #:shuffle? #f
               (let ((bytes (op-msg-encode 1 '(("ping" . 1)))))
                 ;; 2004 is `OP_QUERY', which MongoDB 6 removed from the server.
                 (bytevector-s32-set! bytes 12 2004 (endianness little))
                 (assert-contains #:expect (or (error-message (lambda ()
                                                                (op-msg-decode bytes)))
                                               "")
                                  #:got "only OP_MSG"
                                  #:name "the opcode is named in the refusal")))
         (test "a server older than wire version 6 is refused"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    ((assert-contains
                      #:expect (or (error-message (lambda () (connect port))) "")
                      #:got "upgrade the server"
                      #:name "the version the server reported is named")
                     vc))
                  #:handlers
                  (handlers-with-hello
                   (lambda ()
                     (mock-reply (list (cons "ok" 1)
                                       (cons "minWireVersion" 0)
                                       (cons "maxWireVersion" 5)
                                       (cons "version" "3.4.7"))))))))
         (test "TLS is refused by name, before connecting"
               #:shuffle? #f
               (assert-contains
                #:expect
                (or (error-message
                     (lambda ()
                       (mongo-connect
                        (parse-connection-url
                         "mongodb://127.0.0.1:27017/app?tls=true"))))
                    "")
                #:got "does not speak TLS"
                #:name "the client says what to do instead"))
         (test "an srv URL is refused while parsing"
               #:shuffle? #f
               (assert-contains
                #:expect
                (or (error-message
                     (lambda ()
                       (parse-connection-url
                        "mongodb+srv://cluster0.example.com/app")))
                    "")
                #:got "DNS SRV lookup"
                #:name "and says which URL to use instead"))))

;;; ---------------------------------------------------------------------------
;;; Cursors and commands
;;; ---------------------------------------------------------------------------

(define cursor-suite
  (suite "cursors and commands"
         #:concurrent? #f
         (test "a cursor is read across its batches"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           ;; The mock answers `find' with one document and a
                           ;; cursor id, and `getMore' with the second: a client
                           ;; that reads one batch and stops loses documents.
                           (users (mongo-command-cursor session
                                                        '(("find" . "users"))))
                           (cases
                            (list ((assert-length #:expect 2 #:got users
                                                  #:name "both batches are read")
                                   vc)
                                  ((assert-equal #:expect '("joe" "amy")
                                                 #:got (map (lambda (document)
                                                              (assoc-ref document
                                                                         "name"))
                                                            users)
                                                 #:name "and in order")
                                   vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))
         (test "a create is visible to listCollections"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           (_ (mongo-command session '(("create" . "users"))))
                           (cases
                            (list ((assert-equal #:expect '("users")
                                                 #:got (map (lambda (document)
                                                              (assoc-ref document
                                                                         "name"))
                                                            (mongo-command-cursor
                                                             session
                                                             '(("listCollections" . 1))))
                                                 #:name "the server keeps what was created")
                                   vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))
         (test "commands run in the order they were written"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           (cases
                            (list
                             ((assert-equal
                               #:expect '(1 1)
                               #:got (map (lambda (reply) (assoc-ref reply "ok"))
                                          (mongo-commands
                                           session
                                           (list
                                            '(("create" . "users"))
                                            '(("insert" . "users")
                                              ("documents" . #((("name" . "joe"))))))))
                               #:name "a create before an insert is answered in turn")
                              vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))))

;;; ---------------------------------------------------------------------------
;;; Errors
;;; ---------------------------------------------------------------------------

(define error-suite
  (suite "what the server says when it refuses"
         #:concurrent? #f
         (test "an unknown command comes back with its own words"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           (message (error-message
                                     (lambda ()
                                       (mongo-command session
                                                      '(("nosuchcmd" . 1))))))
                           (cases
                            (list ((assert-contains #:expect (or message "")
                                                    #:got "no such command: nosuchcmd"
                                                    #:name "the server's sentence is kept")
                                   vc)
                                  ((assert-contains #:expect (or message "")
                                                    #:got "CommandNotFound"
                                                    #:name "and its code name")
                                   vc)
                                  ((assert-contains #:expect (or message "")
                                                    #:got "59"
                                                    #:name "and its code")
                                   vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))
         (test "the code is available as a number too"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port))
                           (cases
                            (list
                             ((assert-equal
                               #:expect 59
                               #:got (catch #t
                                       (lambda ()
                                         (mongo-command session
                                                        '(("nosuchcmd" . 1)))
                                         #f)
                                       (lambda (key . args)
                                         (let ((condition (and (pair? args)
                                                               (car args))))
                                           (and (mongo-error? condition)
                                                (mongo-error-code condition)))))
                               #:name "a caller can branch on the code")
                              vc))))
                      (mongo-close! session)
                      (verdict vc cases))))))))

;;; ---------------------------------------------------------------------------
;;; Authentication
;;; ---------------------------------------------------------------------------

(define auth-suite
  (suite "SCRAM authentication"
         #:concurrent? #f
         (test "a correct password authenticates"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    (let* ((session (connect port "alice" "s3cret"))
                           (cases
                            (list
                             ((assert-equal
                               #:expect 1
                               #:got (assoc-ref (mongo-command session
                                                               '(("ping" . 1)))
                                                "ok")
                               #:name "the command after the handshake is answered")
                              vc))))
                      (mongo-close! session)
                      (verdict vc cases)))
                  #:user "alice"
                  #:password "s3cret")))
         (test "a wrong password is refused, in the server's words"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    ((assert-contains
                      #:expect (or (error-message
                                    (lambda () (connect port "alice" "wrong")))
                                   "")
                      #:got "Authentication failed"
                      #:name "the failure happens at the handshake")
                     vc))
                  #:user "alice"
                  #:password "s3cret")))
         (test "no credentials means no handshake"
               #:shuffle? #f
               (lambda (vc)
                 (with-mock-server
                  (lambda (port)
                    ;; The mock wants a password and the client sent none, and
                    ;; the mock still answers: what this shows is that nothing
                    ;; was asked of it.
                    (let* ((session (connect port))
                           (cases
                            (list
                             ((assert-equal
                               #:expect 1
                               #:got (assoc-ref (mongo-command session
                                                               '(("ping" . 1)))
                                                "ok")
                               #:name "a client with no user connects")
                              vc))))
                      (mongo-close! session)
                      (verdict vc cases)))
                  #:user "alice"
                  #:password "s3cret")))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               handshake-suite
               cursor-suite
               error-suite
               auth-suite))
