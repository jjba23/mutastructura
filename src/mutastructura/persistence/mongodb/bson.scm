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
;; BSON, the binary form of a MongoDB document, in both directions.
;; (`https://bsonspec.org/spec.html' is the whole specification; it is a page.)
;;
;; The Scheme-to-BSON mapping is the one `(mutastructura persistence json)' uses,
;; so a JSON migration file goes straight in with no intermediate form:
;;
;;   alist (string keys) -> document      vector -> array
;;   string -> string                     #nil   -> null
;;   #t / #f -> boolean                   exact integer -> int32, or int64 when
;;                                        it does not fit
;;   flonum -> double                     <bson-object-id>, <bson-date>,
;;                                        <bson-binary>, <bson-timestamp>,
;;                                        <bson-decimal128> -> their types
;;
;; Anything else is refused by name rather than written as a string, because a
;; silently stringified value in a migration is a data bug that only shows up
;; when somebody queries the field.
;;
;; Reading is deliberately total for the types a server is likely to answer
;; with, including the ones mutastructura never writes: a reply that carries an
;; ObjectId or a regex must not crash the tool.

(define-module (mutastructura persistence mongodb bson)
  #:declarative? #t
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 textual-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (gcrypt base16)
  #:use-module (gcrypt random)
  #:export (bson-encode
            bson-encode-document
            bson-decode
            bson-decode-at
            bson-decode-from-port
            bson-document-size
            ;; The framing primitives, shared with the wire protocol next door:
            ;; a MongoDB message is a length-prefixed run of BSON documents, so
            ;; the codec and the transport want the same four functions.
            u8-bytes
            int32-bytes
            concatenate-bytes
            bytevector-slice
            bson-object-id
            bson-object-id?
            bson-object-id-bytes
            bson-object-id-string
            bson-object-id-from-hex
            bson-object-id-now
            bson-date
            bson-date?
            bson-date-millis
            bson-date-now-millis
            bson-binary
            bson-binary?
            bson-binary-subtype
            bson-binary-bytes
            bson-timestamp
            bson-timestamp?
            bson-timestamp-time
            bson-timestamp-increment
            bson-decimal128
            bson-decimal128?
            bson-decimal128-bytes
            bson-regex
            bson-regex?
            bson-regex-pattern
            bson-regex-options))

;;; ---------------------------------------------------------------------------
;;; The values BSON has and Scheme does not
;;; ---------------------------------------------------------------------------

(define-record-type <bson-object-id>
  (bson-object-id bytes)
  bson-object-id?
  ;; The twelve bytes of the id, in order.
  (bytes bson-object-id-bytes))

(define (bson-object-id-string id)
  "ID as the 24 hexadecimal characters everybody prints it as."
  (bytevector->base16-string (bson-object-id-bytes id)))

(define (bson-object-id-from-hex hex)
  "The object id HEX names, or #f when HEX is not 24 hexadecimal characters."
  (and (string? hex)
       (= (string-length hex) 24)
       (catch #t
              (lambda () (bson-object-id (base16-string->bytevector
                                          (string-downcase hex))))
              (lambda _ #f))))

(define (bson-object-id-now)
  "A fresh object id: four bytes of time, five of randomness, three counting.

Randomness and a counter rather than a machine identifier and a process id: the
uniqueness that matters here is within one run, and a process id is not
something a portable Scheme can be sure of."
  (let ((bytes (make-bytevector 12 0))
        (seconds (car (gettimeofday))))
    (bytevector-u32-set! bytes 0 (modulo seconds 4294967296)
                         (endianness big))
    (bytevector-copy! (gen-random-bv 5) 0 bytes 4 5)
    (bytevector-copy! (gen-random-bv 3) 0 bytes 9 3)
    (bson-object-id bytes)))

(define-record-type <bson-date>
  (bson-date millis)
  bson-date?
  ;; Milliseconds since the Unix epoch, UTC, as BSON stores them.
  (millis bson-date-millis))

(define (bson-date-now-millis)
  "The current time in milliseconds since the Unix epoch."
  (let ((now (gettimeofday)))
    (+ (* (car now) 1000) (quotient (cdr now) 1000))))

(define-record-type <bson-binary>
  (bson-binary subtype bytes)
  bson-binary?
  (subtype bson-binary-subtype)
  (bytes bson-binary-bytes))

(define-record-type <bson-timestamp>
  (bson-timestamp time increment)
  bson-timestamp?
  (time bson-timestamp-time)
  (increment bson-timestamp-increment))

(define-record-type <bson-decimal128>
  (bson-decimal128 bytes)
  bson-decimal128?
  ;; The sixteen raw bytes: decimal128 is a decimal floating point format whose
  ;; arithmetic nobody wants reimplemented here, and storing it faithfully is
  ;; enough for a migration tool to read a document and write it back.
  (bytes bson-decimal128-bytes))

(define-record-type <bson-regex>
  (bson-regex pattern options)
  bson-regex?
  (pattern bson-regex-pattern)
  (options bson-regex-options))

;;; ---------------------------------------------------------------------------
;;; Little-endian primitives
;;; ---------------------------------------------------------------------------
;;
;; Every one of these returns a bytevector rather than writing to a port.  The
;; encoder is then a fold of pure functions -- `document-body' is the only place
;; that concatenates -- which is what makes BSON testable without a stream, and
;; what keeps the framing (a length in front of a body) written down once.

(define (u8-bytes n)
  (make-bytevector 1 (modulo n 256)))

(define (int32-bytes n)
  "N as four little-endian bytes."
  (let ((bytes (make-bytevector 4 0)))
    (bytevector-u32-set! bytes 0 (modulo n 4294967296) (endianness little))
    bytes))

(define (int64-bytes n)
  "N as eight little-endian bytes."
  (let ((bytes (make-bytevector 8 0)))
    (bytevector-u64-set! bytes 0 (modulo n 18446744073709551616)
                         (endianness little))
    bytes))

(define (double-bytes n)
  "N as an eight-byte little-endian IEEE-754 double."
  (let ((bytes (make-bytevector 8 0)))
    (bytevector-ieee-double-set! bytes 0 (exact->inexact n)
                                 (endianness little))
    bytes))

(define (cstring-bytes text)
  "TEXT as a BSON cstring: its UTF-8 bytes and a terminating NUL.

Keys, regex patterns and options are written this way."
  (let* ((bytes (string->utf8 (if (string? text) text (format #f "~a" text))))
         (out (make-bytevector (+ (bytevector-length bytes) 1) 0)))
    (bytevector-copy! bytes 0 out 0 (bytevector-length bytes))
    out))

(define (bson-string-bytes text)
  "TEXT as a BSON string: its length *including* the terminating NUL, then the
NUL, then the bytes."
  (let* ((bytes (string->utf8 (if (string? text) text (format #f "~a" text))))
         (out (make-bytevector (+ (bytevector-length bytes) 5) 0)))
    (bytevector-copy! (int32-bytes (+ (bytevector-length bytes) 1)) 0 out 0 4)
    (bytevector-copy! bytes 0 out 4 (bytevector-length bytes))
    out))

(define (length-prefixed body)
  "BODY with its own length, plus four, written in front of it.

This is how BSON frames a document and an array: an int32 that counts the body
*and* itself, then the body."
  (let* ((length (bytevector-length body))
         (out (make-bytevector (+ 4 length) 0)))
    (bytevector-copy! (int32-bytes (+ 4 length)) 0 out 0 4)
    (bytevector-copy! body 0 out 4 length)
    out))

(define (bytevector-slice bytes start end)
  "BYTES from START up to, but not including, END, as a fresh bytevector.

`(rnrs bytevectors)' gives us R6RS's `bytevector-copy', which copies a whole
bytevector; the slicing form with a start and an end is R7RS.  One helper keeps
that difference in a single place instead of in every reader below."
  (let* ((count (max 0 (- end start)))
         (out (make-bytevector count 0)))
    (bytevector-copy! bytes start out 0 count)
    out))

(define (concatenate-bytes parts)
  "PARTS, one bytevector after another."
  (let* ((total (apply + (map bytevector-length parts)))
         (out (make-bytevector total 0)))
    (let loop ((parts parts) (at 0))
      (if (null? parts)
          out
          (let ((part (car parts)))
            (bytevector-copy! part 0 out at (bytevector-length part))
            (loop (cdr parts) (+ at (bytevector-length part))))))))

(define (int32? n)
  (and (exact? n) (integer? n) (>= n -2147483648) (<= n 2147483647)))

(define (int64? n)
  (and (exact? n) (integer? n)
       (>= n -9223372036854775808)
       (<= n 9223372036854775807)))

;;; ---------------------------------------------------------------------------
;;; Encoding
;;; ---------------------------------------------------------------------------

(define (bson-encode value)
  "VALUE as BSON.  VALUE must be a document (an alist)."
  (unless (document? value)
    (raise-bson-error "a BSON document must be an alist, not ~s" value))
  (bson-encode-document value))

(define (document? value)
  "Whether VALUE is an alist, and so a BSON document rather than an array."
  (and (list? value)
       (or (null? value)
           (pair? (car value)))))

(define (bson-encode-document document)
  "DOCUMENT as BSON bytes, with its length prefix."
  (length-prefixed (document-body document)))

(define (document-body document)
  "DOCUMENT's elements, followed by the document's terminating NUL."
  (concatenate-bytes
   (append (map (lambda (pair)
                  (write-element (car pair) (cdr pair)))
                document)
           (list (make-bytevector 1 0)))))

(define (write-element key value)
  "One KEY/VALUE pair of a document, as bytes."
  (concatenate-bytes (list (u8-bytes (value-type value))
                           (cstring-bytes key)
                           (value-bytes value))))

(define (value-type value)
  "The BSON element type byte for VALUE."
  (cond
    ((bson-object-id? value) #x07)
    ((bson-date? value) #x09)
    ((bson-binary? value) #x05)
    ((bson-timestamp? value) #x11)
    ((bson-decimal128? value) #x13)
    ((bson-regex? value) #x0b)
    ((eq? value #nil) #x0a)
    ((null? value) #x03)                ; an empty document
    ((eq? value #t) #x08)
    ((eq? value #f) #x08)
    ((string? value) #x02)
    ((vector? value) #x04)
    ((document? value) #x03)
    ((number? value)
     (cond
       ((inexact? value) #x01)
       ((int32? value) #x10)
       ((int64? value) #x12)
       (else (raise-bson-error
              "~a is too large for BSON, whose largest integer is 64-bit"
              value))))
    (else (raise-bson-error
           "~s cannot be written as BSON; use a string, a number, a boolean,\n   a document (alist), an array (vector), or one of the bson-* records"
           value))))

(define (value-bytes value)
  "VALUE's BSON payload, without its type byte or key."
  (cond
    ((bson-object-id? value) (bson-object-id-bytes value))
    ((bson-date? value) (int64-bytes (bson-date-millis value)))
    ((bson-binary? value)
     (let ((bytes (bson-binary-bytes value)))
       (concatenate-bytes (list (int32-bytes (bytevector-length bytes))
                                (u8-bytes (bson-binary-subtype value))
                                bytes))))
    ((bson-timestamp? value)
     ;; A uint64 whose high half is seconds and low half a counter.
     (concatenate-bytes (list (int32-bytes (bson-timestamp-increment value))
                              (int32-bytes (bson-timestamp-time value)))))
    ((bson-decimal128? value) (bson-decimal128-bytes value))
    ((bson-regex? value)
     (concatenate-bytes (list (cstring-bytes (bson-regex-pattern value))
                              (cstring-bytes (bson-regex-options value)))))
    ((eq? value #nil) (make-bytevector 0 0))
    ((or (eq? value #t) (eq? value #f)) (u8-bytes (if value 1 0)))
    ((string? value) (bson-string-bytes value))
    ((vector? value)
     (length-prefixed
      (document-body (let loop ((items (vector->list value)) (n 0) (acc '()))
                       (if (null? items)
                           (reverse acc)
                           (loop (cdr items)
                                 (+ n 1)
                                 (cons (cons (number->string n) (car items))
                                       acc)))))))
    ((document? value) (bson-encode-document value))
    ((number? value)
     (cond
       ((inexact? value) (double-bytes value))
       ((int32? value) (int32-bytes value))
       (else (int64-bytes value))))
    (else (raise-bson-error "~s cannot be written as BSON" value))))

(define (bson-document-size document)
  "How many bytes DOCUMENT takes, its length prefix included."
  (bytevector-length (bson-encode-document document)))

(define (raise-bson-error format-string . args)
  (raise-exception
   (make-exception-with-message (apply format #f format-string args))))

;;; ---------------------------------------------------------------------------
;;; Decoding
;;; ---------------------------------------------------------------------------

(define (bson-decode bytes)
  "The document the BSON BYTES hold.

The inverse of `bson-encode' for the common types: documents are alists with
string keys, arrays are vectors, and the types Scheme has no equivalent for come
back as the `bson-*' records above."
  (receive (value _)
      (bson-decode-at bytes 0)
    value))

(define (bson-decode-from-port port)
  "Read one BSON document from PORT, or `#nil' at the end of it."
  (let ((length-bytes (get-bytevector-n port 4)))
    (if (or (eof-object? length-bytes) (< (bytevector-length length-bytes) 4))
        #nil
        (let* ((length (bytevector-u32-ref length-bytes 0 (endianness little)))
               (rest (get-bytevector-n port (- length 4))))
          (if (or (eof-object? rest) (< (bytevector-length rest) (- length 4)))
              #nil
              (receive (value _)
                  (bson-decode-at (concatenate-bytes (list length-bytes rest)) 0)
                value))))))

(define (bson-decode-at bytes offset)
  "Read a document from BYTES at OFFSET, answering with it and the offset after it.

This is the reader the wire protocol uses too: a message body is a run of
documents, and the offset each one ends at is what finds the next."
  (let* ((length (bytevector-u32-ref bytes offset (endianness little)))
         (end (+ offset length -1)))
    (unless (and (>= length 5) (<= (+ offset length) (bytevector-length bytes)))
      (raise-bson-error "a BSON document claims a length of ~a, which does not fit"
                        length))
    (let loop ((at (+ offset 4)) (acc '()))
      (if (>= at end)
          (values (reverse acc) (+ offset length))
          (let* ((type (bytevector-u8-ref bytes at)))
            ;; The key's length is measured in bytes, and a key may hold any
            ;; UTF-8 character, so the offset after the key comes from the
            ;; reader rather than from counting characters.
            (receive (key after-key) (read-cstring-value bytes (+ at 1))
              (receive (value next) (read-value type bytes after-key)
                (loop next (cons (cons key value) acc)))))))))

(define (read-cstring bytes offset)
  "The NUL-terminated string at OFFSET, as bytes and end offset."
  (let loop ((at offset) (acc '()))
    (if (>= at (bytevector-length bytes))
        (raise-bson-error "a BSON string runs past the end of the message")
        (let ((byte (bytevector-u8-ref bytes at)))
          (if (zero? byte)
              (values (u8-list->bytevector (reverse acc)) (+ at 1))
              (loop (+ at 1) (cons byte acc)))))))

(define (read-cstring-value bytes offset)
  "The NUL-terminated string at OFFSET, and the offset just after its NUL."
  (receive (raw next) (read-cstring bytes offset)
    (values (utf8->string raw) next)))

(define (read-value type bytes offset)
  "Read the value of TYPE at OFFSET, returning it and the next offset."
  (case type
    ((#x01) (values (bytevector-ieee-double-ref bytes offset
                                                (endianness little))
                    (+ offset 8)))
    ((#x02)
     (let* ((length (bytevector-u32-ref bytes offset (endianness little)))
            (start (+ offset 4)))
       (values (utf8->string (bytevector-slice bytes start
                                                 (+ start length -1)))
               (+ start length))))
    ((#x03) (bson-decode-at bytes offset))
    ((#x04)
     (receive (document next) (bson-decode-at bytes offset)
       (values (list->vector (map cdr document)) next)))
    ((#x05)
     (let* ((length (bytevector-u32-ref bytes offset (endianness little)))
            (subtype (bytevector-u8-ref bytes (+ offset 4)))
            (start (+ offset 5)))
       (values (bson-binary subtype (bytevector-slice bytes start
                                                        (+ start length)))
               (+ start length))))
    ((#x06) (values #nil (+ offset 0)))  ; `undefined', which is now deprecated
    ((#x07)
     (values (bson-object-id (bytevector-slice bytes offset (+ offset 12)))
             (+ offset 12)))
    ((#x08) (values (not (zero? (bytevector-u8-ref bytes offset))) (+ offset 1)))
    ((#x09)
     (values (bson-date (bytevector-s64-ref bytes offset (endianness little)))
             (+ offset 8)))
    ((#x0a) (values #nil offset))
    ((#x0b)
     (receive (pattern next) (read-cstring-value bytes offset)
       (receive (options next2) (read-cstring-value bytes next)
         (values (bson-regex pattern options) next2))))
    ((#x0d)
     ;; JavaScript code, which a server can echo back in an error.
     (receive (value next) (read-value #x02 bytes offset)
       (values (bson-regex value "code") next)))
    ((#x0e)
     ;; A symbol, deprecated since 2012 and still found in old data.
     (receive (value next) (read-value #x02 bytes offset)
       (values value next)))
    ((#x0f)
     (receive (code next) (read-value #x0d bytes offset)
       (receive (_ next2) (bson-decode-at bytes next)
         (values code next2))))
    ((#x0c)
     ;; A DBPointer: a namespace and an object id.  Reported as its namespace,
     ;; the way the shell's own JSON output does.
     (receive (namespace next) (read-value #x02 bytes offset)
       (receive (_ next2) (read-value #x07 bytes next)
         (values namespace next2))))
    ((#x10)
     (values (bytevector-s32-ref bytes offset (endianness little))
             (+ offset 4)))
    ((#x11)
     (let ((increment (bytevector-u32-ref bytes offset (endianness little)))
           (time (bytevector-u32-ref bytes (+ offset 4) (endianness little))))
       (values (bson-timestamp time increment) (+ offset 8))))
    ((#x12)
     (values (bytevector-s64-ref bytes offset (endianness little))
             (+ offset 8)))
    ((#x13)
     (values (bson-decimal128 (bytevector-slice bytes offset (+ offset 16)))
             (+ offset 16)))
    ((#x7f) (values 'max-key offset))
    ((#xff) (values 'min-key offset))
    (else (raise-bson-error
           "no reader for BSON type 0x~2,'0x; mutastructura does not know this type"
           type))))
