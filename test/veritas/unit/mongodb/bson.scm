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
;; The BSON codec, against the specification rather than against itself.
;;
;; Every byte vector below is written by hand from the BSON specification, so a
;; test that passes says the codec writes what a MongoDB server reads -- not
;; merely that it can read back what it wrote.  The round trips come after: they
;; are what catches a type that encodes correctly and decodes to something else.

(define-module (veritas unit mongodb bson)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 binary-ports)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (mutastructura persistence mongodb bson)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define (hex bytes)
  "BYTES as lower-case hexadecimal, two digits per byte."
  (string-concatenate
   (map (lambda (byte) (format #f "~2,'0x" byte))
        (bytevector->u8-list bytes))))

(define (bytes-of hex-string)
  "The byte vector HEX-STRING spells, for handing the reader broken input."
  (u8-list->bytevector
   (map (lambda (pair)
          (string->number (list->string pair) 16))
        (let loop ((chars (string->list hex-string)) (out '()))
          (if (null? chars)
              (reverse out)
              (loop (cddr chars) (cons (list (car chars) (cadr chars)) out)))))))

(define (encodes-as hex-string document)
  "Whether DOCUMENT encodes to exactly HEX-STRING."
  (equal? (hex (bson-encode document)) hex-string))

(define (round-trips document)
  "DOCUMENT after one trip through the codec."
  (bson-decode (bson-encode document)))

;;; ---------------------------------------------------------------------------
;;; The specification's own examples
;;; ---------------------------------------------------------------------------

(define framing-suite
  (suite "BSON framing and the specification's examples"
         (test "an empty document is five bytes"
               (assert-equal #:expect "0500000000"
                             #:got (hex (bson-encode '()))
                             #:name "the length prefix counts itself"))
         (test "a document holding one string"
               (assert-equal
                #:expect "160000000268656c6c6f0006000000776f726c640000"
                #:got (hex (bson-encode '(("hello" . "world"))))
                #:name "the string's length counts its NUL"))
         (test "the specification's array example"
               ;; {"BSON": ["awesome", 5.05, 1986]}, from bsonspec.org.  The
               ;; array is a document whose keys are "0", "1", "2", and the
               ;; double is the one the specification's own example uses.
               (assert-equal
                #:expect
                (string-append
                 "310000000442534f4e002600000002300008000000617765736f6d6500"
                 "0131003333333333331440103200c20700000000")
                #:got (hex (bson-encode '(("BSON" . #("awesome" 5.05 1986)))))
                #:name "an array is a document with numbered keys"))
         (test "an empty object is not an empty array"
               ;; {"filter": {}} and {"filter": []} are different commands to a
               ;; server, which is why they are different here.
               (assert-equal
                #:expect
                (string-append
                 "220000000266696e6400060000007573657273000366696c746572000500"
                 "00000000")
                #:got (hex (bson-encode '(("find" . "users")
                                          ("filter" . ()))))
                #:name "an empty object is type 03"))
         (test "an empty array is type 04"
               (assert-equal
                #:expect
                (string-append
                 "220000000266696e6400060000007573657273000466696c746572000500"
                 "00000000")
                #:got (hex (bson-encode '(("find" . "users")
                                          ("filter" . #()))))
                #:name "an empty array is type 04"))))

(define scalar-suite
  (suite "BSON scalars"
         (test "a small integer is an int32"
               (assert-equal #:expect "0c0000001069000100000000"
                             #:got (hex (bson-encode '(("i" . 1))))
                             #:name "1 fits in 32 bits"))
         (test "a negative int32"
               (assert-equal #:expect "0c000000106e00ffffffff00"
                             #:got (hex (bson-encode '(("n" . -1))))
                             #:name "-1 is two's complement"))
         (test "an integer past 32 bits is an int64"
               (assert-equal #:expect "10000000126200000000800000000000"
                             #:got (hex (bson-encode '(("b" . 2147483648))))
                             #:name "2^31 does not fit in an int32"))
         (test "a fraction is a double"
               (assert-equal #:expect "10000000016400333333333333144000"
                             #:got (hex (bson-encode '(("d" . 5.05))))
                             #:name "5.05 as IEEE-754 little-endian"))
         (test "booleans are one byte"
               (assert-equal #:expect "0c000000086200010a6e0000"
                             #:got (hex (bson-encode '(("b" . #t)
                                                       ("n" . #nil))))
                             #:name "true is 01 and null carries no payload"))
         (test "the integer limits are the ones BSON has"
               (assert-true
                (encodes-as "1700000012610000000000000000801062000000008000"
                            (list (cons "a" -9223372036854775808)
                                  (cons "b" -2147483648)))
                #:name "-2^63 is the smallest int64 there is")
               (assert-true
                (encodes-as "17000000126100ffffffffffffff7f106200ffffff7f00"
                            (list (cons "a" 9223372036854775807)
                                  (cons "b" 2147483647)))
                #:name "2^63-1 is the largest int64 there is"))))

(define value-suite
  (suite "the values BSON has and Scheme does not"
         (test "an object id is twelve bytes and round-trips"
               (let ((id (bson-object-id-from-hex "5f1f9b6a3a4b5c6d7e8f9a0b")))
                 (assert-true (bson-object-id? id)
                              #:name "a hex string makes an object id")
                 (assert-equal #:expect "5f1f9b6a3a4b5c6d7e8f9a0b"
                               #:got (bson-object-id-string id)
                               #:name "an object id prints as 24 hex characters")
                 (assert-equal #:expect id
                               #:got (assoc-ref (round-trips (list (cons "id" id)))
                                                "id")
                               #:name "an object id survives the codec")
                 (assert-false (bson-object-id-from-hex "not-a-hex-string")
                               #:name "a string that is not 24 hex characters is refused")))
         (test "a date is milliseconds since the epoch"
               (assert-equal #:expect "10000000096400000000000000000000"
                             #:got (hex (bson-encode `(("d" . ,(bson-date 0)))))
                             #:name "the epoch is zero")
               (assert-equal #:expect 1700000000000
                             #:got (bson-date-millis
                                    (assoc-ref
                                     (round-trips
                                      `(("d" . ,(bson-date 1700000000000))))
                                     "d"))
                             #:name "milliseconds survive"))
         (test "binary data keeps its subtype"
               (let* ((value (bson-binary 4 (string->utf8 "abc")))
                      (back (assoc-ref (round-trips (list (cons "b" value))) "b")))
                 (assert-true (bson-binary? back)
                              #:name "binary comes back as binary")
                 (assert-equal #:expect 4 #:got (bson-binary-subtype back)
                               #:name "the subtype survives")
                 (assert-equal #:expect "abc"
                               #:got (utf8->string (bson-binary-bytes back))
                               #:name "the bytes survive")))
         (test "a regex keeps its pattern and options"
               (let ((back (assoc-ref (round-trips
                                       (list (cons "r" (bson-regex "^a" "i"))))
                                      "r")))
                 (assert-equal #:expect "^a" #:got (bson-regex-pattern back)
                               #:name "the pattern survives")
                 (assert-equal #:expect "i" #:got (bson-regex-options back)
                               #:name "the options survive")))
         (test "a timestamp is seconds and a counter"
               (let ((back (assoc-ref (round-trips
                                       (list (cons "t" (bson-timestamp 7 2))))
                                      "t")))
                 (assert-equal #:expect 7 #:got (bson-timestamp-time back)
                               #:name "the seconds survive")
                 (assert-equal #:expect 2 #:got (bson-timestamp-increment back)
                               #:name "the counter survives")))
         (test "decimal128 is kept byte for byte"
               (let ((back (assoc-ref (round-trips
                                       (list (cons "d"
                                                   (bson-decimal128
                                                    (make-bytevector 16 5)))))
                                      "d")))
                 (assert-equal #:expect (make-bytevector 16 5)
                               #:got (bson-decimal128-bytes back)
                               #:name "sixteen bytes in, sixteen bytes out")))))

(define shape-suite
  (suite "nested documents, arrays and ports"
         (test "a nested document round-trips"
               (assert-equal #:expect '(("outer" . (("inner" . "value"))))
                             #:got (round-trips
                                    '(("outer" . (("inner" . "value")))))
                             #:name "an object inside an object"))
         (test "an array round-trips as a vector"
               (assert-equal #:expect '(("a" . #("x" 1 #t)))
                             #:got (round-trips '(("a" . #("x" 1 #t))))
                             #:name "a vector, not a list"))
         (test "keys are read in the order they were written"
               (assert-equal #:expect "keyname"
                             #:got (string-concatenate
                                    (map car (round-trips
                                              '(("key" . 1) ("name" . 2)))))
                             #:name "a `createIndexes' command keeps its order"))
         (test "#nil is null"
               ;; A migration that writes `{"success": null}' has to reach the
               ;; server as a BSON null rather than as the string "null".
               (assert-equal #:expect #nil
                             #:got (assoc-ref (round-trips '(("n" . #nil))) "n")
                             #:name "null is #nil, not a symbol"))
         (test "a document is read back from a port"
               (let* ((document '(("hello" . "world") ("n" . 42)))
                      (bytes (bson-encode document))
                      (port (open-bytevector-input-port bytes)))
                 (assert-equal #:expect document
                               #:got (bson-decode-from-port port)
                               #:name "one document")
                 (assert-equal #:expect #nil
                               #:got (bson-decode-from-port port)
                               #:name "and the end of the port is #nil")))
         (test "two documents in one stream are read one at a time"
               ;; This is the shape of an `OP_MSG' body: a run of documents.
               (let* ((bytes (concatenate-bytes
                              (list (bson-encode '(("a" . 1)))
                                    (bson-encode '(("b" . 2))))))
                      (port (open-bytevector-input-port bytes)))
                 (assert-equal #:expect '((("a" . 1)) (("b" . 2)))
                               #:got (list (bson-decode-from-port port)
                                           (bson-decode-from-port port))
                               #:name "the second starts where the first ended")))))

(define failure-suite
  (suite "what the codec refuses"
         (test "a number past 64 bits is refused"
               (assert-error (lambda ()
                               (bson-encode (list (cons "n" (expt 2 64)))))
                             #:name "no BSON type holds it"))
         (test "a value with no BSON type is refused"
               (assert-error (lambda () (bson-encode (list (cons "v" 'symbol))))
                             #:name "a symbol is not BSON"))
         (test "an array where a document belongs is refused"
               (assert-error (lambda () (bson-encode #(1 2 3)))
                             #:name "bson-encode wants an alist"))
         (test "a document claiming a length it does not have is refused"
               (assert-error
                (lambda ()
                  ;; A five-byte length in front of four bytes: the reader has
                  ;; to notice rather than read past the end of the buffer.
                  (bson-decode (bytes-of "05000000")))
                #:name "the length is checked against the bytes present"))
         (test "a truncated document is refused"
               (assert-error
                (lambda ()
                  (bson-decode (bytes-of "160000000268656c6c6f0006000000776f72")))
                #:name "a string that runs off the end"))))

(define (spec)
  (veritas-run framing-suite
               scalar-suite
               value-suite
               shape-suite
               failure-suite))
