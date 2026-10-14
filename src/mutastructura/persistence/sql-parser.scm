;;; mutastructura - Relational Schema Migrations Powered by Lisp

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

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
;; Splitting a SQL file into statements by looking for `;' is wrong often
;; enough to hurt: a semicolon inside a string, a comment, an identifier or a
;; PostgreSQL function body is not the end of a statement.  So this is a real
;; lexer -- a finite state machine walked one character at a time -- and it
;; knows the quoting rules of the engines we speak to.
;;
;; What it understands, and why each piece is here:
;;
;;   'single quoted'   strings, with `''` as the escape of a quote.
;;   "double quoted"   identifiers in the SQL standard, `""` escaped.
;;   `back quoted`     identifiers in MySQL, `` `` `` escaped.
;;   -- line comment   to the end of the line.
;;   # line comment    MySQL's other line comment, only when asked for: `#' is
;;                     PostgreSQL's XOR operator, so it is not universal.
;;   /* block */       with *nesting*, which PostgreSQL allows and others do
;;                     not.  Accepting nesting is the safe reading.
;;   $tag$ ... $tag$   PostgreSQL dollar quoting: the only sane way to write a
;;                     function body containing semicolons.
;;   E'\''             PostgreSQL escape strings, where a backslash escapes.
;;   \'  and  \\       MySQL strings, where a backslash always escapes.  Turned
;;                     on for MySQL and MariaDB only: in SQLite and standard
;;                     PostgreSQL a backslash is just a backslash, and guessing
;;                     otherwise would break a literal that ends in one.
;;   DELIMITER $$      the mysql client's way of writing a trigger in a script.
;;                     A directive for the client, not SQL: it retargets the
;;                     splitter and is not sent on.  It is handled a line at a
;;                     time, *before* lexing, because the whole point of the
;;                     directive is to change what ends a statement, and the
;;                     semicolons it protects would otherwise already have been
;;                     treated as ends.
;;
;;   BEGIN ... END     the body of a `CREATE TRIGGER', and of the routines that
;;                     are written without a quoting device to protect them.
;;                     SQLite has no `DELIMITER' and no dollar quoting, so
;;                     `CREATE TRIGGER ... BEGIN ... END;' is the *only* way to
;;                     write a trigger, and a splitter that stops at the first
;;                     `;' inside the body cuts a working trigger in half.  The
;;                     block is counted, and `END IF' / `END LOOP' / `END CASE' /
;;                     `END WHILE' do not close it: those ENDs belong to
;;                     statements inside the body that never opened a block of
;;                     their own.  Only a statement that *begins* with `CREATE'
;;                     is counted, so a migration that writes `BEGIN;' / `COMMIT;'
;;                     around its own work is left exactly as it is.
;;
;; Every statement is reported with the line its first real character sits on,
;; which is what turns "a migration failed" into "line 47 of this file failed".

(define-module (mutastructura persistence sql-parser)
  #:declarative? #t
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-8)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (mutastructura mutastructura)
  #:export (string-split-to-queries
            parse-sql-statements
            statements->queries
            statement-sql
            statement-line
            statement-index
            strip-sql-comments
            statement-meaningful?
            delimiter-directive?
            sql-comments-only?))

;;; ---------------------------------------------------------------------------
;;; Small scanning helpers
;;; ---------------------------------------------------------------------------

(define (prefix-match? chars str)
  "Whether the beginning of CHARS spells STR."
  (let loop ((cs chars) (i 0))
    (cond
      ((= i (string-length str))
       #t)
      ((null? cs)
       #f)
      ((char=? (car cs) (string-ref str i))
       (loop (cdr cs) (+ i 1)))
      (else #f))))

(define (drop-chars chars n)
  "CHARS without its first N characters."
  (if (or (<= n 0) (null? chars))
      chars
      (drop-chars (cdr chars) (- n 1))))

(define (dollar-tag chars)
  "The dollar-quote tag CHARS opens with, or #f.

CHARS must start with `$'.  Both `$$' and `$tag$' are tags; `$1' is a
positional parameter and not a tag at all."
  (let loop ((cs (cdr chars)) (name '()) (first? #t))
    (cond
      ((null? cs)
       #f)
      ((char=? (car cs) #\$)
       (list->string (append (list #\$) (reverse name) (list #\$))))
      (first?
       (if (or (char-alphabetic? (car cs)) (char=? (car cs) #\_))
           (loop (cdr cs) (list (car cs)) #f)
           #f))
      ((or (char-alphabetic? (car cs))
           (char-numeric? (car cs))
           (char=? (car cs) #\_))
       (loop (cdr cs) (cons (car cs) name) #f))
      (else #f))))

(define (identifier-char? c)
  "Whether C may continue a word; used to tell an `E' apart from `WHERE'."
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\_ #\$))))

(define (escape-string-start? emitted backslash-escapes?)
  "Whether the string opening after text EMITTED is an escape string.

MySQL treats `\\' as an escape everywhere, so there the answer is simply the
configuration.  PostgreSQL does so only inside the explicit `E'...' form, so we
look at the character already emitted: a string that follows an `E' which is not
the tail of a longer word (`WHERE' must not count) is an escape string."
  (if backslash-escapes?
      #t
      (and (pair? emitted)
           (memv (car emitted) '(#\E #\e))
           (let ((rest (cdr emitted)))
             (or (null? rest)
                 (not (identifier-char? (car rest))))))))

;;; ---------------------------------------------------------------------------
;;; Keywords that change how a statement ends
;;; ---------------------------------------------------------------------------

;; The word-level state of the statement being read: which keyword was read
;; first (a `CREATE' may hold a `BEGIN ... END' block), whether a block is open,
;; and an `END' that is still waiting to find out whether it closes that block or
;; belongs to an `IF' inside it.  Kept apart from the character-level state in
;; `lex-statements' because it changes only when a word ends, and mutated rather
;; than threaded so that the lexer keeps reading as a character machine.
(define-record-type <statement-keywords>
  (make-statement-keywords create? first? pending-end? blocks word)
  statement-keywords?
  (create? keywords-create? set-keywords-create?!)
  (first? keywords-first? set-keywords-first?!)
  (pending-end? keywords-pending-end? set-keywords-pending-end?!)
  (blocks keywords-blocks set-keywords-blocks!)
  (word keywords-word set-keywords-word!))

;; The words `END' may be followed by, and still be the end of something that
;; opened inside the block rather than the block itself.
(define end-followers '("if" "loop" "case" "while" "repeat"))

(define (word-char? c)
  "Whether C may be part of a keyword."
  (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_)))

(define (end-keyword! keywords word)
  "Note that WORD ended, and answer with nothing.

Three words matter.  The statement's first decides whether the rest of it is a
`CREATE' at all.  `BEGIN' inside a `CREATE' opens a block.  `END' inside one is
either the block's end or the end of an `IF'/'LOOP'/'CASE'/'WHILE' inside it,
which is decided by the word after it."
  (let ((lower (string-downcase word)))
    (cond
      ((keywords-first? keywords)
       (set-keywords-first?! keywords #f)
       (set-keywords-create?! keywords (string=? lower "create")))
      (else
       (if (keywords-pending-end? keywords)
           (begin
             (set-keywords-pending-end?! keywords #f)
             ;; `END IF' closes an `IF' that never opened a counted block, so it
             ;; must not close the one we are counting.
             (unless (member lower end-followers)
               (set-keywords-blocks! keywords
                                     (max 0 (- (keywords-blocks keywords)
                                               1))))))
       (when (and (keywords-create? keywords) (string=? lower "begin"))
         (set-keywords-blocks! keywords (+ 1 (keywords-blocks keywords))))
       (when (and (keywords-create? keywords) (string=? lower "end"))
         (set-keywords-pending-end?! keywords #t))))))

(define (resolve-pending-end! keywords)
  "Settle a pending `END' that was not followed by a word: it closes a block."
  (when (keywords-pending-end? keywords)
    (set-keywords-pending-end?! keywords #f)
    (set-keywords-blocks! keywords
                          (max 0 (- (keywords-blocks keywords) 1)))))

(define (reset-keywords! keywords)
  "Forget everything known about the statement that just ended."
  (set-keywords-create?! keywords #f)
  (set-keywords-first?! keywords #t)
  (set-keywords-pending-end?! keywords #f)
  (set-keywords-blocks! keywords 0)
  (set-keywords-word! keywords '()))

;;; ---------------------------------------------------------------------------
;;; The lexer
;;; ---------------------------------------------------------------------------

(define* (lex-statements sql-string
                         #:key
                         (engine #f)
                         (backslash-escapes? #f)
                         (hash-comments? #f)
                         (delimiter ";")
                         (line-offset 0))
  "The state machine itself: SQL-STRING in, statements out.

LINE-OFFSET is added to every reported line, so a chunk of a larger script still
reports the lines of the file it came from."
  (let* ((id (and engine (engine-id engine)))
         (mysql? (memq id '(mysql mariadb)))
         (postgres? (memq id '(postgresql cockroachdb)))
         (backslash? (or backslash-escapes? mysql?))
         (hash? (or hash-comments? mysql?))
         (keywords (make-statement-keywords #f #t #f 0 '())))
    (let loop ((chars (string->list (strip-bom sql-string)))
               (state 'normal)
               (acc '())          ; the statement as written, comments included
               (content '())      ; the same, comments removed
               (statements '())
               (line 1)
               (content-line #f)  ; the line the first real character is on
               (tag #f)           ; the dollar-quote tag we are inside
               (escapes? backslash?)
               (depth 0))         ; block comment nesting

      (define (finish)
        "Close the current statement and return the statement list."
        (let* ((sql (trim-whitespace (list->string (reverse acc))))
               (spoken (trim-whitespace (list->string (reverse content)))))
          ;; A statement that is only whitespace and comments is not a
          ;; statement: it must not be sent, and it must not be reported.
          (if (or (string=? sql "") (string=? spoken ""))
              statements
              (append statements
                      (list (list (cons 'index (length statements))
                                  (cons 'line (+ line-offset
                                                 (or content-line line)))
                                  (cons 'sql sql)))))))

      (if (null? chars)
          (finish)
          (let ((c (car chars))
                (rest (cdr chars)))
            (case state
              ((normal)
               (cond
                 ;; A keyword has ended: note it, and read the same character
                 ;; again.  This is what counts the `BEGIN ... END' block of a
                 ;; `CREATE TRIGGER', so that the semicolons inside it are not
                 ;; mistaken for the end of the statement.
                 ((and (pair? (keywords-word keywords))
                       (not (word-char? c)))
                  (end-keyword! keywords
                                (list->string (reverse (keywords-word keywords))))
                  (set-keywords-word! keywords '())
                  (loop chars 'normal acc content statements line content-line #f
                        escapes? depth))
                 ;; An `END' that is not followed by a word closes the block, so
                 ;; the character that follows it decides.  A word character
                 ;; does not decide anything yet: it may still turn out to be
                 ;; the `IF' of `END IF', which closes no block of ours.
                 ((and (keywords-pending-end? keywords)
                       (not (word-char? c))
                       (not (char-whitespace? c)))
                  (resolve-pending-end! keywords)
                  (loop chars 'normal acc content statements line content-line #f
                        escapes? depth))
                 ;; The end of a statement: the delimiter, which is never seen
                 ;; inside a string, a comment, an identifier or a dollar quote.
                 ;; It is not one inside an open `BEGIN ... END' block either.
                 ((and (prefix-match? chars delimiter)
                       (zero? (keywords-blocks keywords)))
                  (reset-keywords! keywords)
                  (loop (drop-chars chars (string-length delimiter))
                        'normal '() '() (finish) line #f #f backslash? 0))
                 ((char=? c #\')
                  (loop rest 'string (cons c acc) (cons c content) statements
                        line (or content-line line) #f
                        (escape-string-start? content backslash?) 0))
                 ((char=? c #\")
                  (loop rest 'identifier (cons c acc) (cons c content)
                        statements line (or content-line line) #f escapes? 0))
                 ((and mysql? (char=? c #\`))
                  (loop rest 'backtick (cons c acc) (cons c content) statements
                        line (or content-line line) #f escapes? 0))
                 ((and (char=? c #\-) (pair? rest) (char=? (car rest) #\-))
                  (loop (cdr rest) 'line-comment (cons #\- (cons #\- acc))
                        content statements line content-line #f escapes? 0))
                 ((and (char=? c #\/) (pair? rest) (char=? (car rest) #\*))
                  (loop (cdr rest) 'block-comment (cons #\* (cons #\/ acc))
                        content statements line content-line #f escapes? 1))
                 ((and hash? (char=? c #\#))
                  (loop rest 'line-comment (cons c acc) content statements
                        line content-line #f escapes? 0))
                 ((char=? c #\$)
                  (let ((tag* (dollar-tag chars)))
                    (if tag*
                        (loop (drop-chars chars (string-length tag*))
                              'dollar
                              (append (reverse (string->list tag*)) acc)
                              (append (reverse (string->list tag*)) content)
                              statements line (or content-line line) tag*
                              escapes? 0)
                        (loop rest 'normal (cons c acc) (cons c content)
                              statements line (or content-line line) #f
                              escapes? 0))))
                 ((char=? c #\newline)
                  (loop rest 'normal (cons c acc) content statements
                        (+ line 1) content-line #f escapes? 0))
                 (else
                  (when (word-char? c)
                    (set-keywords-word! keywords
                                        (cons c (keywords-word keywords))))
                  (loop rest 'normal (cons c acc) (cons c content) statements
                        line (or content-line (and (not (char-whitespace? c))
                                                   line))
                        #f escapes? 0))))

              ((string)
               (cond
                 ((and escapes? (char=? c #\\))
                  (if (null? rest)
                      (loop '() state (cons c acc) (cons c content) statements
                            line content-line #f escapes? 0)
                      (loop (cdr rest) 'string
                            (cons (car rest) (cons c acc))
                            (cons (car rest) (cons c content))
                            statements
                            (if (char=? (car rest) #\newline) (+ line 1) line)
                            content-line #f escapes? 0)))
                 ;; `''` is one quote, not the end of a string and a new one.
                 ((and (char=? c #\') (pair? rest) (char=? (car rest) #\'))
                  (loop (cdr rest) 'string (cons #\' (cons c acc))
                        (cons #\' (cons c content)) statements line content-line
                        #f escapes? 0))
                 ((char=? c #\')
                  (loop rest 'normal (cons c acc) (cons c content) statements
                        line content-line #f escapes? 0))
                 ((char=? c #\newline)
                  (loop rest 'string (cons c acc) (cons c content) statements
                        (+ line 1) content-line #f escapes? 0))
                 (else
                  (loop rest 'string (cons c acc) (cons c content) statements
                        line content-line #f escapes? 0))))

              ((identifier)
               (if (and (char=? c #\") (pair? rest) (char=? (car rest) #\"))
                   (loop (cdr rest) 'identifier (cons #\" (cons c acc))
                         (cons #\" (cons c content)) statements line content-line
                         #f escapes? 0)
                   (if (char=? c #\")
                       (loop rest 'normal (cons c acc) (cons c content)
                             statements line content-line #f escapes? 0)
                       (loop rest 'identifier (cons c acc) (cons c content)
                             statements line content-line #f escapes? 0))))

              ((backtick)
               (if (and (char=? c #\`) (pair? rest) (char=? (car rest) #\`))
                   (loop (cdr rest) 'backtick (cons #\` (cons c acc))
                         (cons #\` (cons c content)) statements line content-line
                         #f escapes? 0)
                   (if (char=? c #\`)
                       (loop rest 'normal (cons c acc) (cons c content)
                             statements line content-line #f escapes? 0)
                       (loop rest 'backtick (cons c acc) (cons c content)
                             statements line content-line #f escapes? 0))))

              ((line-comment)
               (if (char=? c #\newline)
                   (loop rest 'normal (cons c acc) content statements
                         (+ line 1) content-line #f escapes? 0)
                   (loop rest 'line-comment (cons c acc) content statements
                         line content-line #f escapes? 0)))

              ((block-comment)
               (cond
                 ((and (char=? c #\*) (pair? rest) (char=? (car rest) #\/))
                  (if (> depth 1)
                      (loop (cdr rest) 'block-comment
                            (cons #\/ (cons #\* acc)) content statements line
                            content-line #f escapes? (- depth 1))
                      (loop (cdr rest) 'normal (cons #\/ (cons #\* acc)) content
                            statements line content-line #f escapes? 0)))
                 ;; Only PostgreSQL nests block comments, so only there may a
                 ;; second `/*' open one.  Elsewhere the first `*/' closes,
                 ;; which is exactly what that engine will do with the text.
                 ((and postgres?
                       (char=? c #\/)
                       (pair? rest)
                       (char=? (car rest) #\*))
                  (loop (cdr rest) 'block-comment (cons #\* (cons #\/ acc))
                        content statements line content-line #f escapes?
                        (+ depth 1)))
                 ((char=? c #\newline)
                  (loop rest 'block-comment (cons c acc) content statements
                        (+ line 1) content-line #f escapes? depth))
                 (else
                  (loop rest 'block-comment (cons c acc) content statements
                        line content-line #f escapes? depth))))

              ((dollar)
               ;; Inside `$tag$ ... $tag$' every character is data, semicolons
               ;; included, until the very same tag shows up again.
               (if (prefix-match? chars tag)
                   (loop (drop-chars chars (string-length tag)) 'normal
                         (append (reverse (string->list tag)) acc)
                         (append (reverse (string->list tag)) content)
                         statements line content-line #f escapes? 0)
                   (loop rest 'dollar (cons c acc) (cons c content) statements
                         line content-line tag escapes? 0)))))))))

;;; ---------------------------------------------------------------------------
;;; Comments and directives
;;; ---------------------------------------------------------------------------

(define (delimiter-directive? sql)
  "The delimiter SQL sets with a `DELIMITER' directive, or #f.

Recognised by hand rather than by regular expression: the engine Guile ships has
neither case-insensitive groups nor POSIX character classes, and a directive is
far too simple to need them."
  (let* ((trimmed (trim-whitespace sql))
         (length (string-length trimmed)))
    (and (> length 10)
         (string-ci=? "delimiter" (substring trimmed 0 9))
         (char-whitespace? (string-ref trimmed 9))
         (let ((token (trim-whitespace (substring trimmed 9))))
           (and (> (string-length token) 0)
                (not (string-any char-whitespace? token))
                (cons 'delimiter token))))))

(define (number-lines sql)
  "SQL as a list of `(line-number . text-with-its-newline)'."
  (let loop ((i 0) (start 0) (line 1) (acc '()))
    (cond
      ((>= i (string-length sql))
       (reverse (if (> i start)
                    (cons (cons line (substring sql start i)) acc)
                    acc)))
      ((char=? (string-ref sql i) #\newline)
       (loop (+ i 1) (+ i 1) (+ line 1)
             (cons (cons line (substring sql start (+ i 1))) acc)))
      (else (loop (+ i 1) start line acc)))))

(define* (delimiter-chunks sql #:key (delimiter ";"))
  "Split SQL into `(delimiter start-line text)' chunks at DELIMITER directives.

The directive has to be applied before lexing, not after, because the semicolons
it is there to protect would otherwise already have been read as ends of
statements.  The mysql client works the same way: a line at a time."
  (let loop ((lines (number-lines sql))
             (delimiter delimiter)
             (start-line 1)
             (current '())
             (chunks '()))
    (define (chunk)
      "The chunk accumulated so far, or #f when it holds nothing."
      (if (null? current)
          #f
          (list delimiter start-line
                (apply string-append (reverse current)))))
    (if (null? lines)
        (reverse (if (chunk) (cons (chunk) chunks) chunks))
        (let* ((entry (car lines))
               (line (car entry))
               (text (cdr entry))
               (directive (delimiter-directive? text)))
          (if directive
              (loop (cdr lines) (cdr directive) (+ line 1) '()
                    (if (chunk) (cons (chunk) chunks) chunks))
              (loop (cdr lines) delimiter start-line (cons text current)
                    chunks))))))

(define* (parse-sql-statements sql-string
                               #:key
                               (engine #f)
                               (backslash-escapes? #f)
                               (hash-comments? #f)
                               (delimiter ";"))
  "Split SQL-STRING into statements, each with the line it starts on.

The answer is a list of alists:

@example
((index . 0) (line . 17) (sql . \"CREATE TABLE \\\"user\\\" (...)\"))
@end example

ENGINE, when given, selects the dialect's lexical quirks: MySQL and MariaDB get
backslash escapes, `#' comments, backtick identifiers and `DELIMITER' support.
The keyword arguments override that choice individually, which is what the tests
use."
  (let loop ((chunks (delimiter-chunks (or sql-string "") #:delimiter delimiter))
             (statements '()))
    (if (null? chunks)
        statements
        (let* ((chunk (car chunks))
               (chunk-delimiter (car chunk))
               (start-line (cadr chunk))
               (text (caddr chunk))
               (found (lex-statements text
                                      #:engine engine
                                      #:backslash-escapes? backslash-escapes?
                                      #:hash-comments? hash-comments?
                                      #:delimiter chunk-delimiter
                                      #:line-offset (- start-line 1))))
          (loop (cdr chunks)
                (append statements
                        (map (lambda (statement)
                               (list (cons 'index (+ (length statements)
                                                     (assq-ref statement
                                                               'index)))
                                     (cons 'line (assq-ref statement 'line))
                                     (cons 'sql (assq-ref statement 'sql))))
                             found)))))))

(define* (strip-sql-comments sql #:key (engine #f) (backslash-escapes? #f))
  "SQL with every `--', `#' and `/* */' comment removed.

Used to decide whether a statement has anything *in* it: one holding nothing but
a licence header is not a statement, and some drivers reject an empty query
outright.  The original text, comments and all, is what actually gets executed --
comments are useful in a query log, and stripping them would make the SHA-256 of
a file disagree with what ran."
  (let* ((id (and engine (engine-id engine)))
         (mysql? (memq id '(mysql mariadb)))
         (hash? mysql?)
         (backslash? (or backslash-escapes? mysql?)))
    (let loop ((chars (string->list (or sql "")))
               (state 'normal)
               (acc '())
               (tag #f)
               (escapes? backslash?))
      (if (null? chars)
          (list->string (reverse acc))
          (let ((c (car chars))
                (rest (cdr chars)))
            (case state
              ((normal)
               (cond
                 ((char=? c #\')
                  (loop rest 'string (cons c acc) #f
                        (escape-string-start? acc backslash?)))
                 ((char=? c #\")
                  (loop rest 'identifier (cons c acc) #f escapes?))
                 ((and (char=? c #\-) (pair? rest) (char=? (car rest) #\-))
                  (loop (cdr rest) 'line-comment acc #f escapes?))
                 ((and (char=? c #\/) (pair? rest) (char=? (car rest) #\*))
                  (loop rest 'block-comment acc #f escapes?))
                 ((and hash? (char=? c #\#))
                  (loop rest 'line-comment acc #f escapes?))
                 ((char=? c #\$)
                  (let ((tag* (dollar-tag chars)))
                    (if tag*
                        (loop (drop-chars chars (string-length tag*)) 'dollar
                              (append (reverse (string->list tag*)) acc) tag*
                              escapes?)
                        (loop rest 'normal (cons c acc) #f escapes?))))
                 (else (loop rest 'normal (cons c acc) #f escapes?))))
              ((string)
               (cond
                 ((and escapes? (char=? c #\\))
                  (if (null? rest)
                      (loop '() 'string (cons c acc) #f escapes?)
                      (loop (cdr rest) 'string (cons (car rest) (cons c acc)) #f
                            escapes?)))
                 ((and (char=? c #\') (pair? rest) (char=? (car rest) #\'))
                  (loop (cdr rest) 'string (cons #\' (cons c acc)) #f escapes?))
                 ((char=? c #\')
                  (loop rest 'normal (cons c acc) #f escapes?))
                 (else (loop rest 'string (cons c acc) #f escapes?))))
              ((identifier)
               (if (char=? c #\")
                   (loop rest 'normal (cons c acc) #f escapes?)
                   (loop rest 'identifier (cons c acc) #f escapes?)))
              ((line-comment)
               (if (char=? c #\newline)
                   (loop rest 'normal (cons c acc) #f escapes?)
                   (loop rest 'line-comment acc #f escapes?)))
              ((block-comment)
               (cond
                 ((and (char=? c #\*) (pair? rest) (char=? (car rest) #\/))
                  (loop (cdr rest) 'normal acc #f escapes?))
                 ((char=? c #\newline)
                  (loop rest 'block-comment (cons c acc) #f escapes?))
                 (else (loop rest 'block-comment acc #f escapes?))))
              ((dollar)
               (if (prefix-match? chars tag)
                   (loop (drop-chars chars (string-length tag)) 'normal
                         (append (reverse (string->list tag)) acc) #f escapes?)
                   (loop rest 'dollar (cons c acc) tag escapes?)))))))))

(define (statement-meaningful? sql)
  "Whether SQL contains anything other than whitespace and comments."
  (not (string=? (trim-whitespace (strip-sql-comments sql)) "")))

(define (sql-comments-only? sql)
  "Whether SQL holds nothing but comments and whitespace."
  (not (statement-meaningful? sql)))

;;; ---------------------------------------------------------------------------
;;; Public API
;;; ---------------------------------------------------------------------------

(define* (statements->queries statements #:key (engine #f))
  "The SQL text of STATEMENTS, as plain strings."
  (map (lambda (statement)
         (assq-ref statement 'sql))
       statements))

(define* (string-split-to-queries sql-string #:key (engine #f))
  "Split SQL-STRING into a list of statement strings.

Kept as the simple string-in/list-of-strings-out entry point, because that is
what callers of the first version of this program expect; `parse-sql-statements'
is what the runner uses, since it also reports the line each statement starts
on."
  (statements->queries (parse-sql-statements sql-string #:engine engine)
                       #:engine engine))

(define (statement-sql statement) (assq-ref statement 'sql))
(define (statement-line statement) (assq-ref statement 'line))
(define (statement-index statement) (assq-ref statement 'index))
