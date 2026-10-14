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
;; The thing every migration file meets first: the splitter that decides where
;; one statement ends and the next begins.  Splitting on every `;' is wrong often
;; enough to hurt, so almost every case below is a `;' that must *not* split --
;; inside a string, an identifier, a comment, a dollar quote or a trigger body --
;; and the rest are the dialect differences that decide whether it does.
;;
;; The engines are reached through `find-engine' with the names people type, so
;; `mysql' here is the same engine `--database mariadb' resolves to: a lexer that
;; is generous about backslashes for one and not the other would be a bug that
;; only shows up in a migration nobody tested.

(define-module (veritas unit sql-parser)
  #:declarative? #t
  #:use-module (veritas veritas)
  #:use-module (veritas reporter)
  #:use-module (ice-9 format)
  #:use-module (mutastructura mutastructura)
  #:use-module (mutastructura persistence sql-parser)
  #:export (spec))

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(define mysql (find-engine 'mysql))
(define postgres (find-engine 'postgres))
(define mariadb (find-engine 'mariadb))

(define* (sqls text #:optional (engine #f))
  "The statements TEXT splits into, as their SQL text."
  (statements->queries (if engine
                           (parse-sql-statements text #:engine engine)
                           (parse-sql-statements text))))

(define (lines text)
  "The line each of TEXT's statements starts on."
  (map statement-line (parse-sql-statements text)))

(define (contains? haystack needle)
  "Whether HAYSTACK, a string, holds NEEDLE."
  (and haystack (string-contains haystack needle) #t))

(define* (first-statement text #:optional (engine #f))
  "The SQL of the first statement TEXT splits into, or #f."
  (let ((statements (sqls text engine)))
    (and (pair? statements) (car statements))))

;;; ---------------------------------------------------------------------------
;;; Where a statement ends
;;; ---------------------------------------------------------------------------

(define splitting-suite
  (suite "a semicolon is not always the end"
         #:concurrent? #f
         (test "two statements separated by semicolons are two"
               #:shuffle? #f
               (let ((statements (parse-sql-statements "SELECT 1; SELECT 2;")))
                 (assert-length #:expect 2 #:got statements
                                #:name "both are read")
                 (assert-equal #:expect '("SELECT 1" "SELECT 2")
                               #:got (map statement-sql statements)
                               #:name "and the delimiter is not part of either")
                 (assert-equal #:expect '(0 1)
                               #:got (map statement-index statements)
                               #:name "each carries its position")))
         (test "the line a statement starts on is reported"
               #:shuffle? #f
               ;; What turns ``a migration failed'' into ``line 3 of this file
               ;; failed''.  The line is the one the first *real* character sits
               ;; on, so blank lines before a statement do not shift it.
               (assert-equal #:expect '(1 3)
                             #:got (lines "SELECT 1;\n\n  SELECT 2\n     , 3;")
                             #:name "the line of the first real character"))
         (test "a semicolon inside a string is part of the string"
               #:shuffle? #f
               (let ((statements (sqls "INSERT INTO t VALUES ('a;b');\nSELECT 1;")))
                 (assert-length #:expect 2 #:got statements
                                #:name "the insert is not cut in half")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "'a;b'")
                               #:name "and it still holds its semicolon")))
         (test "a doubled quote does not end the string"
               #:shuffle? #f
               (assert-length #:expect 1 #:got (sqls "SELECT 'it''s; fine';")
                              #:name "the escaped quote is part of the value"))
         (test "a semicolon inside a quoted identifier is part of the identifier"
               #:shuffle? #f
               (assert-length #:expect 1
                              #:got (sqls "CREATE TABLE \"we;ird\" (x int);")
                              #:name "the identifier is not cut in half"))
         (test "a statement that is never terminated is still a statement"
               #:shuffle? #f
               ;; A file whose last line has no `;' is a mistake people make,
               ;; and dropping the statement silently is the worst reading of it.
               (assert-equal #:expect '("SELECT 1" "SELECT 2")
                             #:got (sqls "SELECT 1;\nSELECT 2")
                             #:name "the last statement is read"))
         (test "nothing between two semicolons is not a statement"
               #:shuffle? #f
               (assert-equal #:expect '("SELECT 1" "SELECT 2")
                             #:got (sqls "SELECT 1;;SELECT 2;")
                             #:name "the empty one is dropped"))
         (test "a file of whitespace and comments holds no statements"
               #:shuffle? #f
               ;; A licence header is not a statement, and some drivers refuse an
               ;; empty query outright.
               (assert-empty
                #:got (parse-sql-statements
                       "-- mutastructura; a licence header\n\n/* nothing; here */\n")
                #:name "nothing to run")
               (assert-empty #:got (parse-sql-statements "")
                             #:name "and an empty file has nothing either"))))

;;; ---------------------------------------------------------------------------
;;; Comments
;;; ---------------------------------------------------------------------------

(define comment-suite
  (suite "comments"
         #:concurrent? #f
         (test "a `--' comment runs to the end of the line"
               #:shuffle? #f
               (assert-length #:expect 1
                              #:got (sqls "-- a; comment\nSELECT 1;")
                              #:name "the semicolon in it is not one"))
         (test "a trailing comment neither splits nor extends a statement"
               #:shuffle? #f
               (assert-equal #:expect '("SELECT 1")
                             #:got (sqls "SELECT 1; -- and; more\n")
                             #:name "the comment is not a second statement"))
         (test "a block comment nests where the engine nests them"
               #:shuffle? #f
               ;; PostgreSQL nests block comments and the others do not, so the
               ;; first `*/' closes for them -- which is exactly what that
               ;; server will do with the text, and the reading that keeps a
               ;; migration honest on both.
               (let ((text "/* outer /* inner; */ still; */ SELECT 1;"))
                 (assert-length #:expect 1 #:got (sqls text postgres)
                                #:name "PostgreSQL reads one comment and one statement")
                 (assert-equal #:expect #t
                               #:got (contains? (first-statement text postgres)
                                                "SELECT 1")
                               #:name "with the select after it")
                 (assert-length #:expect 2 #:got (sqls text)
                                #:name "and the others split at the inner semicolon")))
         (test "`#' is a comment for MySQL and an operator for PostgreSQL"
               #:shuffle? #f
               (assert-length #:expect 1
                              #:got (sqls "# a; comment\nSELECT 1;" mysql)
                              #:name "MySQL reads it as a comment")
               (assert-length #:expect 2
                              #:got (sqls "# a; comment\nSELECT 1;")
                              #:name "PostgreSQL does not, so the `;' splits"))
         (test "a comment can be stripped for the question of what is in a statement"
               #:shuffle? #f
               (let ((stripped (strip-sql-comments
                                "-- mutastructura\nSELECT 1; -- trailing\n")))
                 (assert-equal #:expect #t
                               #:got (contains? stripped "SELECT 1;")
                               #:name "the SQL is kept")
                 (assert-equal #:expect #f
                               #:got (contains? stripped "--")
                               #:name "and every comment is gone"))
               (assert-equal #:expect #t
                             #:got (contains? (strip-sql-comments
                                               "/* a; header */ SELECT 1;")
                                              "SELECT 1")
                             #:name "a block comment goes too"))
         (test "what a statement means is decided without its comments"
               #:shuffle? #f
               (assert-equal #:expect #t
                             #:got (statement-meaningful? "SELECT 1")
                             #:name "SQL is meaningful")
               (assert-equal #:expect #f
                             #:got (statement-meaningful? "-- just a; comment")
                             #:name "a comment is not")
               (assert-equal #:expect #f
                             #:got (statement-meaningful? "   \n\t ")
                             #:name "and neither is whitespace")
               (assert-equal #:expect #t
                             #:got (sql-comments-only? "/* nothing */")
                             #:name "the two questions are each other's opposite"))))

;;; ---------------------------------------------------------------------------
;;; The dialect's own lexing
;;; ---------------------------------------------------------------------------

(define dialect-suite
  (suite "what only one dialect does"
         #:concurrent? #f
         (test "a backtick identifier is MySQL's alone"
               #:shuffle? #f
               ;; `"` is the standard's identifier quote; a backtick is MySQL's,
               ;; so a semicolon inside one splits a PostgreSQL file.
               (assert-length #:expect 1
                              #:got (sqls "CREATE TABLE `we;ird` (x int);" mysql)
                              #:name "MySQL reads the backtick as an identifier")
               (assert-length #:expect 2
                              #:got (sqls "CREATE TABLE `we;ird` (x int);")
                              #:name "and PostgreSQL does not"))
         (test "a backslash escapes inside a MySQL string"
               #:shuffle? #f
               ;; Everywhere else a backslash is just a backslash, and guessing
               ;; otherwise would break a literal that ends in one.
               (assert-length #:expect 1
                              #:got (sqls "INSERT INTO t VALUES ('a\\';b');" mysql)
                              #:name "MySQL's escaped quote keeps the string open")
               (assert-length #:expect 2
                              #:got (sqls "INSERT INTO t VALUES ('a\\';b');")
                              #:name "PostgreSQL's string ends at the quote"))
         (test "PostgreSQL's `E' strings are escape strings"
               #:shuffle? #f
               (assert-length #:expect 1
                              #:got (sqls "SELECT E'it\\'s; fine';")
                              #:name "the `E' makes the backslash an escape"))
         (test "an `E' that is the tail of a word is not one"
               #:shuffle? #f
               ;; `WHERE'' must not count, which is the whole reason the
               ;; character before the quote is inspected at all.
               (assert-length #:expect 2
                              #:got (sqls "SELECT W'it\\'s; fine';")
                              #:name "a word ending in W is not an escape string"))))

;;; ---------------------------------------------------------------------------
;;; BEGIN ... END
;;; ---------------------------------------------------------------------------

(define body-suite
  (suite "a body a semicolon must not end"
         #:concurrent? #f
         (test "SQLite's trigger body is one statement"
               #:shuffle? #f
               ;; SQLite has no `DELIMITER' and no dollar quoting, so
               ;; `CREATE TRIGGER ... BEGIN ... END;' is the only way to write a
               ;; trigger at all, and a splitter that stops at the first `;'
               ;; inside cuts a working trigger in half.
               (let* ((text (string-append
                             "CREATE TRIGGER touch AFTER INSERT ON users\n"
                             "BEGIN\n"
                             "  UPDATE users SET n = 1;\n"
                             "  DELETE FROM tmp;\n"
                             "END;\n"
                             "SELECT 1;"))
                      (statements (sqls text)))
                 (assert-length #:expect 2 #:got statements
                                #:name "the trigger and the select")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements)
                                                "DELETE FROM tmp;")
                               #:name "the whole body is in the first")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "END")
                               #:name "including its END")))
         (test "an `END IF' inside the body does not close it"
               #:shuffle? #f
               ;; Those ENDs belong to statements inside the body that never
               ;; opened a block of their own.
               (let* ((text (string-append
                             "CREATE PROCEDURE p()\n"
                             "BEGIN\n"
                             "  IF x THEN\n"
                             "    SELECT 1;\n"
                             "  END IF;\n"
                             "  SELECT 2;\n"
                             "END;\n"
                             "SELECT 3;"))
                      (statements (sqls text)))
                 (assert-length #:expect 2 #:got statements
                                #:name "the routine and the select")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "SELECT 2;")
                               #:name "the body after the END IF is still in it")))
         (test "a statement that is not a CREATE is left exactly as it is"
               #:shuffle? #f
               ;; A migration that writes its own BEGIN/COMMIT around its work
               ;; must not have its statements merged by the splitter.
               (assert-equal #:expect '("BEGIN" "SELECT 1" "COMMIT")
                             #:got (sqls "BEGIN;\nSELECT 1;\nCOMMIT;")
                             #:name "three statements, not one"))))

;;; ---------------------------------------------------------------------------
;;; DELIMITER
;;; ---------------------------------------------------------------------------

(define delimiter-suite
  (suite "the mysql client's own directive"
         #:concurrent? #f
         (test "the directive retargets the splitter"
               #:shuffle? #f
               ;; It has to be applied a line at a time, *before* lexing,
               ;; because the semicolons it protects would otherwise already
               ;; have been read as ends of statements.
               (let* ((text (string-append
                             "DELIMITER $$\n"
                             "CREATE TRIGGER t AFTER INSERT ON u\n"
                             "BEGIN\n"
                             "  SELECT 1;\n"
                             "END$$\n"
                             "DELIMITER ;\n"
                             "SELECT 2;"))
                      (statements (sqls text)))
                 (assert-length #:expect 2 #:got statements
                                #:name "the trigger and the select")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "SELECT 1;")
                               #:name "the body survived the new delimiter")
                 (assert-equal #:expect #f
                               #:got (contains? (car statements) "DELIMITER")
                               #:name "and the directive is not sent anywhere")
                 (assert-equal #:expect #f
                               #:got (contains? (cadr statements) "DELIMITER")
                               #:name "nor is the one that restored it")))
         (test "a DELIMITER line is recognised, and only when it is one"
               #:shuffle? #f
               (assert-equal #:expect (cons 'delimiter "$$")
                             #:got (delimiter-directive? "DELIMITER $$")
                             #:name "the delimiter is reported")
               (assert-equal #:expect (cons 'delimiter "//")
                             #:got (delimiter-directive? "delimiter //")
                             #:name "in any case")
               (assert-equal #:expect #f
                             #:got (delimiter-directive? "DELIMITERS $$")
                             #:name "a word that only starts with it is not it")
               (assert-equal #:expect #f
                             #:got (delimiter-directive? "DELIMITER")
                             #:name "nor is one with nothing to set")
               (assert-equal #:expect #f
                             #:got (delimiter-directive? "DELIMITER $$ extra")
                             #:name "nor one asking for two things")
               (assert-equal #:expect #f
                             #:got (delimiter-directive? "-- DELIMITER $$")
                             #:name "nor a commented-out one"))))

;;; ---------------------------------------------------------------------------
;;; The entry points
;;; ---------------------------------------------------------------------------

(define dollar-suite
  (suite "dollar quoting"
         #:concurrent? #f
         (test "a function body written with `$$' is one statement"
               #:shuffle? #f
               ;; The only sane way to write a PostgreSQL function whose body
               ;; holds semicolons.
               (let* ((text (string-append
                             "CREATE FUNCTION f() RETURNS int AS $$\n"
                             "BEGIN\n"
                             "  RETURN 1;\n"
                             "END;\n"
                             "$$ LANGUAGE plpgsql;\n"
                             "SELECT 1;"))
                      (statements (sqls text)))
                 (assert-length #:expect 2 #:got statements
                                #:name "the function and the select")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "RETURN 1;")
                               #:name "the body is in the first statement")))
         (test "a tag makes the quoting explicit, and can appear in the body"
               #:shuffle? #f
               ;; `$body$' ... `$body$' is the form that lets the body contain a
               ;; bare `$$', which is why a tag is worth having at all.
               (let* ((text (string-append
                             "CREATE FUNCTION f() RETURNS int AS $body$\n"
                             "  SELECT 1; -- $$ is just text in here\n"
                             "$body$ LANGUAGE sql;\n"
                             "SELECT 2;"))
                      (statements (sqls text)))
                 (assert-length #:expect 2 #:got statements
                                #:name "the function and the select")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "$$ is just text")
                               #:name "and the `$$' inside is data")))
         (test "a positional parameter is not a tag"
               #:shuffle? #f
               ;; `$1' is a parameter; treating it as a dollar quote would
               ;; swallow the rest of the file.
               (assert-equal #:expect '("SELECT $1, $2" "SELECT 3")
                             #:got (sqls "SELECT $1, $2; SELECT 3;")
                             #:name "the statement still ends at its semicolon"))))

(define entry-points-suite
  (suite "the entry points agree with each other"
         #:concurrent? #f
         (test "the string-in/list-of-strings-out entry point"
               #:shuffle? #f
               (assert-equal #:expect '("SELECT 1" "SELECT 2")
                             #:got (string-split-to-queries "SELECT 1; SELECT 2;")
                             #:name "the simple entry point splits the same way")
               ;; The original text is what runs, comments and all, so the
               ;; statement read here still holds the one the `#' made.
               (let ((statements (string-split-to-queries
                                  "# a; comment\nSELECT 1;"
                                  #:engine mysql)))
                 (assert-length #:expect 1 #:got statements
                                #:name "and knows the engine it was given")
                 (assert-equal #:expect #t
                               #:got (contains? (car statements) "SELECT 1")
                               #:name "with the `#' line read as a comment")))
         (test "an alias resolves to the dialect it names"
               #:shuffle? #f
               ;; `--database mariadb' is the same engine as `--database mysql',
               ;; backslash escapes and all.
               (assert-equal #:expect (sqls "# a; comment\nSELECT 1;" mysql)
                             #:got (sqls "# a; comment\nSELECT 1;" mariadb)
                             #:name "MariaDB lexes as MySQL"))
         (test "the first statement of a file, and the line it is on"
               #:shuffle? #f
               (assert-equal #:expect "SELECT 1"
                             #:got (first-statement "SELECT 1;\nSELECT 2;")
                             #:name "the first statement is the first")
               (assert-equal #:expect 3
                             #:got (statement-line
                                    (cadr (parse-sql-statements
                                           "SELECT 1;\n\nSELECT 2;")))
                             #:name "and it knows which line it starts on"))
         (test "the byte order mark some editors write is not part of the SQL"
               #:shuffle? #f
               ;; A UTF-8 BOM in front of a statement is a parse error on some
               ;; servers, and it is invisible in every editor.
               (assert-equal #:expect "SELECT 1"
                             #:got (first-statement
                                    (string-append (string #\xfeff)
                                                   "SELECT 1;"))
                             #:name "the BOM is dropped"))))

(define (spec)
  (veritas-run #:concurrent? #f
               #:shuffle? #f
               splitting-suite
               comment-suite
               dialect-suite
               body-suite
               dollar-suite
               delimiter-suite
               entry-points-suite))
