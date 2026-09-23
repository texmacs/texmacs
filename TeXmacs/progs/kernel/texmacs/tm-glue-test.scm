
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-glue-test.scm
;; DESCRIPTION : Test suite for the conversions between C++ and Scheme values
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-glue-test)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define nul-string (string #\a (integer->char 0) #\b))

(define (regtest-glue-basic)
  (regression-test-group
   "glue, basic types" "basic"
   :none :none
   (test "scheme dialect" (scheme-dialect) "s7")
   (test "booleans" (list (boolean? (os-macos?)) (boolean? (url-exists? "/")))
         '(#t #t))
   (test "integers" (integer? (texmacs-time)) #t)
   (test "string arguments and results"
         (string-replace "hello world" "world" "there") "hello there")
   (test "strings with NUL characters survive C++"
         (string-length (string-replace nul-string "b" "c")) 3)
   (test "NUL character is kept"
         (char->integer (string-ref (string-replace nul-string "b" "c") 1)) 0)
   (test "UTF-8 strings" (string-length (cork->utf8 (utf8->cork "é"))) 2)
   (test "utf8->cork and back" (cork->utf8 (utf8->cork "déjà vu")) "déjà vu")
   (test "TeXmacs string length counts characters"
         (tmstring-length "a<alpha>b") 3)
   (test "array of strings"
         (let ((l (tmstring-split "a<alpha>b c")))
           (list (list? l) (list-and (map string? l)) (apply string-append l)))
         '(#t #t "a<alpha>b c"))
   (test "C++ predicate on strings"
         (list (string-alpha? "abc") (string-alpha? "ab1")) '(#t #f))
   (test "cpp-string-number?"
         (list (cpp-string-number? "1.5") (cpp-string-number? "x")) '(#t #f))
   (test "string-search-forwards"
         (string-search-forwards "lo" 0 "hello") 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sample-stree '(document (concat "a" (strong "b")) "c"))

(define (regtest-glue-trees)
  (regression-test-group
   "glue, trees" "trees"
   :none :none
   (test "stree->tree and back" (tree->stree (stree->tree sample-stree))
         sample-stree)
   (test "trees are blackboxes" (tree? (stree->tree sample-stree)) #t)
   (test "strees are not trees" (tree? sample-stree) #f)
   (test "tree-label" (tree-label (stree->tree '(strong "b"))) 'strong)
   (test "tree-arity" (tree-arity (stree->tree sample-stree)) 2)
   (test "tree-children"
         (map tree->stree (tree-children (stree->tree '(concat "a" "b"))))
         '("a" "b"))
   (test "equal trees are equal?"
         (equal? (stree->tree sample-stree) (stree->tree sample-stree)) #t)
   (test "different trees are not equal?"
         (equal? (stree->tree "a") (stree->tree "b")) #f)
   (test "tree-eq? tests identity"
         (let ((t (stree->tree sample-stree)))
           (list (tree-eq? t t) (tree-eq? t (tree-copy t))))
         '(#t #f))
   (test "tree-copy is equal" (let ((t (stree->tree sample-stree)))
                                (equal? t (tree-copy t)))
         #t)
   (test "string trees" (tree->string (string->tree "abc")) "abc")
   (test "content to tree" (tree->stree (tm->tree '(strong "x"))) '(strong "x"))
   (test "printing a tree"
         (object->string (stree->tree '(strong "b"))) "<tree <strong|b>>")
   (test "trees in hash tables"
         (let ((h (make-ahash-table)))
           (ahash-set! h (stree->tree "k") 1)
           (ahash-ref h (stree->tree "k")))
         1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Urls, commands and other blackboxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define glue-test-counter 0)

(define (regtest-glue-blackboxes)
  (regression-test-group
   "glue, urls, commands and other blackboxes" "blackboxes"
   :none :none
   (test "url-unix" (url->unix (url-unix "dir" "file.tm")) "dir/file.tm")
   (test "url-append"
         (url->unix (url-append (url-unix "a" "b") (url-unix "c" "d")))
         "a/b/c/d")
   (test "url-head and url-tail"
         (let ((u (url-unix "dir" "file.tm")))
           (list (url->unix (url-head u)) (url->unix (url-tail u))))
         '("dir" "file.tm"))
   (test "urls are blackboxes" (url? (url-unix "a" "")) #t)
   (test "equal urls" (equal? (url-unix "a" "b") (url-unix "a" "b")) #t)
   (test "printing a url" (object->string (url-unix "a" "b")) "<url a/b>")
   (test "url-none" (url-none? (url-none)) #t)
   (test "array of urls"
         (length (array-url-append (url-unix "a" "") (list (url-unix "b" ""))))
         2)
   (test "commands from closures"
         (begin
           (set! glue-test-counter 0)
           (command-eval (object->command (lambda () (set! glue-test-counter
                                                           (+ glue-test-counter 1)))))
           glue-test-counter)
         1)
   (test "commands with arguments"
         (begin
           (set! glue-test-counter 0)
           (command-apply (object->command (lambda (x y)
                                             (set! glue-test-counter (+ x y))))
                          (list 2 3))
           glue-test-counter)
         5)
   (test "doubles"
         (patch-get-author (patch-birth 3.5 #t)) 3.5)
   (test "blackboxes survive garbage collection"
         (let ((l (map (lambda (i) (stree->tree (number->string i))) (iota 100))))
           (gc)
           (map tree->stree (list (car l) (list-ref l 99))))
         '("0" "99"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-tm-glue)
  (let ((n (+ (regtest-glue-basic)
              (regtest-glue-trees)
              (regtest-glue-blackboxes))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-glue: ok\n")))
