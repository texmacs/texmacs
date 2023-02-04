
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml-test.scm
;; DESCRIPTION : Test suite for tmhtml.scm
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml-test)
  (:use (convert html tmhtml) (convert tools sxml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-basic)
  (regression-test-group
   "tmhtml, basic features" "basic"
   tmhtml-root :none
   (test "string" '"hello" '("hello"))
   (test "document, empty" '(document) '())
   (test "document, one string" '(document "aaa") '((h:p "aaa")))
   (test "document, two strings"
	 '(document "aaa" "bbb") '((h:p "aaa") (h:p "bbb")))
   (test "para, empty" '(para) '())
   (test "para, string" '(para "aaa") '("aaa"))
   (test "para, two strings"
	 '(para "aaa" "bbb") '("aaa" (h:br) "bbb"))
   (test "document, para, strings"
	 '(document (para) (para "bbb") (para "ccc" "ddd"))
	 '((h:p) (h:p "bbb") (h:p "ccc" (h:br) "ddd")))
   (test "concat, empty" '(concat) '())
   (test "concat, one string" '(concat "aaa") '("aaa"))
   (test "concat, two strings" '(concat "aaa" "bbb") '("aaabbb"))
   (test "para, concat"
	 '(para (concat "aaa" "bbb") "ccc")
	 '("aaabbb" (h:br) "ccc"))))

(define (regtest-tmhtml-format)
  (regression-test-group
   "tmhtml, format nodes" "format"
   tmhtml-root :none
   (test "line break"
	 '(document (concat "aaa" (line-break) "bbb"))
	 '((h:p "aaabbb")))
   (test "new line"
	 '(document (concat "aaa" (new-line) "bbb"))
	 '((h:p "aaa" (h:br) "bbb")))
   (test "next line"
	 '(document (concat "aaa" (next-line) "bbb"))
	 '((h:p "aaa" (h:br) "bbb")))))

(define (regtest-tmhtml-extra)
  (regression-test-group
   "tmhtml, misc features" "extra"
   tmhtml-root :none
   (test "label" '(label "aaa") '((h:a (@ (id "aaa")))))
   (test "hlink" '(hlink "aaa" "bbb") '((h:a (@ (href "bbb")) "aaa")))
   (test "hlink, concat, em" '(hlink (concat (em "aa") "bb") "cc")
	 '((h:a (@ (href "cc")) (h:em "aa") "bb")))
   (test "specific, LaTeX (ignored)" '(specific "LaTeX" "aaa") '())
   (test "specific, HTML (included)" '(specific "html" "aaa") '("aaa"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Physical markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-with)
  (regression-test-group
   "tmhtml, general 'with' handler" "with"
   tmhtml-root list
   ;; Use font variants as an exemple.
   (test "typewriter" '(with "font-family" "tt" "a") '(h:tt "a"))
   (test "bold" '(with "font-series" "bold" "a") '(h:b "a"))
   (test "italic" '(with "font-shape" "italic" "a") '(h:i "a"))
   (test "composite with" '(with "font-family" "tt" "font-series" "bold" "a")
	 '(h:tt (h:b "a")))))

(define (regtest-tmhtml-font-size)
  (define (result x) (tmhtml-root `(with "font-size" ,x "aaa")))
  (define (expected x) (if x `((h:font (@ (size ,x)) "aaa")) '("aaa")))
  (regression-test-group
   "tmhtml, font size" "font-size"
   result expected
   (test "too small" "0.009" #f)
   (test "size=-4" "0.5" "-4")
   (test "size=-3" "0.6" "-3")
   (test "size=-2" "0.7" "-2")
   (test "size=-1" "0.85" "-1")
   (test "size=0" "1" "0")
   (test "size=+1" "1.2" "+1")
   (test "size=+2" "1.45" "+2")
   (test "size=+3" "1.7" "+3")
   (test "size=+4" "2" "+4")
   (test "size=+5" "4" "+5")
   (test "too big" "5.001" #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple logical markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-logical)
  (regression-test-group
   "tmhtml, logical markup, basic" "logical"
   tmhtml-root list
   ;; Phrase elements
   (test "strong" '(strong "aaa") '(h:strong "aaa"))
   (test "em" '(em "aaa") '(h:em "aaa"))
   (test "dfn" '(dfn "aaa") '(h:dfn "aaa"))
   (test "code*" '(code* "aaa") '(h:code "aaa"))
   (test "samp" '(samp "aaa") '(h:samp "aaa"))
   (test "kbd" '(kbd "aaa") '(h:kbd "aaa"))
   (test "var" '(var "aaa") '(h:var "aaa"))
   (test "abbr" '(abbr "aaa") '(h:abbr "aaa"))
   (test "acronym" '(acronym "aaa") '(h:acronym "aaa"))
   ;; Presentation
   (test "tt" '(tt "aaa") '(h:tt "aaa"))
   ;; Composite
   (test "em, concat" '(em (concat "a" (strong "b")))
	 '(h:em "a" (h:strong "b")))
   (test "with, em" '(with "font-family" "tt" (em "a")) '(h:tt (h:em "a")))
   (test "em, with" '(em (with "font-family" "tt" "a")) '(h:em (h:tt "a")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-section)
  (regression-test-group
   "tmhtml, sectioning markup, basic" "section"
   tmhtml-root :none
   (test "chapter*" '(chapter* "aaa") '((h:h1 "aaa")))
   (test "section*" '(section* "aaa") '((h:h2 "aaa")))
   (test "section*, empty" '(section* "") '((h:h2)))
   (test "subsection*" '(subsection* "aaa") '((h:h3 "aaa")))
   (test "subsubsection*" '(subsubsection* "aaa") '((h:h4 "aaa")))
   (test "paragraph*" '(paragraph* "aaa")
	 '((h:strong (@ (class "paragraph")) "aaa")))
   (test-fails "para (primitive)" '(para "aaa") '((h:h5 "aaa")))
   (test "subparagraph*" '(subparagraph* "aaa")
	 '((h:strong (@ (class "subparagraph")) "aaa")))
   (test "paragraph*, empty" '(paragraph* "")
	 '((h:strong (@ (class "paragraph")))))))

(define (regtest-tmhtml-section-post)
  ;; FIXME: check that numbered sectioning macros have their ids handled
  ;; correctly. Now, this involves the typesetter evaluation.
  (regression-test-group
   "tmhtml, section post-processing" "section-post"
   tmhtml-root :none
   (test "alone" '(concat (section* "a")) '((h:h2 "a")))
   (test "with label before" '(concat (label "l1") (section* "a"))
	 '((h:h2 (@ (id "l1")) "a")))
   (test "with label after" '(concat (section* "a") (label "l1"))
	 '((h:h2 (@ (id "l1")) "a")))
   (test "with label before and after"
	 '(concat (label "l1") (section* "a") (label "l2"))
	 '((h:h2 (@ (id "l1")) (h:a (@ (id "l2"))) "a")))
   (test "with two labels after"
	 '(concat (section* "a") (label "l1") (label "l2"))
	 '((h:h2 (@ (id "l1")) (h:a (@ (id "l2"))) "a")))
   (test "with string" '(concat (section* "a") "b")
	 '((h:h2 "a") "b"))
   (test "with label, string, label"
	 '(concat (section* "a") (label "l1") "s" (label "l2"))
	 '((h:h2 (@ (id "l1")) (h:a (@ (id "l2"))) "a") "s"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-itemize-head)
  (regression-test-group
   "tmhtml, item list heads" "itemize-head"
   tmhtml-root :none
   (test "itemize" '(itemize (document (item))) '((h:ul (h:li))))
   (test "enumerate" '(enumerate (document (item))) '((h:ol (h:li))))))

(define (regtest-tmhtml-list-document)
  (regression-test-group
   "tmhtml, item lists content" "list-document"
   tmhtml-root :none
   (test "empty list" '(itemize (document)) '((h:ul (h:li))))
   (test "empty item" '(itemize (document (item))) '((h:ul (h:li))))
   (test "item with text" '(itemize (document (concat (item) "a")))
	 '((h:ul (h:li "a"))))
   (test "two items" '(itemize (document (item) (item)))
	 '((h:ul (h:li) (h:li))))
   (test "text only" '(itemize (document "a")) '((h:ul (h:li "a"))))
   (test "missing first item" '(itemize (document "a" (item)))
	 '((h:ul (h:li "a") (h:li))))
   (test "multipar item" '(itemize (document (concat (item) "a") "b"))
	 '((h:ul (h:li (h:p "a") (h:p "b")))))
   (test "multipar item, item"
	 '(itemize (document (concat (item) "a") "b" (item)))
	 '((h:ul (h:li (h:p "a") (h:p "b")) (h:li))))
   (test "multiconcat item"
	 '(itemize (document (para (concat (item) "a") "b")))
	 '((h:ul (h:li "a" (h:br) "b"))))
   (test "multipar2 item"
	 '(itemize (document (para (concat (item) "a") "b") "c"))
	 '((h:ul (h:li (h:p "a" (h:br) "b") (h:p "c")))))
   (test "item, multipar item"
	 '(itemize (document (item) (concat (item) "a") "b"))
	 '((h:ul (h:li) (h:li (h:p "a") (h:p "b")))))
   (test "item, multiconcat item"
	 '(itemize (document (item) (para (concat (item) "a") "b")))
	 '((h:ul (h:li) (h:li "a" (h:br) "b"))))
   (test "item, multipar2 item"
	 '(itemize (document (item) (para (concat (item) "a") "b") "c"))
	 '((h:ul (h:li) (h:li (h:p "a" (h:br) "b") (h:p "c")))))))

(define (regtest-tmhtml-description-head)
  (regression-test-group
   "tmhtml, description heads" "description-head"
   tmhtml-root :none
   (test "description" '(description (document (item* "a")))
	 '((h:dl (h:dt "a"))))))

(define (regtest-tmhtml-description-content)
  (regression-test-group
   "tmhtml, description content" "description-content"
   ;; description conversion uses the same iterator as item list
   ;; conversions, so the extensive tests for detection of the list
   ;; mark in different document structures need not be redone here.
   tmhtml-root :none
   (test "empty" '(description (document)) '((h:dl (h:dd))))
   (test "string" '(description (document "a")) '((h:dl (h:dd "a"))))
   (test "empty item*" '(description (document (item* ""))) '((h:dl (h:dt))))
   (test "item*/string" '(description (document (item* "a")))
	 '((h:dl (h:dt "a"))))
   (test "item*, string" '(description (document (concat (item* "") "a")))
	 '((h:dl (h:dt) (h:dd "a"))))
   (test "item*, item*" '(description (document (item* "a") (item* "b")))
	 '((h:dl (h:dt "a") (h:dt "b"))))
   (test "string, string" '(description (document "a" "b"))
	 '((h:dl (h:dd (h:p "a") (h:p "b")))))
   (test "string, item*, string"
	 '(description (document "a" (concat (item* "b") "c")))
	 '((h:dl (h:dd "a") (h:dt "b") (h:dd "c"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-verbatim)
  (define (h:tt . content) `(h:tt (@ (class "verbatim")) ,@content))
  (define (h:pre . content)
    `(h:pre (@ (class "verbatim") (xml:space "preserve")) ,@content))
  (regression-test-group
   "tmhtml, verbatim" "verbatim"
   tmhtml-root :none
   (test "inline verbatim"
         '(concat (verbatim "a") "b c") `(,(h:tt "a") "b c"))
   (test "one-line block verbatim"
         '(verbatim (document "  a  b  ")) `(,(h:pre "  a  b  ")))
   (test "multiline block verbatim"
         '(verbatim (document "  a" "   b")) `(,(h:pre "  a\n   b")))
   (test "verbatim line"
         '(document "a" (verbatim "  b ") "c d")
         `((h:p "a") ,(h:pre "  b ") (h:p "c d")))
   ;; (test "verbatim in item*"
   ;;   '(description (document (concat (item* (verbatim "a")) "b")
   ;;     (item* (verbatim "c"))
   ;;     "d e"))
   ;;   `((h:dl (h:dt ,(h:tt "a")) (h:dd "b")
   ;;   (h:dt ,(h:tt "c")) (h:dd (h:p) (h:p "d e")))))
   (test "vicious nesting"
         '(strong (verbatim (em (document "a" "b"))))
         `((h:strong ,(h:pre '(h:em "a\nb")))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The first table conversion uses refactored tmtex conversion code
;; and is pretty limited.

(define-macro (regtest-html-table-tools)
  ;; basic shorthands for html output
  `(begin
     (define (h:td x) `(h:td ,x))
     (define (h:tr l) `(h:tr ,@(map h:td l)))
     (define (h:align x)
       `(align ,(cadr (assoc x '(("l" "left") ("r" "right") ("c" "center"))))))
     (define (h:col x) `(h:col (@ ,@x)))
     (define (h:table b c ll)
       `(h:table ,@(if b '((@ (border "1"))) '())
		  ,@(map h:col (map list (map h:align c)))
		  (h:tbody ,@(map h:tr ll))))))

(define (regtest-tmhtml-table)
  (regtest-table-library)
  (regtest-html-table-tools)
  (define (simple-table) (table '(("a"))))
  (define (simple-tformat) (tformat '() '(("a"))))
  (define (expect-simple b c) (list (h:table b c '(("a")))))
  (regression-test-group
   "tmhtml, table conversion" "table"
   tmhtml-root :none
   (test "naked table" (simple-table) (expect-simple #f '("l")))
   (test "naked tformat" (simple-tformat) (expect-simple #f '("l")))
   (test "simple tabular" `(tabular ,(simple-tformat))
	 (expect-simple #f '("l")))
   (test "simple tabular*"
	 `(tabular* ,(simple-tformat)) (expect-simple #f '("c")))
   (test "simple block"
	 `(block ,(simple-tformat)) (expect-simple #t '("l")))
   (test "simple block*"
	 `(block* ,(simple-tformat)) (expect-simple #t '("c")))
   (test "tabular*, two cells" `(tabular* ,(tformat '() '(("a" "b"))))
	 (list (h:table #f '("c" "c") '(("a" "b")))))
   (test "tabular*, four cells"
	 `(tabular* ,(tformat '() '(("a" "b") ("c" "d"))))
	 (list (h:table #f '("c" "c") '(("a" "b") ("c" "d")))))
   (test "tabular*, first col aligned right"
	 `(tabular* ,(tformat (list (colwith "1" "cell-halign" "r"))
			      '(("a" "b") ("c" "d"))))
	 (list (h:table #f '("r" "c") '(("a" "b") ("c" "d")))))
   (test "tabular*, whole table aligned right"
	 `(tabular* ,(tformat (list (allwith "cell-halign" "r"))
			      '(("a" "b") ("c" "d"))))
	 (list (h:table #f '("r" "r") '(("a" "b") ("c" "d")))))
   (test "tabular*, one row border"
	 `(tabular* ,(tformat (list (rowwith "1" "cell-halign" "r"))
			      '(("a" "b") ("c" "d"))))
	 (list (h:table #f '("c" "c") '(("a" "b") ("c" "d")))))
   (test "tabular*, one col border"
	 `(tabular* ,(tformat (list (colwith "1" "cell-bborder" "1ln"))
			      '(("a" "b") ("c" "d"))))
	 (list (h:table #t '("c" "c") '(("a" "b") ("c" "d")))))))

(define (regtest-tmhtml-table-post)
  (regtest-table-library)
  (regtest-html-table-tools)
  (define (simple-table) `(tabular ,(tformat '() '(("a")))))
  (define (simple-h:table) (h:table #f '("l") '(("a"))))
  (define (simple-h:table-id id) (sxml-set-attr (simple-h:table) `(id ,id)))
  (regression-test-group
   "tmhtml, table post-processing" "table-post"
   tmhtml-root :none
   (test "table, label"
	 `(concat ,(simple-table) (label "l"))
	 (list (simple-h:table-id "l")))
   (test "label, table"
	 `(concat  (label "l") ,(simple-table))
	 (list (simple-h:table-id "l")))
   (test "table, label1, label2"
	 `(concat ,(simple-table) (label "l1") (label "l2"))
	 `(,(simple-h:table-id "l1") (h:a (@ (id "l2")))))
   (test "label1, table, label2"
	 `(concat (label "l1") ,(simple-table) (label "l2"))
	 `((h:a (@ (id "l1"))) ,(simple-h:table-id "l2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-picture)
  (define (make-result l) (tmhtml-root `(image ,@l "" "")))
  (define (make-expected l) (if (null? l) '() (apply make-expected-sub l)))
  (define (make-expected-sub f w h)
    `((h:img (@ (class "image") (src ,f)
		,@(if w `((width ,w)) '())
		,@(if h `((height ,h)) '())))))
  (regression-test-group
   "tmhtml, pictures" "image"
   make-result make-expected
   (test "simple link" '("foo.png" "" "") '("foo.png" #f #f))
   ;; (test "inclusion" '((tuple (raw-data "...") "png") "" "") '())
   (test "width percent" '("foo.png" ".666par" "") '("foo.png" "66.6%" #f))
   (test "width absolute" '("foo.png" "128px" "") '("foo.png" "128" #f))
   (test "width bad unit" '("foo.png" "10spc" "") '("foo.png" "80" #f))
   (test "width factor 1" '("foo.png" "*2" "") '("foo.png" #f #f))
   (test "width factor 2" '("foo.png" "/2" "") '("foo.png" #f #f))
   (test "width factor 3" '("foo.png" "*2/3" "") '("foo.png" #f #f))
   (test "height absolute" '("foo.png" "" "64px") '("foo.png" #f "64"))
   (test "height bad unit" '("foo.png" "" "10fn") '("foo.png" #f "160"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Postprocessing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-tmhtml-document-post)
  (regtest-table-library)
  (regtest-html-table-tools)
  (define (simple-table) `(tabular ,(tformat '() '(("a")))))
  (define (simple-h:table) (h:table #f '("l") '(("a"))))
  (regression-test-group
   "tmhtml, document post processing" "document-post"
   tmhtml-root :none
   (test "text, section" '(document "a" (section* "b"))
	 '((h:p "a") (h:h2 "b")))
   (test "section with trail" '(document (concat (section* "a") "b"))
	 '((h:h2 "a") (h:p "b")))
   (test "section with trail, text" '(document (concat (section* "a") "b") "c")
	 '((h:h2 "a") (h:p "b" "c")))
   (test "section with trail, subsection"
	 '(document (concat (section* "a") "b") (subsection* "c"))
	 '((h:h2 "a") (h:p "b") (h:h3 "c")))
   (test "section with trail, list"
	 '(document (concat (section* "a") "b") (itemize (document)))
	 '((h:h2 "a") (h:p "b") (h:ul (h:li))))
   (test "table" `(document ,(simple-table)) (list (simple-h:table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-tmhtml)
  (let ((n (+ (regtest-tmhtml-basic)
	      (regtest-tmhtml-format)
	      (regtest-tmhtml-with)
	      ;; (regtest-tmhtml-font-size)
	      (regtest-tmhtml-extra)
	      (regtest-tmhtml-logical)
	      ;; (regtest-tmhtml-section)
	      ;; (regtest-tmhtml-section-post)
	      ;; (regtest-tmhtml-itemize-head)
	      ;; (regtest-tmhtml-list-document)
	      ;; (regtest-tmhtml-description-head)
	      ;; (regtest-tmhtml-description-content)
	      (regtest-tmhtml-verbatim)
	      ;; (regtest-tmhtml-table)
	      ;; (regtest-tmhtml-table-post)
	       (regtest-tmhtml-picture))))
	      ;; (regtest-tmhtml-document-post))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tmhtml: ok\n")))
