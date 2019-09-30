
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : htmltm-test.scm
;; DESCRIPTION : Test suite for htmltm
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html htmltm-test)
  (:use (convert html htmltm) (convert tools xmltm)
        (convert tools sxml) (convert tools sxhtml)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sxml-postorder t proc)
  (let sub ((t t))
    (cond ((string? t) t)
          ((sxml-top-node? t) (sxml-set-content t (map sub (sxml-content t))))
          ((sxml-control-node? t) t)
          (else (proc (sxml-set-content t (map sub (sxml-content t))))))))

(define (shtml->sxhtml t)
  (sxml-postorder t (cut sxml-set-ns-prefix "h" <>)))

(define (shtml->stm t)
  (htmltm-as-serial `(*TOP* ,(shtml->sxhtml t))))

(define (html->stm s)
  (htmltm-as-serial (htmltm-parse s)))

(define (regtest-htmltm-grouping)
  (regression-test-group
   "htmltm, grouping markup" "grouping"
   shtml->stm :none
   (test "div" '(div "a") '(document "a"))
   (test "span" '(span "a") "a")))

(define (regtest-htmltm-headings)
  (regression-test-group
   "htmltm, heading markup" "headings"
   shtml->stm :none
   (test "h1" '(h1 "a") '(document (chapter* "a")))
   (test "h2" '(h2 "a") '(document (section* "a")))
   (test "h3" '(h3 "a") '(document (subsection* "a")))
   (test "h4" '(h4 "a") '(document (subsubsection* "a")))
   (test "h5" '(h5 "a") '(document (paragraph* "a")))
   (test "h6" '(h6 "a") '(document (subparagraph* "a")))))

(define (regtest-htmltm-address-bdo)
  (regression-test-group
   "htmltm, address and text direction" "address-bdo"
   shtml->stm :none
   (test "address" '(address "a") '(document "a"))
   (test "bdo" '(bdo "a") "a")))

(define (regtest-htmltm-phrase)
  (regression-test-group
   "htmltm, phrase markup" "phrase"
   shtml->stm :none
   (test "em" '(em "a") '(em "a"))
   (test "strong" '(strong "a") '(strong "a"))
   (test "cite" '(cite "a") '(cite* "a"))
   (test "dfn" '(dfn "a") '(dfn "a"))
   (test "code" '(code "a") '(code* "a"))
   (test "samp" '(samp "a") '(samp "a"))
   (test "kbd" '(kbd "a") '(kbd "a"))
   (test "var" '(var "a") '(var "a"))
   (test "abbr" '(abbr "a") '(abbr "a"))
   (test "acronym" '(acronym "a") '(acronym "a"))))

(define (regtest-htmltm-quotation)
  (regression-test-group
   "htmltm, quotation markup" "quotation"
   shtml->stm :none
   (test "blockquote, empty"
         '(blockquote) '(document (quotation (document ""))))
   (test "blockquote, one para"
         '(blockquote "a") '(document (quotation (document "a"))))
   (test "blockquote, two paras"
         '(blockquote "a" (p "b")) '(document (quotation (document "a" "b"))))
   (test "inline quote" '(q "a")  "``a''")))

(define (regtest-htmltm-subsuper)
  (regression-test-group
   "htmltm, subscripts and superscripts" "subsuper"
   shtml->stm :none
   (test "superscript" '(sup "a") '(rsup "a"))
   (test "subscript" '(sub "a") '(rsub "a"))))

(define (regtest-htmltm-para)
  (+ (regtest-htmltm-para-normal) (regtest-htmltm-preformatted)))

(define (regtest-htmltm-para-normal)
  (regression-test-group
   "htmltm, lines and paragraphs" "para"
   shtml->stm :none
   (test "p, empty" '(p) '(document ""))
   (test "two p, empty" '(html (p) (p)) '(document "" ""))
   (test "p, text" '(p "a") '(document "a"))
   (test "p text, p empty, p text"
         '(html (p "a") (p) (p "b")) '(document "a" "" "b"))
   (test "br" '(br) '(next-line))
   (test "br in p"
         '(p "a" (br) "b") '(document (concat "a" (next-line) "b")))))

(define (regtest-htmltm-preformatted)
  (define (code l) `(document (code ,@l)))
  (regression-test-group
   "htmltm, preformatted text" "pre"
   shtml->stm code
   (test "empty" '(pre) '(""))
   (test "emtpy, newlines" '(pre "\n\n") '(""))
   (test "one line" '(pre "hello") '("hello"))
   (test "one line, whitespace" '(pre " hello ") '(" hello "))
   (test "one line, newlines" '(pre "\n hello \n") '(" hello "))
   (test "two lines" '(pre "hello\nworld") '((document "hello" "world")))
   (test "two lines, whitespace"
         '(pre " hello \n world ") '((document " hello " " world ")))
   (test "two lines, newlines"
         '(pre "\n hello \n world \n") '((document " hello " " world ")))
   (test "two lines, newlines, trailing white line (non-standard)"
         '(pre "\n hello \n world \n ") '((document " hello " " world ")))
   (test "inline tags"
         '(pre (em " hello ")) '((em " hello ")))
   (test "inline tags, newlines"
         '(pre "\n" (em " hello ") "\n") '((em " hello ")))))

(define (regtest-htmltm-changes)
  (regression-test-group
   "htmltm, document changes" "changes"
   shtml->stm :none
   ))

(define (regtest-htmltm-list-items)
  (define (make-itemize l)
    `(document (itemize (document ,@l))))
  (regression-test-group
   "htmltm, lists items" "list-items"
   shtml->stm make-itemize
   (test "empty list" '(ul) '(""))
   (test "one empty item" '(ul (li)) '((item)))
   (test "two empty items" '(ul (li) (li)) '((item) (item)))
   (test "one item inline"
         '(ul (li "a")) '((concat (item) "a")))
   (test "one item, id, inline"
         '(ul (li (@ (id "x")) "a")) '((concat (item) (label "x") "a")))
   (test "one item one p"
         '(ul (li (p "a"))) '((concat (item) "a")))
   (test "one item two p"
         '(ul (li (p "a") (p "b"))) '((concat (item) "a") "b"))
   (test "one item, inline and p"
         '(ul (li "a" (p "b"))) '((concat (item) "a") "b"))
   (test "one item, inline and p with structure"
         '(ul (li "a" (em "b") (p "c")))
         '((concat (item) "a" (em "b")) "c"))
   (test "two items, inline"
         '(ul (li "a") (li "b")) '((concat (item) "a") (concat (item) "b")))
   (test "two items, blocks"
         '(ul (li (p "a")) (li (p "b")))
         '((concat (item) "a") (concat (item) "b")))))

(define (regtest-htmltm-list-definition)
  (define (make-description l)
    `(document (description (document ,@l))))
  (regression-test-group
   "htmltm, definition lists" "list-definition"
   shtml->stm make-description
   (test "empty list" '(dl) '(""))
   (test "empty dt" '(dl (dt)) '((item* "")))
   (test "emtpy dd" '(dl (dd)) '(""))
   (test "empty dt, dd" '(dl (dt) (dd)) '((item* "")))
   (test "empty dd, dt" '(dl (dd) (dt)) '("" (item* "")))
   (test "empty dt, dt, dd, dd"
         '(dl (dt) (dt) (dd) (dd)) '((item* "") (item* "") ""))
   (test "dt, dt, dd, dd with text"
         '(dl (dt "a") (dt "b") (dd "c") (dd "d"))
         '((item* "a") (concat (item* "b") "c") "d"))))

(define (regtest-htmltm-list-kinds)
  (regression-test-group
   "htmltm, other kinds of lists" "list-kinds"
   shtml->stm :none
   (test "enumeration" '(ol (li "a"))
         '(document (enumerate (document (concat (item) "a")))))))

(define (regtest-htmltm-list-br)
  (define nl '(next-line))
  (define (make-itemize l)
    `(document (itemize (document ,@l))))
  (regression-test-group
   "htmltm, ignore BR in lists (invalid structure)" "list-br"
   shtml->stm make-itemize
   (test "ul/br" '(ul (br)) '(""))
   (test "ul/li/br" '(ul (li (br))) `((concat (item) ,nl)))
   ;; TODO: xpath test suite and remove the following tests
   (test "html/ul/br" '(html (ul (br))) '(""))
   (test "html/ul/li/br" '(html (ul (li (br)))) `((concat (item) ,nl)))))

(define (regtest-htmltm-lists)
  (+ (regtest-htmltm-list-items)
     (regtest-htmltm-list-definition)
     (regtest-htmltm-list-kinds)
     (regtest-htmltm-list-br)))

(define (define-self-evaluating* syms)
  (map (lambda (name)
         (let ((sym (gensym)))
           `(define (,name . ,sym) (cons (quote ,name) ,sym))))
       syms))

(define-macro (regtest-html-table-library . body)
  `(begin
     ,@(define-self-evaluating* '(tbody thead tfoot tr td th col colgroup))
     ,@(map (lambda (name)
              `(define ,name (quote (td ,(symbol->string name)))))
            '(A B C D E F G H I J K L))
     (begin ,@body)))

(define (regtest-htmltm-table-correction)
  (regtest-html-table-library
   (define table-1x1 (list (tbody (tr A))))
   (define table-1x2 (list (tbody (tr A B))))
   (define table-2x2 (list (tbody (tr A B) (tr C D))))
   (define table-2x1x2 (list (tbody (tr A B)) (tbody (tr C D))))
   (define (table-shtml->sxhtml x) (shtml->sxhtml (cons 'table x)))
   (regression-test-group
    "htmltm, table correction" "table-correct"
    (compose sxhtml-correct-table table-shtml->sxhtml) table-shtml->sxhtml
    (test "empty table" (list) (list))
    (test "1x1, correct" table-1x1 table-1x1)
    (test "1x1, w/o tbody" (list (tr A)) table-1x1)
    (test "1x1, w/o tbody, tr" (list A) table-1x1)
    (test "1x1, w/o tr" (list (tbody A)) table-1x1)
    (test "1x2, correct" table-1x2 table-1x2)
    (test "1x2, w/o tbody" (list A B) table-1x2)
    (test "1x2, w/o tr" (list (tbody A B)) table-1x2)
    (test "1x2, w/o tbody, tr" (list A B) table-1x2)
    (test "2x2, correct" table-2x2 table-2x2)
    (test "2x2, w/o first tr" (list (tbody A B (tr C D))) table-2x2)
    (test "2x2, w/o last tr" (list (tbody (tr A B) C D)) table-2x2)
    (test "2x2, w/o tbody" (list (tr A B) (tr C D)) table-2x2)
    (test "2x2, w/o tbody, first tr" (list A B (tr C D)) table-2x2)
    (test "2x2, w/o tbody, last tr" (list (tr A B) C D) table-2x2)
    (test "2x1x2, w/o first tbody"
          (list (tr A B) (tbody (tr C D))) table-2x1x2)
    (test "full table"
          (list "  " (col) '(foo) " " (colgroup (col))
                (thead (tr A " " B) (td) '(bar) (td))
                (tfoot '(baar) C D) " "
                (tr (th "e") " " (td "f") '(baz) " ")
                " " (th "g") " " (td "h") " "
                (tbody " " '(spam) (tr (th "i") (td "j"))
                       (tr " " (th "k") (td "l")) " ")
                " " (th "m") (td "n") '(eggs) (tr " " (th "o") (td "p")) " ")
          (list (col) (colgroup (col))
                (thead (tr A B) (tr (td) (td)))
                (tfoot (tr C D))
                (tbody (tr (th "e") (td "f")) (tr (th "g") (td "h")))
                (tbody (tr (th "i") (td "j")) (tr (th "k") (td "l")))
                (tbody (tr (th "m") (td "n")) (tr (th "o") (td "p")))))
    (test "nested tables"
          (list (tr (td `(table ,A ,B)) C)
                (tr D E))
          (list (tbody (tr (td `(table ,(tbody (tr A B)))) C)
                       (tr D E)))))))

(define (regtest-htmltm-table-atts)
  (regtest-html-table-library
   (regtest-table-library)
   (define (table-shtml->stm x) (shtml->stm (cons 'table x)))
   (define (document-tabular x) `(document (tabular ,x)))
   (define default-formats '((twith "table-width" "1par")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")))
   (define (html-content) (tbody (tr A B) (tr C D)))
   (define tm-content '(("A" "B") ("C" "D")))
   (define (frame-box n)
     (let ((v (string-append (number->string n) "px")))
       `((twith "table-rborder" ,v) (twith "table-lborder" ,v)
         (twith "table-bborder" ,v) (twith "table-tborder" ,v))))
   (define (frame-hsides n)
     (let ((v (string-append (number->string n) "px")))
       `((twith "table-bborder" ,v) (twith "table-tborder" ,v))))
   (define rules-all
     '((cwith "1" "-1" "1" "-2" "cell-rborder" "1px")
       (cwith "1" "-2" "1" "-1" "cell-bborder" "1px")))
   (+ (regression-test-group
       "htmltm, table outer attributes" "table-atts-outer"
       table-shtml->stm :none
       (test "label" (list '(@ (id "foo")) (html-content))
             `(document (concat (tabular ,(tformat default-formats tm-content))
                                (label "foo"))))
       (test "align=center" (list '(@ (align "center")) (html-content))
             `(document (with "par-mode" "center"
                          (tabular ,(tformat default-formats tm-content)))))
       (test "align=CeNtEr" (list '(@ (align "CeNtEr")) (html-content))
             `(document (with "par-mode" "center"
                          (tabular ,(tformat default-formats tm-content)))))
       (test "align=foo" (list '(@ (align "foo")) (html-content))
             `(document (tabular ,(tformat default-formats tm-content))))
       (test "label, align=right"
             (list '(@ (id "foo") (align "RIGHT")) (html-content))
             `(document (with "par-mode" "right"
                          (concat
                           (tabular ,(tformat default-formats tm-content))
                           (label "foo")))))
       (test "width=100%"
             (list '(@ (width "100%")) (html-content))
             `(document (tabular
                         ,(tformat default-formats tm-content))))
       (test "width= 100 %"
             (list '(@ (width " 100 % ")) (html-content))
             `(document (tabular
                         ,(tformat default-formats tm-content))))
       (test "width=75%"
             (list '(@ (width "75%")) (html-content))
             `(document (tabular
                         ,(tformat
                           '((twith "table-width" "0.75par")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t"))
                           tm-content))))
       (test "width=42"
             (list '(@ (width "42")) (html-content))
             `(document (tabular
                         ,(tformat
                           '((twith "table-width" "42px")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t"))
                           tm-content)))))
      (regression-test-group
       "htmltm, table inner attributes" "table-atts-inner"
       table-shtml->stm document-tabular
       (test "border" (list '(@ (border)) (html-content))
             (tformat `(,@default-formats ,@(frame-box 1) ,@rules-all)
                      tm-content))
       (test "border=2" (list '(@ (border "2")) (html-content))
             (tformat `(,@default-formats ,@(frame-box 2) ,@rules-all)
                      tm-content))
       (test "border=foo" (list '(@ (border "foo")) (html-content))
             (tformat `(,@default-formats ,@(frame-box 1) ,@rules-all)
                      tm-content))
       (test "border=-1" (list '(@ (border "-1")) (html-content))
             (tformat default-formats tm-content))
       (test "frame=hsides" (list '(@ (frame "hsides")) (html-content))
             (tformat `(,@default-formats ,@(frame-hsides 1)) tm-content))
       (test "frame" (list '(@ (frame)) (html-content))
             (tformat `(,@default-formats ,@(frame-box 1)) tm-content))
       (test "frame=foo"  (list '(@ (frame "foo")) (html-content))
             (tformat `(,@default-formats ,@(frame-box 1)) tm-content))
       (test "rules" (list '(@ (rules)) (html-content))
             (tformat `(,@default-formats ,@rules-all) tm-content))
       (test "rules=rows" (list '(@ (rules "rows")) (html-content))
             (tformat `(,@default-formats
                         (cwith "1" "-2" "1" "-1" "cell-bborder" "1px"))
                      tm-content))
       (test "rules=foo" (list '(@ (rules "foo")) (html-content))
             (tformat `(,@default-formats ,@rules-all) tm-content))
       (test "frame=hsides rules=rows"
             (list '(@ (frame "hsides") (rules "rows")) (html-content))
             (tformat `(,@default-formats ,@(frame-hsides 1)
                         (cwith "1" "-2" "1" "-1" "cell-bborder" "1px"))
                      tm-content))
       (test "border=2 frame=hsides"
             (list '(@ (border "2") (frame "hsides")) (html-content))
             (tformat `(,@default-formats ,@(frame-hsides 2) ,@rules-all)
                      tm-content))
       (test "border=0 frame=hsides"
             (list '(@ (border "0") (frame "hsides")) (html-content))
             (tformat default-formats tm-content))
       (test "border=2 rules=rows"
             (list '(@ (border "2") (rules "rows")) (html-content))
             (tformat `(,@default-formats ,@(frame-box 2)
                         (cwith "1" "-2" "1" "-1" "cell-bborder" "1px"))
                      tm-content))
       (test "border=0 rules=rows"
             (list '(@ (border "0") (rules "rows")) (html-content))
             (tformat `(,@default-formats
                         (cwith "1" "-2" "1" "-1" "cell-bborder" "1px"))
                      tm-content)))
   ;; TODO: rules=groups
   )))

(define (regtest-htmltm-table-content)
  (regtest-html-table-library
   (regtest-table-library)
   (define (table-shtml->stm x) (shtml->stm (cons 'table x)))
   (define (document-tabular x) `(document (tabular ,x)))
   (define default-formats '((twith "table-width" "1par")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")))
   (+ (regression-test-group
       "htmltm, empty table content" "table-content-empty"
       table-shtml->stm :none
       (test "empty table" '() '(document "")))
      (regression-test-group
       "htmltm, table content" "table-content"
       table-shtml->stm document-tabular
       (test "thead, tfoot, tbody"
             (list (thead (tr A B)) (tfoot (tr G H)) (tbody (tr C D) (tr E F)))
             (tformat default-formats '(("A" "B") ("C" "D")
                                        ("E" "F") ("G" "H"))))
       (test "invalid 2xthead, 2xtfoot, 2xtbody"
             (list (thead (tr A B)) (tfoot (tr I J))
                   (thead (tr C D)) (tfoot (tr K L))
                   (tbody (tr E F)) (tbody (tr G H)))
             (tformat default-formats '(("A" "B") ("C" "D") ("E" "F")
                                        ("G" "H") ("I" "J") ("K" "L"))))
       (test "th"
             (list (tbody (tr (th "A") B C) (tr (th "D") E F)))
             (tformat default-formats '(("A" "B" "C") ("D" "E" "F"))))
       (test "cell content"
             (list (tbody (tr (th '(strong "A"))
                              (td `(table ,(tbody (tr B (td '(em "C")))
                                                  (tr D E)))))
                          (tr F G)))
             (tformat default-formats
                      `(((strong "A")
                         ,(document-tabular
                           (tformat default-formats '(("B" (em "C"))
                                                      ("D" "E")))))
                        ("F" "G"))))))))

(define (regtest-htmltm-table-span)
  (regtest-html-table-library
   (regtest-table-library)
   (define (table-shtml->stm x) (shtml->stm (cons 'table x)))
   (define (document-tabular x) `(document (tabular ,x)))
   (define default-formats '((twith "table-width" "1par")
                             (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")))
   (regression-test-group
    "htmltm, spanning cells" "table-span"
    table-shtml->stm document-tabular
    (test "span 1x2"
          (list (tbody (tr (td '(@ (colspan "2")) "A") C) (tr D E F)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "1" "1" "cell-col-span" "2"))
                   '(("A" "" "C") ("D" "E" "F"))))
    (test "missing cells"
          (list (tbody (tr A B C) (tr D)))
          (tformat default-formats '(("A" "B" "C") ("D" "" ""))))
    (test "span 2x1"
          (list (tbody (tr (td '(@ (rowspan "2")) "A") B) (tr D) (tr E F)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "1" "1" "cell-row-span" "2"))
                   '(("A" "B") ("" "D") ("E" "F"))))
    (test "span 2x1, missing cells"
          (list (tbody (tr (td '(@ (rowspan "2")) "A") B) (tr) (tr E)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "1" "1" "cell-row-span" "2"))
                   '(("A" "B") ("" "") ("E" ""))))
    (test "span 2x2"
          (list (tbody (tr (td '(@ (rowspan "2") (colspan "2")) "A") C)
                       (tr F) (tr G H I)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "1" "1" "cell-row-span" "2")
                      (cwith "1" "1" "1" "1" "cell-col-span" "2"))
                   '(("A" "" "C") ("" "" "F") ("G" "H" "I"))))
    (test "span 2x1, span 3x1"
          (list (tbody (tr A (td '(@ (rowspan "3")) "B") C)
                       (tr (td '(@ (rowspan "2")) "D") F)
                       (tr I) (tr J K L)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "2" "2" "cell-row-span" "3")
                      (cwith "2" "2" "1" "1" "cell-row-span" "2"))
                   '(("A" "B" "C") ("D" "" "F")
                     ("" "" "I") ("J" "K" "L"))))
    (test "overlapping 1"
          (list (tbody (tr A (td '(@ (rowspan "2")) "B") C)
                       (tr (td '(@ (colspan "2")) "D") F)
                       (tr G H I)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "2" "2" "cell-row-span" "2")
                      (cwith "2" "2" "1" "1" "cell-col-span" "2"))
                   '(("A" "B" "C") ("D" "" "F") ("G" "H" "I"))))
    (test "overlapping 2"
          (list (tbody (tr A (td '(@ (rowspan "3")) "B") C)
                       (tr (td '(@ (colspan "3")) "D"))
                       (tr G I) (tr J K L)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "2" "2" "cell-row-span" "3")
                      (cwith "2" "2" "1" "1" "cell-col-span" "3"))
                   '(("A" "B" "C") ("D" "" "") ("G" "" "I") ("J" "K" "L"))))
    (test "overlapping 3"
          (list (tbody (tr A (td '(@ (rowspan "2")) "B"))
                       (tr (td '(@ (colspan "2") (rowspan "2")) "C"))
                       (tr ) (tr G H)))
          (tformat `(,@default-formats
                      (cwith "1" "1" "2" "2" "cell-row-span" "2")
                      (cwith "2" "2" "1" "1" "cell-row-span" "2")
                      (cwith "2" "2" "1" "1" "cell-col-span" "2"))
                   '(("A" "B") ("C" "") ("" "") ("G" "H")))))))

(define (regtest-htmltm-table)
  (+ (regtest-htmltm-table-correction)
     (regtest-htmltm-table-atts)
     (regtest-htmltm-table-content)
     (regtest-htmltm-table-span)))

(define (regtest-htmltm-links)
  (regression-test-group
   "htmltm, links" "links"
   shtml->stm :none
   (test "name" '(a (@ (name "foo")) "bar") '(concat (label "foo") "bar"))
   (test "id" '(a (@ (id "foo")) "bar") '(concat (label "foo") "bar"))
   (test "href" '(a (@ (href "foo")) "bar") '(hlink "bar" "foo"))
   (test "name and href" '(a (@ (name "foo") (href "bar")) "baz")
         '(hlink (concat (label "foo") "baz") "bar"))
   (test "id and href" '(a (@ (id "foo") (href "bar")) "baz")
         '(hlink (concat (label "foo") "baz") "bar"))
   (test "id, name and href"
         '(a (@ (id "eggs") (name "foo") (href "bar")) "baz")
         '(hlink (concat (label "eggs") (label "foo") "baz") "bar"))))

(define (regtest-htmltm-objects)
  (define (image s w h) `(image ,s ,w ,h "" ""))
  (regression-test-group
   "htmltm, object images and applets" "objects"
   shtml->stm :none
   (test "img" '(img (@ (src "foo"))) (image "foo" "0.6383w" ""))
   (test "img, px px"
         '(img (@ (src "foo") (width "100") (height "50")))
         (image "foo" "100px" "50px"))
   (test "img, %"
         '(img (@ (src "foo") (width "50%")))
         (image "foo" "1/2par" ""))))

(define (regtest-htmltm-alignement)
  (regression-test-group
   "htmltm, alignement" "alignement"
   shtml->stm :none
   ))

(define (regtest-htmltm-style)
  (regression-test-group
   "htmltm, font style" "style"
   shtml->stm :none
   ))

(define (regtest-htmltm-font)
  (regression-test-group
   "htmltm, font modifiers" "font"
   shtml->stm :none
   ))

(define (regtest-htmltm-rules)
  (regression-test-group
   "htmltm, horizontal rules" "rules"
   shtml->stm :none
   ))

(define (regtest-htmltm-frames)
  (regression-test-group
   "htmltm, frames" "frames"
   shtml->stm :none
   ))

(define (regtest-htmltm-forms)
  (regression-test-group
   "htmltm, forms" "forms"
   shtml->stm :none
   ))

(define (regtest-htmltm-scripting)
  (regression-test-group
   "htmltm, scripting" "scripting"
   shtml->stm :none
   ))

(define (regtest-htmltm-handlers-extra)
  ;; Maybe those tests should be split in actual unit tests. This extra
  ;; handling is so complicated it might well work for a wrong reason.
  (+ (regtest-htmltm-text-cleaning)
     (regtest-htmltm-trimming)
     (regtest-htmltm-ids)))

(define (regtest-htmltm-text-cleaning)
  (regression-test-group
   "htmltm, extra features of handlers" "handlers"
   shtml->stm :none
   ;; TODO: test limitations in cleaning in preformatted text.
   (test "mixed space"
         '(em " a\n  " (strong " b ") "\n ") '(em (concat " a " (strong " b ") " ")))
   (test "text in element content"
         '(ul "foo" (li "hello") "bar" (li "world") "baz")
         '(document (itemize (document (concat (item) "hello")
                                       (concat (item) "world")))))))

(define (regtest-htmltm-trimming)
  (define nl '(next-line))
  (define (id s) `(a (@ (id ,s))))
  (define (make-itemize l) `(document (itemize (document ,@l))))
  (regression-test-group
   ;; TODO: test disabling of trimming in preformatted text.
   "htmltm, trimming converted nodes" "trimming"
   shtml->stm :none
   (test "block after"
         '(div "a " (p "b")) '(document "a" "b"))
   (test "block before"
         '(div (p "a") " b") '(document "a" "b"))
   (test "blocks across"
         '(div "a " (p "b") " c " (p "d") " e")
         '(document "a" "b" "c" "d" "e"))
   (test "white inlines between blocks"
         '(div " " (p "a") " " (p "b") " ")
         '(document "a" "b"))

   (test "block after, label"
         `(div "a " ,(id "x") (p "b"))
         '(document (concat "a" (label "x")) "b"))
   (test "block before, label"
         `(div (p "a") ,(id "x") " b")
         '(document "a" (concat (label "x") "b")))
   (test "blocks across, labels"
         `(div "a " ,(id "x") (p "b")
             ,(id "y") " c " ,(id "z") (p "d") ,(id "t") " e")
         '(document (concat "a" (label "x")) "b"
                    (concat (label "y") "c" (label "z")) "d"
                    (concat (label "t") "e")))
   (test "block after, padded label"
         `(div "a " ,(id "x") " " (p "b"))
         '(document (concat "a" (label "x")) "b"))
   (test "block before, padded label"
         `(div (p "a") " " ,(id "x") " b")
         '(document "a" (concat (label "x") "b")))
   (test "blocks across, padded labels"
         `(div "a " ,(id "x") " " (p "b")
             " " ,(id "y") " c " ,(id "z") " " (p "d") " " ,(id "t") " e")
         '(document (concat "a" (label "x")) "b"
                    (concat (label "y") "c" (label "z")) "d"
                    (concat (label "t") "e")))

   (test "newline after"
         '(p "a " (br)) `(document (concat "a" ,nl)))
   (test "newline before"
         '(p (br) " b") `(document (concat ,nl "b")))
   (test "newlines across"
         '(p "a " (br) " b " (br) " c")
         `(document (concat "a" ,nl "b" ,nl "c")))
   (test "newline in list"
         '(ul (li " hello " (br) " world "))
         (make-itemize `((concat (item) "hello" ,nl "world"))))

   (test "newline after, label"
         `(p "a " ,(id "x") (br))
         `(document (concat "a" (label "x") ,nl)))
   (test "newline before, label"
         `(p (br) ,(id "x") " b") `(document (concat ,nl (label "x") "b")))
   (test "newlines across, labels"
         `(p "a ",(id "x")  (br) ,(id "y") " b " ,(id "z") (br) ,(id "t") " c")
         `(document (concat "a" (label "x") ,nl
                            (label "y") "b" (label "z") ,nl (label "t") "c")))
   (test "newline after, padded label"
         `(p "a " ,(id "x") " " (br))
         `(document (concat "a" (label "x") ,nl)))
   (test "newline before, padded label"
         `(p (br) " " ,(id "x") " b") `(document (concat ,nl (label "x") "b")))
   (test "newlines across, padded labels"
         `(p "a ",(id "x") " " (br)
             " " ,(id "y") " b " ,(id "z") " " (br) " " ,(id "t") " c")
         `(document (concat "a" (label "x") ,nl (label "y") "b" (label "z") ,nl
                            (label "t") "c")))))

(define (regtest-htmltm-ids)
  (regression-test-group
   "htmltm, conversion of id attributes" "ids"
   shtml->stm :none
   ;; TODO: Test deep insertion of labels, and correct behaviour with list
   ;; markers and section titles.
   (test "string" '(em (@ (id "x")) "y") '(em (concat (label "x") "y")))
   (test "concat"
         '(em (@ (id "x")) "y" (strong "z"))
         '(em (concat (label "x") "y" (strong "z"))))
   ;; TODO: test insertion in para when supported"
   (test "document"
         '(p (@ (id "x")) "y") '(document (concat (label "x") "y")))
   ;; TODO: test insertion in explicit expand documents
   (test "with document"
         '(center (@ (id "x")) "y")
         '(document (with "par-mode" "center"
                          (document (concat (label "x") "y")))))
   (test "itemize 1"
         '(ul (@ (id "x")) (li "y"))
         '(document (itemize (document (concat (item) (label "x") "y")))))
   (test "itemize 2"
         '(ul (@ (id "x")) (li (@ (id "y")) "z"))
         '(document (itemize (document
                              (concat (item) (label "x") (label "y") "z")))))
   (test "description"
         '(dl (@ (id "x")) (dt "y") (dd "z"))
         '(document (description
                     (document
                      (concat (item* (concat (label "x") "y")) "z")))))
   (test "section"
         '(center (@ (id "x")) (h1 "y"))
         '(document (with "par-mode" "center"
                          (document (concat (label "x") (chapter* "y"))))))
   (test "section 2"
         '(center (@ (id "x")) (h1 (@ (id "y")) "z"))
         '(document (with "par-mode" "center"
                          (document (concat (label "x")
                                    (chapter* (concat (label "y") "z")))))))
   (test "implicit expand document"
         '(pre (@ (id "x")) "y")
         '(document (code "y")))
   (test "otherwise"
         '(em (@ (id "x")) (strong "y"))
         '(em (concat (label "x") (strong "y"))))))

(define (regtest-htmltm-less-gtr)
  (define html1 "&lt;x&gt;")
  (define stm1 "<less>x<gtr>")
  (define stm-label '(label "<less>y<gtr>"))
  ;; Be sure that angle brackets are escaped only once
  (regression-test-group
   "htmltm, escaping less and gtr" "less-gtr"
   html->stm :none
   (test "string node" html1 stm1)
   (test "tag with id"
         (string-append "<em id='<y>'>" html1 "</em>")
         `(em (concat ,stm-label ,stm1)))
   (test "anchor with id"
         (string-append "<a id='<y>'>" html1 "</a>")
         `(concat ,stm-label ,stm1))
   (test "anchor with name"
         (string-append "<a name='<y>'>" html1 "</a>")
         `(concat ,stm-label ,stm1))
   (test "anchor with href"
         (string-append "<a href='<z>'>" html1 "</a>")
         `(hlink ,stm1 "<less>z<gtr>"))
   (test "image source"
         "<img src='<z>'>"
         '(image "<less>z<gtr>" "0.6383w" "" "" ""))))

;; Utilities for testing url-decoding

(define (map-on-128-in-hex proc)
  ((cut append-map <> (iota 8))
   (lambda (i)
     (map (cut proc i <>) (iota 16)))))

(define (list-cross l1 l2) ;; cross product of two lists
  (append-map
   (lambda (x1)
     (map (lambda (x2)
            (list x1 x2))
          l2))
   l1))

(define (integer->alt-hex i) ;; various hex representations of a 0-15 integer
  (if (< i 10)
      (list (number->string i))
      (map (lambda (s) (char->string (string-ref s (- i 10))))
           '("ABCDEF" "abcdef"))))

(define (url-encoded-all)
  ;; WARNING: Maybe we should not encode characters which are allowed literally
  (string-concatenate
   (map-on-128-in-hex
    (lambda (i j)
      (let ((hex1 (integer->alt-hex i))
            (hex2 (integer->alt-hex j)))
        (let ((hexes (list-cross hex1 hex2)))
          (string-concatenate
           (map (lambda (ss) (apply string-append "%" ss)) hexes))))))))

(define (url-decoded-all)
  (string-concatenate
   (map-on-128-in-hex
    (lambda (i j)
      (let ((hex1 (integer->alt-hex i))
            (hex2 (integer->alt-hex j)))
        ;; Trick! Duplicate i and j to match the length of hex1 and hex2.
        (let ((n1 (map first (list-cross (list i) hex1)))
              (n2 (map first (list-cross (list j) hex2))))
          (let ((numbers (list-cross n1 n2)))
            (string-concatenate
             (map (lambda (is)
                    (let ((tm (utf8->cork
                               (char->string
                                (integer->char
                                 (+ (* 16 (first is)) (second is)))))))
                      (if (== tm (char->string (integer->char 0)))
                          "`" tm)))
                  numbers)))))))))

(define (regtest-htmltm-backquote)
  ;; In 1.0.1.20, the font handling is inadequate.
  ;;
  ;; The GRAVE ACCENT (U+0060, Cork #00) in visible HTML text must be converted
  ;; to LEFT SINGLE QUOTATION MARK (U+201C, Cork #60) to be displayed as
  ;; intended by many HTML editors.
  ;;
  ;; In addition, URL attributes must be decoded when imported to TeXmacs.
  ;;
  (regression-test-group
   "htmltm, backquote" "backquote"
   shtml->stm :none
   ;; NOTE: "`" in source is GRAVE, but in result it is LEFT SINGLE QUOTATION
   (test "text node" "`hello' `world'" "`hello' `world'")
   (test "element" '(em "`hello`") '(em "`hello`"))
   (test "href element"
         '(a (@ (href "%60hello%20world%60")) "`how are you?`")
         '(hlink "`how are you?`" "%60hello%20world%60"))
   (test "img element"
         '(img (@ (src "%60hello%60")))
         '(image "%60hello%60" "0.6383w" "" "" ""))
   ;; No test for htmltm-with-color of 1.0.1.16, since it is mostly broken.
   (test "anchor element"
         '(a (@ (name "%60hello%60")) "`world`")
         '(concat (label "%60hello%60") "`world`"))
   (test "id attribute"
         '(em (@ (id "%60hello%60")) "`world`")
         '(em (concat (label "%60hello%60") "`world`")))
   ;; (test "map url decoder"
   ;;      `(a (@ (href ,(url-encoded-all))) "x")
   ;;      `(hlink "x" ,(url-decoded-all)))
   (test "url encoding error 1"
         '(a (@ (href "%")) "x") '(hlink "x" "%"))
   (test "url encoding error 2"
         '(a (@ (href "%z")) "x") '(hlink "x" "%z"))
   (test "url encoding error 3"
         '(a (@ (href "%6")) "x") '(hlink "x" "%6"))
   (test "url encoding error 4"
         '(a (@ (href "%6z")) "x") '(hlink "x" "%6z"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test suite
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-htmltm)
  (let ((n (+ (regtest-htmltm-grouping)
              (regtest-htmltm-headings)
              (regtest-htmltm-address-bdo)
              (regtest-htmltm-phrase)
              (regtest-htmltm-quotation)
              (regtest-htmltm-subsuper)
              (regtest-htmltm-para)
              (regtest-htmltm-changes)
              (regtest-htmltm-lists)
              ;(regtest-htmltm-table)
              (regtest-htmltm-links)
              (regtest-htmltm-objects)
              (regtest-htmltm-alignement)
              (regtest-htmltm-style)
              (regtest-htmltm-font)
              (regtest-htmltm-rules)
              (regtest-htmltm-frames)
              (regtest-htmltm-forms)
              (regtest-htmltm-scripting)
              (regtest-htmltm-handlers-extra)
              (regtest-htmltm-less-gtr)
              (regtest-htmltm-backquote))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of htmltm: ok\n")))
