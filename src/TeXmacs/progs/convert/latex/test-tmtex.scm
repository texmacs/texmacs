
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : test-tmtex.scm
;; DESCRIPTION : Test suite for tmtex.scm
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven,
;;               (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex test-tmtex)
  (:use (convert latex init-latex)
        (convert latex tmtex)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stree->tm-snippet st)
  (texmacs->generic (stree->tree st) "texmacs-snippet"))

(define (tm-snippet->tree s)
  (generic->texmacs s "texmacs-snippet"))

(define (texmacs->latex s)
  (convert s "texmacs-snippet" "latex-snippet"))

(define (latex->texmacs s)
  (convert s "latex-snippet" "texmacs-snippet"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Idempotence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define (regtest-tmtex-table)
;;   (regtest-table-library)
;;   ;; basic shorthands for latex table output
;;   (define (!row l) `(!row ,@l))
;;   (define (!table ll) `(!table ,@(map !row ll)))
;;   (define (tabular c ll) `((!begin "tabular" ,c) ,(!table ll)))
;;   (define (!row-hline l) `((!row ,@l) (hline)))
;;   (define (!table-hline ll) `(!table (hline) ,@(append-map !row-hline ll)))
;;   (define (tabular-hline c ll) `((!begin "tabular" ,c) ,(!table-hline ll)))
;;   ;; more shorthands
;;   (define (simple-table) (table '(("a"))))
;;   (define (simple-tformat) (tformat '() '(("a"))))
;;   (define (expect-simple c) (tabular c '(("a"))))
;;   (define (expect-hline c) (tabular-hline c '(("a"))))
;;   ;(tmtex-initialize)
;;   (regression-test-group
;;    "tmtex, tables" "table"
;;    tmtex :none
;;    (test "naked table" (simple-table) (expect-simple "l"))
;;    (test "naked tformat" (simple-tformat) (expect-simple "l"))
;;    (test "simple tabular" `(tabular ,(simple-tformat)) (expect-simple "l"))
;;    (test "simple tabular*" `(tabular* ,(simple-tformat)) (expect-simple "c"))
;;    (test "simple block" `(block ,(simple-tformat)) (expect-hline "|l|"))
;;    (test "simple block*" `(block* ,(simple-tformat)) (expect-hline "|c|"))
;;    ;; These conversions are only meaningful in math mode!
;;    ;; (test "simple matrix" `(matrix ,(simple-tformat))
;;    ;;         `(!concat (#{left\(}#) ,(expect-simple "c") (#{right\)}#)))
;;    ;; (test "simple det" `(det ,(simple-tformat))
;;    ;;         `(!concat (left|) ,(expect-simple "c")  (right|)))
;;    ;; (test "simple choice" `(choice ,(simple-tformat))
;;    ;;         `(!concat (left\{) ,(expect-simple "c") (right.)))
;;    (test "tabular*, two cells" `(tabular* ,(tformat '() '(("a" "b"))))
;;       (tabular "cc" '(("a" "b"))))
;;    (test "tabular*, four cells"
;;       `(tabular* ,(tformat '() '(("a" "b") ("c" "d"))))
;;       (tabular "cc" '(("a" "b") ("c" "d"))))
;;    (test "tabular*, first col aligned right"
;;       `(tabular* ,(tformat (list (colwith "1" "cell-halign" "r"))
;;                            '(("a" "b") ("c" "d"))))
;;       (tabular "rc" '(("a" "b") ("c" "d"))))
;;    (test "tabular*, whole table aligned right"
;;       `(tabular* ,(tformat (list (allwith "cell-halign" "r"))
;;                            '(("a" "b") ("c" "d"))))
;;       (tabular "rr" '(("a" "b") ("c" "d"))))
;;    (test "tabular*, one row border"
;;       `(tabular* ,(tformat (list (rowwith "1" "cell-bborder" "1ln"))
;;                            '(("a" "b") ("c" "d"))))
;;       `((!begin "tabular" "cc") (!table ,@(!row-hline '("a" "b"))
;;                                         ,(!row '("c" "d")))))
;;    (test "tabular*, one col border"
;;       `(tabular* ,(tformat (list (colwith "1" "cell-bborder" "1ln"))
;;                            '(("a" "b") ("c" "d"))))
;;       (tabular "cc" '(("a" "b") ("c" "d"))))))

;; TODO: getting menus entries :
;; find . -name '*menu.scm' -exec grep 'make' {} \; | sed -e 's/^\s\+//' | grep -v '^;'
(define idempotence-test-suite-0
  '(
    ;; Cork table
    "\x00 \x01 \x02 \x03 \x04 \x05 \x06 \x07"
    "\x08 \x09 \x0a \x0b \x0c \x0d \x0e \x0f"
    "\x10 \x11 \x12 \x13 \x14 \x15 \x16 \x17"
    "\x18 \x19 \x1a \x1b \x1c \x1d \x1e \x1f"
    "\x20 \x21 \x22 \x23 \x24 \x25 \x26 \x27"
    "\x28 \x29 \x2a \x2b \x2c \x2d \x2e \x2f"
    "\x30 \x31 \x32 \x33 \x34 \x35 \x36 \x37"
    "\x38 \x39 \x3a \x3b <less> \x3d <gtr> \x3f"
    "\x40 \x41 \x42 \x43 \x44 \x45 \x46 \x47"
    "\x48 \x49 \x4a \x4b \x4c \x4d \x4e \x4f"
    "\x50 \x51 \x52 \x53 \x54 \x55 \x56 \x57"
    "\x58 \x59 \x5a \x5b \x5c \x5d \x5e \x5f"
    "\x60 \x61 \x62 \x63 \x64 \x65 \x66 \x67"
    "\x68 \x69 \x6a \x6b \x6c \x6d \x6e \x6f"
    "\x70 \x71 \x72 \x73 \x74 \x75 \x76 \x77"
    "\x78 \x79 \x7a \x7b \x7c \x7d \x7e  Esc"
    "\x80 \x81 \x82 \x83 \x84 \x85 \x86 \x87"
    "\x88 \x89 \x8a \x8b \x8c \x8d \x8e \x8f"
    "\x90 \x91 \x92 \x93 \x94 \x95 \x96 \x97"
    "\x98 \x99 \x9a \x9b \x9c \x9d \x9e \x9f"
    "\xa0 \xa1 \xa2 \xa3 \xa4 \xa5 \xa6 \xa7"
    "\xa8 \xa9 \xaa \xab \xac \xad \xae \xaf"
    "\xb0 \xb1 \xb2 \xb3 \xb4 \xb5 \xb6 \xb7"
    "\xb8 \xb9 \xba \xbb \xbc \xbd \xbe \xbf"
    "\xc0 \xc1 \xc2 \xc3 \xc4 \xc5 \xc6 \xc7"
    "\xc8 \xc9 \xca \xcb \xcc \xcd \xce \xcf"
    "\xd0 \xd1 \xd2 \xd3 \xd4 \xd5 \xd6 \xd7"
    "\xd8 \xd9 \xda \xdb \xdc \xdd \xde \xdf"
    "\xe0 \xe1 \xe2 \xe3 \xe4 \xe5 \xe6 \xe7"
    "\xe8 \xe9 \xea \xeb \xec \xed \xee \xef"
    "\xf0 \xf1 \xf2 \xf3 \xf4 \xf5 \xf6 \xf7"
    "\xf8 \xf9 \xfa \xfb \xfc \xfd \xfe \xff"

    ;; breaking
    (next-line)
    (no-indent)
    (line-break)
    (no-break)
    (no-break)
    (page-break)
    (no-page-break)
    (new-page)
    (new-dpage)
    (new-page)
    (new-dpage)
      
    ;; spaces and lengths
    (hline)
    (htab "0pt")

    (space "-0.17em")
    (space "0.17em")
    (space "0.17em")
    (space "0.22em")
    (space "0.5em")
    (space "0.27em")
    (space "1em")
    (space "2em")
    (vspace "0.5fn")
    (vspace "1fn")
    (vspace "2fn")

    ;; symbols
    "..."
    (math "*")
    (math "|")
    (math "<||>")
    (math "<nin>")
    (math "<udots>")
    (math "<infty>")
    (math "<rangle>")
    (math ":")
    (math "<ldots>")
    (math "<cdots>")
    (math "|")
    (math "<||>")
    (math (math "<Box>"))
    (math "<neg>")
    (math "<vee>")
    (math "\\")
    (math "|")
    (math "<||>")
    (math "{")
    (math "}")

    ;; Greek letters
    (math "<Gamma>" "<Delta>" "<Theta>" "<Lambda>" "<Xi>" "<Pi>" "<Sigma>")
    (math "<Upsilon>" "<Phi>" "<Psi>" "<Omega>" "<alpha>" "<beta>" "<gamma>")
    (math "<delta>" "<epsilon>" "<varepsilon>" "<zeta>" "<eta>" "<theta>")
    (math "<vartheta>" "<iota>" "<kappa>" "<lambda>" "<mu>" "<nu>" "<omicron>")
    (math "<xi>" "<pi>" "<varpi>" "<rho>")
    (math "<varrho>" "<sigma>" "<varsigma>" "<tau>" "<upsilon>")
    (math "<phi>" "<varphi>" "<chi>" "<psi>" "<omega>")

    ;; Binary>" "<operations>")
    (math "<pm>" "<mp>" "<times>" "<div>" "<ast>" "<star>" "<circ>" "<bullet>")
    (math "<cdot>" "<cap>" "<cup>" "<uplus>" "<sqcap>" "<sqcup>" "<vee>")
    (math "<wedge>" "<setminus>" "<wr>" "<diamond>" "<triangleleft>")
    (math "<triangleright>" "<land>" "<lor>" "<lnot>" "<oplus>" "<ominus>")
    (math "<otimes>" "<oslash>" "<odot>" "<bigcirc>" "<amalg>" "<notin>")

    ;; Relations>")
    (math "<leq>" "<le>" "<geq>" "<ge>" "<equiv>" "<models>" "<prec>")
    (math "<succ>" "<sim>" "<perp>" "<preceq>" "<succeq>")
    (math "<simeq>" "<mid>" "<ll>" "<gg>" "<asymp>")
    (math "<parallel>" "<subset>" "<supset>" "<approx>" "<bowtie>")
    (math "<subseteq>" "<supseteq>" "<cong>" "<Join>" "<sqsubset>")
    (math "<sqsupset>" "<ne>" "<neq>" "<smile>" "<sqsubseteq>" "<sqsupseteq>")
    (math "<doteq>" "<frown>" "<in>" "<ni>" "<propto>")
    (math "<vdash>" "<dashv>")

    ;; Arrows>")
    (math "<leftarrow>" "<rightarrow>" "<uparrow>" "<downarrow>")
    (math "<Leftarrow>" "<Rightarrow>" "<Uparrow>" "<Downarrow>")
    (math "<nearrow>" "<searrow>" "<swarrow>" "<nwarrow>")
    (math "<leftrightarrow>" "<updownarrow>" "<Updownarrow>" "<Leftrightarrow>")
    (math "<leftharpoonup>" "<leftharpoondown>" "<rightharpoonup>")
    (math "<rightharpoondown>" "<hookleftarrow>" "<hookrightarrow>")
    (math "<to>" "<mapsto>" "<longmapsto>")
    (math "<longrightarrow>" "<longleftarrow>" "<longleftrightarrow>")
    (math "<Longrightarrow>" "<Longleftarrow>" "<Longleftrightarrow>")

    ;;   Delimiters
    (math "<uparrow>" "<Uparrow>" "<downarrow>" "<Downarrow>")
    (math "<updownarrow>" "<Updownarrow>")

  ;; misc
  (date "")
  (with-TeXmacs-text)
  (the-index "idx" "")
  (table-of-contents "toc" (document ""))))

(define idempotence-test-suite-1
  '(
    ;; Content based tags
    strong em dfn samp name person cite cite* abbr acronym verbatim kbd code
    code* var))

(define (test-latex-idempotence)
  (letrec ((conv (lambda (s)
                   (with ltx (texmacs->latex s)
                     (latex->texmacs ltx))))
           (test (lambda (s)
                   (== s (conv s))))
           (proc (lambda (st)
                   (let* ((s   (stree->tm-snippet st))
                          (test? (test s))
                          (tag   (if test? 'concat 'document))
                          (msg   (if test? "test passed: " "test failed: "))
                          (color (if test? "dark green"    "dark red"))
                          (res   (if test? '()
                                   `((math " <rightarrow> ")
                                     ,(tm-snippet->tree (conv s))))))
                     `(concat (with "color" ,color ,msg) (,tag ,st ,@res))))))
    `(document
       (strong (concat "Idempotence testing: " (TeXmacs)(math " <rightarrow> ")
                       (LaTeX) (math " <rightarrow> ") (TeXmacs)))
       ,@(map proc idempotence-test-suite-0)
       ,@(map (lambda (x) (proc `(,x "test"))) idempotence-test-suite-1))))

(define (test-latex-idempotence*)
  (letrec ((conv (lambda (s)
                   (with ltx (texmacs->latex s)
                     (texmacs->latex (latex->texmacs ltx)))))
           (test (lambda (s) (== (texmacs->latex s) (conv s))))
           (proc (lambda (st)
                   (let* ((s   (stree->tm-snippet st))
                          (test? (test s))
                          (tag   (if test? 'concat 'document))
                          (msg   (if test? "test passed: " "test failed: "))
                          (color (if test? "dark green"    "dark red"))
                          (res   (if test? '()
                                   `((math " <rightarrow> ")
                                     ,(tm-snippet->tree (conv s))))))
                     `(concat (with "color" ,color ,msg) (,tag ,st ,@res))))))
    `(document
       (strong (concat "Idempotence testing: " (LaTeX) (math " <rightarrow> ")
                       (TeXmacs) (math " <rightarrow> ") (LaTeX)))
       ,@(map proc idempotence-test-suite-0)
       ,@(map (lambda (x) (proc `(,x "test"))) idempotence-test-suite-1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-tmtex)
  (stree->tree
    `(document
       ,(test-latex-idempotence)
       ""
       ,(test-latex-idempotence*))))
