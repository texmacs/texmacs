
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-edit.scm
;; DESCRIPTION : routines for manipulating tables
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-edit)
  (:use (utils library tree)
	(utils base environment)
	(utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some drd properties, which should go into table-drd.scm later on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag (table-tag))
(define-group similar-tag (table-tag))

(define-group table-tag
  tabular tabular* block block*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside table)
  (let ((x (inside-which '(table document))))
    (cond ((== x 'document)
	   (insert-return))
	  (else
	   (table-insert-row #t)
	   (table-go-to (table-which-row) 1)))))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (table-markup-context? t))
  (table-insert-column forwards?))

(tm-define (structured-insert-vertical t downwards?)
  (:require (table-markup-context? t))
  (table-insert-row downwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (table-markup-context? t))
  (table-remove-column forwards?))

(tm-define (structured-remove-vertical t downwards?)
  (:require (table-markup-context? t))
  (table-remove-row downwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posititioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (geometry-default t)
  (:require (table-markup-context? t))
  (with-focus-after t
    (cell-del-format "")))

(tm-define (geometry-horizontal t forward?)
  (:require (table-markup-context? t))
  (with-focus-after t
    (if forward? (cell-halign-right) (cell-halign-left))))

(tm-define (geometry-vertical t down?)
  (:require (table-markup-context? t))
  (with-focus-after t
    (if down? (cell-valign-down) (cell-valign-up))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (similar-complex-context? t)
  (:case cell row table tformat)
  #f)

(define (cell-context? t)
  (and (tree-is? t 'cell)
       (tree-is? t :up 'row)
       (tree-is? t :up :up 'table)))

(define (cell-not-at-top-context? t)
  (and (cell-context? t)
       (> (tree-index (tree-up t)) 0)))

(define (cell-not-at-bottom-context? t)
  (and (cell-context? t)
       (< (tree-index (tree-up t)) (- (tree-arity (tree-up t 2)) 1))))

(define (cell-move-absolute c row col)
  (let* ((r (tree-up c))
	 (t (tree-up r)))
    (if (and (>= row 0) (< row (tree-arity t))
	     (>= col 0) (< col (tree-arity r)))
	(begin
	  (tree-go-to c :start)
	  (table-go-to (+ row 1) (+ col 1))))))

(define (cell-move-relative c drow dcol)
  (let* ((r (tree-up c))
	 (t (tree-up r))
	 (row (+ (tree-index r) drow))
	 (col (+ (tree-index c) dcol)))
    (cell-move-absolute c row col)))

(tm-define (traverse-up)
  (:context cell-not-at-top-context?)
  (with-innermost c cell-not-at-top-context?
    (cell-move-relative c -1 0)))

(tm-define (traverse-down)
  (:context cell-not-at-bottom-context?)
  (with-innermost c cell-not-at-bottom-context?
    (cell-move-relative c 1 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cell-search-downwards t)
  (if (tree-is? t 'cell) t
      (and (tree-down t)
           (cell-search-downwards (tree-down t)))))

(tm-define (structured-horizontal t forwards?)
  (:require (table-markup-context? t))
  (with-focus-after t
    (and-with c (cell-search-downwards t)
      (cell-move-relative c 0 (if forwards? 1 -1)))))

(tm-define (structured-vertical t downwards?)
  (:require (table-markup-context? t))
  (with-focus-after t
    (and-with c (cell-search-downwards t)
      (cell-move-relative c (if downwards? 1 -1) 0))))

(define (cell-simple-context? t)
  (and (nleaf? t)
       (simple-context? (tree-down t))
       (cell-context? t)))

(tm-define (structured-exit-left)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (with t (tree-ref c :up :up)
      (while (tree-in? t :up '(tformat document)) (set! t (tree-up t)))
      (tree-go-to t :up :start))))

(tm-define (structured-exit-right)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (with t (tree-ref c :up :up)
      (while (tree-in? t :up '(tformat document)) (set! t (tree-up t)))
      (tree-go-to t :up :end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (table-interactive-set var)
  (:interactive #t)
  (interactive (lambda (s) (table-set-format var s))
    (drd-ref env-var-description% var)))

(define (table-set-format-list vars vals)
  (map table-set-format vars vals))

(define (table-get-width) (table-get-format "table-width"))
(define (table-get-hmode) (table-get-format "table-hmode"))

(define (table-test-automatic-width?)
  (or (== (table-get-width) "") (== (table-get-hmode) "auto")))
(tm-define (table-set-automatic-width)
  (:synopsis "Automatic determination of table width.")
  (:check-mark "o" table-test-automatic-width?)
  (table-set-format-list '("table-width" "table-hmode") '("" "auto")))

(tm-define (table-test-minimal-width? . args)
  (and (!= (table-get-width) "") (== (table-get-hmode) "max")))
(tm-define (table-set-minimal-width w)
  (:synopsis "Set minimal table width.")
  (:argument w "Minimal table width")
  (:check-mark "o" table-test-minimal-width?)
  (table-set-format-list '("table-width" "table-hmode") `(,w "max")))

(tm-define (table-test-exact-width? . args)
  (and (if (null? args)
	   (!= (table-get-width) "")
	   (== (table-get-width) (car args)))
       (== (table-get-hmode) "exact")))
(tm-define (table-set-exact-width w)
  (:synopsis "Set table width.")
  (:argument w "Table width")
  (:check-mark "o" table-test-exact-width?)
  (table-set-format-list '("table-width" "table-hmode") `(,w "exact")))

(tm-define (table-test-maximal-width? . args)
  (and (!= (table-get-width) "") (== (table-get-hmode) "min")))
(tm-define (table-set-maximal-width w)
  (:synopsis "Set maximal table width.")
  (:argument w "Maximal table width")
  (:check-mark "o" table-test-maximal-width?)
  (table-set-format-list '("table-width" "table-hmode") `(,w "min")))

(define (table-get-height) (table-get-format "table-height"))
(define (table-get-vmode) (table-get-format "table-vmode"))

(define (table-test-automatic-height?)
  (or (== (table-get-height) "") (== (table-get-vmode) "auto")))
(tm-define (table-set-automatic-height)
  (:synopsis "Automatic determination of table height.")
  (:check-mark "o" table-test-automatic-height?)
  (table-set-format-list '("table-height" "table-vmode") '("" "auto")))

(tm-define (table-test-minimal-height? . args)
  (and (!= (table-get-height) "") (== (table-get-vmode) "max")))
(tm-define (table-set-minimal-height h)
  (:synopsis "Set minimal table height.")
  (:argument h "Minimal table height")
  (:check-mark "o" table-test-minimal-height?)
  (table-set-format-list '("table-height" "table-vmode") `(,h "max")))

(tm-define (table-test-exact-height? . args)
  (and (!= (table-get-height) "") (== (table-get-vmode) "exact")))
(tm-define (table-set-exact-height h)
  (:synopsis "Set table height.")
  (:argument h "Table height")
  (:check-mark "o" table-test-exact-height?)
  (table-set-format-list '("table-height" "table-vmode") `(,h "exact")))

(tm-define (table-test-maximal-height? . args)
  (and (!= (table-get-height) "") (== (table-get-vmode) "min")))
(tm-define (table-set-maximal-height h)
  (:synopsis "Set maximal table height.")
  (:argument h "Maximal table height")
  (:check-mark "o" table-test-maximal-height?)
  (table-set-format-list '("table-height" "table-vmode") `(,h "min")))

(tm-define (table-set-padding padding)
  (:argument padding "Padding")
  (table-set-format "table-lsep" padding)
  (table-set-format "table-rsep" padding)
  (table-set-format "table-bsep" padding)
  (table-set-format "table-tsep" padding))

(tm-define (table-set-border border)
  (:argument border "Border width")
  (table-set-format "table-lborder" border)
  (table-set-format "table-rborder" border)
  (table-set-format "table-bborder" border)
  (table-set-format "table-tborder" border))

(define (table-get-halign) (table-get-format "table-halign"))
(define (table-test-halign? s) (== (table-get-halign) s))
(tm-define (table-set-halign s)
  (:synopsis "Set horizontal table alignment.")
  (:check-mark "*" table-test-halign?)
  (table-set-format "table-halign" s))

(define (table-get-valign) (table-get-format "table-valign"))
(define (table-test-valign? s) (== (table-get-valign) s))
(tm-define (table-set-valign s)
  (:synopsis "Set vertical table alignment.")
  (:check-mark "*" table-test-valign?)
  (table-set-format "table-valign" s))

(define (table-hyphen?) (== "y" (table-get-format "table-hyphen")))
(define (table-set-hyphen s) (table-set-format "table-hyphen" s))
(tm-define (toggle-table-hyphen)
  (:synopsis "Toggle table hyphenation.")
  (:check-mark "v" table-hyphen?)
  (table-set-hyphen (if (table-hyphen?) "n" "y")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for cells in tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-cell-mode? s) (== (get-cell-mode) s))
(tm-property (set-cell-mode s)
  (:check-mark "*" test-cell-mode?))

(tm-define (cell-interactive-set var)
  (:interactive #t)
  (interactive (lambda (s) (cell-set-format var s))
    (drd-ref env-var-description% var)))

(define (cell-set-format-list vars vals)
  (let ((sp (position-new))
	(ep (position-new)))
    (position-set sp (selection-get-start))
    (position-set ep (selection-get-end))
    (map (lambda (var val)
	   (selection-set-start-path (position-get sp))
	   (selection-set-end-path (position-get ep))
	   (cell-set-format var val))
	 vars vals)
    (position-delete sp)
    (position-delete ep)))

(tm-define (table-insert-blank-row h)
  (:synopsis "Insert a blank row below cursor.")
  (:argument h "Height of row")
  (:default h "0.5spc")
  (with old-mode (get-cell-mode)
    (set-cell-mode "row")
    (table-insert-row #t)
    (cell-set-format "cell-background" "")
    (cell-set-format "cell-lborder" "0ln")
    (cell-set-format "cell-rborder" "0ln")
    (cell-set-format "cell-lsep" "0ln")
    (cell-set-format "cell-rsep" "0ln")
    (cell-set-format "cell-tsep" "0ln")
    (cell-set-format "cell-bsep" "0ln")
    (cell-set-format "cell-vcorrect" "n")
    (cell-set-format "cell-vmode" "exact")
    (cell-set-format "cell-height" h)
    (set-cell-mode old-mode)))

(tm-define (table-insert-blank-column w)
  (:synopsis "Insert a blank column at the right of the cursor.")
  (:argument w "Width of the column")
  (:default w "0.5spc")
  (with old-mode (get-cell-mode)
    (set-cell-mode "column")
    (table-insert-column #t)
    (cell-set-format "cell-background" "")
    (cell-set-format "cell-tborder" "0ln")
    (cell-set-format "cell-bborder" "0ln")
    (cell-set-format "cell-lsep" "0ln")
    (cell-set-format "cell-rsep" "0ln")
    (cell-set-format "cell-tsep" "0ln")
    (cell-set-format "cell-bsep" "0ln")
    (cell-set-format "cell-vcorrect" "n")
    (cell-set-format "cell-hmode" "exact")
    (cell-set-format "cell-width" w)
    (set-cell-mode old-mode)))

(define (cell-get-width) (cell-get-format "cell-width"))
(define (cell-get-hmode) (cell-get-format "cell-hmode"))

(define (cell-test-automatic-width?)
  (or (== (cell-get-width) "") (== (cell-get-hmode) "auto")))
(tm-define (cell-set-automatic-width)
  (:synopsis "Automatic determination of cell width.")
  (:check-mark "o" cell-test-automatic-width?)
  (cell-set-format-list '("cell-width" "cell-hmode") '("" "auto")))

(tm-define (cell-test-minimal-width? . args)
  (and (!= (cell-get-width) "") (== (cell-get-hmode) "max")))
(tm-define (cell-set-minimal-width w)
  (:synopsis "Set minimal cell width.")
  (:argument w "Minimal cell width")
  (:check-mark "o" cell-test-minimal-width?)
  (cell-set-format-list '("cell-width" "cell-hmode") `(,w "max")))

(tm-define (cell-test-exact-width? . args)
  (and (!= (cell-get-width) "") (== (cell-get-hmode) "exact")))
(tm-define (cell-set-exact-width w)
  (:synopsis "Set cell width.")
  (:argument w "Cell width")
  (:check-mark "o" cell-test-exact-width?)
  (cell-set-format-list '("cell-width" "cell-hmode") `(,w "exact")))

(tm-define (cell-test-maximal-width? . args)
  (and (!= (cell-get-width) "") (== (cell-get-hmode) "min")))
(tm-define (cell-set-maximal-width w)
  (:synopsis "Set maximal cell width.")
  (:argument w "Maximal cell width")
  (:check-mark "o" cell-test-maximal-width?)
  (cell-set-format-list '("cell-width" "cell-hmode") `(,w "min")))

(define (cell-get-height) (cell-get-format "cell-height"))
(define (cell-get-vmode) (cell-get-format "cell-vmode"))

(define (cell-test-automatic-height?)
  (or (== (cell-get-height) "") (== (cell-get-vmode) "auto")))
(tm-define (cell-set-automatic-height)
  (:synopsis "Automatic determination of cell height.")
  (:check-mark "o" cell-test-automatic-height?)
  (cell-set-format-list '("cell-height" "cell-vmode") '("" "auto")))

(tm-define (cell-test-minimal-height? . args)
  (and (!= (cell-get-height) "") (== (cell-get-vmode) "max")))
(tm-define (cell-set-minimal-height h)
  (:synopsis "Set minimal cell height.")
  (:argument h "Minimal cell height")
  (:check-mark "o" cell-test-minimal-height?)
  (cell-set-format-list '("cell-height" "cell-vmode") `(,h "max")))

(tm-define (cell-test-exact-height? . args)
  (and (!= (cell-get-height) "") (== (cell-get-vmode) "exact")))
(tm-define (cell-set-exact-height h)
  (:synopsis "Set cell height.")
  (:argument h "Cell height")
  (:check-mark "o" cell-test-exact-height?)
  (cell-set-format-list '("cell-height" "cell-vmode") `(,h "exact")))

(tm-define (cell-test-maximal-height? . args)
  (and (!= (cell-get-height) "") (== (cell-get-vmode) "min")))
(tm-define (cell-set-maximal-height h)
  (:synopsis "Set maximal cell height.")
  (:argument h "Maximal cell height")
  (:check-mark "o" cell-test-maximal-height?)
  (cell-set-format-list '("cell-height" "cell-vmode") `(,h "min")))

(tm-define (cell-set-padding padding)
  (:argument padding "Cell padding")
  (cell-set-format-list
   '("cell-lsep" "cell-rsep" "cell-bsep" "cell-tsep")
   (make-list 4 padding)))

(tm-define (cell-set-border border)
  (:argument border "Cell border width")
  (cell-set-format-list
   '("cell-lborder" "cell-rborder" "cell-bborder" "cell-tborder")
   (make-list 4 border)))

(tm-define (cell-set-span rs cs)
  (:argument rs "Row span")
  (:argument cs "Column span")
  (cell-set-format-list '("cell-row-span" "cell-col-span") (list rs cs)))

(define (cell-get-halign) (cell-get-format "cell-halign"))
(define (cell-test-halign? s) (== (cell-get-halign) s))
(tm-define (cell-set-halign s)
  (:synopsis "Set horizontal cell alignment.")
  (:check-mark "o" cell-test-halign?)
  (cell-set-format "cell-halign" s))

(define (cell-get-valign) (cell-get-format "cell-valign"))
(define (cell-test-valign? s) (== (cell-get-valign) s))
(tm-define (cell-set-valign s)
  (:synopsis "Set vertical cell alignment.")
  (:check-mark "o" cell-test-valign?)
  (cell-set-format "cell-valign" s))

(define (cell-get-background) (cell-get-format "cell-background"))
(define (cell-test-background? s) (== (cell-get-background) s))
(tm-define (cell-set-background s)
  (:synopsis "Set background color of cell.")
  (:argument s "Cell color")
  (:check-mark "o" cell-test-background?)
  (cell-set-format "cell-background" s))

(define (cell-get-vcorrect) (cell-get-format "cell-vcorrect"))
(define (cell-test-vcorrect? s) (== (cell-get-vcorrect) s))
(tm-define (cell-set-vcorrect s)
  (:synopsis "Set vertical correction mode for cell.")
  (:check-mark "o" cell-test-vcorrect?)
  (cell-set-format "cell-vcorrect" s))

(define (cell-get-hyphen) (cell-get-format "cell-hyphen"))
(define (cell-test-hyphen? s) (== (cell-get-hyphen) s))
(tm-define (cell-set-hyphen s)
  (:synopsis "Set hyphenation mode for cell.")
  (:check-mark "o" cell-test-hyphen?)
  (cell-set-format "cell-hyphen" s))

(define (cell-get-block) (cell-get-format "cell-block"))
(define (cell-test-block? s) (== (cell-get-block) s))
(tm-define (cell-set-block s)
  (:synopsis "Does the cell contain block content?")
  (:check-mark "o" cell-test-block?)
  (cell-set-format "cell-block" s))

(tm-define (cell-halign-left)
  (let* ((var "cell-halign")
	 (old (cell-get-format var)))
    (cond
     ((== old "r") (cell-set-format var "c"))
     (else (cell-set-format var "l")))))

(tm-define (cell-halign-right)
  (let* ((var "cell-halign")
	 (old (cell-get-format var)))
    (cond
     ((== old "l") (cell-set-format var "c"))
     (else (cell-set-format var "r")))))

(tm-define (cell-valign-down)
  (let* ((var "cell-valign")
	 (old (cell-get-format var)))
    (cond
     ((== old "c") (cell-set-format var "B"))
     ((== old "t") (cell-set-format var "c"))
     (else (cell-set-format var "b")))))

(tm-define (cell-valign-up)
  (let* ((var "cell-valign")
	 (old (cell-get-format var)))
    (cond
     ((== old "b") (cell-set-format var "B"))
     ((== old "B") (cell-set-format var "c"))
     (else (cell-set-format var "t")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special commands for full width math tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-search-subtree-sub t u i)
  (if (>= i (tree-arity t)) #f
      (let ((r (tree-search-subtree (tree-ref t i) u)))
	(if r r (tree-search-subtree-sub t u (+ i 1))))))

(define (tree-search-subtree t u)
  (cond ((== t u) t)
	((tree-atomic? t) #f)
	(else (tree-search-subtree-sub t u 0))))

(define (table-search-number-equation)
  (let* ((row (table-which-row))
	 (st  (table-cell-tree row -1)))
    (tree-search-subtree st (stree->tree '(eq-number)))))

(tm-define (numbered-numbered? t)
  (:require (tree-in? t '(eqnarray eqnarray*)))
  (and (== t (tree-innermost '(eqnarray eqnarray*)))
       (if (table-search-number-equation) #t #f)))

(tm-define (table-number-equation)
  (let* ((row (table-which-row))
	 (st  (table-cell-tree row -1)))
    (tree-go-to st :end)
    (insert-go-to '(eq-number) '(0))))

(tm-define (table-nonumber-equation)
  (let ((r (table-search-number-equation)))
    (if r (clipboard-cut-at (tree->path r)))))

(define (table-inside-sub? t1 t2)
  (or (== t1 t2)
      (and (tree-in? t2 '(tformat document))
	   (table-inside-sub? t1 (tree-up t2)))))

(tm-define (table-inside? which)
  (let* ((t1 (tree-innermost which))
	 (t2 (tree-innermost 'table)))
    (and t1 t2
	 (tree-inside? t2 t1)
	 (table-inside-sub? t1 (tree-up t2)))))

(tm-define (numbered-toggle t)
  (:require (tree-in? t '(eqnarray eqnarray*)))
  (when (== t (tree-innermost '(eqnarray eqnarray*)))
    (if (table-search-number-equation)
        (table-nonumber-equation)
        (table-number-equation))))
