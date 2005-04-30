
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table.scm
;; DESCRIPTION : routines for manipulating tables
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-table)
  (:use (texmacs tools tm-circulate)))

(export ;; interactive table commands
        table-set-width-ia table-use-paragraph-width table-set-height-ia
	table-set-padding-ia table-set-lpadding-ia table-set-rpadding-ia
	table-set-bpadding-ia table-set-tpadding-ia table-set-border-ia
	table-set-lborder-ia table-set-rborder-ia table-set-bborder-ia
	table-set-tborder-ia table-set-row-origin-ia table-set-column-origin-ia
	table-set-min-rows-ia table-set-max-rows-ia
	table-set-min-columns-ia table-set-max-columns-ia
	;; table actions
	table-set-halign table-set-valign toggle-table-hyphen
	;; interactive cell commands
	cell-set-width-ia cell-set-height-ia cell-set-hpart-ia
	cell-set-vpart-ia cell-set-padding-ia cell-set-lpadding-ia
	cell-set-rpadding-ia cell-set-bpadding-ia cell-set-tpadding-ia
	cell-set-border-ia cell-set-lborder-ia cell-set-rborder-ia
	cell-set-bborder-ia cell-set-tborder-ia cell-set-span-ia
	;; cell actions
	cell-set-halign cell-set-valign cell-set-hmode cell-set-vmode
	cell-set-background cell-set-vcorrect cell-set-hyphen
	cell-toggle-multi-paragraph cell-halign-left cell-halign-right
	cell-valign-down cell-valign-up
	;; other table commands
	table-inside?
	table-equation-numbered? table-number-equation
	table-nonumber-equation table-toggle-number-equation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (table-set-width-ia)
  (interactive '("Table width:")
	       '(lambda (s) (table-set-format "table-width" s))))

(define (table-use-paragraph-width)
  (table-set-format "table-width" "1par"))

(define (table-set-height-ia)
  (interactive '("Table height:")
	       '(lambda (s) (table-set-format "table-height" s))))

(define (table-set-padding-ia)
  (interactive '("Padding:")
	       '(lambda (s)
		  (table-set-format "table-lsep" s)
		  (table-set-format "table-rsep" s)
		  (table-set-format "table-bsep" s)
		  (table-set-format "table-tsep" s))))

(define (table-set-lpadding-ia)
  (interactive '("Left padding:")
	       '(lambda (s) (table-set-format "table-lsep" s))))

(define (table-set-rpadding-ia)
  (interactive '("Right padding:")
	       '(lambda (s) (table-set-format "table-rsep" s))))

(define (table-set-bpadding-ia)
  (interactive '("Bottom padding:")
	       '(lambda (s) (table-set-format "table-bsep" s))))

(define (table-set-tpadding-ia)
  (interactive '("Top padding:")
	       '(lambda (s) (table-set-format "table-tsep" s))))

(define (table-set-border-ia)
  (interactive '("Border width:")
	       '(lambda (s)
		  (table-set-format "table-lborder" s)
		  (table-set-format "table-rborder" s)
		  (table-set-format "table-bborder" s)
		  (table-set-format "table-tborder" s))))

(define (table-set-lborder-ia)
  (interactive '("Left border width:")
	       '(lambda (s) (table-set-format "table-lborder" s))))

(define (table-set-rborder-ia)
  (interactive '("Right border width:")
	       '(lambda (s) (table-set-format "table-rborder" s))))

(define (table-set-bborder-ia)
  (interactive '("Bottom border width:")
	       '(lambda (s) (table-set-format "table-bborder" s))))

(define (table-set-tborder-ia)
  (interactive '("Top border width:")
	       '(lambda (s) (table-set-format "table-tborder" s))))

(define (table-set-row-origin-ia)
  (interactive '("Origin row:")
	       '(lambda (s) (table-set-format "table-row-origin" s))))

(define (table-set-column-origin-ia)
  (interactive '("Origin column:")
	       '(lambda (s) (table-set-format "table-col-origin" s))))

(define (table-set-min-rows-ia)
  (interactive '("Minimal number of rows:")
	       '(lambda (s) (table-set-format "table-min-rows" s))))

(define (table-set-max-rows-ia)
  (interactive '("Maximal number of rows:")
	       '(lambda (s) (table-set-format "table-max-rows" s))))

(define (table-set-min-columns-ia)
  (interactive '("Minimal number of columns:")
	       '(lambda (s) (table-set-format "table-min-cols" s))))

(define (table-set-max-columns-ia)
  (interactive '("Maximal number of columns:")
	       '(lambda (s) (table-set-format "table-max-cols" s))))

(define (table-get-halign) (table-get-format "table-halign"))
(define (table-test-halign? s) (string=? (table-get-halign) s))
(tm-define (table-set-halign s)
  (:synopsis "Set horizontal table alignment.")
  (:check-mark "*" table-test-halign?)
  (table-set-format "table-halign" s))

(define (table-get-valign) (table-get-format "table-valign"))
(define (table-test-valign? s) (string=? (table-get-valign) s))
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

(define (test-cell-mode? s) (string=? (get-cell-mode) s))

(tm-property (set-cell-mode s)
  (:check-mark "*" test-cell-mode?))

(define (cell-set-width-ia)
  (interactive '("Cell width:")
	       '(lambda (s) (cell-set-format "cell-width" s))))

(define (cell-set-height-ia)
  (interactive '("Cell height:")
	       '(lambda (s) (cell-set-format "cell-height" s))))

(define (cell-set-hpart-ia)
  (interactive '("Part in unused horizontal space:")
	       '(lambda (s) (cell-set-format "cell-hpart" s))))

(define (cell-set-vpart-ia)
  (interactive '("Part in unused vertical space:")
	       '(lambda (s) (cell-set-format "cell-vpart" s))))

(define (make-set-cell-multiple-formats vars)
  `(lambda (val)
     (let ((sp (tm-position-new))
	   (ep (tm-position-new)))
       (tm-position-set sp (selection-get-start))
       (tm-position-set ep (selection-get-end))
       (map (lambda (var)
	      (selection-set-start-path (tm-position-get sp))
	      (selection-set-end-path (tm-position-get ep))
	      (cell-set-format var val))
	    (quote ,vars))
       (tm-position-delete sp)
       (tm-position-delete ep))))

(define (cell-set-padding-ia)
  (interactive '("Padding:")
	       (make-set-cell-multiple-formats
		'("cell-lsep" "cell-rsep" "cell-bsep" "cell-tsep"))))

(define (cell-set-lpadding-ia)
  (interactive '("Left padding:")
	       '(lambda (s) (cell-set-format "cell-lsep" s))))

(define (cell-set-rpadding-ia)
  (interactive '("Right padding:")
	       '(lambda (s) (cell-set-format "cell-rsep" s))))

(define (cell-set-bpadding-ia)
  (interactive '("Bottom padding:")
	       '(lambda (s) (cell-set-format "cell-bsep" s))))

(define (cell-set-tpadding-ia)
  (interactive '("Top padding:")
	       '(lambda (s) (cell-set-format "cell-tsep" s))))

(define (cell-set-border-ia)
  (interactive '("Border width:")
	       (make-set-cell-multiple-formats
		'("cell-lborder" "cell-rborder"
		  "cell-bborder" "cell-tborder"))))

(define (cell-set-lborder-ia)
  (interactive '("Left border width:")
	       '(lambda (s) (cell-set-format "cell-lborder" s))))

(define (cell-set-rborder-ia)
  (interactive '("Right border width:")
	       '(lambda (s) (cell-set-format "cell-rborder" s))))

(define (cell-set-bborder-ia)
  (interactive '("Bottom border width:")
	       '(lambda (s) (cell-set-format "cell-bborder" s))))

(define (cell-set-tborder-ia)
  (interactive '("Top border width:")
	       '(lambda (s) (cell-set-format "cell-tborder" s))))

(define (cell-set-span-ia)
  (interactive '("Row span:" "Column span: ")
	       '(lambda (rs cs)
		  (let ((sp (tm-position-new))
			(ep (tm-position-new)))
		    (tm-position-set sp (selection-get-start))
		    (tm-position-set ep (selection-get-end))
		    (cell-set-format "cell-row-span" rs)
		    (selection-set-start-path (tm-position-get sp))
		    (selection-set-end-path (tm-position-get ep))
		    (cell-set-format "cell-col-span" cs)
		    (tm-position-delete sp)
		    (tm-position-delete ep)))))

(define (cell-get-halign) (cell-get-format "cell-halign"))
(define (cell-test-halign? s) (string=? (cell-get-halign) s))
(tm-define (cell-set-halign s)
  (:synopsis "Set horizontal cell alignment.")
  (:check-mark "o" cell-test-halign?)
  (cell-set-format "cell-halign" s))

(define (cell-get-valign) (cell-get-format "cell-valign"))
(define (cell-test-valign? s) (string=? (cell-get-valign) s))
(tm-define (cell-set-valign s)
  (:synopsis "Set vertical cell alignment.")
  (:check-mark "o" cell-test-halign?)
  (cell-set-format "cell-valign" s))

(define (cell-get-hmode) (cell-get-format "cell-hmode"))
(define (cell-test-hmode? s) (string=? (cell-get-hmode) s))
(tm-define (cell-set-hmode s)
  (:synopsis "Set horizontal cell mode.")
  (:check-mark "o" cell-test-hmode?)
  (cell-set-format "cell-hmode" s))

(define (cell-get-vmode) (cell-get-format "cell-vmode"))
(define (cell-test-vmode? s) (string=? (cell-get-vmode) s))
(tm-define (cell-set-vmode s)
  (:synopsis "Set vertical cell mode.")
  (:check-mark "o" cell-test-vmode?)
  (cell-set-format "cell-vmode" s))

(define (cell-get-background) (cell-get-format "cell-background"))
(define (cell-test-background? s) (string=? (cell-get-background) s))
(tm-define (cell-set-background s)
  (:synopsis "Set background color of cell.")
  (:check-mark "o" cell-test-background?)
  (cell-set-format "cell-background" s))

(define (cell-get-vcorrect) (cell-get-format "cell-vcorrect"))
(define (cell-test-vcorrect? s) (string=? (cell-get-vcorrect) s))
(tm-define (cell-set-vcorrect s)
  (:synopsis "Set vertical correction mode for cell.")
  (:check-mark "o" cell-test-vcorrect?)
  (cell-set-format "cell-vcorrect" s))

(define (cell-get-hyphen) (cell-get-format "cell-hyphen"))
(define (cell-test-hyphen? s) (string=? (cell-get-hyphen) s))
(tm-define (cell-set-hyphen s)
  (:synopsis "Set hyphenation mode for cell.")
  (:check-mark "o" cell-test-hyphen?)
  (cell-set-format "cell-hyphen" s))

(tm-define (cell-toggle-multi-paragraph)
  (:synopsis "Toggle multiparagraph property of cell.")
  (:check-mark "v" cell-multi-paragraph?)
  (cell-multi-paragraph (not (cell-multi-paragraph?))))

(define (cell-halign-left)
  (let* ((var "cell-halign")
	 (old (cell-get-format var)))
    (cond
     ((== old "r") (cell-set-format var "c"))
     (else (cell-set-format var "l")))))

(define (cell-halign-right)
  (let* ((var "cell-halign")
	 (old (cell-get-format var)))
    (cond
     ((== old "l") (cell-set-format var "c"))
     (else (cell-set-format var "r")))))

(define (cell-valign-down)
  (let* ((var "cell-valign")
	 (old (cell-get-format var)))
    (cond
     ((== old "c") (cell-set-format var "B"))
     ((== old "t") (cell-set-format var "c"))
     (else (cell-set-format var "b")))))

(define (cell-valign-up)
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
      (let ((p (tree-search-subtree (tree-ref t i) u)))
	(if p (cons i p) (tree-search-subtree-sub t u (+ i 1))))))

(define (tree-search-subtree t u)
  (cond ((== t u) '())
	((tree-atomic? t) #f)
	(else (tree-search-subtree-sub t u 0))))

(define (table-search-number-equation)
  (let* ((row (table-which-row))
	 (p   (table-search-cell row -1))
	 (st  (tm-subtree p))
	 (q   (tree-search-subtree st (stree->tree '(eq-number)))))
    (if q (append p q) #f)))

(define (table-equation-numbered?)
  (if (table-search-number-equation) #t #f))

(define (table-number-equation)
  (let* ((row (table-which-row))
	 (p   (table-search-cell row -1))
	 (end (tm-end p)))
    ;(insert-at '(eq-number) end)
    (tm-go-to end)
    (insert '(eq-number))))

(define (table-nonumber-equation)
  (let ((p (table-search-number-equation)))
    (if p (clipboard-cut-at p))))

(define (table-inside-sub? p q)
  (or (== p q)
      (and (nnull? q)
	   (or (== (tree-get-label (tm-subtree q)) 'tformat)
	       (== (tree-get-label (tm-subtree q)) 'document))
	   (table-inside-sub? p (cDr q)))))

(define (table-inside? which)
  (if (and (inside? "table") (inside? which))
      (let ((p (search-upwards which))
	    (q (search-upwards "table")))
	(table-inside-sub? p (cDr q)))
      #f))

(define (table-toggle-number-equation)
  (cond ((inside? "equation") (variant-replace "equation" "equation*"))
	((inside? "equation*") (variant-replace "equation*" "equation"))
	(else (if (or (table-inside? "eqnarray") (table-inside? "eqnarray*"))
		  (if (table-equation-numbered?)
		      (table-nonumber-equation)
		      (table-number-equation))))))
