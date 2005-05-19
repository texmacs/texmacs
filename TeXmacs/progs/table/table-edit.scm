
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-edit.scm
;; DESCRIPTION : routines for manipulating tables
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-edit)
  (:use (utils base environment) (utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting rows and columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside table)
  (let ((x (inside-which '(table document))))
    (if (== x 'document)
	(insert-return)
	(begin
	  (table-insert-row #t)
	  (table-go-to (table-which-row) 1)))))

(tm-define (structured-insert forwards?)
  (:inside table)
  (table-insert-column forwards?))

(tm-define (structured-insert-up)
  (:inside table)
  (table-insert-row #f))

(tm-define (structured-insert-down)
  (:inside table)
  (table-insert-row #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Posititioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (position-default)
  (:inside table)
  (cell-del-format ""))

(tm-define (position-left)
  (:inside table)
  (cell-halign-left))

(tm-define (position-right)
  (:inside table)
  (cell-halign-right))

(tm-define (position-up)
  (:inside table)
  (cell-valign-up))

(tm-define (position-down)
  (:inside table)
  (cell-valign-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands for tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (table-interactive-set var)
  (:interactive #t)
  (interactive (lambda (s) (table-set-format var s))
    (drd-ref env-var-description% var)))

(tm-define (table-use-paragraph-width)
  (table-set-format "table-width" "1par"))

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

(tm-define (cell-interactive-set var)
  (:interactive #t)
  (interactive (lambda (s) (cell-set-format var s))
    (drd-ref env-var-description% var)))

(define (make-set-cell-multiple-formats vars val)
  (let ((sp (tm-position-new))
	(ep (tm-position-new)))
    (tm-position-set sp (selection-get-start))
    (tm-position-set ep (selection-get-end))
    (map (lambda (var)
	   (selection-set-start-path (tm-position-get sp))
	   (selection-set-end-path (tm-position-get ep))
	   (cell-set-format var val))
	 vars)
    (tm-position-delete sp)
    (tm-position-delete ep)))

(tm-define (cell-set-padding padding)
  (:argument padding "Cell padding")
  (make-set-cell-multiple-formats
   '("cell-lsep" "cell-rsep" "cell-bsep" "cell-tsep")
   padding))

(tm-define (cell-set-border border)
  (:argument border "Cell border width")
  (make-set-cell-multiple-formats
   '("cell-lborder" "cell-rborder" "cell-bborder" "cell-tborder")
   border))

(tm-define (cell-set-span rs cs)
  (:argument rs "Row span")
  (:argument cs "Column span")
  (let ((sp (tm-position-new))
	(ep (tm-position-new)))
    (tm-position-set sp (selection-get-start))
    (tm-position-set ep (selection-get-end))
    (cell-set-format "cell-row-span" rs)
    (selection-set-start-path (tm-position-get sp))
    (selection-set-end-path (tm-position-get ep))
    (cell-set-format "cell-col-span" cs)
    (tm-position-delete sp)
    (tm-position-delete ep)))

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

(tm-define (table-equation-numbered?)
  (if (table-search-number-equation) #t #f))

(tm-define (table-number-equation)
  (let* ((row (table-which-row))
	 (p   (table-search-cell row -1))
	 (end (tm-end p)))
    (tm-go-to end)
    (insert '(eq-number))))

(tm-define (table-nonumber-equation)
  (let ((p (table-search-number-equation)))
    (if p (clipboard-cut-at p))))

(define (table-inside-sub? p q)
  (or (== p q)
      (and (nnull? q)
	   (or (== (tree-get-label (tm-subtree q)) 'tformat)
	       (== (tree-get-label (tm-subtree q)) 'document))
	   (table-inside-sub? p (cDr q)))))

(tm-define (table-inside? which)
  (if (and (inside? 'table) (inside? which))
      (let ((p (search-upwards which))
	    (q (search-upwards 'table)))
	(table-inside-sub? p (cDr q)))
      #f))

(tm-define (table-toggle-number-equation)
  (cond ((inside? 'equation) (variant-replace 'equation 'equation*))
	((inside? 'equation*) (variant-replace 'equation* 'equation))
	(else (if (or (table-inside? 'eqnarray) (table-inside? 'eqnarray*))
		  (if (table-equation-numbered?)
		      (table-nonumber-equation)
		      (table-number-equation))))))
