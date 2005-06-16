
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

(tm-define (positioning-default)
  (:inside table)
  (cell-del-format ""))

(tm-define (positioning-left)
  (:inside table)
  (cell-halign-left))

(tm-define (positioning-right)
  (:inside table)
  (cell-halign-right))

(tm-define (positioning-up)
  (:inside table)
  (cell-valign-up))

(tm-define (positioning-down)
  (:inside table)
  (cell-valign-down))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(define (cell-simple-context? t)
  (and (nleaf? t)
       (simple-context? (tree-down t))
       (cell-context? t)))

(tm-define (structured-left)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (cell-move-relative c 0 -1)))

(tm-define (structured-right)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (cell-move-relative c 0 1)))

(tm-define (structured-up)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (cell-move-relative c -1 0)))

(tm-define (structured-down)
  (:context cell-simple-context?)
  (with-innermost c cell-simple-context?
    (cell-move-relative c 1 0)))

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
  (let ((sp (position-new))
	(ep (position-new)))
    (position-set sp (selection-get-start))
    (position-set ep (selection-get-end))
    (map (lambda (var)
	   (selection-set-start-path (position-get sp))
	   (selection-set-end-path (position-get ep))
	   (cell-set-format var val))
	 vars)
    (position-delete sp)
    (position-delete ep)))

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
  (let ((sp (position-new))
	(ep (position-new)))
    (position-set sp (selection-get-start))
    (position-set ep (selection-get-end))
    (cell-set-format "cell-row-span" rs)
    (selection-set-start-path (position-get sp))
    (selection-set-end-path (position-get ep))
    (cell-set-format "cell-col-span" cs)
    (position-delete sp)
    (position-delete ep)))

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
  (:argument s "Cell color")
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

(tm-define (table-equation-numbered?)
  (if (table-search-number-equation) #t #f))

(tm-define (table-number-equation)
  (let* ((row (table-which-row))
	 (st  (table-cell-tree row -1)))
    (tree-go-to st :end)
    (insert '(eq-number))))

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

(tm-define (table-toggle-number-equation)
  (cond ((inside? 'equation) (variant-replace 'equation 'equation*))
	((inside? 'equation*) (variant-replace 'equation* 'equation))
	(else (if (or (table-inside? 'eqnarray) (table-inside? 'eqnarray*))
		  (if (table-equation-numbered?)
		      (table-nonumber-equation)
		      (table-number-equation))))))
