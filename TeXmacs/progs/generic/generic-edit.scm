
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-edit.scm
;; DESCRIPTION : Generic editing routines
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editing via the keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-left) (go-left))
(tm-define (kbd-right) (go-right))
(tm-define (kbd-up) (go-up))
(tm-define (kbd-down) (go-down))
(tm-define (kbd-page-up) (go-page-up))
(tm-define (kbd-page-down) (go-page-down))
(tm-define (kbd-start-line) (go-start-line))
(tm-define (kbd-end-line) (go-end-line))

(tm-define (kbd-select r)
  (select-from-shift-keyboard)
  (r)
  (select-from-cursor))

(tm-define (kbd-return) (insert-return))
(tm-define (kbd-shift-return) (insert-return))

(tm-define (kbd-remove forward?) (remove-text forward?))
(tm-define (kbd-remove forward?)
  (:mode with-active-selection?)
  (clipboard-cut "primary"))

(tm-define (kbd-tab)
  (if (not (complete-try?))
      (set-message "Use M-tab in order to insert a tab" "tab")))

(tm-define (kbd-tab)
  (:inside label reference)
  (if (complete-try?) (noop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (complex-context? t)
  (and (nleaf? t) (nin? (tree-label t) '(concat document))))

(tm-define (simple-context? t)
  (or (leaf? t)
      (and (tree-in? t '(concat document))
	   (simple-context? (tree-down t)))))

(tm-define (document-context? t)
  (tree-is? t 'document))

(tm-define (traverse-right) (go-to-next-word))
(tm-define (traverse-left) (go-to-previous-word))
(tm-define (traverse-up) (noop))
(tm-define (traverse-down) (noop))

(tm-define (traverse-up)
  (:context document-context?)
  (go-to-previous-tag 'document))

(tm-define (traverse-down)
  (:context document-context?)
  (go-to-next-tag 'document))

(define (traverse-tree . l)
  (cond ((null? l) (traverse-tree (tree-up (cursor-tree))))
	((tree-in? (car l) '(concat document))
	 (traverse-tree (tree-up (car l))))
	(else (car l))))

(define (traverse-label . l)
  (tree-label (apply traverse-tree l)))

(tm-define (traverse-previous)
  (with-innermost t complex-context?
    (if t (go-to-previous-tag (tree-label t)))))

(tm-define (traverse-next)
  (with-innermost t complex-context?
    (if t (go-to-next-tag (tree-label t)))))

(tm-define (traverse-first)
  (go-to-repeat traverse-previous)
  (structured-start))

(tm-define (traverse-last)
  (go-to-repeat traverse-next)
  (structured-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert forwards?) (insert-argument forwards?))
(tm-define (structured-remove forwards?) (remove-argument forwards?))
(tm-define (structured-insert-up) (noop))
(tm-define (structured-insert-down) (noop))
(tm-define (structured-insert-start) (noop))
(tm-define (structured-insert-end) (noop))
(tm-define (structured-insert-top) (noop))
(tm-define (structured-insert-bottom) (noop))

(tm-define (structured-left)
  (with-innermost t complex-context?
    (if t
	(with p (path-previous-argument (root-tree) (tree->path (tree-down t)))
	  (if (nnull? p) (go-to p))))))

(tm-define (structured-right)
  (with-innermost t complex-context?
    (if t
	(with p (path-next-argument (root-tree) (tree->path (tree-down t)))
	  (if (nnull? p) (go-to p))))))

(tm-define (structured-up) (noop))
(tm-define (structured-down) (noop))

(tm-define (structured-start)
  (with-innermost t complex-context?
    (if t (tree-go-to t :down :start))))

(tm-define (structured-end)
  (with-innermost t complex-context?
    (if t (tree-go-to t :down :end))))

(tm-define (structured-exit-left)
  (with-innermost t complex-context?
    (if t (tree-go-to t :start))))

(tm-define (structured-exit-right)
  (with-innermost t complex-context?
    (if t (tree-go-to t :end))))

(tm-define (structured-first)
  (go-to-repeat structured-left)
  (structured-start))

(tm-define (structured-last)
  (go-to-repeat structured-right)
  (structured-end))

(tm-define (structured-top)
  (go-to-repeat structured-up)
  (structured-start))

(tm-define (structured-bottom)
  (go-to-repeat structured-down)
  (structured-end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (positioning-default) (noop))
(tm-define (positioning-left) (noop))
(tm-define (positioning-right) (noop))
(tm-define (positioning-up) (noop))
(tm-define (positioning-down) (noop))
(tm-define (positioning-start) (noop))
(tm-define (positioning-end) (noop))
(tm-define (positioning-top) (noop))
(tm-define (positioning-bottom) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-simple-context? t)
  (and (nleaf? t)
       (tree-is? t 'tree)
       (simple-context? (tree-down t))))

(tm-define (structured-insert forwards?)
  (:inside tree)
  (with-innermost t 'tree
    (if (== (tree-down-index t) 0) (set! t (tree-up t)))
    (if (== (tm-car t) 'tree)
	(with pos (tree-down-index t)
	  (if forwards? (set! pos (1+ pos)))
	  (tree-insert! t pos '(tree ""))
	  (tree-go-to t pos 0)))))

(tm-define (structured-remove forwards?)
  (:inside tree)
  (with-innermost t 'tree
    (if (== (tree-down-index t) 0) (set! t (tree-up t)))
    (if (== (tm-car t) 'tree)
	(with pos (tree-down-index t)
	  (cond (forwards?
		 (tree-remove! t pos 1)
		 (if (== pos (tree-arity t))
		     (tree-go-to t :end)
		     (tree-go-to t pos :start)))
		((== pos 1) (tree-go-to t 0 :end))
		(else (tree-remove t (- pos 1) 1)))))))

(tm-define (structured-insert-up)
  (:inside tree)
  (with-innermost t 'tree
    (if (!= (tree-down-index t) 0) (set! t (tree-down t)))
    (tree-set! t `(tree "" ,t))
    (tree-go-to t 0 0)))

(tm-define (structured-insert-down)
  (:inside tree)
  (with-innermost t 'tree
    (if (== (tree-down-index t) 0)
	(with pos (tree-arity t)
	  (tree-insert! t pos '(tree ""))
	  (tree-go-to t pos 0))
	(begin
	  (set! t (tree-down t))
	  (tree-set! t `(tree ,t ""))
	  (tree-go-to t 1 0)))))

(define (branch-active)
  (with-innermost t 'tree
    (with i (tree-down-index t)
      (if (and (= i 0) (tree-is? t :up 'tree))
	  (tree-up t)
	  t))))

(define (branch-go-to . l)
  (apply tree-go-to l)
  (if (tree-is? (cursor-tree) 'tree)
      (with last (cAr l)
	(if (nin? last '(:start :end)) (set! last :start))
	(tree-go-to (cursor-tree) 0 last))))

(tm-define (structured-left)
  (:context tree-simple-context?)
  (let* ((t (branch-active))
	 (i (tree-down-index t)))
    (if (> i 1) (branch-go-to t (- i 1) :end))))

(tm-define (structured-right)
  (:context tree-simple-context?)
  (let* ((t (branch-active))
	 (i (tree-down-index t)))
    (if (and (!= i 0) (< i (- (tree-arity t) 1)))
	(branch-go-to t (+ i 1) :start))))

(tm-define (structured-up)
  (:context tree-simple-context?)
  (let* ((t (branch-active))
	 (i (tree-down-index t)))
    (if (!= i 0) (tree-go-to t 0 :end))))

(tm-define (structured-down)
  (:context tree-simple-context?)
  (with-innermost t 'tree
    (if (== (tree-down-index t) 0)
	(branch-go-to t (quotient (tree-arity t) 2) :start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kill-paragraph)
  (selection-set-start)
  (go-end-paragraph)
  (selection-set-end)
  (clipboard-cut "primary"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting inactive content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (insert-inactive-stree-go-to t p)
  (:type (-> stree path void))
  (:synopsis "Insert an inactive stree @t and go to @p inside @t.")
  (if (in-source?)
      (insert-go-to t p)
      (insert-go-to (list 'inactive t) (cons 0 p))))

(tm-define (make-assign-arg s)
  (:type (-> string void))
  (:synopsis "Make an inactive assignment for the variable @s.")
  (insert-inactive-stree-go-to `(assign ,s "") '(1 0))
  (if (not (in-source?)) (set-message "return: activate" "assign")))

(tm-define (make-assign-macro s)
  (:type (-> string void))
  (:synopsis "Make an inactive macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-stree-go-to '(macro "") '(0 0))
  (if (not (in-source?))
      (set-message "return (2x): activate" "assign#macro")))

(tm-define (make-assign-macro-arg s)
  (:type (-> string void))
  (:synopsis "Make an inactive unary macro assignment for the variable @s.")
  (make-assign-arg s)
  (insert-inactive-stree-go-to '(macro "s" "") '(1 0))
  (if (not (in-source?))
      (set-message "return (2x): activate" "assign#macro")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting miscellaneous content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-specific s)
  (if (or (== s "texmacs") (in-source?))
      (insert-go-to `(specific ,s "") '(1 0))
      (insert-go-to `(inactive (specific ,s "")) '(0 1 0))))

(tm-define (make-include u)
  (insert `(include ,(string-slash (url->string u)))))

(tm-define (make-inline-image l)
  (apply make-postscript (cons* (url->string (car l)) #f (cdr l))))

(tm-define (make-link-image l)
  (apply make-postscript (cons* (url->string (car l)) #t (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for floats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-insertion s)
  (:synopsis "Make an insertion of type @s.")
  (with pos (if (== s "float") "tbh" "")
    (insert-go-to (list 'float s pos (list 'document ""))
		  (list 2 0 0))))

(tm-define (insertion-positioning what flag)
  (:synopsis "Allow/disallow the position @what for innermost float.")
  (with-innermost t 'float
    (let ((op (if flag string-union string-minus))
	  (st (tree-ref t 1)))
      (tree-set st (op (tree->string st) what)))))

(define (test-insertion-positioning? what)
  (with-innermost t 'float
    (with c (string-ref what 0)
      (char-in-string? c (tree->string (tree-ref t 1))))))

(define (not-test-insertion-positioning? s)
  (not (test-insertion-positioning? s)))

(tm-define (toggle-insertion-positioning what)
  (:check-mark "v" test-insertion-positioning?)
  (insertion-positioning what (not-test-insertion-positioning? what)))

(tm-define (toggle-insertion-positioning-not s)
  (:check-mark "v" not-test-insertion-positioning?)
  (toggle-insertion-positioning s))
