
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
;; Structured editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert forwards?) (insert-argument forwards?))
(tm-define (structured-remove forwards?) (remove-structure-upwards))
(tm-define (structured-insert-up) (noop))
(tm-define (structured-insert-down) (noop))
(tm-define (structured-insert-start) (noop))
(tm-define (structured-insert-end) (noop))
(tm-define (structured-insert-top) (noop))
(tm-define (structured-insert-bottom) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (position-default) (noop))
(tm-define (position-left) (noop))
(tm-define (position-right) (noop))
(tm-define (position-up) (noop))
(tm-define (position-down) (noop))
(tm-define (position-start) (noop))
(tm-define (position-end) (noop))
(tm-define (position-top) (noop))
(tm-define (position-bottom) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert forwards?)
  (:inside tree)
  (branch-insert forwards?))

(tm-define (structured-insert-up)
  (:inside tree)
  (let* ((q (search-parent-upwards 'tree))
	 (l (cAr q))
	 (p (if (== l 0) (cDr q) q)))
    (tm-assign p `(tree "" ,(tm-subtree p)))
    (tm-go-to (rcons* p 0 0))))

(tm-define (structured-insert-down)
  (:inside tree)
  (let* ((q (search-parent-upwards 'tree))
	 (l (cAr q))
	 (p (if (== l 0) (cDr q) q)))
    (tm-insert-node (rcons p 0) '(tree ""))
    (tm-go-to (rcons* p 1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kill-line)
  (selection-set-start)
  (go-end-line)
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

;(define (make-insertion s)
;  (insert-go-to
;   (list 'float s
;	 (if (string=? s "float") "tbh" "")
;	 (list 'document ""))
;   (list 2 0 0)))

;(define (position-insertion what flag)
;"Allow/disallow a position for the inner float the caret in is.
;what <char>   : position to allow/disallow
;flag <boolean>: allow if true, disallow is false."
;
;  (let ((p (search-upwards 'float)))
;    (if (nnull? p)
;	(tm-assign
;	 (rcons p 1)
;	 (string->tree ((if flag
;			    string-include
;			    string-exclude) (tree->string
;					     (tm-subtree (rcons p 1)))
;			    what))))))

(define (test-insertion-position? what)
  (let ((p (search-upwards 'float))
	(c (string-ref what 0)))
    (if (nnull? p)
	(char-in-string? c (tree->string (tm-subtree (rcons p 1)))))))

(define (not-test-insertion-position? s)
  (not (test-insertion-position? s)))

(tm-define (toggle-insertion-position what)
  (:check-mark "v" test-insertion-position?)
  (position-insertion what (not-test-insertion-position? what)))

(tm-define (toggle-insertion-position-not s)
  (:check-mark "v" not-test-insertion-position?)
  (toggle-insertion-position s))
