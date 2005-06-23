
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-edit.scm
;; DESCRIPTION : routines for on-the-fly evaluation of scripts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-edit)
  (:use (utils library tree)
	(utils plugins plugin-cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-keep-input-flag? #f)
(define plugin-eval-math-flag? #t)

(tm-define (plugin-keep-input?) plugin-keep-input-flag?)
(tm-define (toggle-keep-input)
  (:synopsis "Toggle whether we keep the input of evaluations.")
  (:check-mark "v" plugin-keep-input?)
  (toggle! plugin-keep-input-flag?))

(tm-define (plugin-eval-math?) plugin-eval-math-flag?)
(tm-define (toggle-eval-math)
  (:synopsis "Toggle whether we evaluate the innermost non-selected formulas.")
  (:check-mark "v" plugin-eval-math?)
  (toggle! plugin-eval-math-flag?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level evaluation and function application via plug-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (plugin-evaluable?)
  (or (selection-active-any?)
      (nnot (tree-innermost formula-context? #t))))

(tm-define (plugin-modified-evaluate fun1 fun2)
  (let* ((name (get-env "prog-scripts"))
	 (session (get-env "prog-session"))
	 (scripts? (supports-scripts? name)))
    (cond ((and (selection-active-any?) scripts?)
	   (let* ((t (fun1 (tree->stree (selection-tree))))
		  (r (plugin-eval name session t :math-input))
		  (m1? (in-var-math?))
		  (m2? (tm-func? r 'math 1)))
	     (clipboard-cut "primary")
	     (if m2? (set! r (cadr r)))
	     (if (and (not m1?) m2?) (insert-go-to '(math "") '(0 0)))
	     (if (plugin-keep-input?)
		 (begin
		   (insert "=")
		   (with-cursor (cursor-after (go-to-previous))
		     (fun2)
		     (clipboard-paste "primary"))))
	     (insert r)))
	  ((and (tree-innermost formula-context? #t)
		scripts? plugin-eval-math-flag?)
	   (with t (tree-innermost formula-context? #t)
	     (tree-select t)
	     (plugin-modified-evaluate fun1 fun2)))
	  ((selection-active-any?)
	   (clipboard-cut "primary")
	   (plugin-modified-evaluate fun1 fun2)
	   (clipboard-paste "primary"))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (fun2)))))

(tm-define (plugin-evaluate)
  (plugin-modified-evaluate
   (lambda (t) t)
   noop))

(define (insert-function fun)
  (insert fun)
  (if (in-var-math?)
      (insert-go-to '(concat (left "(") (right ")")) '(0 1))
      (insert-go-to "()" '(1))))

(tm-define (plugin-apply-function fun . opts)
  (with n (if (null? opts) 1 (car opts))
    (cond ((= n 1)
	   (plugin-modified-evaluate
	    (lambda (t) (list 'concat fun "(" t ")"))
	    (lambda () (insert-function fun))))
	  ((selection-active-any?)
	   (clipboard-cut "primary")
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (clipboard-paste "primary")
	   (insert ",")
	   (repeat (- n 2) (insert-go-to "," '(0))))
	  ((and (tree-innermost formula-context? #t)
		(supports-scripts? (get-env "prog-scripts"))
		plugin-eval-math-flag?)
	   (with t (tree-innermost formula-context? #t)
	     (tree-select t)
	     (plugin-apply-function fun n)))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (repeat (- n 1) (insert-go-to "," '(0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-the-fly plug-in evaluations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside script-eval)
  (with-innermost t 'script-eval
    (let* ((name (get-env "prog-scripts"))
	   (session (get-env "prog-session"))
	   (in (tree->stree (tree-ref t 0))))
      (tree-select t)
      (clipboard-cut "primary")
      (insert (plugin-eval name session in :math-correct :math-input)))))

(tm-define (make-script-input)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session")))
    (insert-go-to `(script-input ,lan ,session "" "")
		  '(2 0))))

(tm-define (hidden-variant)
  (:inside script-input)
  (with-innermost t 'script-input
    (let* ((name (tree->string (tree-ref t 0)))
	   (session (tree->string (tree-ref t 1)))
	   (in (tree->stree (tree-ref t 2)))
	   (out (plugin-eval name session in :math-correct :math-input)))
      (tree-set! t 3 out)
      (tree-assign-node! t 'script-output)
      (tree-go-to t 3 :end))))

(tm-define (hidden-variant)
  (:inside script-output)
  (with-innermost t 'script-output
    (tree-assign-node! t 'script-input)
    (tree-go-to t 2 :end)))
