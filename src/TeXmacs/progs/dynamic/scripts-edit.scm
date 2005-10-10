
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
	(utils library cursor)
	(utils plugins plugin-cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define script-keep-input-flag? #f)
(define script-eval-math-flag? #t)

(tm-define (script-keep-input?) script-keep-input-flag?)
(tm-define (toggle-keep-input)
  (:synopsis "Toggle whether we keep the input of evaluations.")
  (:check-mark "v" script-keep-input?)
  (toggle! script-keep-input-flag?))

(tm-define (script-eval-math?) script-eval-math-flag?)
(tm-define (toggle-eval-math)
  (:synopsis "Toggle whether we evaluate the innermost non-selected formulas.")
  (:check-mark "v" script-eval-math?)
  (toggle! script-eval-math-flag?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level evaluation and function application via plug-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (script-evaluable?)
  (or (selection-active-any?)
      (nnot (tree-innermost formula-context? #t))))

(tm-define (script-modified-evaluate fun1 fun2)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session"))
	 (scripts? (supports-scripts? lan)))
    (cond ((and (selection-active-any?) scripts?)
	   (let* ((t (fun1 (tree->stree (selection-tree))))
		  (r (plugin-eval lan session t :math-input))
		  (m1? (in-var-math?))
		  (m2? (tm-func? r 'math 1)))
	     (clipboard-cut "primary")
	     (if m2? (set! r (cadr r)))
	     (if (and (not m1?) m2?) (insert-raw-go-to '(math "") '(0 0)))
	     (if (script-keep-input?)
		 (begin
		   (insert "=")
		   (with-cursor (cursor-after (go-to-previous))
		     (fun2)
		     (clipboard-paste "primary"))))
	     (insert r)))
	  ((and (tree-innermost formula-context? #t)
		scripts? script-eval-math-flag?)
	   (with t (tree-innermost formula-context? #t)
	     (tree-select t)
	     (script-modified-evaluate fun1 fun2)))
	  ((selection-active-any?)
	   (clipboard-cut "primary")
	   (script-modified-evaluate fun1 fun2)
	   (clipboard-paste "primary"))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (fun2)))))

(tm-define (script-eval)
  (script-modified-evaluate
   (lambda (t) t)
   noop))

(define (insert-function fun)
  (insert fun)
  (if (in-var-math?)
      (insert-raw-go-to '(concat (left "(") (right ")")) '(0 1))
      (insert-raw-go-to "()" '(1))))

(tm-define (script-apply fun . opts)
  (with n (if (null? opts) 1 (car opts))
    (cond ((= n 1)
	   (script-modified-evaluate
	    (lambda (t) (list 'concat fun "(" t ")"))
	    (lambda () (insert-function fun))))
	  ((selection-active-any?)
	   (clipboard-cut "primary")
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (clipboard-paste "primary")
	   (insert ",")
	   (repeat (- n 2) (insert-raw-go-to "," '(0))))
	  ((and (tree-innermost formula-context? #t)
		(supports-scripts? (get-env "prog-scripts"))
		script-eval-math-flag?)
	   (with t (tree-innermost formula-context? #t)
	     (tree-select t)
	     (script-apply fun n)))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (repeat (- n 1) (insert-raw-go-to "," '(0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-the-fly plug-in evaluations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside script-eval)
  (with-innermost t 'script-eval
    (let* ((lan (get-env "prog-scripts"))
	   (session (get-env "prog-session"))
	   (in (tree->stree (tree-ref t 0))))
      (tree-select t)
      (clipboard-cut "primary")
      (insert (plugin-eval lan session in :math-correct :math-input)))))

(tm-define (make-script-input)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session")))
    (insert-go-to `(script-input ,lan ,session "" "")
		  '(2 0))))

(tm-define (hidden-variant)
  (:inside script-input)
  (with-innermost t 'script-input
    (let* ((lan (tree->string (tree-ref t 0)))
	   (session (tree->string (tree-ref t 1)))
	   (in (tree->stree (tree-ref t 2)))
	   (out (plugin-eval lan session in :math-correct :math-input)))
      (tree-set! t 3 out)
      (tree-assign-node! t 'script-output)
      (tree-go-to t 3 :end))))

(tm-define (hidden-variant)
  (:inside script-output)
  (with-innermost t 'script-output
    (tree-assign-node! t 'script-input)
    (tree-go-to t 2 :end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm-ref t . l)
  ;; FIXME: this routine should go into the standard library
  (and (tm? t)
       (with r (select t l)
	 (and (nnull? r) (car r)))))

(tm-define (script-plot-command lan t)
  (cond ((== (car t) 'plot-curve)
	 `(concat "set samples 1000 ~ "
		  "set xrange [" ,(tm-ref t 1) ":" ,(tm-ref t 2) "] ~ "
		  "plot " ,(tm-ref t 0)))
	((== (car t) 'plot-curve*)
	 `(concat "set samples 1000 ~ "
		  "set parametric ~ "
		  "set trange [" ,(tm-ref t 2) ":" ,(tm-ref t 3) "] ~ "
		  "plot " ,(tm-ref t 0) ", " ,(tm-ref t 1)))
	((== (car t) 'plot-surface)
	 `(concat "set samples 50 ~ set isosamples 50 ~ set hidden3d ~"
		  "set pm3d ~ "
		  "set xrange [" ,(tm-ref t 1) ":" ,(tm-ref t 2) "] ~ "
		  "set yrange [" ,(tm-ref t 3) ":" ,(tm-ref t 4) "] ~ "
		  "splot " ,(tm-ref t 0)))
	((== (car t) 'plot-surface*)
	 `(concat "set samples 50 ~ set isosamples 50 ~ set hidden3d ~"
		  "set parametric ~ "
		  "set pm3d ~ "
		  "set urange [" ,(tm-ref t 3) ":" ,(tm-ref t 4) "] ~ "
		  "set vrange [" ,(tm-ref t 5) ":" ,(tm-ref t 6) "] ~ "
		  "splot " ,(tm-ref t 0)
		  ", " ,(tm-ref t 1)
		  ", " ,(tm-ref t 2)))))

(define (activate-plot)
  (with-innermost t '(plot-curve plot-curve* plot-surface plot-surface*)
    (let* ((lan "gnuplot")
	   (session "default")
	   (in (script-plot-command lan (tree->stree t)))
	   (out (plugin-eval lan session in :math-correct :math-input)))
      (tree-set! t `(plot-output ,t ,out))
      (tree-go-to t 1 :end))))

(tm-define (kbd-return)
  (:inside plot-curve plot-curve* plot-surface plot-surface*)
  (with-innermost t '(plot-curve plot-curve* plot-surface plot-surface*)
    (if (= (tree-down-index t) (- (tree-arity t) 1))
	(activate-plot)
	(tree-go-to t (1+ (tree-down-index t)) :end))))

(tm-define (hidden-variant)
  (:inside plot-curve plot-curve* plot-surface plot-surface*)
  (activate-plot))

(tm-define (hidden-variant)
  (:inside plot-output)
  (with-innermost t 'plot-output
    (tree-remove-node! t 0)
    (tree-go-to t 0 :end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-return)
  (:inside converter-eval)
  (with-innermost t 'converter-eval
    (let* ((format (string-append (tree->string (tree-ref t 0)) "-snippet"))
	   (in (texmacs->verbatim (tree-ref t 1))))
      (tree-select t)
      (clipboard-cut "primary")
      (insert (convert in format "texmacs-tree")))))

(tm-define (hidden-variant)
  (:inside converter-input)
  (with-innermost t 'converter-input
    (let* ((format (string-append (tree->string (tree-ref t 0)) "-snippet"))
	   (in (texmacs->verbatim (tree-ref t 1))))
      (tree-set! t 2 (convert in format "texmacs-tree"))
      (tree-assign-node! t 'converter-output)
      (tree-go-to t 2 :end))))

(tm-define (hidden-variant)
  (:inside converter-output)
  (with-innermost t 'converter-output
    (tree-assign-node! t 'converter-input)
    (tree-go-to t 1 :end)))
