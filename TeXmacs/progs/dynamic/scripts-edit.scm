
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
	(utils plugins plugin-cmd)
	(convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define script-keep-input-flag? #f)
(define script-eval-math-flag? #t)
(tm-define script-approx-cmd (make-ahash-table))

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

(define (script-result-context? t)
  (tm-in? t '(script-result script-approx script-hidden)))

(tm-define (script-modified-evaluate-sub what fun opts)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session")))
    ;;(display* "Evaluating " what "\n")
    (insert-go-to `(script-status "") '(0 0))
    (fun)
    (insert what)
    (let* ((handle (tree-innermost 'script-status))
	   (t (tree->stree (tree-ref handle 0)))
	   (declaration? #f))
      ;;(display* "handle= " handle "\n")
      ;;(display* "t= " t "\n")
      (cond ((and (func? what 'concat) (in? '(script-assign) what))
	     (set! declaration? #t))
	    ((and (script-keep-input?) (== opts '(:approx)))
	     (tree-set! handle `(script-approx ,what (script-busy)))
	     (set! handle (tree-ref handle 1))
	     (tree-go-to handle :end))
	    ((script-keep-input?)
	     (tree-set! handle `(script-result ,t (script-busy)))
	     (set! handle (tree-ref handle 1))
	     (tree-go-to handle :end)))
      (if declaration?
	  (auto-eval handle lan session t :math-input :declaration)
	  (auto-eval handle lan session t :math-input :simplify-output)))))

(tm-define (script-modified-evaluate fun . opts)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session"))
	 (scripts? (supports-scripts? lan)))
    (cond ((and (selection-active-any?) scripts?)
	   (with sel (tree->stree (selection-tree))
	     (clipboard-cut "primary")
	     (script-modified-evaluate-sub sel fun opts)))
	  ((tree-innermost script-result-context?)
	   (let* ((t (tree-innermost script-result-context?))
		  (what (tree->stree (tree-ref t 0))))
	     (clipboard-cut-at (tree->path t))
	     (script-modified-evaluate-sub what fun opts)))
	  ((and (tree-innermost formula-context? #t)
		scripts? script-eval-math-flag?)
	   (let* ((t (tree-innermost formula-context? #t))
		  (what (tree->stree t)))
	     (clipboard-cut-at (tree->path t))
	     (script-modified-evaluate-sub what fun opts)))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (fun)))))

(tm-define (script-eval)
  (script-modified-evaluate noop))

(define (insert-function fun)
  (insert fun)
  (if (in-var-math?)
      (insert-raw-go-to '(concat (left "(") (right ")")) '(0 1))
      (insert-raw-go-to "()" '(1))))

(tm-define (script-approx)
  (and-with cmd (ahash-ref script-approx-cmd (get-env "prog-scripts"))
    (with fun (lambda () (insert-function cmd))
      (script-modified-evaluate fun :approx))))

(tm-define (script-apply fun . opts)
  (with n (if (null? opts) 1 (car opts))
    ;;(display* "Script apply " fun ", " n "\n")
    (cond ((= n 1)
	   (script-modified-evaluate (lambda () (insert-function fun))))
	  ((selection-active-any?)
	   (clipboard-cut "primary")
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (clipboard-paste "primary")
	   (insert ",")
	   (repeat (- n 2) (insert-raw-go-to "," '(0))))
	  ((tree-innermost script-result-context?)
	   (with t (tree-innermost script-result-context?)
	     (tree-set! t (tree-ref t 0))
	     (script-apply fun n)))
	  ((and (tree-innermost formula-context? #t)
		(supports-scripts? (get-env "prog-scripts"))
		script-eval-math-flag?)
	   ;;(display* "Apply to math\n")
	   (with t (tree-innermost formula-context? #t)
	     (tree-select t)
	     (script-apply fun n)))
	  (else
	   (if (not-in-session?) (make 'script-eval))
	   (insert-function fun)
	   (repeat (- n 1) (insert-raw-go-to "," '(0)))))))

;; check behaviour inside sessions
;; script-eval tag and keep evaluated expressions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-the-fly plug-in evaluations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (auto-eval t lan session in . opts)
  (tree-set! t '(script-busy))
  (with ptr (tree->tree-pointer t)
    (dialogue
      (with r (apply plugin-async-eval (cons* lan session in opts))
	(with u (tree-pointer->tree ptr)
	  (tree-pointer-detach ptr)
	  (when (== u t)
	    (with-cursor (tree->path u :end)
	      (tree-select t)
	      (clipboard-cut "dummy")
	      (if (and (in-var-math?) (tm-func? r 'math 1)) (set! r (cadr r)))
	      (if (in? :declaration opts)
		  (insert in)
		  (insert r)))))))))

(tm-define (kbd-return)
  (:inside script-eval)
  (with-innermost t 'script-eval
    (let* ((lan (get-env "prog-scripts"))
	   (session (get-env "prog-session"))
	   (in (tree->stree (tree-ref t 0))))
      (auto-eval t lan session in :math-input :simplify-output))))

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
	   (in (tree->stree (tree-ref t 2))))
      (auto-eval (tree-ref t 3) lan session in :math-input :simplify-output)
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
	   (in (script-plot-command lan (tree->stree t))))
      (tree-set! t `(plot-output ,t ""))
      (auto-eval (tree-ref t 1) lan session in :math-correct :math-input)
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
