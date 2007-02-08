
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : form-edit.scm
;; DESCRIPTION : Editing forms
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-form)
  (:use (kernel gui gui-factory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building widgets for interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-fraction num den)
  (:argument num "content" "Numerator")
  (:argument den "content" "Denominator")
  (insert `(frac ,num ,den)))

(define (interactive-vars args nr)
  (if (null? args) '()
      (cons (string-append "arg" (number->string nr))
	    (interactive-vars (cdr args) (+ nr 1)))))

(define (interactive-value var type)
  (with val `(form-ref ,var)
    (cond ((== type "string") `(tree->string ,val))
	  ((== type "password") `(tree->string ,val))
	  ((== type "content") val)
	  (else `(tree->string ,val)))))

(define (interactive-field prompt var type val)
  `(,prompt (field ,var ,val)))

(tm-define (widget-std-fields prompts vars types defaults)
  (:synopsis "Construct a standard widget for input fields of given types")
  (let* ((vals (map (lambda (x) (or (and (nnull? x) (car x)) "")) defaults))
	 (rows (map interactive-field prompts vars types vals)))
    (cons 'table rows)))

(define (interactive-learn fun nr results)
  (if (null? results) '()
      (cons `(learn-interactive-arg ,fun ,nr ,(car results))
	    (interactive-learn fun (+ nr 1) (cdr results)))))

(tm-define (widget-std-ok fun vars* results)
  (:synopsis "Construct an Ok button for applying @fun to the form's @results")
  (with vars (map string->symbol vars*)
    `(button "Ok"
       (let* ,(map list vars results)
	 (dismiss)
	 ,@(interactive-learn fun 0 vars)
	 (delayed
	   (:idle 1)
	   (,fun ,@vars))))))

(tm-define (widget-std-form fun prompts vars types defaults results)
  (:synopsis "Standard form for a simple function application")
  `(,(widget-std-fields prompts vars types defaults)
    -
    (bar >>>
      (button "Cancel" (dismiss))
      ,(widget-std-ok fun vars results))))

(tm-define (widget-interactive fun . args)
  (lazy-define-force fun)
  (if (null? args) (set! args (compute-interactive-args fun)))
  (let* ((fun-args (build-interactive-args fun args 0))
	 (prompts (map car fun-args))
	 (vars (interactive-vars prompts 1))
	 (types (map cadr fun-args))
	 (defaults (map cddr fun-args))
	 (results (map interactive-value vars types))
	 (widget (widget-std-form fun prompts vars types defaults results)))
    (if (procedure-name fun) (set! fun (procedure-name fun)))
    (if (symbol? fun) (set! fun (symbol->string fun)))
    (if (nstring? fun) (set! fun "Enter function arguments"))
    (eval (widget-surround fun (build-widgets widget)))))
