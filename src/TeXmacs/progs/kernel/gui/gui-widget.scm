
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-widget.scm
;; DESCRIPTION : Definition of widgets
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-widget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call back management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-call-back-nr 0)
(define widget-call-back-table (make-ahash-table))

(tm-define (widget-call-back handle)
  (:secure #t)
  (and-with fun (ahash-ref widget-call-back-table handle)
    (fun)))

(tm-define (widget-new-call-back fun)
  (set! widget-call-back-nr (+ widget-call-back-nr 1))
  (ahash-set! widget-call-back-table widget-call-back-nr fun)
  (string-append "(widget-call-back "
		 (number->string widget-call-back-nr)
		 ")"))

(tm-define (widget-delete-call-backs start end)
  (when (< start end)
    (ahash-set! widget-call-back-table start #f)
    (widget-delete-call-backs (+ start 1) end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal widget fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define widget-serial-table (make-ahash-table))
(define widget-internal-table (make-ahash-table))

(tm-define (widget-internal-new serial id)
  (ahash-set! widget-serial-table serial id)
  (ahash-set! widget-internal-table id (make-ahash-table)))

(tm-define (widget-internal-delete serial id)
  (ahash-remove! widget-serial-table serial)
  (ahash-remove! widget-internal-table id))

(tm-define (widget-internal-set! id var val)
  (and-with t (ahash-ref widget-internal-table id)
    (ahash-set! t var val)))

(tm-define (widget-internal-ref id var)
  (and-with t (ahash-ref widget-internal-table id)
    (ahash-ref t var)))

(tm-define (widget-set! var val)
  (when (context-has? "form-prefix")
    (and-with serial (string-drop-right (get-env "form-prefix") 1)
      (and-with id (ahash-ref widget-serial-table serial)
	(widget-internal-set! id var val)))))

(tm-define (widget-ref var)
  (when (context-has? "form-prefix")
    (and-with serial (string-drop-right (get-env "form-prefix") 1)
      (and-with id (ahash-ref widget-serial-table serial)
	(widget-internal-ref id var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntactic sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (Quote x)
  `(quote ,x))

(define (List . l) 
  `(list ,@l))

(define (Cons h t) 
  `(cons ,h ,t))

(define (Concat . l)
  `((lambda (l)
      (cond ((null? l) "")
	    ((null? (cdr l)) (car l))
	    (else (cons 'concat l))))
    ,l))

(define (Document . l)
  `((lambda (l)
      (cond ((null? l) "")
	    ((null? (cdr l)) (car l))
	    (else (cons 'document l))))
    ,l))

(define (get-options-sub l)
  (if (and (nnull? l) (keyword? (car l)))
      (with (options . args) (get-options-sub (cdr l))
	(cons (cons (car l) options) args))
      (cons '() l)))

(define (get-options l)
  (get-options-sub (cdr l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-widget w)
  (:synopsis "Build a lazy widget constructor from a scheme program @w")
  (cond ((list? w) (List (Cons (Quote (car w)) (build-widgets (cdr w)))))
	((== w '-) (List (List (Quote 'gui-vspace))))
	((== w '---) (List (List (Quote 'gui-hrule))))
	((== w '>>>) (List (List (Quote 'htab) "1em")))
	(else (List w))))

(tm-define (build-widgets ws)
  `(append ,@(map build-widget ws)))

(tm-define (build-widget w)
  (:case let)
  (with (cmd bindings . body) w
    `(let* ,bindings
       ,(build-widgets body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aspect attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-aspect x)
  (cond ((== x :red) '("gui-toggle-color" "pastel red"))
	((== x :green) '("gui-toggle-color" "pastel green"))
	((== x :blue) '("gui-toggle-color" "pastel blue"))
	((== x :yellow) '("gui-toggle-color" "pastel yellow"))
	((== x :orange) '("gui-toggle-color" "pastel orange"))
	((== x :grey) '("gui-toggle-color" "light grey"))
	((== x :circle) '("gui-toggle-type" "circle"))
	((== x :square) '("gui-toggle-type" "square"))
	((== x :checked) '("gui-marker-type" "checked"))
	((== x :bullet) '("gui-marker-type" "bullet"))
	(else '())))

(tm-define (build-widget w)
  (:case aspect)
  (with (opts . body) (get-options w)
    (let* ((bindings (append-map build-aspect opts))
	   (fun (lambda (x) `(with ,@bindings ,x)))
	   (builder (build-widgets body)))
      `(map ,fun ,builder))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons and button related markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-widget w)
  (:case action)
  (with (cmd body . cmds) w
    (List (List (Quote 'form-action)
		(apply Concat (build-widget body))
		`(widget-new-call-back (lambda () ,@cmds))))))

(tm-define (build-widget w)
  (:case button)
  (with (cmd body . cmds) w
    (List (List (Quote 'form-button)
		(apply Concat (build-widget body))
		`(widget-new-call-back (lambda () ,@cmds))))))

(tm-define (build-widget w)
  (:case toggle)
  (with (cmd name val) w
    (List (List (Quote 'form-toggle) name (if val "true" "false")))))

(tm-define (build-widget w)
  (:case button-toggle)
  (with (cmd name val . body) w
    (List (List (Quote 'form-button-toggle) name (if val "true" "false")
		(apply Concat (build-widgets body))))))

(tm-define (build-widget w)
  (:case alternatives)
  (with (cmd name val . body) w
    (List (List (Quote 'form-alternatives) name val
		(apply Document (build-widgets body))))))

(tm-define (build-widget w)
  (:case alternative)
  (with (cmd name val) w
    (List (List (Quote 'form-alternative) name val))))

(tm-define (build-widget w)
  (:case button-alternative)
  (with (cmd name val . body) w
    (List (List (Quote 'form-button-alternative) name val
		(apply Concat (build-widgets body))))))

(tm-define (build-widget w)
  (:case header)
  (with (cmd . body) w
    (List (List (Quote 'gui-centered-switch)
		(apply Concat (build-widgets body))))))

(tm-define (build-widget w)
  (:case sheet)
  (with (cmd name val . body) w
    (List (List (Quote 'form-sheet) name val
		(apply Document (build-widgets body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-widget w)
  (:case internal)
  `(begin
     (widget-internal-set! aux-id ,(cadr w) ,(caddr w))
     '()))

(tm-define (build-widget w)
  (:case field)
  (with (opts name val) (get-options w)
    (with f (cond ((in? :short opts) 'form-short-input)
		  ((in? :multiline opts) 'form-big-input)
		  (else 'form-line-input))
      (List (List (Quote f) name (apply Concat (build-widget val)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-cell w)
  (with c (build-widget w)
    (if (== w '>>>) (set! c (List (List (Quote 'gui-tab)))))
    (List (List (Quote 'cell) (apply Concat c)))))

(define (build-cells ws)
  `(append ,@(map build-cell ws)))

(define (build-row l)
  (List (Cons (Quote 'row)
	      (build-cells l))))

(define (build-rows ls)
  `(append ,@(map build-row ls)))

(tm-define (build-widget w)
  (:case table)
  (with (options . rows) (get-options w)
    (with short? (in? :short options)
      (List (List (Quote (if short? 'gui-normal-bar 'gui-normal-table))
		  (List (Quote 'tformat)
			(Cons (Quote 'table)
			      (build-rows rows))))))))

(tm-define (build-widget w)
  (:case bar)
  (with (options . cells) (get-options w)
    (with short? (in? :short options)
      (List (List (Quote (if short? 'gui-normal-bar 'gui-normal-table))
		  (List (Quote 'tformat)
			(List (Quote 'table)
			      (Cons (Quote 'row)
				    (build-cells cells)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-serial-number 0)

(tm-define (define-widget-sub id body)
  `(let* ((aux-id ,id)
	  (aux-num (number->string widget-serial-number))
	  (aux-serial (string-append "widget-" aux-num))
	  (aux-begin (+ widget-call-back-nr 1))
	  (aux-end (+ widget-call-back-nr 1))
	  (aux-result #f)
	  (internal-ref
	   (lambda (var)
	     (widget-internal-ref aux-id var)))
	  (internal-set!
	   (lambda (var val)
	     (widget-internal-set! aux-id var val)))
	  (dismiss
	   (lambda ()
	     (widget-delete-call-backs aux-begin aux-end)
	     (widget-internal-delete aux-serial aux-id)
	     (kill-window-and-buffer))))
     (set! widget-serial-number (+ widget-serial-number 1))
     (widget-internal-new aux-serial aux-id)
     (set! aux-result ,(build-widgets body))
     (set! aux-end (+ widget-call-back-nr 1))
     (tm->tree `(form ,aux-serial (document ,@aux-result)))))

(tm-define-macro (define-widget proto . body)
  `(tm-define ,proto
     ,(define-widget-sub "default" body)))

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
    (eval (define-widget-sub fun widget))))
