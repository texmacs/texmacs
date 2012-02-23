
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : old-gui-form.scm
;; DESCRIPTION : Standard forms for interactive commands
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel old-gui old-gui-form)
  (:use (kernel old-gui old-gui-factory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typed form entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (default-value-of-type type)
  (cond ((in? type '("string" "password" "file")) "")
	((== type "boolean") #f)
	((== type "integer") 0)
	((== type "floating") 0.0)
	((== type "content") "")
	((== type "tree") (tree ""))
	(else "")))

(tm-define (form->type val type)
  (cond ((in? type '("string" "password" "file")) (tree->string val))
	((== type "boolean") (== (tree->string val) "true"))
	((== type "integer") (string->number (tree->string val)))
	((== type "floating") (string->number (tree->string val)))
	((== type "content") (tree->stree val))
	((== type "tree") val)
	(else val)))

(tm-define (type->form val type)
  (cond ((in? type '("string" "password" "file")) (string->tree val))
	((== type "boolean") (string->tree (if val "true" "false")))
	((== type "integer") (string->tree (number->string val)))
	((== type "floating") (string->tree (number->string val)))
	((== type "content") (stree->tree val))
	((== type "tree") val)
	((not (tree? val)) (tree ""))
	(else val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for remembering previous values of form fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (form-load name vars)
  (learned-interactive name))

(tm-define (form-save name vars vals)
  (learn-interactive name (map cons vars vals)))

(tm-define (form-get-proposal var type nr memo suggest)
  (if (null? suggest) (set! nr (- nr 1)))
  (if (and (< nr 0) (> (- nr) (length memo))) (set! nr (- (length memo))))
  (cond ((and (< nr 0) (assoc-ref (list-ref memo (- -1 nr)) var)) =>
	 identity)
	((with l (assoc-ref suggest var)
	   (and l (>= nr 0) (< nr (length l)) (list-ref l nr))) =>
	   (lambda (x) (tree->stree (type->form x type))))
	(else (tree->stree (type->form (default-value-of-type type) type)))))

(tm-define (form-fill-out var type nr memo suggest)
  (with val (form-get-proposal var type nr memo suggest)
    (widget-set! var val)))

(tm-define (form-equalize positions)
  (when (nnull? positions)
    (with pos (cdar positions)
      (map (lambda (x) (cons (car x) pos)) positions))))

(tm-define (form-increment positions plus start end)
  (with inside (lambda (p) (min (max p start) end))
    (map (lambda (x) (cons (car x) (inside (+ (cdr x) plus)))) positions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build (form proto . body)
  (with (name . vars) proto
    (with f (lambda (x) (if (string? x) (cons x #f) (cons (car x) (cadr x))))
      (set! vars (map f vars)))
    `(let* ((form-name ,name)
	    (form-vars (map car ',vars))
	    (form-type ',vars)
	    (form-types (map cdr form-type))
	    (form-suggest '())
	    (form-memo (form-load form-name form-vars))
	    (form-position (map (cut cons <> 0) form-vars)))
       (letrec ((form-auto
		 (lambda (var type)
		   (form-get-proposal var type 0 form-memo form-suggest)))
		(form-field-values
		 (lambda ()
		   (map widget-ref form-vars)))
		(form-return-values
		 (lambda ()
		   (map form->type (form-field-values) form-types)))
		(form-ok?
		 (lambda ()
		   (== (form-field-values)
		       (map type->form (form-return-values) form-types))))
		(form-memorize
		 (lambda ()
		   (form-save form-name form-vars (form-field-values)))))
	 ,(build-content-list body)))))

(tm-build (suggestions var l)
  `(begin
     (set! form-suggest (assoc-set! form-suggest ,var ,l))
     '()))

(tm-build-macro (form-previous)
  `(button "<less>"
     ;;(:button-shape "circular")
     ;;(:button-color "pastel blue")
     (set! form-position (form-equalize form-position))
     (let* ((start (- (if (null? form-suggest) 1 0) (length form-memo)))
	    (lengths (map length (map cdr form-suggest)))
	    (end (if (null? lengths) 0 (max 0 (- (apply max lengths) 1)))))
       (set! form-position (form-increment form-position -1 start end)))
     (for-each (cut form-fill-out <> <> <> form-memo form-suggest)
	       form-vars (map cdr form-type) (map cdr form-position))))

(tm-build-macro (form-next)
  `(button "<gtr>"
     ;;(:button-shape "circular")
     ;;(:button-color "pastel blue")
     (set! form-position (form-equalize form-position))
     (let* ((start (- (if (null? form-suggest) 1 0) (length form-memo)))
	    (lengths (map length (map cdr form-suggest)))
	    (end (if (null? lengths) 0 (max 0 (- (apply max lengths) 1)))))
       (set! form-position (form-increment form-position 1 start end)))
     (for-each (cut form-fill-out <> <> <> form-memo form-suggest)
	       form-vars (map cdr form-type) (map cdr form-position))))

(tm-build-macro (form-cancel)
  `(button "Cancel" (dismiss)))		 

(tm-build-macro (form-done body fun)
  `(button ,body
     (when (form-ok?)
       (with args (form-return-values)
	 (form-memorize)
	 (dismiss)
	 (delayed
	   (:idle 1)
	   (apply ,fun args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building forms for interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-test num den)
  (:argument num "content" "Numerator")
  (:argument den "content" "Denominator")
  (:proposals num '("1" "x" "a+b"))
  (insert `(frac ,num ,den)))

(define (interactive-vars args nr)
  (if (null? args) '()
      (cons (number->string nr)
	    (interactive-vars (cdr args) (+ nr 1)))))

(define (interactive-proposals var proposals)
  (and (nnull? proposals)
       `(suggestions ,var ',proposals)))

(define (interactive-field prompt var type)
  `(,prompt (input ,var :auto)))

(define (interactive-fields prompts vars types)
  (with rows (map interactive-field prompts vars types)
    `(association-tile ,@rows)))

(tm-define (interactive-form fun prompts vars types defaults)
  (:synopsis "Standard form for a simple function application")
  (with name (or (procedure-string-name fun) "Enter function arguments")
    `(form (,name ,@(map list vars types))
       ,@(list-filter (map interactive-proposals vars defaults) identity)
       ,(interactive-fields prompts vars types)
       ===
       (bar
	 (form-previous)
	 (form-next)
	 >>>
	 (form-cancel)
	 (form-done "Ok" ,fun)))))

(tm-define (interactive-popup fun . args)
  (lazy-define-force fun)
  (if (null? args) (set! args (compute-interactive-args fun)))
  (let* ((name (or (procedure-string-name fun) "Enter function arguments"))
	 (fun-args (build-interactive-args fun args 0 #f))
	 (prompts (map car fun-args))
	 (vars (interactive-vars prompts 0))
	 (types (map cadr fun-args))
	 (defaults (map cddr fun-args))
	 (widget (interactive-form fun prompts vars types defaults)))
    (widget-popup name widget)))
