
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-factory.scm
;; DESCRIPTION : Different widget construction routines
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-factory)
  (:use (kernel gui gui-widget)))

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

(tm-define (boolean->string b)
  (if b "true" "false"))

(tm-define (widget-entry name val type)
  (when (== val :auto)
    (set! val `(begin
		 (assoc-set! form-type ,name ,type)
		 (form-auto ,name ,type))))
  (when (== type "boolean")
    (set! val `(boolean->string ,val)))
  val)

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
    (List (List (Quote 'form-toggle) name
		(widget-entry name val "boolean")))))

(tm-define (build-widget w)
  (:case button-toggle)
  (with (cmd name val . body) w
    (List (List (Quote 'form-button-toggle) name
		(widget-entry name val "boolean")
		(apply Concat (build-widgets body))))))

(tm-define (build-widget w)
  (:case alternatives)
  (with (cmd name val . body) w
    (List (List (Quote 'form-alternatives) name
		(widget-entry name val "string")
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
      (List (List (Quote f) name
		  (widget-entry name val "content"))))))

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

(tm-define-macro (define-widget proto . body)
  `(tm-define ,proto
     ,(widget-armour "default" (build-widgets body))))
