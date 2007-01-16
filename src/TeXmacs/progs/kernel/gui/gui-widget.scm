
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

(define widget-call-back-nr 0)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-widget w)
  (:synopsis "Build a lazy widget constructor from a scheme program @w")
  (cond ((list? w) (List (Cons (Quote (car w)) (build-widgets (cdr w)))))
	((== w :::) (List (List (Quote 'gui-vspace))))
	((== w '---) (List (List (Quote 'gui-hrule))))
	((== w '>>>) (List (List (Quote 'gui-tab))))
	(else (List w))))

(tm-define (build-widgets ws)
  `(append ,@(map build-widget ws)))

(tm-define (build-widget w)
  (:case let)
  (with (cmd bindings . body) w
    `(let* ,bindings
       ,(build-widgets body))))

(tm-define (build-widget w)
  (:case action)
  (with (cmd body . cmds) w
    (List (List (Quote 'action)
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
    (List (List (Quote 'form-circ-toggle)
		name
		(if val "true" "false")))))

(tm-define (build-widget w)
  (:case field)
  (with (cmd name val) w
    (List (List (Quote 'form-line-input)
		name
		(apply Concat (build-widget val))))))

(define (build-cell w)
  (List (List (Quote 'cell)
	      (apply Concat (build-widget w)))))

(define (build-cells ws)
  `(append ,@(map build-cell ws)))

(define (build-row l)
  (List (Cons (Quote 'row)
	      (build-cells l))))

(define (build-rows ls)
  `(append ,@(map build-row ls)))

(define (get-options-sub l)
  (if (and (nnull? l) (keyword? (car l)))
      (with (options . args) (get-options-sub (cdr l))
	(cons (cons (car l) options) args))
      (cons '() l)))

(define (get-options l)
  (get-options-sub (cdr l)))

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

(define-public-macro (define-widget proto . body)
  `(tm-define ,proto
     (tm->tree (cons 'document ,(build-widgets body)))))
