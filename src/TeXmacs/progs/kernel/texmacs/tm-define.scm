
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-define.scm
;; DESCRIPTION : Macros for defining TeXmacs functions
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-define)
  (:export
    recursive-car definition-body definition-props ;; for tm-define macro
    tm-define
    help
    lazy-define))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (recursive-car l)
  (if (or (null? l) (not (pair? l))) l
      (recursive-car (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (definition-body l)
  (if (or (null? l) (not (list? (car l))) (not (keyword? (caar l)))) l
      (definition-body (cdr l))))

(define (definition-props l)
  (if (or (null? l) (not (list? (car l))) (not (keyword? (caar l)))) '()
      (cons (if (== (caar l) :check-mark)
		(list :check-mark (cadar l) (list 'unquote (caddar l)))
		(car l))
	    (definition-props (cdr l)))))

(define-macro (tm-define fun . l)
  (with name (recursive-car fun)
    `(begin
       (define ,fun ,@(definition-body l))
       (set-symbol-procedure! ',name ,name)
       (set-procedure-properties! ,name
				  ,(list 'quasiquote (definition-props l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Obtaining help (very provisional)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (help about)
  (cond ((procedure-property about :synopsis)
	 (procedure-property about :synopsis))
	((procedure-documentation about)
	 (procedure-documentation about))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy function declations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (lazy-define module name)
  (with name-star (string->symbol (string-append (symbol->string name) "*"))
    `(define (,name . args)
       (let* ((m (resolve-module ',module))
	      (p (module-ref m '%module-public-interface))
	      (r (module-ref p ',name #f)))
	 (apply r args)))))
