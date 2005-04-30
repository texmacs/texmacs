
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
  (:use (kernel texmacs tm-overload))
  (:export
    ovl-conds ovl-table ; for macro expansion
    property-set! property help
    tm-define-sub tm-define-overloaded tm-define
    tm-define-macro-sub tm-define-macro-overloaded tm-define-macro
    tm-property-sub tm-property-overloaded tm-property
    lazy-define))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables and subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define define-option-table (make-hash-table 100))

(define ovl-table (make-ahash-table))
(define ovl-props-table (make-ahash-table))

(define ovl-conds '())
(define ovl-props '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ovl-add-option l kind opt)
  (cond ((null? l) (list kind opt))
	((== kind (car l))
	 (texmacs-error "ovl-add-option" "Conflicting option"))
	((< kind (car l)) (cons kind (cons opt l)))
	(else (cons (car l)
		    (cons (cadr l)
			  (ovl-add-option (cddr l) kind opt))))))

(define (ovl-add-option! kind opt)
  (set! ovl-conds (ovl-add-option ovl-conds kind opt)))

(define (define-option-mode opt decl)
  (ovl-add-option! 0 (car opt))
  decl)

(define (predicate-option? x)
  (or (and (symbol? x) (string-ends? (symbol->string x) "?"))
      (and (pair? x) (== (car x) 'lambda))))

(define (define-option-context opt decl)
  (if (predicate-option? (car opt))
      (ovl-add-option! 1 (car opt))
      (ovl-add-option! 1 `(lambda (t) (match? t ',(car opt)))))
  decl)

(define (define-option-inside opt decl)
  (define-option-context
    `((lambda (t)
	(and (tm-compound? t)
	     (in? (tm-car t) ',opt))))
    decl))

(define (define-option-case opt decl)
  (ovl-add-option! 2 (list quote (list->vector opt)))
  decl)

(define (define-option-match opt decl)
  (cond ((predicate-option? opt) (ovl-add-option! 3 opt))
	((and (pair? opt) (null? (cdr opt))
	      (predicate-option? (car opt))
	      (list? (cadr decl)) (= (length (cadr decl)) 3))
	 (ovl-add-option! 3 (car opt)))
	(else (ovl-add-option! 3 `(lambda args (match? args ',opt)))))
  decl)

(define (define-option-require opt decl)
  (define-option-match
    `(lambda ,(cdadr decl) ,(car opt))
    decl))

(hash-set! define-option-table :mode define-option-mode)
(hash-set! define-option-table :context define-option-context)
(hash-set! define-option-table :inside define-option-inside)
(hash-set! define-option-table :case define-option-case)
(hash-set! define-option-table :match define-option-match)
(hash-set! define-option-table :require define-option-require)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of overloaded functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter-conds l)
  "Remove conditions which depend on arguments from list"
  (cond ((null? l) l)
	((>= (car l) 2) (filter-conds (cddr l)))
	(else (cons (car l) (cons (cadr l) (filter-conds (cddr l)))))))

(define (property-set! var prop what conds*)
  "Associate a property to a function symbol under conditions"
  (let* ((key (cons var prop))
	 (conds (filter-conds conds*)))
    (ahash-set! ovl-props-table key
		(ovl-insert (ahash-ref ovl-props-table key) what conds))))

(define (property var prop)
  "Retrieve a property of a function symbol"
  (if (procedure? var) (set! var (procedure-name var)))
  (let* ((key (cons var prop)))
    (ovl-resolve (ahash-ref ovl-props-table key) #f)))

(define (property-rewrite l)
  `(property-set! ,@l (list ,@ovl-conds)))

(define ((define-property which) opt decl)
  (set! ovl-props (cons `(',(caadr decl) ,which ',opt) ovl-props))
  decl)

(define ((define-property* which) opt decl)
  (set! ovl-props (cons `(',(caadr decl) ,which (list ,@opt)) ovl-props))
  decl)

(hash-set! define-option-table :type (define-property :type))
(hash-set! define-option-table :synopsis (define-property :synopsis))
(hash-set! define-option-table :args (define-property :args))
(hash-set! define-option-table :returns (define-property :returns))
(hash-set! define-option-table :note (define-property :note))
(hash-set! define-option-table :secure (define-property* :secure))
(hash-set! define-option-table :check-mark (define-property* :check-mark))

(define (help about)
  ;; very provisional
  (cond ((property about :synopsis)
	 (property about :synopsis))
	((procedure-documentation about)
	 (procedure-documentation about))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New style define
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-define-sub head body)
  (if (and (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-define-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-define-overloaded (cons head body))))

(define-macro (tm-define head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-define-sub head body))

(define-macro (tm-define-overloaded head . body)
  (let* ((var (car head))
	 (val `(lambda ,(cdr head) ,@body)))
    (if (and (null? ovl-conds) (not (ahash-ref ovl-table var)))
	`(begin
	   (ahash-set! ovl-table ',var (cons 100 ,val))
	   (define ,head ,@body)
	   ,@(map property-rewrite ovl-props))
	`(begin
	   (ahash-set! ovl-table ',var
		       (ovl-insert (ahash-ref ovl-table ',var) ,val
				   (list ,@ovl-conds)))
	   (define (,var . args)
	     (ovl-apply (ahash-ref ovl-table ',var) args))
	   ,@(map property-rewrite ovl-props)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloaded macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-define-macro-sub head body)
  (if (and (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-define-macro-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-define-macro-overloaded (cons head body))))

(define-macro (tm-define-macro head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-define-macro-sub head body))

(define-macro (tm-define-macro-overloaded head . body)
  (let* ((var (car head))
	 (val `(lambda ,(cdr head) ,@body)))
    (if (and (null? ovl-conds) (not (ahash-ref ovl-table var)))
	`(begin
	   (ahash-set! ovl-table ',var (cons 100 ,val))
	   (define-macro ,head ,@body)
	   ,@(map property-rewrite ovl-props))
	`(begin
	   (ahash-set! ovl-table ',var
		       (ovl-insert (ahash-ref ovl-table ',var) ,val
				   (list ,@ovl-conds)))
	   (define-macro (,var . args)
	     (ovl-apply (ahash-ref ovl-table ',var) args))
	   ,@(map property-rewrite ovl-props)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associating extra properties to existing function symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-property-sub head body)
  (if (null? body)
      (cons 'tm-property-overloaded (cons head body))
      (let ((decl (tm-property-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))))

(define-macro (tm-property head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-property-sub head body))

(define-macro (tm-property-overloaded head . body)
  `(begin
     ,@(map property-rewrite ovl-props)))

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
