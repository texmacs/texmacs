
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-define.scm
;; DESCRIPTION : Macros for defining TeXmacs functions
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-define)
  (:inherit (kernel texmacs tm-overload)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables and subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public define-option-table (make-hash-table 100))

(define-public ovl-table (make-ahash-table))
(define-public ovl-conds '())

(define ovl-props-table (make-ahash-table))
(define ovl-props '())

(define (ca*r x) (if (pair? x) (ca*r (car x)) x))
(define (ca*adr x) (ca*r (cadr x)))

(define (lambda* head body)
  (if (pair? head)
      (lambda* (car head) `((lambda ,(cdr head) ,@body)))
      (car body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (define-option-profile opt decl)
  (if (in? (get-look-and-feel) opt) decl '(begin)))

(define (conditions-insert! kind opt)
  (set! ovl-conds (conditions-insert ovl-conds kind opt)))

(define (define-option-mode opt decl)
  (conditions-insert! 0 (car opt))
  decl)

(define-public (predicate-option? x)
  (or (and (symbol? x) (string-ends? (symbol->string x) "?"))
      (and (pair? x) (== (car x) 'lambda))))

(define (define-option-context opt decl)
  (if (predicate-option? (car opt))
      (conditions-insert! 1 (car opt))
      (conditions-insert! 1 `(lambda (t) (match? t ',(car opt)))))
  decl)

(define (define-option-inside opt decl)
  (define-option-context
    `((lambda (t)
	(and (tm-compound? t)
	     (in? (tm-car t) ',opt))))
    decl))

(define (define-option-case opt decl)
  (conditions-insert! 2 (list quote (list->vector opt)))
  decl)

(define (define-option-match opt decl)
  (cond ((predicate-option? opt) (conditions-insert! 3 opt))
	((and (pair? opt) (null? (cdr opt))
	      (predicate-option? (car opt))
	      (list? (cadr decl)) (= (length (cadr decl)) 3))
	 (conditions-insert! 3 (car opt)))
	(else (conditions-insert! 3 `(lambda args (match? args ',opt)))))
  decl)

(define (define-option-require opt decl)
  (define-option-match
    `(lambda ,(cdadr decl) ,(car opt))
    decl))

(hash-set! define-option-table :profile define-option-profile)
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

(define-public (property-set! var prop what conds*)
  "Associate a property to a function symbol under conditions"
  (let* ((key (cons var prop))
	 (conds (filter-conds conds*)))
    (ahash-set! ovl-props-table key
		(ovl-insert (ahash-ref ovl-props-table key) what conds))))

(define-public (property var prop)
  "Retrieve a property of a function symbol"
  (if (procedure? var) (set! var (procedure-name var)))
  (let* ((key (cons var prop)))
    (ovl-resolve (ahash-ref ovl-props-table key) #f)))

(define (property-rewrite l)
  `(property-set! ,@l (list ,@ovl-conds)))

(define ((define-property which) opt decl)
  (set! ovl-props (cons `(',(ca*adr decl) ,which ',opt) ovl-props))
  decl)

(define ((define-property* which) opt decl)
  (set! ovl-props (cons `(',(ca*adr decl) ,which (list ,@opt)) ovl-props))
  decl)

(define (compute-arguments decl)
  (cond ((pair? (cadr decl)) (cdadr decl))
	((and (pair? (caddr decl)) (== (caaddr decl) 'lambda))
	 (cadr (caddr decl)))
	(else
	 (texmacs-error "compute-arguments" "Bad argument documentation"))))

(define (define-option-argument opt decl)
  (let* ((var (ca*adr decl))
	 (args (compute-arguments decl))
	 (arg (list :argument (car opt))))
    (set! ovl-props (cons `(',var :arguments ',args) ovl-props))
    (set! ovl-props (cons `(',var ',arg ',(cdr opt)) ovl-props))
    decl))

(define (define-option-default opt decl)
  (let* ((var (ca*adr decl))
	 (arg (list :default (car opt))))
    (set! ovl-props (cons `(',var ',arg (lambda () ,@(cdr opt))) ovl-props))
    decl))

(define (define-option-proposals opt decl)
  (let* ((var (ca*adr decl))
	 (arg (list :proposals (car opt))))
    (set! ovl-props (cons `(',var ',arg (lambda () ,@(cdr opt))) ovl-props))
    decl))

(hash-set! define-option-table :type (define-property :type))
(hash-set! define-option-table :synopsis (define-property :synopsis))
(hash-set! define-option-table :returns (define-property :returns))
(hash-set! define-option-table :note (define-property :note))
(hash-set! define-option-table :argument define-option-argument)
(hash-set! define-option-table :default define-option-default)
(hash-set! define-option-table :proposals define-option-proposals)
(hash-set! define-option-table :secure (define-property* :secure))
(hash-set! define-option-table :check-mark (define-property* :check-mark))
(hash-set! define-option-table :interactive (define-property* :interactive))

(define-public (help about)
  ;; very provisional
  (cond ((property about :synopsis)
	 (property about :synopsis))
	((procedure-documentation about)
	 (procedure-documentation about))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloaded functions with properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-define-sub head body)
  (if (and (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-define-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-define-overloaded (cons head body))))

(define-public-macro (tm-define head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-define-sub head body))

(define-public-macro (tm-define-overloaded head . body)
  (let* ((var (ca*r head))
	 (val (lambda* head body))
	 (default? (and (null? ovl-conds) (not (ahash-ref ovl-table var)))))
    `(begin
       (set! temp-module ,(current-module))
       (set! temp-value ,val)
       ,(if default?
	    `(ahash-set! ovl-table ',var (cons 100 temp-value))
	    `(ahash-set! ovl-table ',var
			 (ovl-insert (ahash-ref ovl-table ',var) temp-value
				     (list ,@ovl-conds))))
       (set-current-module texmacs-user)
       (cond ((not (procedure? temp-value))
	      (define-public ,head temp-value))
	     (,default?
	      (define-public ,var temp-value))
	     (else
	      (define-public (,var . args)
		(ovl-apply (ahash-ref ovl-table ',var) args))))
       (set-current-module temp-module)
       ,@(map property-rewrite ovl-props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloaded macros with properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-define-macro-sub head body)
  (if (and (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-define-macro-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-define-macro-overloaded (cons head body))))

(define-public-macro (tm-define-macro head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-define-macro-sub head body))

(define-public-macro (tm-define-macro-overloaded head . body)
  (let* ((var (ca*r head))
	 (val (lambda* head body))
	 (default? (and (null? ovl-conds) (not (ahash-ref ovl-table var)))))
    `(begin
       (set! temp-module ,(current-module))
       (set! temp-value ,val)
       ,(if default?
	    `(ahash-set! ovl-table ',var (cons 100 temp-value))
	    `(ahash-set! ovl-table ',var
		       (ovl-insert (ahash-ref ovl-table ',var) temp-value
				   (list ,@ovl-conds))))
       (set-current-module texmacs-user)
       (define-public-macro (,var . args)
	 (ovl-apply (ahash-ref ovl-table ',var) args))
       (set-current-module temp-module)
       ,@(map property-rewrite ovl-props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Associating extra properties to existing function symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-property-sub head body)
  (if (null? body)
      (cons 'tm-property-overloaded (cons head body))
      (let ((decl (tm-property-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))))

(define-public-macro (tm-property head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-property-sub head body))

(define-public-macro (tm-property-overloaded head . body)
  `(begin
     ,@(map property-rewrite ovl-props)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieve definitions and redefinition of functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-definition-sub head body)
  (if (and (nnull? body) (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-definition-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-definition-overloaded (cons head body))))

(define-public-macro (tm-definition head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-definition-sub head body))

(define-public-macro (tm-definition-overloaded head . body)
  (with var (ca*r head)
    `(ovl-find (ahash-ref ovl-table ',var) (list ,@ovl-conds))))

(define-public (tm-redefine-sub head body)
  (if (and (pair? (car body)) (keyword? (caar body)))
      (let ((decl (tm-redefine-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))
      (cons 'tm-redefine-overloaded (cons head body))))

(define-public-macro (tm-redefine head . body)
  (set! ovl-conds '())
  (set! ovl-props '())
  (tm-redefine-sub head body))

(define-public-macro (tm-redefine-overloaded head . body)
  (let* ((var (ca*r head))
	 (val (lambda* head body)))
    `(begin
       (ahash-set! ovl-table ',var
		   (ovl-insert (ahash-ref ovl-table ',var) ,val
			       (list ,@ovl-conds)))
       (set! ,var (lambda args (ovl-apply (ahash-ref ovl-table ',var) args)))
       ,@(map property-rewrite ovl-props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy function declations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-define-table (make-ahash-table))

(define-public (not-define-option? item)
  (not (and (pair? item) (keyword? (car item)))))

(define-public (lazy-define-one module opts name)
  (let* ((old (ahash-ref lazy-define-table name))
	 (new (if old (cons module old) (list module))))
    (ahash-set! lazy-define-table name new))
  (with name-star (string->symbol (string-append (symbol->string name) "*"))
    `(if (not (tm-definition ,@opts ,name))
	 (tm-define (,name . args)
	   ,@opts
	   (let* ((m (resolve-module ',module))
		  (p (module-ref texmacs-user '%module-public-interface))
		  (r (module-ref p ',name #f)))
	     (if (not r)
		 (texmacs-error "lazy-define"
				,(string-append "Could not retrieve "
						(symbol->string name))))
	     (apply r args))))))

(define-public-macro (lazy-define module . names)
  (receive (opts real-names) (list-break names not-define-option?)
    `(begin
       ,@(map (lambda (name) (lazy-define-one module opts name)) names))))

(define-public (lazy-define-force name)
  (if (procedure? name) (set! name (procedure-name name)))
  (let* ((im (ahash-ref lazy-define-table name))
	 (modules (if im im '())))
    (ahash-remove! lazy-define-table name)
    (map module-load modules)))
