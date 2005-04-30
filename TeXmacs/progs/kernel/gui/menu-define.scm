
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-define.scm
;; DESCRIPTION : Definition of menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-define)
  (:use (kernel regexp regexp-match))
  (:export
    make-promise promise-source ;; for menu-widget and kbd-define
    menu-set! menu-get ;; low level
    menu-pre ;; for menu-dynamic, menu-bind and menu-extend macros
    menu-dynamic menu-bind menu-extend
    menu-lazy-menu ;; for lazy-menu macro
    lazy-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTENTION: in menu-pre-entry, we also perform some convenience
;; rewritings in order to let menus match the grammar below.
;; The convience rewritings allow the use of ..., (check :string? :1),
;; (shortcut :string?) and multiple actions in menu entries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-grammar
  (:menu-label (:or
    :string?
    (text :tuple? :string?)
    (icon :string?)
    (balloon :menu-label :string?)))
  (:menu-wide-label (:or
    :menu-label
    (check :menu-wide-label :string? :1)
    (shortcut :menu-wide-label :string?)))
  (:menu-item (:or
    ---
    |
    (group :string?)
    (:menu-wide-label :1)
    (symbol :string? :*)
    (horizontal :menu-item-list)
    (vertical :menu-item-list)
    (-> :menu-label :menu-item-list)
    (=> :menu-label :menu-item-list)
    (tile :integer? :menu-item-list)
    (if :1 :menu-item-list)
    (when :1 :menu-item-list)
    (link :1)
    (promise :1)
    (:menu-item-list)))
  (:menu-item-list (:repeat :menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation of menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-format-error where which)
  (texmacs-error where "Bad menu format in: ~S" which))

(define (make-promise x)
  (list 'unquote `(lambda () ,x)))

(define (promise-source action)
  (and (procedure? action)
       (with source (procedure-source action)
	 (and (== (car source) 'lambda)
	      (== (cadr source) '())
	      (null? (cdddr source))
	      (caddr source)))))

(define (menu-label-add-dots l)
  (cond ((match? l ':string?) (string-append l "..."))
	((match? l '(text :tuple? :string?))
	 `(text ,(cadr l) ,(string-append (caddr l) "...")))
	((match? l '(icon :string?)) l)
	(else `(,(car l) ,(menu-label-add-dots (cadr l)) ,(caddr l)))))

(define (menu-pre-wide-label l)
  (cond ((string? l) l)
	((== (car l) 'text) l)
	((== (car l) 'icon) l)
	((== (car l) 'balloon)
	 (list 'balloon (menu-pre-wide-label (cadr l)) (caddr l)))
	((== (car l) 'shortcut)
	 (list 'shortcut (menu-pre-wide-label (cadr l)) (caddr l)))
	((== (car l) 'check)
	 (list 'check (menu-pre-wide-label (cadr l)) (caddr l)
	       (make-promise (cadddr l))))
	(else l)))

(define (menu-pre-entry p)
  (if (= (length p) 2)
      (list (menu-pre-wide-label (car p)) (make-promise (cadr p)))
      (cond ((null? (cdr p)) (menu-format-error "menu-pre-entry" p))
	    ;; convenience rewritings
	    ((match? (cdr p) '(... :1 :*))
	     (menu-pre-entry `(,(menu-label-add-dots (car p)) ,@(cddr p))))
	    ((match? (cdr p) '(:string? :1 :*))
	     (menu-pre-entry `((shortcut ,(car p) ,(cadr p)) ,@(cddr p))))
	    ((match? (cdr p) '((check :2) :1 :*))
	     (menu-pre-entry `((check ,(car p) ,@(cdadr p)) ,@(cddr p))))
	    (else (menu-pre-entry `(,(car p) (begin ,@(cdr p))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-pre-list l)
  (map menu-pre l))

(define (menu-pre p)
  (if (pair? p)
      (cond ((string? (car p)) (menu-pre-entry p))
	    ((symbol? (car p))
	     (with result (ahash-ref menu-pre-table (car p))
	       (if (or (not result) (not (match? (cdr p) (car result))))
		   (menu-pre-list p)
		   ((cadr result) p))))
	    ((match? (car p) ':menu-wide-label) (menu-pre-entry p))
	    (else (menu-pre-list p)))
      (cond ((== p '---) p)
	    ((== p '|) p)
	    ((== p '()) p)
	    (else (menu-format-error "menu-pre" p)))))

(define-table menu-pre-table
  (group (:string?) ,(lambda (p) p))
  (symbol (:string? :*) ,(lambda (p) p))
  (link (:1) ,(lambda (p) p))
  (horizontal (:*)
    ,(lambda (p) `(horizontal ,@(menu-pre-list (cdr p)))))
  (vertical (:*)
    ,(lambda (p) `(vertical ,@(menu-pre-list (cdr p)))))
  (-> (:menu-label :*)
    ,(lambda (p) `(-> ,(cadr p) ,@(menu-pre-list (cddr p)))))
  (=> (:menu-label :*)
    ,(lambda (p) `(=> ,(cadr p) ,@(menu-pre-list (cddr p)))))
  (tile (:integer? :*)
    ,(lambda (p) `(tile ,(cadr p) ,@(menu-pre-list (cddr p)))))
  (if (:1 :*)
    ,(lambda (p) `(if ,(make-promise (cadr p)) ,@(menu-pre-list (cddr p)))))
  (when (:1 :*)
    ,(lambda (p) `(when ,(make-promise (cadr p)) ,@(menu-pre-list (cddr p)))))
  (promise (:1)
    ,(lambda (p) `(promise ,(make-promise (cadr p))))))

(ahash-set! menu-pre-table 'unquote `((:1) ,(lambda (p) p)))
(ahash-set! menu-pre-table 'unquote-splicing `((:1) ,(lambda (p) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-lazy-table (make-ahash-table))

(define (menu-lazy-force name)
  (if (ahash-ref menu-lazy-table name)
      (with module (ahash-ref menu-lazy-table name)
	(ahash-remove! menu-lazy-table name)
	(module-load module))))

(define (menu-lazy-menu module name)
  (menu-lazy-force name)
  (ahash-set! menu-lazy-table name module))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulating menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define menu-bindings-table (make-ahash-table))

(define (menu-match? what label)
  (or (== what label)
      (match? label `(text :1 ,what))
      (and (match? label '((:or balloon check shortcut) :1 :*))
	   (menu-match? what (cadr label)))))

(define (menu-set-in! menu name what)
  (cond ((or (null? menu) (npair? menu)) #f)
	;((and (null? (cdr name))
	;      (match? (car menu) '(:menu-wide-label :*))
	;      (menu-match? (car name) (caar menu)))
	; (set-car! (cdar menu) what))
	((and (match? (car menu) '((:or -> =>) :menu-label :*))
	      (menu-match? (car name) (cadar menu)))
	 (if (null? (cdr name))
	     (begin (set-cdr! (cdar menu) what) what)
	     (menu-set-in! (cddar menu) (cdr name) what)))
	((match? (car menu) '(link :1))
	 (let ((ok (menu-set! (cons (cadar menu) name) what)))
	   (if ok ok (menu-set-in! (cdr menu) name what))))
	(else (menu-set-in! (cdr menu) name what))))

(define (menu-set! name menu)
  (menu-lazy-force name)
  (cond ((null? name) (menu-set! 'texmacs-menu menu))
	((nlist? name) (menu-set! (list name) menu))
	((symbol? (car name))
	 (if (null? (cdr name))
	     (ahash-set! menu-bindings-table (car name) menu)
	     (let ((old-menu (ahash-ref menu-bindings-table (car name))))
	       (if old-menu (menu-set-in! old-menu (cdr name) menu) #f))))
	((string? (car name))
	 (menu-set! (cons 'texmacs-menu name) menu))
	(else #f)))

(define (menu-get-in menu name)
  (cond ((null? name) menu)
	((or (null? menu) (npair? menu)) #f)
	;((and (null? (cdr name))
	;      (match? (car menu) '(:menu-wide-label :*))
	;      (menu-match? (car name) (caar menu)))
	; (cadar menu))
	((and (match? (car menu) '((:or -> =>) :menu-label :*))
	      (menu-match? (car name) (cadar menu)))
	 (menu-get-in (cddar menu) (cdr name)))
	((match? (car menu) '(link :1))
	 (let ((submenu (menu-get (cons (cadar menu) name))))
	   (if submenu submenu (menu-get-in (cdr menu) name))))
	(else (menu-get-in (cdr menu) name))))

(define (menu-get name)
  (menu-lazy-force name)
  (cond ((null? name) (menu-get 'texmacs-menu))
	((nlist? name) (menu-get (list name)))
	((symbol? (car name))
	 (let ((menu (ahash-ref menu-bindings-table (car name))))
	   (if menu (menu-get-in menu (cdr name)) #f)))
	((string? (car name))
	 (menu-get (cons 'texmacs-menu name)))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (menu-dynamic . body)
  (list 'quasiquote (menu-pre body)))

(define-macro (menu-bind name . body)
  `(menu-set! ',name ,(list 'quasiquote (menu-pre body))))

(define-macro (menu-extend name . body)
  `(menu-set! ',name (append (menu-get ',name)
			     ,(list 'quasiquote (menu-pre body)))))

(define-macro (lazy-menu module . menus)
  `(for-each (lambda (x) (menu-lazy-menu ',module x)) ',menus))
