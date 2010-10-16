
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-define.scm
;; DESCRIPTION : Definition of menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-define)
  (:use (kernel regexp regexp-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ATTENTION: in menu-pre-entry, we also perform some convenience
;; rewritings in order to let menus match the grammar below.
;; The convience rewritings allow the use of ..., (check :string? :%1),
;; (shortcut :string?) and multiple actions in menu entries.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-regexp-grammar
  (:menu-label (:or
    :string?
    (concat :*)
    (verbatim :%1)
    (text :tuple? :string?)
    (icon :string?)
    (balloon :menu-label :string?)))
  (:menu-wide-label (:or
    :menu-label
    (check :menu-wide-label :string? :%1)
    (shortcut :menu-wide-label :string?)))
  (:menu-item (:or
    ---
    |
    (group :string?)
    (:menu-wide-label :%1)
    (symbol :string? :*)
    (horizontal :menu-item-list)
    (vertical :menu-item-list)
    (-> :menu-label :menu-item-list)
    (=> :menu-label :menu-item-list)
    (tile :integer? :menu-item-list)
    (if :%1 :menu-item-list)
    (when :%1 :menu-item-list)
    (link :%1)
    (promise :%1)
    (:menu-item-list)))
  (:menu-item-list (:repeat :menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation of menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-format-error where which)
  (texmacs-error where "Bad menu format in: ~S" which))

(define-public (make-promise x)
  "Helper routines for menu-widget and kbd-define"
  (list 'unquote `(lambda () ,x)))

(define-public (promise-source action)
  "Helper routines for menu-widget and kbd-define"
  (and (procedure? action)
       (with source (procedure-source action)
	 (and (== (car source) 'lambda)
	      (== (cadr source) '())
	      (null? (cdddr source))
	      (caddr source)))))

(define-public (menu-label-add-dots l)
  (cond ((match? l ':string?) (string-append l "..."))
	((match? l '(concat :*))
	 `(,@(cDr l) ,(menu-label-add-dots (cAr l))))
	((match? l '(verbatim :*))
	 `(,@(cDr l) ,(menu-label-add-dots (cAr l))))
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
	    ((match? (cdr p) '(... :%1 :*))
	     (menu-pre-entry `(,(menu-label-add-dots (car p)) ,@(cddr p))))
	    ((match? (cdr p) '(:string? :%1 :*))
	     (menu-pre-entry `((shortcut ,(car p) ,(cadr p)) ,@(cddr p))))
	    ((match? (cdr p) '((check :%2) :%1 :*))
	     (menu-pre-entry `((check ,(car p) ,@(cdadr p)) ,@(cddr p))))
	    (else (menu-pre-entry `(,(car p) (begin ,@(cdr p))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preparation of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-pre-list l)
  (map menu-pre l))

(define-public (menu-pre p)
  "Helper routine for menu-dynamic, menu-bind and menu-extend macros"
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
  (symbol (:string? :*)
    ,(lambda (p)
       (if (<= (length p) 2) p
	   `(symbol ,(cadr p) ,(make-promise (caddr p))))))
  (link (:%1) ,(lambda (p) p))
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
  (if (:%1 :*)
    ,(lambda (p) `(if ,(make-promise (cadr p)) ,@(menu-pre-list (cddr p)))))
  (when (:%1 :*)
    ,(lambda (p) `(when ,(make-promise (cadr p)) ,@(menu-pre-list (cddr p)))))
  (promise (:%1)
    ,(lambda (p) `(promise ,(make-promise (cadr p))))))

(ahash-set! menu-pre-table 'unquote `((:%1) ,(lambda (p) p)))
(ahash-set! menu-pre-table 'unquote-splicing `((:%1) ,(lambda (p) p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (menu-dynamic . body)
  (list 'quasiquote (menu-pre body)))

(define-public-macro (menu-bind name . body)
  (receive (opts real-body) (list-break body not-define-option?)
    `(tm-define (,name) ,@opts ,(list 'quasiquote (menu-pre real-body)))))

(define-public-macro (menu-extend name . body)
  (receive (opts real-body) (list-break body not-define-option?)
    `(tm-redefine ,name ,@opts
       (with old-menu (tm-definition ,name ,@opts)
	 (lambda () (append (old-menu)
			    ,(list 'quasiquote (menu-pre body))))))))

(define-public-macro (lazy-menu module . menus)
  `(begin
     (lazy-define ,module ,@menus)
     (delayed
       (:idle 500)
       (import-from ,module))))
