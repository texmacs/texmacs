
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : tm-language.scm
;; DESCRIPTION : formal language support based on packrat grammars
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-language)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define packrat-definition-table (make-ahash-table))

(define (scheme->packrat x)
  (cond ((string? x) (string->tree x))
	((symbol? x) (tree 'symbol (string->tree (symbol->string x))))
	((== x :<) (tm->tree '(tm-open)))
	((== x :/) (string->tree "<|>"))
	((== x :>) (string->tree "</>"))
	((== x :any) (tm->tree '(tm-any)))
	((== x :args) (tm->tree '(tm-args)))
	((== x :leaf) (tm->tree '(tm-leaf)))
	((== x :char) (tm->tree '(tm-char)))
	((== x :cursor) (tm->tree '(tm-cursor)))
	((and (keyword? x) (string-starts? (keyword->string x) "<"))
	 (with s (string-drop (keyword->string x) 1)
	   (string->tree (string-append "<\\" s ">"))))
	((func? x 'or) (tm->tree `(or ,@(map scheme->packrat (cdr x)))))
	((func? x '* 1) (tm->tree `(while ,@(map scheme->packrat (cdr x)))))
	((func? x '+ 1) (tm->tree `(repeat ,@(map scheme->packrat (cdr x)))))
	((func? x '- 2) (tm->tree `(range ,@(map scheme->packrat (cdr x)))))
	((func? x 'not 1) (tm->tree `(not ,@(map scheme->packrat (cdr x)))))
	((func? x 'except 2)
	 (tm->tree `(except ,@(map scheme->packrat (cdr x)))))
	((list? x) (tm->tree `(concat ,@(map scheme->packrat x))))
        (else (texmacs-error "scheme->packrat"
			     "~S is not a packrat grammar" x))))

(define (penalty? x)
  (or (symbol? x) (number? x)))

(define (penalty->string x)
  (if (symbol? x)
      (symbol->string x)
      (number->string x)))

(define (define-rule-one lan sym l)
  (cond ((null? l)
	 (packrat-define lan sym '(or)))
	((and (func? (car l) :type 1) (symbol? (cadar l)))
	 (packrat-property lan sym "type" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :penalty 1) (penalty? (cadar l)))
	 (packrat-property lan sym "right-penalty" (penalty->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :penalty 2)
	      (penalty? (cadar l)) (penalty? (caddar l)))
	 (packrat-property lan sym "left-penalty"
			   (penalty->string (cadar l)))
	 (packrat-property lan sym "right-penalty"
			   (penalty->string (caddar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :spacing 2)
	      (symbol? (cadar l)) (symbol? (caddar l)))
	 (packrat-property lan sym "left-spacing" (symbol->string (cadar l)))
	 (packrat-property lan sym "right-spacing" (symbol->string (caddar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :limits 1) (symbol? (cadar l)))
	 (packrat-property lan sym "limits" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :highlight 1) (symbol? (cadar l)))
	 (packrat-property lan sym "highlight" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :transparent 1) (symbol? (cadar l)))
	 (packrat-property lan sym "highlight" (symbol->string (cadar l)))
	 (packrat-property lan sym "transparent" "true")
	 (define-rule-one lan sym (cdr l)))
	((func? (car l) :atomic 0)
	 (packrat-property lan sym "atomic" "true")
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :focus 1) (symbol? (cadar l)))
	 (packrat-property lan sym "focus" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :selectable 1) (symbol? (cadar l)))
	 (packrat-property lan sym "selectable" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((and (func? (car l) :operator 1) (symbol? (cadar l)))
	 (packrat-property lan sym "operator" "true")
	 (packrat-property lan sym "associativity" (symbol->string (cadar l)))
	 (define-rule-one lan sym (cdr l)))
	((func? (car l) :operator 0)
	 (packrat-property lan sym "operator" "true")
	 (define-rule-one lan sym (cdr l)))
	(else
          (ahash-set! packrat-definition-table (list lan sym) l)
	  ;;(display* "Define " lan "::" sym " := " l "\n")
	  ;;(display* "Packrat= " (scheme->packrat `(or ,@l)) "\n")
	  (packrat-define lan sym (scheme->packrat `(or ,@l))))))

(define (attach-macros lan l)
  (for (x l)
    (when (and (list-2? x) (string? (car x)) (symbol? (cadr x)))
      (packrat-property lan (car x) "macro" (symbol->string (cadr x))))))

(define (define-rule-impl lan x)
  (cond ((func? x :synopsis)
	 (noop))
	((func? x 'define)
	 (define-rule-one lan (symbol->string (cadr x)) (cddr x)))
	((func? x 'inherit 1)
	 (packrat-inherit lan (symbol->string (cadr x))))
        ((func? x 'attach-macro)
         (attach-macros lan (cdr x)))
	(else (error "invalid packrat rule"))))

(tm-define (define-language-impl lan gr)
  (for-each (lambda (x) (define-rule-impl lan x)) gr))

(tm-define (get-packrat-definition lan sym)
  (ahash-ref packrat-definition-table (list lan sym)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy language definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-language-waiting (make-ahash-table))
(define lazy-language-done (make-ahash-table))

(tm-define (lazy-language-impl lan m)
  (ahash-set! lazy-language-waiting lan m))

(tm-define (lazy-language-force-impl lan)
  (when (not (ahash-ref lazy-language-done lan))
    (and-with m (ahash-ref lazy-language-waiting lan)
      ;;(display* "Lazy definition of " lan " in " m "\n")
      (ahash-remove! lazy-language-waiting lan)
      (ahash-set! lazy-language-done lan #t)
      (module-load m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for language definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (define-language lan . gr)
  (:synopsis "Define the formal language @lan")
  `(define-language-impl (symbol->string ',lan) ',gr))

(tm-define-macro (lazy-language m . lans)
  (:synopsis "Promise that the languages @lans are defined in the module @m")
  `(for-each (lambda (lan) (lazy-language-impl lan ',m)) ',lans))

(tm-define-macro (lazy-language-force lan)
  (:synopsis "Execute promise to define the language @lan")
  `(lazy-language-force-impl ',lan))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic routines based on current language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (semantic-end lan gr in)
  (:synopsis "Get rightmost path until where @x can be parsed in @lan")
  `(let* ((lan2 (symbol->string ',lan))
	  (gr2 (symbol->string ',gr))
	  (in2 ,in))
     (packrat-parse lan2 gr2 in2)))

(tm-define-macro (semantic-context lan gr in pos)
  (:synopsis "Get semantic selections englobing @in at @pos")
  `(let* ((lan2 (symbol->string ',lan))
	  (gr2 (symbol->string ',gr))
	  (in2 ,in)
	  (pos1 ,pos)
	  (pos2 (if (number? pos1) (list pos1) pos1)))
     (packrat-context lan2 gr2 in2 pos2)))
