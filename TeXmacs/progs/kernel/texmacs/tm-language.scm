
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

(define (scheme->packrat x)
  (cond ((string? x) (string->tree x))
	((symbol? x) (tree 'symbol (string->tree (symbol->string x))))
	((== x :/) (string->tree "<|>"))
	((== x :>) (string->tree "</>"))
	((keyword? x)
	 (with s (string-drop (keyword->string x) 1)
	   (string->tree (string-append "<\\" s ">"))))
	((func? x 'or) (tm->tree `(or ,@(map scheme->packrat (cdr x)))))
	((func? x '* 1) (tm->tree `(while ,@(map scheme->packrat (cdr x)))))
	((func? x '+ 1) (tm->tree `(repeat ,@(map scheme->packrat (cdr x)))))
	((func? x '- 2) (tm->tree `(range ,@(map scheme->packrat (cdr x)))))
	((list? x) (tm->tree `(concat ,@(map scheme->packrat x))))
        (else (error "invalid packrat"))))

(define (define-rule-impl lan sym l)
  (with gr `(or ,@l)
    ;;(display* "Define " sym " := " l "\n")
    ;;(display* "Packrat= " (scheme->packrat gr) "\n")
    (cpp-packrat-define (symbol->string sym) (scheme->packrat gr))))

(tm-define (define-language-impl lan gr)
  (for-each (lambda (x) (define-rule-impl lan (car x) (cdr x))) gr))

(tm-define-macro (define-language lan . gr)
  (:synopsis "Define the formal language @lan")
  `(define-language-impl ',lan ',gr))

(tm-define-macro (semantic-end lan gr x)
  (:synopsis "Get rightmost path until where @x can be parsed in @lan")
  `(with in ,x
     (if (string? in)
	 (cpp-packrat-parse (symbol->string ',gr) in)
	 (cpp-packrat-parse-tree (symbol->string ',gr) (tm->tree in)))))
