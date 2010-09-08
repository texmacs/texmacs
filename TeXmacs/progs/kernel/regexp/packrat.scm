
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : packrat.scm
;; DESCRIPTION : packrat grammars
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel regexp packrat))

(define-public (scheme->packrat x)
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

(define-public (packrat-define-rule sym l)
  (with gr `(or ,@l)
    ;;(display* "Define " sym " := " l "\n")
    ;;(display* "Packrat= " (scheme->packrat gr) "\n")
    (cpp-packrat-define (symbol->string sym) (scheme->packrat gr))))

(define-public (packrat-define-grammar gr)
  (for-each (lambda (x) (packrat-define-rule (car x) (cdr x))) gr))

(define-public-macro (packrat-rule sym . l)
  `(packrat-define-rule ',sym ',l))

(define-public-macro (packrat-grammar . gr)
  `(packrat-define-grammar ',gr))

(define-public-macro (packrat-parse gr x)
  `(with in ,x
     (if (string? in)
	 (cpp-packrat-parse (symbol->string ',gr) in)
	 (cpp-packrat-parse-tree (symbol->string ',gr) (tm->tree in)))))
