
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : grammar.scm
;; DESCRIPTION : packrat grammars
;; COPYRIGHT   : (C) 2009  Francis Jamet, Joris van der Hoeven
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel regexp grammar))

(define-public (scheme->grammar x)
  (cond ((string? x) (string->tree x))
	((symbol? x) (tree 'DOLLAR (string->tree (symbol->string x))))
	((func? x 'or) (tm->tree `(OR ,@(map scheme->grammar (cdr x)))))
	((func? x '* 1) (tm->tree `(STAR ,@(map scheme->grammar (cdr x)))))
	((func? x '- 2) (tm->tree `(RANGE ,@(map scheme->grammar (cdr x)))))
	((list? x) (tm->tree `(CONCAT ,@(map scheme->grammar x))))
        (else (error "invalid grammar"))))

(define-public (grammar-define-rule sym l)
  (with gr `(or ,@l)
    (display* "Define " sym " := " l "\n")
    (display* "Grammar= " (scheme->grammar gr) "\n")
    (define-grammar-rule
      (tree 'DOLLAR (symbol->string sym))
      (scheme->grammar gr))))

(define-public (grammar-define-grammar gr)
  (for-each (lambda (x) (grammar-define-rule (car x) (cdr x))) gr))

(define-public-macro (define-rule sym . l)
  `(grammar-define-rule ',sym ',l))

(define-public-macro (define-grammar . gr)
  `(grammar-define-grammar ',gr))

(define-public-macro (parse gr s)
  `(grammar-parse (scheme->grammar ',gr) ,s))
