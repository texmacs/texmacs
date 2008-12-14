
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-keywords.scm
;; DESCRIPTION : additional rendering macros written in scheme
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc tm-keywords))

(define kws (string-load (string->url "$TEXMACS_PATH/progs/tm-mode.el")))
(define kwo (string->object (string-append "(" kws ")")))

(define (kw-transform l)
  (cond ((null? l) l)
	((func? (car l) 'setq)
	 (cons (cons 'tm-define (cdar l)) (kw-transform (cdr l))))
	(else (kw-transform (cdr l)))))

(eval (cons 'begin (kw-transform kwo)))

(define indent-arity-table (make-ahash-table))

(define (indent-set-arity x nr)
  (cond ((symbol? x) (indent-set-arity (symbol->string x) nr))
	((string? x) (ahash-set! indent-arity-table x nr))
	((list? x) (for-each (cut indent-set-arity <> nr) x))))

(indent-set-arity nullary-indent 0)
(indent-set-arity unary-indent   1)
(indent-set-arity binary-indent  2)
(indent-set-arity ternary-indent 3)

(tm-define (indent-get-arity s)
  (:synopsis "get indentation arity of keyword @s")
  (if (symbol? s)
      (indent-get-arity (symbol->string s))
      (ahash-ref indent-arity-table s)))
