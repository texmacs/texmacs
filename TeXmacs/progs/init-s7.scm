
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-s7.scm
;; DESCRIPTION : s7-specific start of the initialization
;; COPYRIGHT   : (C) 1999-2020  Joris van der Hoeven & Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loaded by init-texmacs.scm, first thing, when TeXmacs runs on s7.
;; Everything specific to s7 happens here, up to the kernel's
;; compatibility module; the common initialization is in init-texmacs.scm.

;; We use s7's native (run-time) define-macro.  We used to alias it to
;; define-expansion (read-time macros), but s7 10 did not find expansions
;; defined outside the rootlet at read time, so TeXmacs macros effectively
;; ran as run-time macros anyway.  s7 11 does find them, and then also
;; expands them inside quasiquoted templates like `($texmacs-output ,@l).

(define primitive-symbol? symbol?)
(set! symbol? (lambda (s) (and (not (keyword? s)) (primitive-symbol? s))))

;; S7 loads by default in rootlet and eval in curlet
;; but we prefer to load and eval into *texmacs-user-module*
;; (the current toplevel)
;; FIXME: we have to clarify the situation with *current-module* when evaluating
;; in a different environment. In Guile *current-module* is set/reset.

(varlet (rootlet) '*current-module* (curlet))
(let ()
  (define primitive-load load)
  (define primitive-eval eval)
  (define primitive-catch catch)
  
  (varlet (rootlet) 'tm-eval (lambda (obj) (eval obj *texmacs-user-module*)))
  (set! load (lambda (file . env) (primitive-load file (if (null? env) *current-module* (car env)))))
  (set! eval (lambda (obj . env)
    (let ((res (primitive-eval obj (if (null? env) *current-module* (car env)))))
    ;;(format #t "Eval: ~A -> ~A\n" obj res)
    res)
    ))
    
  (set! catch (lambda ( key cl hdl )
    (primitive-catch key cl
      (lambda (type . rest)
        (let ((info (if (pair? rest) (car rest) '())))
          (if (pair? info)
              (hdl type "[not-implemented]" (car info) (cdr info))
              (hdl type "[not-implemented]" "" info)))))))
  )

(define developer-mode? #f)

;(display "Booting TeXmacs kernel functionality\n")
(load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot-s7.scm"))

(inherit-modules (kernel boot compat-s7))
