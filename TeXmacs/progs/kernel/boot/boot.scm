
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (expand load eval)
  (define texmacs-user (current-module))
  (define temp-module (current-module))
  (define temp-value #f))

(cond-expand
 (guile-2
  (use-modules (ice-9 rdelim) (ice-9 pretty-print)))
 (guile-3
  (use-modules (ice-9 copy-tree)))
 (else
  (define (guile-a?) (equal? (scheme-dialect) "guile-a"))
  (define (guile-b?) (equal? (scheme-dialect) "guile-b"))
  (define (guile-c?) (equal? (scheme-dialect) "guile-c"))
  (define (guile-b-c?) (or (guile-b?) (guile-c?)))
  (if (guile-c?) (use-modules (ice-9 rdelim) (ice-9 pretty-print)))))

(define has-look-and-feel? (lambda (x) (equal? x "emacs")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provide functions if not defined and public macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(cond-expand
  (guile-2
    (define-macro (define-public-macro head . body)
      `(defmacro-public ,(car head)
            ,(cdr head) ,@body))
         (export-syntax define-public-macro))
  (else
    (if (guile-a?)
       (define-macro (define-public-macro head . body)
         `(define-public ,(car head)
	     ;; FIXME: why can't we use procedure->macro
	     ;; for a non-memoizing variant?
	     (procedure->memoizing-macro
	       (lambda (cmd env)
	         (apply (lambda ,(cdr head) ,@body) (cdr cmd)))))))
    (if (not (guile-a?))
       (define-macro (define-public-macro head . body)
         `(begin
	        (define-macro ,(car head)
	          (lambda ,(cdr head) ,@body))
	        (export ,(car head)))))))

(define-public-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
	  (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

(cond-expand
  (guile-2)
  (else
    (define-public-macro (define-once head . body)
      (let ((h (if (pair? head) (car head) head)))
        `(when (not (defined? ',h)) (define ,head ,@body))))))

(define-public-macro (define-public-once head . body)
  `(begin (define-once ,head ,@body) (export ,(if (pair? head) (car head) head))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(let ((prev-quit-cmd quit-TeXmacs-scheme))
     (set! quit-TeXmacs-scheme (lambda () ,@cmd (prev-quit-cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module switching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cond-expand (guile-2
(define-macro (with-module module . body)
  `(begin
     (eval-when (expand load eval)
       (set! (@@ (guile-user) temp-module) (current-module))
       (set-current-module ,module))
     ,@body
     (eval-when (expand load eval) (set-current-module (@@ (guile-user) temp-module))))))
     (else
(define-macro (with-module module . body)
  `(begin
     (set! temp-module (current-module))
     (set-current-module ,module)
     ,@body
     (set-current-module temp-module)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the definition
; (define-macro (import-from . modules)
;    `(eval-when (expand load eval) (process-use-modules
;        (list ,@(map (lambda (m) `(list (quote ,m))) modules)))))
; is equivalent in Guile 2.2 to
;    (define-macro (import-from . modules) `(use-modules ,@ modules )))
; but works also in Guile 1.8


(cond-expand
  (guile-2
    (define-macro (import-from . modules) `(use-modules ,@ modules )))
  (else (if (guile-a?)
      (begin
        (define import-from use-modules)
        (define re-export export)))
    (if (guile-b-c?)
      (begin
        (define-macro (import-from . modules)
	  `(process-use-modules
	    (list ,@(map (lambda (m)
	  		 `(list ,@(compile-interface-spec m)))
	  	       modules))))
      ;; FIXME: why does this not work?
      ;; (define-macro (import-from . modules)
      ;;   (define (import-from-body module)
      ;;     `(module-use! (current-module) (resolve-module ',module)))
      ;;   `(begin
      ;;     ,@(map import-from-body modules)))
      ))))

(define-public (module-exported-symbols m)
  (module-map (lambda (sym var) sym) (module-public-interface (resolve-module m))))

(define-public-macro (inherit-modules . which-list)
 `(begin (use-modules ,@which-list)
     (eval-when (expand load) ,@(map (lambda (m) `(module-re-export! (current-module) (module-exported-symbols ',m))) which-list))
     ))

(cond-expand
    (guile-2
      (with-module the-root-module
        (define-macro (texmacs-module name . options)
          (define (transform action)
            (cond ((not (pair? action)) (noop))
              ((equal? (car action) :use) (cons 'use-modules (cdr action)))
              ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
              ((equal? (car action) :export)
               (display "Warning] The option :export is no longer supported\n")
               (display "       ] Please use tm-define instead\n"))
              (else '(noop))))
          (let ((l (map-in-order transform options)))
          ;; (display "Loading ") (display name) (display "\n")
            `(begin
               ;; (eval-when (expand) (display "* IN MODULE: ") (display ',name) (display "\n"))
               (cond-expand (guile-3.0
                  (define-module ,name #:declarative? #f))
                  (else (define-module ,name)))
               ;; (ice-9 curried-definitions) needs to be imported every time since has bindings replacing core bindings
               (use-modules (ice-9 curried-definitions)) 
               ,@l
               (eval-when (expand load eval) (module-use! (current-module) (resolve-interface '(guile-user))))
               ;; (eval-when (expand) (display "* END MODULE HEADER: ") (display ',name) (display "\n"))
              )))
        (export-syntax texmacs-module)))
   (else
        (define-macro (texmacs-module name . options)
          (define (transform action)
            (cond ((not (pair? action)) (noop))
              ((equal? (car action) :use) (cons 'use-modules (cdr action)))
              ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
              ((equal? (car action) :export)
               (display "Warning] The option :export is no longer supported\n")
               (display "       ] Please use tm-define instead\n"))
              (else '(noop))))
          (let ((l (map-in-order transform options)))
    (if (guile-b-c?)
   (set! l (cons `(module-use! (current-module) ,texmacs-user) l)))
            ;;(display "loading ") (display name) (display "\n")
            `(begin
               (define-module ,name)
              ; (module-use! (current-module) (@@ (guile-user) texmacs-user))
               ,@l)))))

(define-public (module-available? module-name)
  (catch #t
    (lambda () (resolve-interface module-name) #t)
    (lambda (key . args) #f)))

(define-public (module-load module)
  ;(display* ">>> module-load" module "\n")
  (resolve-module module)
  ;(display "---\n")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some compatibility utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tagged-lambda marks certain closures in order to recover the code
;; for retrieving the correct shortcuts
;; in Guile 2.2 the procedure procedure-source does not work
;; so we bake our in-house solution tm-procedure-source
;; see e.g. promise-source in kernel/gui/menu-widget.scm

(cond-expand
  (guile-2
    (define-public-macro (tagged-lambda head . body)
      `(let ((p (lambda ,head ,@body)) (s '(lambda ,head ,@body)))
         (set-procedure-property! p 'tm-source s)
         p))

    (define-public (tm-procedure-source action)
      (and (procedure? action) (procedure-property action 'tm-source))))
  (else
    (define-public-macro (tagged-lambda head . body)
     `(lambda ,head ,@body))
    (define-public (tm-procedure-source action)
      (procedure-source action))))

