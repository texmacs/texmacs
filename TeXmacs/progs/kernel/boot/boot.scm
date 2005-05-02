
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot.scm
;; DESCRIPTION : initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define texmacs-user (current-module))

(define-macro (define-public-macro head . body)
  `(define-public ,(car head)
     ;; FIXME: why can't we use procedure->macro for a non-memoizing variant?
     (procedure->memoizing-macro
      (lambda (cmd env)
	(apply (lambda ,(cdr head) ,@body) (cdr cmd))))))

(define (guile-a?) (equal? (scheme-dialect) "guile-a"))
(define (guile-b?) (equal? (scheme-dialect) "guile-b"))

(if (guile-a?)
    (begin
      (define import-from use-modules)
      (define re-export export)
      (define do-export export)))

(if (guile-b?)
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

      (define backup-deprecation-warning noop)
      (define-macro (do-export . syms)
	;; guile-b gives a warning when using the export keyword in
	;; a module which inherits from a module in which the keyword
	;; is already defined. For module inheritance, this has been solved
	;; by using the re-export keyword. But if the intention is to
	;; locally *override* the outer definition, then we really *need* to
	;; use export and not re-export. This is used during lazy definitions,
	;; where the outer definition may call the local definition. If one
	;; uses the outer definition as the local definition, then one obtains
	;; an infinite loop...
	`(begin
	   (set! backup-deprecation-warning issue-deprecation-warning)
	   (set! issue-deprecation-warning (lambda l (noop)))
	   (export ,@syms)
	   (set! issue-deprecation-warning backup-deprecation-warning)))))

(define-macro (inherit-modules . which-list)
  (define (module-exports which)
    (let* ((m (resolve-module which))
	   (m-public (module-ref m '%module-public-interface)))
      (module-map (lambda (sym var) sym) m-public)))
  (let ((l (apply append (map module-exports which-list))))
    `(begin
       (use-modules ,@which-list)
       (re-export ,@l))))

(define-macro (texmacs-module name . options)
  (define (transform action)
    (cond ((not (pair? action)) (noop))
	  ((equal? (car action) :use) (cons 'use-modules (cdr action)))
	  ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
	  ((equal? (car action) :export) (cons 'do-export (cdr action)))
	  (else '(noop))))
  (let ((l (map-in-order transform options)))
    (if (guile-b?)
	(set! l (cons `(module-use! (current-module) ,texmacs-user) l)))
    ;(display "loading ") (display name) (display "\n")
    `(begin
       (define-module ,name)
       ,@l)))
