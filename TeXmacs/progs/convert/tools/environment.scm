
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : environment.scm
;; DESCRIPTION : Converter environments.
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Converter environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Environments are used by converters to accumulate dynamic state.
;;
;; By policy, an environment can only be mutated inside a bounded dynamic scope
;; (i.e. environment-set! must stay private).
;;
;; The intended semantics of environment is "functional operations on
;; hash-tables". However, for simplicity and efficiency a dynamic scoping
;; implementation is used.
;;
;; WARNING: environments are NOT continuation-correct. A continuation which
;; alters an environment is only garanteed to evaluate correctly _once_.
;;
;; WARNING: with-environment* does not restore the environment when it is
;; exited by stack unwinding (when an exception occurs).
;;
;; NOTE: a pure functional solution could be implemented using alists. If the
;; corner-case semantics of the hash-table based solution become problematic,
;; that should be the next thing to try. Since the list is not expected to grow
;; longer than a few dozen items, list lookup should not be a big problem.
;; Though a useful optimization may be adaptative ordering based on access
;; count.

;; Environment primitives

(tm-define (environment)
  (make-ahash-table))

(define-macro (environment-set!* env key val) ; must stay private
  `(ahash-set! ,env ,key ,val))

(define-macro (environment-remove! env key) ; must stay private
  `(ahash-remove! ,env ,key))

(tm-define (environment-ref* env key)
  (ahash-ref env key))

(tm-define-macro (environment-ref env key)
  `(environment-ref* ,env (quote ,key)))

(define (environment-binding env key)
  ;; If @key is bound in @env returns (list @key value). Otherwise, return #f.
  ;; Unlike hash-get-handle, the return value is not mutable.
  (let ((h (ahash-get-handle env key)))
    (list key (and (pair? h) (list (cdr h))))))

;; Environment library (only use primitives)

(define (environment-bind! env binding)	; must stay private
  ;; Restore a binding previously saved with environment-binding.
  (if (second binding)      
      (environment-set!* env (first binding) (car (second binding)))
      (environment-remove! env (first binding))))

(tm-define (with-environment* env bindings proc)
  (let ((saves '()) (result #f))
    (for-each (lambda (b)
		(set-cons! saves (environment-binding env (first b)))
		(environment-set!* env (first b) (second b)))
	      bindings)
    (set! result (proc env))
    (for-each (lambda (b)
		(environment-bind! env b))
	      saves)
    result))

(tm-define-macro (with-environment env bindings . body)
  (if (nlist? bindings)
      (syntax-error "with-environment" "Bindings are not a list: ~A" bindings))
  `(with-environment* ,env
       (list ,@(map-in-order
		(lambda (b)
		  (if (not (list-length=2? b))
		      (syntax-error "with-environment"
				    "Ill-formed binding: ~A" b))
		  `(list (quote ,(first b)) ,(second b)))
		bindings))
    (lambda (,env) ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XPath environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Currently, the xpath environment only stores the parent node.
;; Eventually, it should store an inverse list of ancestor nodes.

(tm-define (initialize-xpath env root proc)
  (with-environment* env `((xpath:root ,root)
			   (xpath:parent #f)
			   (xpath:current ,root))
    proc))

(tm-define (xpath-descend env child proc)
  (with-environment* env `((xpath:parent ,(xpath-current env))
			   (xpath:current ,child))
    proc))

(tm-define (xpath-root env)
  (environment-ref env xpath:root))
(tm-define (xpath-parent env)
  (environment-ref env xpath:parent))
(tm-define (xpath-current env)
  (environment-ref env xpath:current))
