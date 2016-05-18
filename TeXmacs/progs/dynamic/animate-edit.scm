
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : animate-edit.scm
;; DESCRIPTION : routines for editing animations
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic animate-edit)
  (:use (utils library tree)
        (utils library cursor)
        (dynamic dynamic-drd)
        (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (reset-players t)
  (players-set-elapsed t -0.5)
  (update-players (tree->path t) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time bending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-get-accelerate t)
  (cond ((not (tree? t)) #f)
        ((tree-is? t 'anim-accelerate) t)
        ((tree-is? t :up 'anim-accelerate) (tree-up t))
        ((tree-in? t (animation-tag-list)) t)
        (else #f)))

(tm-define (accelerate-get-type t)
  (and-with a (anim-get-accelerate t)
    (or (and (tree-func? a 'anim-accelerate 2)
             (tree->stree (tree-ref a 1)))
        "normal")))

(tm-define (accelerate-test-type? dummy new-type)
  (with t (tree-innermost 'anim-accelerate #t)
    (tm-equal? (accelerate-get-type t) new-type)))

(tm-define (accelerate-set-type t new-type)
  (:check-mark "*" accelerate-test-type?)
  (and-with a (anim-get-accelerate t)
    (if (tree-func? a 'anim-accelerate 2)
        (if (== new-type "normal")
            (tree-remove-node! a 0)
            (tree-set (tree-ref a 1) new-type))
        (if (!= new-type "normal")
            (tree-set! a `(anim-accelerate ,a ,new-type))))
    (reset-players a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start and end editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (checkout-animation t len)
  (cond ((tm-func? t 'gr-screen 1)
         (with (r p) (checkout-animation (tm-ref t 0) len)
           (list `(gr-screen ,r) (cons 0 p))))
        (else
          (with r (animate-checkout `(anim-static ,t ,len "0.1s" "0s"))
            (list r (cons 1 (path-start (tm-ref r 1) '())))))))

(tm-define (make-animate t len)
  (with (r p) (checkout-animation t len)
    (insert-go-to r p)))

(tm-define (animate-selection len)
  (:argument len "Duration")
  (with sel (selection-tree)
    (clipboard-cut "primary")
    (make-animate sel len)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start and end editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-checkout t)
  (with r (animate-checkout t)
    (tree-assign-node! t (tree-label r))
    (tree-set! t 0 (tree-ref r 0))
    (tree-insert! t 1 (list (tree-ref r 1)))
    (tree-go-to t 1 :start)))

(tm-define (anim-commit t)
  (with r (animate-commit t)
    (tree-set! t 0 (tree-ref r 0))
    (tree-remove! t 1 1)
    (tree-assign-node! t (tree-label r))
    (tree-go-to t :end)))

(tm-define (anim-set-now t now)
  (with r (animate-commit t)
    (tree-set! t 0 (tree-ref r 0))
    (tree-set! t 4 now))
  (with r (animate-checkout `(anim-static ,(tree-ref t 0)
                                          ,@(cddr (tm-children t))))
    (tree-set! t 0 (tree-ref r 0))
    (tree-set! t 1 (tree-ref r 1))
    (tree-go-to t 1 :start)))
