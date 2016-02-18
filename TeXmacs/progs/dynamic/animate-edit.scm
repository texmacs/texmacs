
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
;; Start and end editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-animate t len)
  (with r (animate-checkout `(anim-static ,t ,len "0.1s" "0s"))
    (insert-go-to r (cons 1 (path-start (tm-ref r 1) '())))))

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
