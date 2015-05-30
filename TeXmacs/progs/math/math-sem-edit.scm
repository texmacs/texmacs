
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-sem-edit.scm
;; DESCRIPTION : semantic mathematical editing
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-sem-edit)
  (:use (math math-edit)
        (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check syntactic correctness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-correct? . opt-p)
  (if (null? opt-p)
      (math-correct? (cDr (cursor-path)))
      (let* ((p (car opt-p))
	     (t (path->tree p))
	     (val (get-env-tree-at "mode" (append p (list 0)))))
	(or (tm-func? t 'cell)
	    (not (tm-equal? val "math"))
	    (and (or (tm-in? t '(lsub lsup rsub rsup))
		     (tree-func? (tree-up t) 'concat)
		     (with ok? (packrat-correct? "std-math" "Main" t)
		       ;;(display* t ", " ok? "\n")
		       ok?))
		 (!= p (buffer-path))
		 (math-correct? (cDr p)))))))

(define (try-correct-rewrite l)
  (cond ((null? l) `#f)
        ((and (null? (cdr l)) (func? (car l) 'else))
         `(begin ,@(cdar l)))
        ((npair? (car l))
         (texmacs-error "try-correct-rewrite" "syntax error"))
        (else
          (let* ((h `(begin ,@(car l) (math-correct?)))
                 (r (try-correct-rewrite (cdr l))))
            `(or (try-modification ,h) ,r)))))

(define-macro (try-correct . l)
  (try-correct-rewrite l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapped insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-suppressed)
  (let* ((bt (before-cursor))
         (at (after-cursor)))
    (when (tm-func? bt 'suppressed)
      (tree-cut bt))
    (when (tm-func? at 'suppressed)
      (tree-cut at))))

(define (add-suppressed-arg t)
  (when (tm-equal? t "")
    (tree-set! t '(suppressed (tiny-box))))
  (when (tm-in? t '(table row cell))
    (for-each add-suppressed-arg (tree-children t))))

(define (add-suppressed-upwards t)
  (when (!= (tree->path t) (buffer-path))
    (when (tree-in? t '(frac sqrt table))
      (for-each add-suppressed-arg (tree-children t)))
    (add-suppressed-upwards (tree-up t))))

(define (add-suppressed)
  (when (not (math-correct?))
    (insert '(suppressed (tiny-box))))
  (add-suppressed-upwards (cursor-tree)))

(define (wrap-insert cmd)
  (if (not (math-correct?))
      (cmd)
      (try-correct
        ((remove-suppressed)
         (cmd)
         (add-suppressed))
        ((when (tm-func? (before-cursor) 'suppressed)
           (tree-go-to (before-cursor) 0))
         (cmd)
         (add-suppressed))
        ((cmd)
         (add-suppressed))
        ((insert '(suppressed (tiny-box)))
         (cmd)
         (add-suppressed)))))

(define (wrap-remove cmd forwards?)
  (if (not (math-correct?))
      (cmd)
      (with st (if forwards? (after-cursor) (before-cursor))
        (remove-suppressed)
        (cmd)
        (when (and (string? st)
                   (in? (math-symbol-type st) (list "infix" "separator")))
          (insert `(suppressed ,st)))
        (add-suppressed))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-insert s)
  (:require (in-sem-math?))
  ;;(display* "Insert " s "\n")
  (wrap-insert (lambda () (former s))))

(tm-define (kbd-backspace)
  (:require (in-sem-math?))
  ;;(display* "Backspace\n")
  (wrap-remove former #f))

(tm-define (kbd-delete)
  (:require (in-sem-math?))
  ;;(display* "Delete\n")
  (wrap-remove former #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make . l)
  (with cmd (lambda () (apply former l))
    (wrap-insert cmd)))

(tm-define (math-big-operator op)
  (:require (in-sem-math?))
  (with cmd (lambda () (former op))
    (wrap-insert cmd)))

(tm-define (math-bracket-open . l)
  (:require (in-sem-math?))
  (with cmd (lambda () (apply former l))
    (wrap-insert cmd)))

(tm-define (math-separator . l)
  (:require (in-sem-math?))
  (with cmd (lambda () (apply former l))
    (wrap-insert cmd)))

(tm-define (math-bracket-close . l)
  (:require (in-sem-math?))
  (with cmd (lambda () (apply former l))
    (wrap-insert cmd)))

(tm-define (make-rigid)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-lprime sym)
  (:require (in-sem-math?))
  (with cmd (lambda () (former sym))
    (wrap-insert cmd)))

(tm-define (make-rprime sym)
  (:require (in-sem-math?))
  (with cmd (lambda () (former sym))
    (wrap-insert cmd)))

(tm-define (make-below)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-above)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-script r? sup?)
  (:require (in-sem-math?))
  (with cmd (lambda () (former r? sup?))
    (wrap-insert cmd)))

(tm-define (make-fraction)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-sqrt)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-var-sqrt)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-wide sym)
  (:require (in-sem-math?))
  (with cmd (lambda () (former sym))
    (wrap-insert cmd)))

(tm-define (make-wide-under sym)
  (:require (in-sem-math?))
  (with cmd (lambda () (former sym))
    (wrap-insert cmd)))

(tm-define (make-neg)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))

(tm-define (make-tree)
  (:require (in-sem-math?))
  (with cmd (lambda () (former))
    (wrap-insert cmd)))
