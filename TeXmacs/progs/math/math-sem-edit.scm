
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

(define (wrap-insert cmd)
  (if (not (math-correct?))
      (cmd)
      (try-correct
        ((remove-suppressed)
         (cmd)
         (when (not (math-correct?))
           (insert '(suppressed (tiny-box)))))
        ((when (tm-func? (before-cursor) 'suppressed)
           (tree-go-to (before-cursor) 0))
         (cmd)
         (when (not (math-correct?))
           (insert '(suppressed (tiny-box)))))
        ((cmd)
         (when (not (math-correct?))
           (insert '(suppressed (tiny-box)))))
        ((insert '(suppressed (tiny-box)))
         (cmd)
         (when (not (math-correct?))
           (insert '(suppressed (tiny-box))))))))

(define (wrap-remove cmd forwards?)
  (if (not (math-correct?))
      (cmd)
      (with st (if forwards? (after-cursor) (before-cursor))
        (remove-suppressed)
        (cmd)
        (when (and (string? st)
                   (in? (math-symbol-type st) (list "infix" "separator")))
          (insert `(suppressed ,st)))
        (when (not (math-correct?))
          (insert '(suppressed (tiny-box)))))))

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

(tm-define (math-make . l)
  (with cmd (lambda () (apply make l))
    (wrap-insert cmd)))

(tm-define (math-big-operator op)
  (:require (in-sem-math?))
  (with cmd (lambda () (former op))
    (wrap-insert cmd)))

(kbd-map
  (:mode in-sem-math?)
  ("_" (math-make 'rsub))
  ("^" (math-make 'rsup))
  ("<#192>" (math-make 'frac))
  ("ÿ" (math-make 'sqrt))
  ("math f" (math-make 'frac))
  ("math s" (math-make 'sqrt)))
