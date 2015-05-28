
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
  (:use (math math-edit)))

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
		       (display* t ", " ok? "\n")
		       ok?))
		 (!= p (buffer-path))
		 (math-correct? (cDr p)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapped insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wrap-insert cmd)
  (let* ((bt (before-cursor))
         (at (after-cursor)))
    (if (not (math-correct?))
        (cmd)
        (begin
          (when (tm-func? bt 'suppressed)
            (tree-cut bt))
          (when (tm-func? at 'suppressed)
            (tree-cut at))
          (cmd)
          (when (not (math-correct?))
            (insert '(suppressed (tiny-box))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-insert s)
  (:require (in-sem-math?))
  (display* "Insert " s "\n")
  (wrap-insert (lambda () (former s))))

(tm-define (kbd-backspace)
  (:require (in-sem-math?))
  (display* "Backspace\n")
  (let* ((bt (before-cursor))
         (at (after-cursor)))
    (if (not (math-correct?))
        (former)
        (begin
          (when (tm-func? bt 'suppressed)
            (tree-cut bt))
          (when (tm-func? at 'suppressed)
            (tree-cut at))
          (former)
          (when (and (string? bt)
		     (in? (math-symbol-type bt) (list "infix" "separator")))
            (insert `(suppressed ,bt)))
          (when (not (math-correct?))
            (insert '(suppressed (tiny-box))))))))

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
