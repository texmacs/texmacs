
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
        (utils library tree)
        (utils library cursor)
        (source source-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-sem?)
  (== (get-preference "semantic correctness") "on"))

(define (math-nary? t)
  (tree-in? t '(frac tfrac dfrac cfrac frac*
                sqrt table tree above below)))

(define (quantifier? s)
  (and (string? s) (== (math-symbol-group s) "Quantifier-symbol")))

(define (suppressed-before?)
  (tm-is? (before-cursor) 'suppressed))

(define (suppressed-after?)
  (tm-is? (after-cursor) 'suppressed))

(define (suppressed-around?)
  (or (suppressed-before?) (suppressed-after?)))

(define (var-infix? t)
  (or (tm-in? t '(lsub lsup lprime rsub rsup rprime))
      (infix? t)))

(define (infix? t)
  (cond ((tm-atomic? t)
         (in? (math-symbol-type (tm->string t))
              (list "infix" "separator")))
        ((tm-func? t 'concat)
         (list-and (map var-infix? (tm-children t))))
        (else #f)))

(define (get-infix-op t)
  (cond ((tm-atomic? t) t)
        ((tm-func? t 'concat)
         (list-or (map get-infix-op (tm-children t))))
        (else #f)))

(define (before-actual-infix?)
  (and (infix? (after-cursor))
       (tree-atomic? (cursor-tree*))
       (let* ((op (after-cursor))
              (op-len (string-length op))
              (cp (cursor-path*))
              (last (cAr cp))
              (ct (cursor-tree*))
              (ct-len (string-length (tree->string ct))))
         (or (and (> last 0) (< (+ last op-len) ct-len))
             (let* ((pt (tree-up ct))
                    (i (cAr (cDr cp)))
                    (n (tree-arity pt)))
               (and (tree-is? pt 'concat)
                    (or (> i 0) (> last 0))
                    (or (< (+ i 1) n) (< (+ last op-len) ct-len))))))))

(define (after-actual-infix?)
  (and (infix? (before-cursor))
       (tree-atomic? (cursor-tree))
       (let* ((op (before-cursor))
              (op-len (string-length op))
              (cp (cursor-path))
              (last (cAr cp))
              (ct (cursor-tree))
              (ct-len (string-length (tree->string ct))))
         (or (and (> last op-len) (< last ct-len))
             (let* ((pt (tree-up ct))
                    (i (cAr (cDr cp)))
                    (n (tree-arity pt)))
               (and (tree-is? pt 'concat)
                    (or (> i 0) (> last op-len))
                    (or (< (+ i 1) n) (< last ct-len))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quick check whether we are in math mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-mode t p mode)
  (if (null? p) mode
      (get-mode (tree-ref t (car p)) (cdr p)
                (tree-child-env t (car p) "mode" mode))))

(define (path-in-math? p)
  (tm-equal? (get-mode (path->tree (list (car p))) (cdr p) "text") "math"))

(define (tree-in-math? t)
  (and (tree->path t) (path-in-math? (tree->path t))))

(define (in-math-mode?)
  (path-in-math? (cDr (cursor-path))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check syntactic correctness
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-correct? . opt-p)
  (if (null? opt-p)
      (math-correct? (cDr (cursor-path)))
      (let* ((p (car opt-p))
	     (t (path->tree p)))
	(or (tm-func? t 'cell)
	    (not (tree-in-math? t))
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
          (let* ((h `(and ,@(car l) (math-correct?)))
                 (r (try-correct-rewrite (cdr l))))
            `(or (try-modification ,h) ,r)))))

(define-macro (try-correct . l)
  (try-correct-rewrite l))

(define-macro (wrap-inserter fun)
  `(tm-define (,fun . l)
     (:require (in-sem-math?))
     (with cmd (lambda () (apply former l))
       (wrap-insert cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapped insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (position-wrt-suppressed after?)
  (when (and (not after?) (suppressed-before?))
    (tree-go-to (before-cursor) :start))
  (when (and after? (suppressed-after?))
    (tree-go-to (after-cursor) :end)))

(define (remove-suppressed)
  (while (tm-func? (before-cursor) 'suppressed)
    (tree-cut (before-cursor)))
  (when (tm-func? (after-cursor) 'suppressed)
    (tree-cut (after-cursor))))

(define (add-suppressed-arg t)
  (when (tm-equal? t "")
    (tree-set! t '(suppressed (tiny-box))))
  (when (tm-in? t '(table row cell))
    (for-each add-suppressed-arg (tree-children t))))

(define (add-suppressed-upwards t)
  (when (!= (tree->path t) (buffer-path))
    (when (math-nary? t)
      (for-each add-suppressed-arg (tree-children t)))
    (add-suppressed-upwards (tree-up t))))

(define (add-suppressed)
  (when (not (math-correct?))
    (insert '(suppressed (tiny-box)) :start))
  (add-suppressed-upwards (cursor-tree))
  #t)

(define (clean-suppressed)
  (when (and (not (math-correct?)) (suppressed-around?))
    (try-correct
      ((remove-suppressed)
       (add-suppressed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (perform-insert cmd)
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
     (add-suppressed))
    ((remove-suppressed)
     (cmd)
     (and (tree-is? (tree-up (cursor-tree)) 'long-arrow)
          (begin
            (with-cursor (append (cDDr (cursor-path)) (list 1))
              (insert '(suppressed (tiny-box))))
            (add-suppressed))))
    ((remove-suppressed)
     (with s (before-cursor)
       (and (quantifier? s)
            (begin
              (cmd)
              (let* ((sep (if (== s "mathlambda") "<point>" ","))
                     (ins `(concat ,sep (tiny-box))))
                (insert `(suppressed ,ins) :start)
                #t)))))
    ((remove-suppressed)
     (cmd)
     (with s (before-cursor)
       (and (quantifier? s)
            (let* ((sep (if (== s "mathlambda") "<point>" ","))
                   (ins `(concat (tiny-box) ,sep (tiny-box))))
              (insert `(suppressed ,ins) :start)
              #t))))))

(define (wrap-insert cmd)
  (clean-suppressed)
  (if (not (math-correct?))
      (cmd)
      (perform-insert cmd)))

(tm-define (kbd-insert s)
  (:require (in-sem-math?))
  ;;(display* "Insert " s "\n")
  (wrap-insert (lambda () (former s))))

(tm-define (make . l)
  (with cmd (lambda () (apply former l))
    (cond ((not (in-sem?)) (cmd))
          ((in-math?) (wrap-insert cmd))
          (else
            (cmd)
            (when (in-math-mode?)
              (add-suppressed))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (perform-remove cmd forwards?)
  (try-correct
    ((and (suppressed-around?)
          (begin
            (remove-suppressed)
            (with empty? (tree-empty? (cursor-tree))
              (cmd)
              (when (not (and empty? (math-correct?)))
                (remove-suppressed))
              (add-suppressed)))))
    ((remove-suppressed)
     (with empty? (tree-empty? (cursor-tree))
       (cmd)
       (if (and empty? (math-correct?))
           (add-suppressed)
           (and (suppressed-around?)
                (begin
                  (remove-suppressed)
                  (add-suppressed))))))
    ((remove-suppressed)
     (let* ((st (if forwards? (after-cursor) (before-cursor)))
            (inf? (if forwards? (before-actual-infix?) (after-actual-infix?))))
       (cmd)
       (when inf? (insert `(suppressed ,st) (if forwards? :end :start)))
       (add-suppressed)))
    ((position-wrt-suppressed forwards?)
     (cmd)
     (add-suppressed))))

(define (remove-selection cmd forwards?)
  (let* ((t (selection-tree)))
    (cmd)
    (try-correct
      ((and (infix? t)
            (with op (get-infix-op t)
              (insert `(suppressed ,op) (if forwards? :end :start))
              (add-suppressed))))
      ((add-suppressed)))))

(define (wrap-remove cmd forwards?)
  (clean-suppressed)
  (cond ((not (math-correct?)) (cmd))
        ((selection-active-any?) (remove-selection cmd forwards?))
        (else (perform-remove cmd forwards?))))

(tm-define (kbd-backspace)
  (:require (in-sem-math?))
  ;;(display* "Backspace\n")
  (wrap-remove former #f))

(tm-define (kbd-delete)
  (:require (in-sem-math?))
  ;;(display* "Delete\n")
  (wrap-remove former #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further wrappers for insertion of new tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(wrap-inserter math-insert)
(wrap-inserter make)
(wrap-inserter math-big-operator)
(wrap-inserter math-bracket-open)
(wrap-inserter math-separator)
(wrap-inserter math-bracket-close)
(wrap-inserter make-rigid)
(wrap-inserter make-lprime)
(wrap-inserter make-rprime)
(wrap-inserter make-below)
(wrap-inserter make-above)
(wrap-inserter make-script)
(wrap-inserter make-fraction)
(wrap-inserter make-sqrt)
(wrap-inserter make-wide)
(wrap-inserter make-wide-under)
(wrap-inserter make-neg)
(wrap-inserter make-tree)
(wrap-inserter make-long-arrow)
(wrap-inserter make-long-arrow*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers for other editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(wrap-inserter kbd-space)
(wrap-inserter kbd-shift-space)
(wrap-inserter kbd-return)
(wrap-inserter kbd-shift-return)
(wrap-inserter kbd-control-return)
(wrap-inserter kbd-shift-control-return)
(wrap-inserter kbd-alternate-return)
(wrap-inserter kbd-shift-alternate-return)

(wrap-inserter structured-insert-left)
(wrap-inserter structured-insert-right)
(wrap-inserter structured-insert-up)
(wrap-inserter structured-insert-down)
(wrap-inserter structured-insert-start)
(wrap-inserter structured-insert-end)
(wrap-inserter structured-insert-top)
(wrap-inserter structured-insert-bottom)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hybrid commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(wrap-inserter make-hybrid)

(tm-define (kbd-enter t forwards?)
  (:require (and (tree-is? t 'hybrid) (tree-in-math? t)))
  (with cmd (lambda () (activate-hybrid #f))
    (perform-insert cmd)))
