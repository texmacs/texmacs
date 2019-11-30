
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cpp-wrap.scm
;; DESCRIPTION : wrappers around C++ functions
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library cpp-wrap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting general tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (path-in t l)
  (let* ((f (and (nnull? l) (car l)))
         (r (and (nnull? l) (cdr l))))
    (cond ((null? l) l)
          ((== l '(:start)) (path-start t (list)))
          ((== l '(:end)) (path-end t (list)))
          ((tm-atomic? t) l)
          ((== f :first) (path-in t (cons 0 (cdr l))))
          ((== f :last) (path-in t (cons (- (tm-arity t) 1) (cdr l))))
          ((and (integer? (car l)) (tm-ref t (car l)))
           (cons (car l) (path-in (tm-ref t (car l)) (cdr l))))
          (else (texmacs-error "path-in" "invalid path")))))

(tm-define (insert t . opt-l)
  (if (null? opt-l)
      (cpp-insert t)
      (cpp-insert-go-to t (path-in t opt-l))))

(tm-define (make tag . opt-arity)
  (if (null? opt-arity)
      (cpp-make tag)
      (cpp-make-arity tag (car opt-arity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrappers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-with var val)
  (cond ((selection-active-table?) (cell-set-format var val))
        ((nstring? val) (insert-go-to `(with ,var ,val "") '(2 0)))
        (else (cpp-make-with var val))))

(tm-define (make-inline lab)
  (if (selection-active-large?)
      (with sel `(par-block ,(selection-tree))
	(clipboard-cut "wrapbuf")
	(make-return-after)
	(make lab)
	(tree-set (cursor-tree) sel)
	(with-innermost t lab
	  (tree-go-to t :end)
	  (make-return-before)))
      (make lab)))

(tm-define (make-wrapped lab)
  (clipboard-cut "wrapbuf")
  (make lab)
  (clipboard-paste "wrapbuf"))

(tm-define (insert-go-to t p) (cpp-insert-go-to t p))
(tm-define (make-hybrid) (cpp-make-hybrid))

(tm-define (make-rigid) (cpp-make-rigid))
(tm-define (make-lprime s) (cpp-make-lprime s))
(tm-define (make-rprime s) (cpp-make-rprime s))
(tm-define (make-below) (cpp-make-below))
(tm-define (make-above) (cpp-make-above))
(tm-define (make-script r? sup?) (cpp-make-script r? sup?))
(tm-define (make-fraction) (cpp-make-fraction))
(tm-define (make-sqrt) (cpp-make-sqrt))
(tm-define (make-var-sqrt) (cpp-make-var-sqrt))
(tm-define (make-wide s) (cpp-make-wide s))
(tm-define (make-wide-under s) (cpp-make-wide-under s))
(tm-define (make-neg) (cpp-make-neg))
(tm-define (make-tree) (cpp-make-tree))

(tm-define (clipboard-copy cb) (cpp-clipboard-copy cb))
(tm-define (clipboard-cut cb) (cpp-clipboard-cut cb))
(tm-define (clipboard-paste cb) (cpp-clipboard-paste cb))

(tm-define (nr-pages) (cpp-nr-pages))

(tm-define (style-clear-cache) (cpp-style-clear-cache))
