
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-markup.scm
;; DESCRIPTION : additional rendering macros written in scheme
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel tools tm-markup)
  (:use (kernel texmacs tm-define))
  (:export
    ext-map
    ext-select
    ext-the-doc-note))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-map fun to)
  (:secure #t)
  (with (op . args) (tree->list to)
    (with f (lambda (x) (list 'compound fun x))
      (list 'quote (cons 'tuple (map f args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting document titles as a function of several style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rewrite-select pat)
  (if (atomic-tree? pat)
      (with s (tree->string pat)
	(if (not (string-starts? s "("))
	    (string->object s)
	    s))
      (with (op . r) (tree->list pat)
	(cond ((== op 'pat-any) :1)
	      ((== op 'pat-any-repeat) :*)
	      ((== op 'pat-or) (cons :or (map rewrite-select r)))
	      ((== op 'pat-and) (cons :and (map rewrite-select r)))
	      ((== op 'pat-group) (cons :group (map rewrite-select r)))
	      ((== op 'pat-and-not) (cons :and-not (map rewrite-select r)))
	      (else #f)))))

(tm-define (ext-select body args)
  (:secure #t)
  (with (op body2 . pat) (tree->list args)
    (list 'quote (cons 'tuple (tm-select body (map rewrite-select pat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-the-doc-note nr)
  (:secure #t)
  (with n (string->number (tree->string nr))
    (cond ((or (not n) (< n 1)) "*")
	  ((= n 1) '(with "mode" "math" "<dag>"))
	  ((= n 2) '(with "mode" "math" "<ddag>"))
	  (else (make-string (- n 2) #\*)))))
