
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-edit.scm
;; DESCRIPTION : routines for formatting text
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-edit)
  (:use (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-with-color s) (make-with "color" s))
(tm-define (make-with-font-base-size s) (make-with "font-base-size" s))

(tm-define (make-line-with var val)
  (:synopsis "Make 'with' with one or more paragraphs as its scope")
  (:check-mark "o" test-env?)
  (if (not (selection-active-normal?))
      (select-line))
  (make-with var val)
  (insert-return)
  (remove-text #f))

(tm-define (set-left-margin s) (make-line-with "par-left" s))
(tm-define (set-right-margin s) (make-line-with "par-right" s))
(tm-define (set-first-indent s) (make-line-with "par-first" s))
(tm-define (set-last-indent s) (make-line-with "last-indentation" s))
(tm-define (set-interline s) (make-line-with "par-sep" s))
(tm-define (set-interline-spc s) (make-line-with "par-line-sep" s))
(tm-define (set-interpar-spc s) (make-line-with "par-par-sep" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-page-break)
  (make 'page-break)
  (insert-return))

(tm-define (make-new-page)
  (make 'new-page)
  (insert-return))

(tm-define (make-new-dpage)
  (make 'new-dpage)
  (insert-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for floats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define (make-insertion s)
;  (insert-go-to
;   (list 'float s
;	 (if (string=? s "float") "tbh" "")
;	 (list 'document ""))
;   (list 2 0 0)))

;(define (position-insertion what flag)
;"Allow/disallow a position for the inner float the caret in is.
;what <char>   : position to allow/disallow
;flag <boolean>: allow if true, disallow is false."
;
;  (let ((p (search-upwards "float")))
;    (if (nnull? p)
;	(tm-assign
;	 (rcons p 1)
;	 (string->tree ((if flag
;			    string-include
;			    string-exclude) (tree->string
;					     (tm-subtree (rcons p 1)))
;			    what))))))

(define (test-insertion-position? what)
  (let ((p (search-upwards "float"))
	(c (string-ref what 0)))
    (if (nnull? p)
	(char-in-string? c (tree->string (tm-subtree (rcons p 1)))))))

(define (not-test-insertion-position? s)
  (not (test-insertion-position? s)))

(tm-define (toggle-insertion-position what)
  (:check-mark "v" test-insertion-position?)
  (position-insertion what (not-test-insertion-position? what)))

(tm-define (toggle-insertion-position-not s)
  (:check-mark "v" not-test-insertion-position?)
  (toggle-insertion-position s))
