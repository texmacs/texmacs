
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format.scm
;; DESCRIPTION : routines for formatting text
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-format)
  (:export
    ;; modifying style parameters
    make-with-color make-line-with
    set-left-margin set-right-margin set-first-indent set-last-indent
    set-interline set-interline-spc set-interpar-spc
    ;; page breaking
    make-page-break make-new-page
    ;; routines for floats
    test-insertion-position?
    toggle-insertion-position toggle-insertion-position-not))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-with-color s) (make-with "color" s))

(tm-define (make-line-with var val)
  (:synopsis "Make 'with' with one or more paragraphs as its scope")
  (:check-mark "o" test-env?)
  (if (not (selection-active-normal?))
      (select-line))
  (make-with var val)
  (insert-return)
  (remove-text #f))

(define (set-left-margin s) (make-line-with "par-left" s))
(define (set-right-margin s) (make-line-with "par-right" s))
(define (set-first-indent s) (make-line-with "par-first" s))
(define (set-last-indent s) (make-line-with "last-indentation" s))
(define (set-interline s) (make-line-with "par-sep" s))
(define (set-interline-spc s) (make-line-with "par-line-sep" s))
(define (set-interpar-spc s) (make-line-with "par-par-sep" s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-page-break)
  (make 'page-break)
  (insert-return))

(define (make-new-page)
  (make 'new-page)
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
;    (if (not (null? p))
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
    (if (not (null? p))
	(char-in-string? c (tree->string (tm-subtree (rcons p 1)))))))

(tm-define (toggle-insertion-position what)
  (:check-mark "v" test-insertion-position?)
  (position-insertion what (not (test-insertion-position? what))))

(define (test-insertion-position-not? s)
  (not (test-insertion-position? s)))

(tm-define (toggle-insertion-position-not s)
  (:check-mark "v" test-insertion-position-not?)
  (toggle-insertion-position s))
