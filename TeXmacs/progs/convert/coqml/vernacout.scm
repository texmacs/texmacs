
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : vernacout.scm
;; DESCRIPTION : generation of Coq Vernacular from scheme expressions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml vernacout)
          (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for string output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-vernac s)
  (output-text s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vernacout-item i)
  (output-indent -2)
  (output-lf)
  (vernacout i)
  (output-indent 2))

(define (vernacout-coqdoc l)
    (output-vernac "(** ")
    (vernacout `(!indent ,l))
    (output-vernac " *)")
    (output-lf))

(define (vernacout-comment l)
    (output-vernac "(* ")
    (vernacout `(!indent ,l))
    (output-vernac " *)")
    (output-lf))

(define (vernacout-preamble l)
  (output-verbatim l))

(define (empty-line? x)
  (or (== x "")
      (and (func? x '!concat)
           (list-and (map empty-line? (cdr x))))))

(define (vernacout-paragraph l)
  (if (nnull? l)
    (begin
      (vernacout (car l))
      (if (nnull? (cdr l)) (output-lf))
      (vernacout-paragraph (cdr l)))))

(define (vernacout-concat-sub prev l)
  (when (nnull? l)
    (if (not (and (== prev " ") (== (car l) " "))) ;; remove double spaces
      (vernacout (car l)))
    (vernacout-concat-sub (car l) (cdr l))))

(define (vernacout-concat l)
  (vernacout-concat-sub #f l))

(define (vernacout-indent x)
  (output-indent 2)
  (output-verb "  ")
  (vernacout x)
  (output-indent -2))

(define (vernacout-unindent x)
  (with old-indent (get-output-indent)
    (set-output-indent 0)
    (vernacout x)
    (set-output-indent old-indent)))

(define (vernacout-linefeed)
  (output-lf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (vernacout x)
  ;; (display* "vernacout " x "\n")
  (cond ((string? x) (output-vernac x))
        ((nlist>0? x) (display* "TeXmacs] bad formated stree:\n" x "\n"))
	((== (car x) '!comment)   (vernacout-comment (cadr x)))
	((== (car x) '!coqdoc)    (vernacout-coqdoc (cadr x)))
	((== (car x) '!item)      (vernacout-item (cadr x)))
	((== (car x) '!paragraph) (vernacout-paragraph (cdr x)))
	((== (car x) '!concat)    (vernacout-concat (cdr x)))
	((== (car x) '!append)    (for-each vernacout (cdr x)))
	((== (car x) '!linefeed)  (vernacout-linefeed))
	((== (car x) '!indent)    (vernacout-indent (cadr x)))
	((== (car x) '!unindent)  (vernacout-unindent (cadr x)))
	(else (display* "TeXmacs] bad formated stree:\n" x "\n"))))

(tm-define (serialize-vernac x)
  (vernacout x)
  (output-produce))
