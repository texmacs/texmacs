
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

(texmacs-module (convert coq vernacout)
          (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for board effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define verb-output-mode #f)
(define com-output-mode #f)
(define doc-output-mode #f)

(define (vernacout-initialize)
  (set! verb-output-mode #f)
  (set! com-output-mode #f)
  (set! doc-output-mode #f))

(define (vernacout-update-indent)
  (output-remove-indentation)
  (with indent (get-output-indent)
    (if (> indent 0)
      (output-verb (make-string (get-output-indent) #\space)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for string output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-vernac s)
  (if verb-output-mode
    (output-verb s)
    (output-text s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (vernacout-file l)
  (vernacout l)
  (output-lf))

(define (vernacout-verbatim l)
  (set! verb-output-mode #t)
  (vernacout l)
  (set! verb-output-mode #f))

(define (vernacout-item i)
  (output-indent -2)
  (vernacout-update-indent)
  (vernacout i)
  (output-indent 2))

(define (vernacout-comment l)
  (set! com-output-mode #t)
  (with old-indent (get-output-indent)
    (set-output-indent 0)
    (vernacout "(*")
    (vernacout l)
    (vernacout "*)")
    (set-output-indent old-indent))
  (set! com-output-mode #f))

(define (vernacout-coqdoc l)
  (set! doc-output-mode #t)
  (vernacout "(** ")
  (output-indent 4)
  (vernacout l)
  (vernacout " *)")
  (output-indent -4)
  (set! doc-output-mode #f))

(define (vernacout-paragraph l)
  (if (nnull? l)
    (begin
      (cond ((== (car l) "") (output-remove-indentation))
            ((and (== (car l) "*)")
                  (or com-output-mode doc-output-mode))
             (begin
               (output-indent (if com-output-mode -3 -4))
               (vernacout-update-indent)
               (output-indent (if com-output-mode 3 4)))))
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
  (vernacout-update-indent)
  (vernacout x)
  (output-indent -2))

(define (vernacout-unindent x)
  (with old-indent (get-output-indent)
    (set-output-indent 0)
    (vernacout-update-indent)
    (vernacout x)
    (set-output-indent old-indent)))

(define (vernacout-linefeed)
  (output-lf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (vernacout x)
  (cond ((string? x) (output-vernac x))
        ((nlist>0? x) (display* "TeXmacs] badly formatted stree:\n" x "\n"))
	((== (car x) '!file)      (vernacout-file (cadr x)))
	((== (car x) '!comment)   (vernacout-comment (cadr x)))
	((== (car x) '!coqdoc)    (vernacout-coqdoc (cadr x)))
	((== (car x) '!verbatim)  (vernacout-verbatim (cadr x)))
	((== (car x) '!item)      (vernacout-item (cadr x)))
	((== (car x) '!paragraph) (vernacout-paragraph (cdr x)))
	((== (car x) '!concat)    (vernacout-concat (cdr x)))
	((== (car x) '!append)    (for-each vernacout (cdr x)))
	((== (car x) '!linefeed)  (vernacout-linefeed))
	((== (car x) '!indent)    (vernacout-indent (cadr x)))
	((== (car x) '!unindent)  (vernacout-unindent (cadr x)))
	(else (display* "TeXmacs] bad formated stree:\n" x "\n"))))

(tm-define (serialize-vernac x)
  (vernacout-initialize)
  (vernacout x)
  (output-produce))
