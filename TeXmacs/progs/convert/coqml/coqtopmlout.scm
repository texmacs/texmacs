
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqtopmlout.scm
;; DESCRIPTION : generation of Xml from scheme expressions
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml coqtopmlout)
  (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining output layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtopmlout-big? tag)
  (nin? tag '(int string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtopmlout-indent plus big?)
  (cond (big? (output-indent plus) (output-lf))
	(else (noop))))

(define (escape-xml-string s)
  (set! s (string-replace s "&"  "&amp;"))
  (set! s (string-replace s "\"" "&quot;"))
  (set! s (string-replace s "'"  "&apos;"))
  (set! s (string-replace s "<"  "&lt;"))
  (set! s (string-replace s ">"  "&gt;"))
  (set! s (string-replace s "\xe2\x80\x98"  "`"))
  s)

(define (output-string s)
  (output-verbatim (escape-xml-string (cork->utf8 s))))

(define (coqtopmlout-attr x)
  ;(display-err* "[coqtopmlout-attr] " x "\n")
  (output-string (string-append " " (symbol->string (car x)) "="))
  (output-text "\"")
  (output-string (cadr x))
  (output-text "\""))

(define (coqtopmlout-stacked-args l)
  (if (nnull? l)
      (begin
	(coqtopmlout (car l))
	(if (nnull? (cdr l))
	    (begin
	      (output-lf)
	      (output-lf)))
	(coqtopmlout-stacked-args (cdr l)))))

(define (coqtopmlout-args l big?)
  ;(display-err* "[coqtopmlout-args] " l ", " big? "\n")
  (cond ((null? l) (noop))
	((string? (car l))
	 (output-string (car l))
	 (coqtopmlout-args (cdr l) big?))
	(else
	 (coqtopmlout (car l))
	 (if (and big?
		  (pair? (cdr l))
		  (nstring? (car l))
		  (nstring? (cadr l)))
	     (begin
	       (output-lf)
	       (if (func? (cadr l) 'tm-par) (output-lf))))
	 (coqtopmlout-args (cdr l) big?))))

(define (coqtopmlout-tag tag attrs args)
  ;(display-err* "[coqtopmlout-tag] " tag ", " attrs ", " args "\n")
  (with big? (coqtopmlout-big? tag)
    (output-text "<")
    (output-text (symbol->string tag))
    (for-each coqtopmlout-attr attrs)
    (if (null? args) (output-text "/"))
    (output-text ">")
    (if (nnull? args)
	(begin
	  (coqtopmlout-indent 2 big?)
	  (coqtopmlout-args args big?)
	  (coqtopmlout-indent -2 big?)
	  (output-text "</" (symbol->string tag) ">")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: escaping strings in text-elements and in attributes is not done.

(define (coqtopmlout x)
  ;(display-err* "[coqtopmlout] " x "\n")
  (cond ((string? x) (output-string (cork->utf8 x)))
	((null? x) (noop))
	((and (pair? (cdr x)) (func? (cadr x) '@))
	 (coqtopmlout-tag (car x) (cdadr x) (cddr x)))
	((func? x '*TOP*) (coqtopmlout-stacked-args (cdr x)))
	(else (coqtopmlout-tag (car x) '() (cdr x)))))

(tm-define (serialize-coqtopml x)
  (coqtopmlout x)
  (output-produce))

;; temp
(tm-define (serialize-coqml x)
  (coqtopmlout x)
  (output-produce))
