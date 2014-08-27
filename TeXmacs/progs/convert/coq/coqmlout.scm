
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqmlout.scm
;; DESCRIPTION : generation of Xml from scheme expressions
;; COPYRIGHT   : (C) 2013  François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq coqmlout)
  (:use (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determining output layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqmlout-big? tag)
  (nin? tag '(int string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqmlout-indent plus big?)
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

(define (coqmlout-attr x)
  ;(display-err* "[coqmlout-attr] " x "\n")
  (output-string (string-append " " (symbol->string (car x)) "="))
  (output-text "\"")
  (output-string (cadr x))
  (output-text "\""))

(define (coqmlout-stacked-args l)
  (if (nnull? l)
      (begin
	(coqmlout (car l))
	(if (nnull? (cdr l))
	    (begin
	      (output-lf)
	      (output-lf)))
	(coqmlout-stacked-args (cdr l)))))

(define (coqmlout-args l big?)
  ;(display-err* "[coqmlout-args] " l ", " big? "\n")
  (cond ((null? l) (noop))
	((string? (car l))
	 (output-string (car l))
	 (coqmlout-args (cdr l) big?))
	(else
	 (coqmlout (car l))
	 (if (and big?
		  (pair? (cdr l))
		  (nstring? (car l))
		  (nstring? (cadr l)))
	     (begin
	       (output-lf)
	       (if (func? (cadr l) 'tm-par) (output-lf))))
	 (coqmlout-args (cdr l) big?))))

(define (coqmlout-tag tag attrs args)
  ;(display-err* "[coqmlout-tag] " tag ", " attrs ", " args "\n")
  (with big? (coqmlout-big? tag)
    (output-text "<")
    (output-text (symbol->string tag))
    (for-each coqmlout-attr attrs)
    (if (null? args) (output-text "/"))
    (output-text ">")
    (if (nnull? args)
	(begin
	  (coqmlout-indent 2 big?)
	  (coqmlout-args args big?)
	  (coqmlout-indent -2 big?)
	  (output-text "</" (symbol->string tag) ">")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Note: escaping strings in text-elements and in attributes is not done.

(define (coqmlout x)
  ;(display-err* "[coqmlout] " x "\n")
  (cond ((string? x) (output-string (cork->utf8 x)))
	((null? x) (noop))
	((and (pair? (cdr x)) (func? (cadr x) '@))
	 (coqmlout-tag (car x) (cdadr x) (cddr x)))
	((func? x '*TOP*) (coqmlout-stacked-args (cdr x)))
	(else (coqmlout-tag (car x) '() (cdr x)))))

(tm-define (serialize-coqml x)
  (coqmlout x)
  (output-produce))
