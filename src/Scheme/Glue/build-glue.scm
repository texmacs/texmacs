
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : build-glue.scm
;; DESCRIPTION : Glue builder for using TeXmacs routines in guile
;; COPYRIGHT   : (C) 2000  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convenient output routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define output-sub
  (lambda (l)
    (if (not (null? l))
	(begin
	  (display (car l))
	  (output-sub (cdr l))))))

(define output
  (lambda l
    (output-sub l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Upcase string (not implemented in older versions of guile)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-upcase
  (lambda (l)
    (if (null? l) l
      (cons (char-upcase (car l)) (list-upcase (cdr l))))))

(define string-upcase
  (lambda (s)
    (list->string (list-upcase (string->list s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Print copyright notice
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-copyright from)
  (output "
/******************************************************************************
*
* This file has been generated automatically using build-glue.scm
* from " from ". Please do not edit its contents.
* Copyright (C) 2000 Joris van der Hoeven
*
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
*
******************************************************************************/
"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determine the name of the glue routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define translate-name-char
  (lambda (c)
    (cond ((eq? c #\?) #\P)
	  ((eq? c #\!) #\S)
	  ((eq? c #\>) #\2)
	  ((eq? c #\-) #\_)
	  (else c))))

(define translate-name-sub
  (lambda (l)
    (if (not (null? l))
	(cons (translate-name-char (car l))
	      (translate-name-sub (cdr l)))
      l)))

(define translate-name
  (lambda (s)
    (let* ((S (symbol->string s))
	   (l (- (string-length S) 1)))
      (if (and (>= l 0) (equal? (string-ref S l) #\*))
	  (set! s (string->symbol (string-append (substring S 0 l) "_dot"))))
      (string-append "tmg_"
        (list->string
	  (translate-name-sub
            (string->list
	      (symbol->string s))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for building a glue code subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Build header

(define (build-header-args type nr)
  (output "SCM arg" nr)
  (if (not (null? (cdr type)))
      (begin
        (output ", ")
	(build-header-args (cdr type) (+ nr 1)))))

(define (build-header name type)
  (output "\nSCM\n" (translate-name name) " (")
  (if (not (null? type))
      (build-header-args type 1))
  (output ") {\n"))

;; Type checking

(define (build-assert-sub name type nr)
  (if (not (null? type))
      (begin
        (output "  SCM_ASSERT_")
	(output (string-upcase (symbol->string (car type))))
	(output " (arg" nr ", SCM_ARG" nr ", \"" name "\");\n")
	(build-assert-sub name (cdr type) (+ nr 1)))))

(define (build-assert name type)
  (build-assert-sub name type 1)
  (if (not (null? type)) (output "\n")))

;; Convert input arguments

(define (build-get-in-sub type nr)
  (if (not (null? type))
      (begin
        (output "  " (car type) " in" nr "= ")
	(output "scm_to_" (car type) " (arg" nr ");\n")
	(build-get-in-sub (cdr type) (+ nr 1)))))

(define (build-get-in arg-type)
  (build-get-in-sub arg-type 1)
  (if (not (null? arg-type)) (output "\n")))

;; Main code

(define (build-code-args type nr)
  (output "in" nr)
  (if (not (null? (cdr type)))
      (begin
        (output ", ")
        (build-code-args (cdr type) (+ nr 1)))))

(define (build-code routine ret-type arg-type)
  (output "  // SCM_DEFER_INTS;\n")
  (output "  ")
  (if (not (equal? ret-type 'void))
      (output ret-type " out= "))
  (output routine " (")
  (if (not (null? arg-type))
      (build-code-args arg-type 1))
  (output ");\n")
  (output "  // SCM_ALLOW_INTS;\n\n"))

;; Terminate

(define (build-footer ret-type)
  (if (equal? ret-type 'void)
      (output "  return SCM_UNSPECIFIED;\n")
    (output "  return " ret-type "_to_scm (out);\n"))
  (output "}\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main build routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-routine prefix l)
  (let ((name (car l))
	(routine (string-append prefix (symbol->string (cadr l))))
	(ret-type (caaddr l))
	(arg-type (cdaddr l)))
    ;; (output "\n" name ", " routine ", " ret-type ", " arg-type "\n")
    (build-header name arg-type)
    (build-assert name arg-type)
    (build-get-in arg-type)
    (build-code routine ret-type arg-type)
    (build-footer ret-type)))

(define (build-routines prefix l)
  (if (not (null? l))
      (begin
	(build-routine prefix (car l))
	(build-routines prefix (cdr l)))))

;; Build initialization routine

(define (build-declare-routine l)
  (let ((name (car l))
	(arg-type (cdaddr l)))
    ;; (output "\n" name ", " arg-type "\n")
    (output "  scm_install_procedure (\"" name "\", ")
    (output " " (translate-name name) ", ")
    (output (length arg-type) ", 0, 0);\n")))

(define (build-declare-routines l)
  (if (not (null? l))
      (begin
        (build-declare-routine (car l))
	(build-declare-routines (cdr l)))))

(define (build-initialization name l)
  (output "\nvoid\n" name " () {\n")
  (build-declare-routines l)
  (output "}\n"))

;; Main build routines

(define (build-main l)
  (build-routines (car l) (cddr l))
  (build-initialization (cadr l) (cddr l)))

(define-macro build
  (lambda l (build-main l)))

