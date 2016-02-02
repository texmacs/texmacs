
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; make-apidoc-doc.scm   (derived from build-glue.scm)
;; DESCRIPTION : generates a minimal doc file for all glue symbols
;; COPYRIGHT   : (C) 2016 The TeXmacs team
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-modules (ice-9 regex))

(define glue-defs '("build-glue-basic.scm" "build-glue-server.scm" "build-glue-editor.scm"))

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

(define (output-copyright from)
  noop)

(define (output-arg arg)
      (output " <scm-arg|" arg ">"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main build routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-routine l) ;; create an "explain" tag
  (let ((name (car l))
	(croutine (cadr l))
	(ret-type (caaddr l))
	(args (cdaddr l)))
(output "  <\\explain>\n    <scm|(" (regexp-substitute/global #f "[>]"  (symbol->string name) 'pre "\\<gtr\\>" 'post))
(map output-arg args)
(output ")>
<explain-synopsis|no synopsis>\n  <|explain>
    Calls the <c++> function <cpp|" croutine "> which returns
    <scm|" ret-type ">.
  </explain>

") 
))

(define build-routines
  (lambda (l)
    (if (not (null? l))
	(begin
	  (build-routine (car l))
	  (build-routines (cdr l))))))


(define (build-main l)
   (build-routines (cddr l)))

(define-macro build
  (lambda l (build-main l)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation of the help document
;;
;; FIXME? it is tempting to define a macro 
;; <assign|details|<\\macro|croutine|ret-type>
;;  Calls the <c++> function <cpp|<arg|croutine>> which returns
;;  <scm|<arg|ret-type>>.
;; </macro>>
;;
;; but presently macros are not expanded when the apidoc cache is collected
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(begin
  (output "<TeXmacs|1.99.4>

<style|<tuple|tmdoc|english>>

<\\body>
<tmdoc-title|All glue functions>

This document lists all available <scheme> functions that are implemented in
the <c++> code and which, consequently, are neither defined nor documented in the
<scheme> modules. Ideally each of these functions should be documented
elsewhere in the documentation.

This document was generated automatically from the glue code definitions by
the script <verbatim|src/src/Scheme/Glue/make-apidoc-doc.scm> in <TeXmacs>
source code.

\\;

\\;

")

  (map load glue-defs)
  (output "
  <tmdoc-copyright|2016|the <TeXmacs> team>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled \"GNU Free
  Documentation License\".>
</body>")
)

  
