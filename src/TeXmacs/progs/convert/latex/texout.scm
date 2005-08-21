
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texout.scm
;; DESCRIPTION : generation of TeX/LaTeX from scheme expressions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex texout)
  (:use (convert latex tmtex-preamble) (convert tools output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting preamble and postamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collection->ahash-table init)
  (let* ((t (make-ahash-table))
	 (l (if (func? init 'collection) (cdr init) '()))
	 (f (lambda (x) (ahash-set! t (cadr x) (caddr x)))))
    (for-each f l)
    t))

(define (texout-file l)
  (let* ((doc-body (car l))
	 (styles (if (null? (cadr l)) '("letter") (cadr l)))
	 (style (car styles))
	 (prelan (caddr l))
	 (lan (if (== prelan "") "english" prelan))
	 (init (collection->ahash-table (cadddr l)))
	 (doc-preamble (car (cddddr l)))
	 (doc-misc (append '(!concat) doc-preamble (list doc-body))))

    (receive
	(tm-uses tm-init tm-preamble)
	(tmtex-preamble-build doc-misc style lan init)
      (output-verbatim "\\documentclass{" style "}\n")
      (if (!= tm-uses "")
	  (output-verbatim "\\usepackage{" tm-uses "}\n"))
      (for-each texout-usepackage (cdr styles))

      (if (!= tm-init "")
	  (begin
	    (output-lf)
	    (output-verbatim tm-init)))
      (if (!= tm-preamble "")
	  (begin
	    (output-lf)
	    (output-verbatim "%%%%%%%%%% Start TeXmacs macros\n")
	    (output-verbatim tm-preamble)
	    (output-verbatim "%%%%%%%%%% End TeXmacs macros\n")))
      (if (nnull? doc-preamble)
	  (begin
	    (output-lf)
	    (map-in-order (lambda (x) (texout x) (output-lf)) doc-preamble))))

    (output-lf)
    (output-text "\\begin{document}")
    (output-lf)
    (output-lf)
    (texout doc-body)
    (output-lf)
    (output-lf)
    (output-text "\\end{document}")
    (output-lf)))

(define (texout-usepackage x)
  (output-verbatim "\\usepackage{" x "}\n"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting main flow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texout-document l)
  (if (nnull? l)
      (begin
	(texout (car l))
	(if (nnull? (cdr l))
	    (begin
	      (output-lf)
	      (output-lf)))
	(texout-document (cdr l)))))

(define (texout-paragraph l)
  (if (nnull? l)
      (begin
	(texout (car l))
	(if (nnull? (cdr l)) (output-lf))
	(texout-paragraph (cdr l)))))

(define (texout-table l)
  (if (nnull? l)
      (begin
	(if (func? (car l) '!row)
	    (begin
	      (texout-row (cdar l))
	      (if (nnull? (cdr l))
		  (begin
		    (output-text "\\\\")
		    (output-lf))))
	    (begin
	      (texout (car l))
	      (if (nnull? (cdr l)) (output-lf))))
	(texout-table (cdr l)))))

(define (texout-row l)
  (if (nnull? l)
      (begin
	(texout (car l))
	(if (nnull? (cdr l)) (output-text " & "))
	(texout-row (cdr l)))))

(define (tex-symbol? l)
  (and (list? l) (= 1 (length l))))

(define (texout-env? l)
  (and (list? l) (nnull? l) (func? (car l) '!begin)))

(define (texout-want-space x1 x2) ;; spacing rules
  (and (not (or (== x2 ",")
		(== x1 " ") (== x2 " ")
		(func? x2 '!nextline)
		(== x2 "'") (func? x2 '!sub) (func? x2 '!sup)
		(func? x1 '&) (func? x2 '&)
		(func? x1 '!nbsp) (func? x2 '!nbsp)
		(and (func? x1 '!math) (func? x2 '!math))
		(and (texout-env? x1) (list? x2))
		(and (list? x1) (texout-env? x2))
		(and (== x1 "'") (nlist? x2))))
       (or (func? x1 'tmop) (func? x2 'tmop)
	   (and (nlist? x1) (tex-symbol? x2))
	   (and (nlist? x2) (tex-symbol? x1))
	   (and (list? x1) (list? x2))
	   (and (nlist? x1) (nlist? x2)))))

(define (texout-concat l)
  (if (nnull? l)
      (begin (texout (car l))
	     (if (nnull? (cdr l))
		 (texout-concat (if (texout-want-space (car l) (cadr l))
				    (cons " " (cdr l))
				    (cdr l)))))))

(define (texout-newline)
  (output-lf)
  (output-lf))

(define (texout-nextline)
  (output-text "\\\\")
  (output-lf))

(define (texout-nbsp)
  (output-text "~"))

(define (texout-verb x)
  (output-verb "\\verb¤" x "¤"))

(define (texout-verbatim x)
  (output-lf-verbatim "\\begin{verbatim}\n" x "\n\\end{verbatim}"))

(define (texout-group x)
  (output-text "{")
  (texout x)
  (output-text "}"))

(define (texout-empty? x)
  (cond ((== x "") #t)
	((func? x '!concat) (list-and (map-in-order texout-empty? (cdr x))))
	((func? x '!document 1) (texout-empty? (cadr x)))
	(else #f)))

(define (texout-double-math? x)
  (or (and (match? x '((:or !document !concat) :1))
	   (texout-double-math? (cadr x)))
      (and (match? x '((!begin :1) :1))
	   (in? (cadar x) '("eqnarray" "eqnarray*" "leqnarray*")))))

(define (texout-math x)
  (cond ((texout-empty? x) (noop))
	((texout-double-math? x) (texout x))
	((and (output-test-end? "$") (not (output-test-end? "\\$")))
	 (output-remove 1)
	 (output-text " ")
	 (texout x)
	 (output-text "$"))
	(else
	 (output-text "$")
	 (texout x)
	 (output-text "$"))))

(define (texout-eqn x)
  (output-text "\\[ ")
  (output-indent 3)
  (texout x)
  (output-indent -3)
  (output-text " \\]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outputting macro applications and environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texout-arg x)
  (output-text "#" x))

(define (texout-args l)
  (if (nnull? l)
      (begin
	(if (and (list? (car l)) (== (caar l) '!option))
	    (begin
	      (output-text "[")
	      (texout (cadar l))
	      (output-text "]"))
	    (begin
	      (output-text "{")
	      (texout (car l))
	      (output-text "}")))
	(texout-args (cdr l)))))

(define (texout-apply what args)
  (output-text "\\" (symbol->string what))
  (texout-args args))

(define (texout-begin what args inside)
  (output-text "\\begin{" what "}")
  (texout-args args)
  (output-indent 2)
  (output-lf)
  (texout inside)
  (output-indent -2)
  (output-lf)
  (output-text "\\end{" what "}"))

(tm-define (texout-contains-table? x)
  (cond ((nlist? x) #f)
	((and (>= (length x) 2) (== (car x) '!table)) #t)
	(else (list-or (map-in-order texout-contains-table? (cdr x))))))

(define (texout-script where l)
  (output-text where)
  (let ((x (car l)))
    (cond ((and (string? x) (= (string-length x) 1)) (output-text x))
	  ((texout-contains-table? x)
	   (output-text "{\\tmscript{")
	   (texout x)
	   (output-text "}}"))
	  (else (texout-args l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texout x)
  (cond ((string? x) (output-text x))
	((== (car x) '!file) (texout-file (cdr x)))
	((== (car x) '!document) (texout-document (cdr x)))
	((== (car x) '!paragraph) (texout-paragraph (cdr x)))
	((== (car x) '!table) (texout-table (cdr x)))
	((== (car x) '!concat) (texout-concat (cdr x)))
	((== (car x) '!newline) (texout-newline))
	((== (car x) '!nextline) (texout-nextline))
	((== (car x) '!nbsp) (texout-nbsp))
	((== (car x) '!verb) (texout-verb (cadr x)))
	((== (car x) '!verbatim) (texout-verbatim (cadr x)))
	((== (car x) '!arg) (texout-arg (cadr x)))
	((== (car x) '!group) (texout-group (cadr x)))
	((== (car x) '!math) (texout-math (cadr x)))
	((== (car x) '!eqn) (texout-eqn (cadr x)))
	((== (car x) '!sub) (texout-script "_" (cdr x)))
	((== (car x) '!sup) (texout-script "^" (cdr x)))
	((and (list? (car x)) (== (caar x) '!begin))
	 (texout-begin (cadar x) (cddar x) (cadr x)))
	(else (texout-apply (car x) (cdr x)))))

(tm-define (serialize-latex x)
  (texout x)
  (output-produce))
