
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texout.scm
;; DESCRIPTION : generation of TeX/LaTeX from scheme expressions
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex texout)
  (:use (convert latex latex-tools)
	(convert tools output)))

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
	 (styles (if (null? (cadr l)) (list "letter") (cadr l)))
	 (style (car styles))
	 (prelan (caddr l))
	 (lan (if (== prelan "") "english" prelan))
	 (init (collection->ahash-table (cadddr l)))
	 (doc-preamble (car (cddddr l)))
	 (doc-misc (append '(!concat) doc-preamble (list doc-body))))

    (receive
	(tm-uses tm-init tm-preamble)
	(latex-preamble doc-misc style lan init)
      (if (and (== lan "japanese") (== style "article"))
	  (set! style "jarticle"))
      (if (and (== lan "japanese") (== style "book"))
	  (set! style "jbook"))
      (output-verbatim "\\documentclass{" style "}\n")
      (if (== lan "korean")
	  (output-verbatim "\\usepackage{dhucs}\n"))
      (if (in? lan '("chinese" "taiwanese"))
	  (output-verbatim "\\usepackage{CJK}\n"))
      (output-verbatim tm-uses)
      (for-each texout-usepackage (cdr styles))
      (output-verbatim tm-init)

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

(tm-define (texout-document l)
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
	      (texout-row* (cdar l))
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

(define (texout-row* l)
  ;; Dirty hack to avoid [ strings at start of a row
  ;; because of confusion with optional argument of \\
  (if (and (pair? l) (string? (car l)) (string-starts? (car l) "["))
      (set! l `((!concat (!group "") ,(car l)) ,@(cdr l))))
  (if (and (pair? l) (func? (car l) '!concat)
	   (string? (cadar l)) (string-starts? (cadar l) "["))
      (set! l `((!concat (!group "") ,@(cdar l)) ,@(cdr l))))
  (texout-row l))

(define (texout-want-space x1 x2) ;; spacing rules
  (and (not (or (in? x1 '("(" "[" ({) (nobreak)))
		(in? x2 '("," ")" "]" (}) (nobreak)))
		(== x1 " ") (== x2 " ")
		(func? x2 '!nextline)
		(== x2 "'") (func? x2 '!sub) (func? x2 '!sup)
		(func? x1 '&) (func? x2 '&)
		(func? x1 '!nbsp) (func? x2 '!nbsp)
		(and (== x1 "'") (nlist? x2))))
       (or (in? x1 '("," ";" ":"))
	   (func? x1 'tmop) (func? x2 'tmop)
	   (func? x1 '!symbol) (func? x2 '!symbol)
	   (and (nlist? x1) (nlist? x2)))))

(define (texout-concat l)
  (when (nnull? l)
    (texout (car l))
    (if (nnull? (cdr l))
	(texout-concat (if (texout-want-space (car l) (cadr l))
			   (cons " " (cdr l))
			   (cdr l))))))

(define (texout-newline)
  (output-lf)
  (output-lf))

(define (texout-nextline)
  (output-text "\\\\")
  (output-lf))

(define (texout-nbsp)
  (output-text "~"))

(define (texout-verb x)
  (cond ((not (string-index x #\|)) (output-verb "\\verb|" x "|"))
	((not (string-index x #\$)) (output-verb "\\verb$" x "$"))
	((not (string-index x #\@)) (output-verb "\\verb@" x "@"))
	((not (string-index x #\!)) (output-verb "\\verb!" x "!"))
	((not (string-index x #\9)) (output-verb "\\verb9" x "9"))
	((not (string-index x #\X)) (output-verb "\\verbX" x "X"))
	(else (output-verb "\\verb¤" x "¤"))))

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
  (or (and (match? x '((:or !document !concat) :%1))
	   (texout-double-math? (cadr x)))
      (and (match? x '((!begin :%1) :%1))
	   (in? (cadar x) '("eqnarray" "eqnarray*" "leqnarray*")))))

(define (texout-math x)
  (cond ((texout-empty? x) (noop))
	((texout-double-math? x) (texout x))
	((match? x '((!begin "center") :%1))
	 (texout `((!begin "equation") ,(cadr x))))
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

(define (texout-script where l)
  (output-text where)
  (let ((x (car l)))
    (cond ((and (string? x) (= (string-length x) 1)) (output-text x))
	  (else (texout-args l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main output routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texout x)
  ;; (display* "texout " x "\n")
  (cond ((string? x) (output-text x))
	((== (car x) '!widechar) (output-text (symbol->string (cadr x))))
	((== (car x) '!file) (texout-file (cdr x)))
	((== (car x) '!document) (texout-document (cdr x)))
	((== (car x) '!paragraph) (texout-paragraph (cdr x)))
	((== (car x) '!table) (texout-table (cdr x)))
	((== (car x) '!concat) (texout-concat (cdr x)))
	((== (car x) '!append) (for-each texout (cdr x)))
	((== (car x) '!symbol) (texout (cadr x)))
	((== (car x) '!newline) (texout-newline))
	((== (car x) '!nextline) (texout-nextline))
	((== (car x) '!nbsp) (texout-nbsp))
	((== (car x) '!verb) (texout-verb (cadr x)))
	((== (car x) '!verbatim) (texout-verbatim (cadr x)))
	((== (car x) '!arg) (texout-arg (cadr x)))
	((== (car x) '!group) (texout-group (cons '!append (cdr x))))
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

