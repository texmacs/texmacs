
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-tools.scm
;; DESCRIPTION : Routines for expansion of macros and preamble construction
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-tools)
  (:use (convert latex latex-drd)
	(convert latex texout)))

(tm-define tmtex-use-catcodes? #t)
(tm-define tmtex-use-macros? #f)

(define latex-language "english")
(define latex-cyrillic-catcode? #f)
(define latex-style "generic")
(define latex-style-hyp 'generic-style%)
(define latex-packages '())
(define latex-amsthm-hyp 'no-amsthm-package%)

(define latex-uses-table (make-ahash-table))
(define latex-catcode-table (make-ahash-table))
(define latex-macro-table (make-ahash-table))
(define latex-env-table (make-ahash-table))
(define latex-preamble-table (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setting global parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-set-language lan)
  (set! latex-language lan)
  (set! latex-cyrillic-catcode?
	(in? lan '("bulgarian" "russian" "ukrainian"))))

(tm-define (latex-set-style sty)
  (set! latex-style sty)
  (set! latex-style-hyp (string->symbol (string-append sty "-style%"))))

(tm-define (latex-set-packages ps)
  (set! latex-packages ps)
  (when (in? "amsthm" ps)
    (set! latex-amsthm-hyp 'amsthm-package%)))

(tm-define (latex-book-style?)
  (in? latex-style '("book")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catcode expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-replace-catcode s)
  (or (if latex-cyrillic-catcode?
	  (drd-ref cyrillic-catcodes% s)
	  (drd-ref corkT1-to-latex-catcodes% s))
      s))

(tm-define (latex-expand-catcodes t)
  (:synopsis "Expand all catcodes in @t")
  (cond ((string? t)
	 (with l (map string (string->list t))
	   (apply string-append (map latex-replace-catcode l))))
	((pair? t) (cons (car t) (map latex-expand-catcodes (cdr t))))
	(else t)))

(define (latex-expand-catcodes* t)
  (if tmtex-use-catcodes? t (latex-expand-catcodes t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute catcode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-catcode-defs-char c)
  (let* ((s (list->string (list c)))
	 (r (latex-replace-catcode s)))
    (if (!= r s) (ahash-set! latex-catcode-table s r))))

(define (latex-catcode-defs-sub doc)
  (cond ((string? doc) (for-each latex-catcode-defs-char (string->list doc)))
	((list? doc) (for-each latex-catcode-defs-sub doc))))

(define (latex-catcode-def key im)
  (string-append "\\catcode`\\" key "=\\active \\def" key "{" im "}\n"))

(tm-define (latex-catcode-defs doc)
  (:synopsis "Return necessary catcode definitions for @doc")
  (set! latex-catcode-table (make-ahash-table))
  (latex-catcode-defs-sub doc)
  (let* ((l1 (ahash-table->list latex-catcode-table))
	 (l2 (list-sort l1 (lambda (x y) (string<=? (car x) (car y)))))
	 (l3 (map (lambda (x) (latex-catcode-def (car x) (cdr x))) l2)))
    (apply string-append l3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro and environment expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-substitute t args)
  (cond ((number? t) (list-ref args t))
	((== t '---) (car args))
	((func? t '!recurse 1)
	 (latex-expand-macros (latex-substitute (cadr t) args)))
	((func? t '!translate 1)
	 (translate (cadr t) "english" latex-language))
	((list? t) (map (cut latex-substitute <> args) t))
	(else t)))

(tm-define (latex-expand-macros t)
  (:synopsis "Expand all TeXmacs macros occurring in @t")
  (if (npair? t) t
      (let* ((head  (car t))
	     (tail  (map latex-expand-macros (cdr t)))
	     (body  (drd-ref latex-texmacs-macro% head
			     latex-style-hyp latex-amsthm-hyp))
	     (arity (drd-ref latex-texmacs-arity% head
			     latex-style-hyp latex-amsthm-hyp))
	     (env   (and (func? head '!begin)
			 (drd-ref latex-texmacs-environment% (cadr head)
				  latex-style-hyp latex-amsthm-hyp)))
	     (envar (and (func? head '!begin)
			 (drd-ref latex-texmacs-env-arity% (cadr head)
				  latex-style-hyp latex-amsthm-hyp))))
	(cond ((and body (== (length tail) arity))
	       (latex-substitute body t))
	      ((and env (== (length tail) 1) (== (length (cddr head)) envar))
	       (latex-substitute env (append (cdr t) (cddr head))))
	      (else (cons head tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute macro and environment definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-expand-def t)
  (cond ((== t '---) "#-#-#")
	((number? t) (string-append "#" (number->string t)))
	((func? t '!recurse 1) (latex-expand-def (cadr t)))
	((func? t '!translate 1) (translate (cadr t) "english" latex-language))
	((list? t) (map latex-expand-def t))
	(else t)))

(define (latex-macro-defs-sub t)
  (when (pair? t)
    (for-each latex-macro-defs-sub (cdr t))
    (let* ((body  (drd-ref latex-texmacs-macro% (car t)
			   latex-style-hyp latex-amsthm-hyp))
	   (arity (drd-ref latex-texmacs-arity% (car t)
			   latex-style-hyp latex-amsthm-hyp)))
      (when (and body (== (length t) (+ arity 1)))
	(ahash-set! latex-macro-table (car t)
		    (list arity (latex-expand-def body)))
	(latex-macro-defs-sub body)))
    (let* ((body  (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-environment% (cadar t))))
	   (arity (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-env-arity% (cadar t)))))
      (when (and body (== (length (cddar t)) arity))
	(ahash-set! latex-env-table (cadar t)
		    (list arity (latex-expand-def body)))
	(latex-macro-defs-sub body)))
    (with body (or (drd-ref latex-texmacs-preamble% (car t)
			    latex-style-hyp latex-amsthm-hyp)
		   (and (func? (car t) '!begin)
			(drd-ref latex-texmacs-env-preamble% (cadar t)
				 latex-style-hyp latex-amsthm-hyp)))
      (when body
	(ahash-set! latex-preamble-table (car t) body)))))

(define (latex<=? x y)
  (if (symbol? x) (set! x (symbol->string x)))
  (if (symbol? y) (set! y (symbol->string y)))
  (if (func? x '!begin) (set! x (cadr x)))
  (if (func? y '!begin) (set! y (cadr y)))
  (string<=? x y))

(tm-define (latex-macro-defs t)
  (:synopsis "Return necessary macro and environment definitions for @doc")
  (set! latex-macro-table (make-ahash-table))
  (set! latex-env-table (make-ahash-table))
  (set! latex-preamble-table (make-ahash-table))
  (latex-macro-defs-sub t)
  (let* ((c1 (ahash-table->list latex-macro-table))
	 (c2 (list-sort c1 (lambda (x y) (latex<=? (car x) (car y)))))
	 (c3 (map (cut cons '!newcommand <>) c2))
	 (e1 (ahash-table->list latex-env-table))
	 (e2 (list-sort e1 (lambda (x y) (latex<=? (car x) (car y)))))
	 (e3 (map (cut cons '!newenvironment <>) e2))
	 (p1 (ahash-table->list latex-preamble-table))
	 (p2 (list-sort p1 (lambda (x y) (latex<=? (car x) (car y)))))
	 (p3 (map cdr (map latex-expand-def p2))))
    (cons '!append (append c3 e3 p3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization of TeXmacs preambles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-macro-def name arity body)
  (set! body (serialize-latex (latex-expand-def body)))
  (set! body (string-replace body "\n\n" "*/!!/*"))
  (set! body (string-replace body "\n" " "))
  (set! body (string-replace body "*/!!/*" "\n\n"))
  (string-append "\\newcommand{\\" (symbol->string name) "}"
		 (if (= arity 0) ""
		     (string-append "[" (number->string arity) "]"))
		 "{" body "}\n"))

(define (latex-env-def name arity body)
  (set! body (serialize-latex (latex-expand-def body)))
  (set! body (string-replace body "\n\n" "*/!!/*"))
  (set! body (string-replace body "\n  " " "))
  (set! body (string-replace body "\n" " "))
  (set! body (string-replace body "   #-#-# " "}{"))
  (set! body (string-replace body "#-#-# " "}{"))
  (set! body (string-replace body "#-#-#" "}{"))
  (set! body (string-replace body "*/!!/*" "\n\n"))
  (string-append "\\newenvironment{" name "}"
		 (if (= arity 0) ""
		     (string-append "[" (number->string arity) "]"))
		 "{" body "}\n"))

(tm-define (latex-serialize-preamble t)
  (:synopsis "Serialize a LaTeX preamble @t")
  (cond ((string? t) t)
	((func? t '!append)
	 (apply string-append (map latex-serialize-preamble (cdr t))))
	((func? t '!newcommand 3) (apply latex-macro-def (cdr t)))
	((func? t '!newenvironment 3) (apply latex-env-def (cdr t)))
	(else (serialize-latex t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute usepackage command for a document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-command-uses s)
  (with packlist (drd-ref-list latex-needs% s)
    (when packlist
      (for-each (cut ahash-set! latex-uses-table <> #t) packlist))))

(define (latex-use-which-package l)
  (when (and (list? l) (nnull? l))
    (let ((x (car l)))
      (if (symbol? x) (latex-command-uses x))
      (if (and (list? x) (>= (length l) 2) (== (car x) '!begin))
	  (latex-command-uses (string->symbol (cadr x))))
      (if (match? x '(!begin "enumerate" (!option :%1)))
	  (ahash-set! latex-uses-table "enumerate" #t))
      (for-each latex-use-which-package (cdr l)))))

(define (latex-use-package-compare l r)
  (let* ((tl (drd-ref latex-package-priority% l))
	 (tr (drd-ref latex-package-priority% r))
	 (vl (if tl tl 999999))
	 (vr (if tr tr 999999)))
    (< vl vr)))

(define (latex-as-use-package l1)
  (let* ((l2 (sort l1 latex-use-package-compare))
	 (l3 (map force-string l2))
	 (l4 (list-intersperse l3 ","))
	 (s  (apply string-append l4)))
    (if (== s "") "" (string-append "\\usepackage{" s "}\n"))))

(tm-define (latex-use-package-command doc)
  (:synopsis "Return the usepackage command for @doc")
  (set! latex-uses-table (make-ahash-table))
  (latex-use-which-package doc)
  (let* ((l1 latex-packages)
	 (s1 (latex-as-use-package (list-difference l1 '("amsthm"))))
	 (l2 (map car (ahash-table->list latex-uses-table)))
	 (s2 (latex-as-use-package (list-difference l2 l1))))
    (string-append s1 s2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-preamble-language lan)
  (if (drd-ref latex-preamble-language-def% lan)
      (string-append (drd-ref latex-preamble-language-def% lan) "\n")
      ""))

(define (latex-preamble-page-type init)
  (let* ((page-type (ahash-ref init "page-type"))
	 (page-size (drd-ref latex-paper-type% page-type)))
    (if page-size `(!append (geometry ,page-size) "\n") "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the preamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-preamble text style lan init)
  (:synopsis "Compute preamble for @text")
  (let* ((Expand       latex-expand-catcodes*)
	 (Page         (Expand (latex-preamble-page-type init)))
	 (Macro        (Expand (latex-macro-defs text)))
	 (Text         (list '!tuple Page Macro text))
	 (pre-language (latex-preamble-language lan))
	 (pre-page     (latex-serialize-preamble Page))
	 (pre-macro    (latex-serialize-preamble Macro))
	 (pre-catcode  (latex-catcode-defs Text))
	 (pre-uses     (latex-use-package-command Text)))
    (values
      (if (in? "amsthm" latex-packages) "[amsthm]" "")
      (string-append pre-uses)
      (string-append pre-page)
      (string-append pre-language pre-catcode pre-macro))))
