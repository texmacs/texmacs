
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-tools.scm
;; DESCRIPTION : Routines for expansion of macros and preamble construction
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-tools)
  (:use (convert latex latex-drd)
	(convert latex latex-texmacs-drd)
	(convert latex texout)))

(define latex-language "english")
(define latex-cyrillic-catcode? #f)
(define latex-style "generic")
(define latex-style-hyp 'generic-style%)

(define latex-macro-table (make-ahash-table))
(define latex-env-table (make-ahash-table))
(define latex-preamble "")

(define tmtex-preamble-uses (make-ahash-table))
(define tmtex-preamble-init "")
(define tmtex-preamble-result "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Catcode substitutions and definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (latex-set-language lan)
  (set! latex-language lan)
  (set! latex-cyrillic-catcode?
	(in? lan '("bulgarian" "russian" "ukrainian"))))

(tm-define (latex-set-style sty)
  (set! latex-style sty)
  (set! latex-style-hyp (string->symbol (string-append sty "-style%"))))

(tm-define (latex-catcode c)
  (with s (list->string (list c))
    (or (if latex-cyrillic-catcode?
	    (drd-ref cyrillic-catcodes% s)
	    (drd-ref iso-latin-catcodes% s))
	s)))

(define latex-catcode-table (make-ahash-table))

(define (latex-catcode-defs-char c)
  (let* ((s (list->string (list c)))
	 (r (latex-catcode c)))
    (if (!= r s) (ahash-set! latex-catcode-table s r))))

(define (latex-catcode-defs-sub doc)
  (cond ((string? doc) (for-each latex-catcode-defs-char (string->list doc)))
	((list? doc) (for-each latex-catcode-defs-sub doc))))

(define (latex-catcode-def key im)
  (string-append "\\catcode`\\" key "=\\active \\def" key "{" im "}\n"))

(tm-define (latex-catcode-defs doc)
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
  (if (npair? t) t
      (let* ((head  (car t))
	     (tail  (map latex-expand-macros (cdr t)))
	     (body  (drd-ref latex-texmacs-macro% head latex-style-hyp))
	     (arity (drd-ref latex-texmacs-arity% head latex-style-hyp))
	     (env   (and (func? head '!begin)
			 (drd-ref latex-texmacs-environment% (cadr head))))
	     (envar (and (func? head '!begin)
			 (drd-ref latex-texmacs-env-arity% (cadr head)))))
	(cond ((and body (== (length tail) arity))
	       (latex-substitute body t))
	      ((and env (== (length tail) 1) (== (length (cddr head)) envar))
	       (latex-substitute env (append (cdr t) (cddr head))))
	      (else (cons head tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macro definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (latex-macro-defs-sub t)
  (when (pair? t)
    (for-each latex-macro-defs-sub (cdr t))
    (let* ((body  (drd-ref latex-texmacs-macro% (car t) latex-style-hyp))
	   (arity (drd-ref latex-texmacs-arity% (car t) latex-style-hyp)))
      (when (and body (== (length t) (+ arity 1)))
	(ahash-set! latex-macro-table (car t) (list arity body))
	(latex-macro-defs-sub body)))
    (let* ((body  (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-environment% (cadar t))))
	   (arity (and (func? (car t) '!begin)
		       (drd-ref latex-texmacs-env-arity% (cadar t)))))
      (when (and body (== (length (cddar t)) arity))
	(ahash-set! latex-env-table (cadar t) (list arity body))
	(latex-macro-defs-sub body)))
    (with body (or (drd-ref latex-texmacs-preamble% (car t))
		   (and (func? (car t) '!begin)
			(drd-ref latex-texmacs-env-preamble% (cadar t))))
      (when body
	(with s (serialize-latex (latex-expand-def body))
	  (set! latex-preamble (string-append latex-preamble s)))))))

(define (latex-expand-def t)
  (cond ((== t '---) "#-#-#")
	((number? t) (string-append "#" (number->string t)))
	((func? t '!recurse 1) (latex-expand-def (cadr t)))
	((func? t '!translate 1) (translate (cadr t) "english" latex-language))
	((list? t) (map latex-expand-def t))
	(else t)))

(define (latex-macro-def l)
  (with (name arity body) l
    (set! body (serialize-latex (latex-expand-def body)))
    (set! body (string-replace body "\n\n" "*/!!/*"))
    (set! body (string-replace body "\n" " "))
    (set! body (string-replace body "*/!!/*" "\n\n"))
    (string-append "\\newcommand{\\" (symbol->string name) "}"
		   (if (= arity 0) ""
		       (string-append "[" (number->string arity) "]"))
		   "{" body "}\n")))

(define (latex-env-def l)
  (with (name arity body) l
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
		   "{" body "}\n")))

(define (symbol<=? x y)
  (string<=? (symbol->string x) (symbol->string y)))

(tm-define (latex-macro-defs t)
  (set! latex-macro-table (make-ahash-table))
  (set! latex-preamble "")
  (latex-macro-defs-sub t)
  (let* ((l1 (ahash-table->list latex-macro-table))
	 (l2 (list-sort l1 (lambda (x y) (symbol<=? (car x) (car y)))))
	 (l3 (map latex-macro-def l2))
	 (e1 (ahash-table->list latex-env-table))
	 (e2 (list-sort e1 (lambda (x y) (string<=? (car x) (car y)))))
	 (e3 (map latex-env-def e2)))
    (apply string-append (append (list latex-preamble) l3 e3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page size settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-preamble-page-type init)
  (let* ((page-type (ahash-ref init "page-type"))
	 (page-size (drd-ref tmpre-paper-type% page-type)))
    (if page-size
	(begin
	  (ahash-set! tmtex-preamble-uses "geometry" #t)
	  (set! tmtex-preamble-init
		(string-append tmtex-preamble-init
			       "\\geometry{" page-size "}\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the preamble
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-preamble-test-insert s)
  (with packlist (drd-ref-list latex-needs% s)
    (if packlist
	(for-each 
	  (lambda (pack)
	    (if (not (ahash-ref tmtex-preamble-uses pack))
		(ahash-set! tmtex-preamble-uses pack #t)))
          packlist))))

(define (tmtex-preamble-build-sub l)
  (if (and (list? l) (nnull? l))
      (let ((x (car l)))
	(if (symbol? x) (tmtex-preamble-test-insert x))
	(if (and (list? x) (>= (length l) 2) (== (car x) '!begin))
	    (tmtex-preamble-test-insert (string->symbol (cadr x))))
	(if (and (in? x '(!sub !sup)) (texout-contains-table? (cadr l)))
	    (tmtex-preamble-test-insert 'tmscript))
	(if (match? x '(!begin "enumerate" (!option :1)))
	    (ahash-set! tmtex-preamble-uses "enumerate" #t))
	(for-each tmtex-preamble-build-sub (cdr l)))))

(define (tmtex-preamble-make-package-list l)
  (cond ((null? l) "")
        ((null? (cdr l)) (force-string (car l)))
        (else (string-append (force-string (car l)) ","
          (tmtex-preamble-make-package-list (cdr l))))))

(tm-define (tmtex-preamble-build text style lan init)
  (set! tmtex-preamble-uses (make-ahash-table))
  (set! tmtex-preamble-init "")
  (set! tmtex-preamble-result "")
  (tmtex-preamble-page-type init)
  (if (drd-ref tmtex-preamble-language-def% lan)
      (set! tmtex-preamble-result
	    (string-append (drd-ref tmtex-preamble-language-def% lan) "\n")))
  (tmtex-preamble-build-sub text)
  (set! tmtex-preamble-result
	(string-append tmtex-preamble-result (latex-macro-defs text)))
  (set! tmtex-preamble-result
	(string-append (latex-catcode-defs text) tmtex-preamble-result))
  (values
    (tmtex-preamble-make-package-list 
      (sort
	(map car (ahash-table->list tmtex-preamble-uses))
	(lambda (l r)
	  (let* ((tl (drd-ref latex-package-priority% l))
		 (tr (drd-ref latex-package-priority% r))
		 (vl (if tl tl 999999))
		 (vr (if tr tr 999999)))
		(< vl vr)))))
    tmtex-preamble-init
    tmtex-preamble-result))
