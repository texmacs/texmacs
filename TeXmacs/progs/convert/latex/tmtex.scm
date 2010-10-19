
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex.scm
;; DESCRIPTION : conversion of TeXmacs trees into TeX/LaTeX trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex)
  (:use (convert tools tmpre)
	(convert tools old-tmtable)
	(convert tools tmlength)
	(convert rewrite tmtm-eqns)
	(convert rewrite tmtm-brackets)
	(convert latex texout)
	(convert latex latex-tools)))

(use-modules (ice-9 format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public tmtex-style "generic")
(define-public tmtex-packages '())
(define tmtex-env (make-ahash-table))
(define tmtex-serial 0)
(define tmtex-auto-produce 0)
(define tmtex-auto-consume 0)
(define tmtex-image-root-url (string->url "image"))
(define tmtex-image-root-string "image")
(define tmtex-appendices? #f)
(define tmtex-replace-style? #t)
(define tmtex-indirect-bib? #f)
(define tmtex-oriental? #f)
(define tmtex-chinese? #f)
(define tmtex-japanese? #f)
(define tmtex-korean? #f)
(define tmtex-taiwanese? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (elsevier-style% (in? tmtex-style '("elsart" "jsc")))
  (jsc-style% (in? tmtex-style '("jsc")) elsevier-style%)
  (natbib-package% (in? "cite-author-year" tmtex-packages)))

(tm-define (tmtex-style-init body)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-set-language lan)
  (set! tmtex-chinese? (== lan "chinese"))
  (set! tmtex-japanese? (== lan "japanese"))
  (set! tmtex-korean? (== lan "korean"))
  (set! tmtex-taiwanese? (== lan "taiwanese"))
  (set! tmtex-oriental? (or tmtex-chinese? tmtex-japanese?
			    tmtex-korean? tmtex-taiwanese?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization from options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-initialize opts)
  (set! tmtex-env (make-ahash-table))
  (set! tmtex-serial 0)
  (set! tmtex-auto-produce 0)
  (set! tmtex-auto-consume 0)
  (if (== (url-suffix current-save-target) "tex")
      (begin
	(set! tmtex-image-root-url (url-unglue current-save-target 4))
	(set! tmtex-image-root-string
	      (url->string (url-tail tmtex-image-root-url))))
      (begin
	(set! tmtex-image-root-url (string->url "image"))
	(set! tmtex-image-root-string "image")))
  (set! tmtex-appendices? #f)
  (set! tmtex-replace-style?
	(== (assoc-ref opts "texmacs->latex:replace-style") "on"))
  (set! tmtex-indirect-bib?
	(== (assoc-ref opts "texmacs->latex:indirect-bib") "on"))
  (set! tmtex-use-catcodes?
	(== (assoc-ref opts "texmacs->latex:use-catcodes") "on"))
  (set! tmtex-use-macros?
	(== (assoc-ref opts "texmacs->latex:use-macros") "on")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tmtex-table-props%
  (block ("" "l" "" #t))
  (block* ("" "c" "" #t))
  (tabular ("" "l" "" #f))
  (tabular* ("" "c" "" #f))
  (matrix ((,(string->symbol "left(")) "c" (,(string->symbol "right)")) #f))
  (det ((left|) "c" (right|) #f))
  (choice ((left\{) "l" (right.) #f)))

(drd-table tex-with-cmd%
  (("font-family" "rm") tmtextrm)
  (("font-family" "ss") tmtextsf)
  (("font-family" "tt") tmtexttt)
  (("font-series" "medium") tmtextmd)
  (("font-series" "bold") tmtextbf)
  (("font-shape" "right") tmtextup)
  (("font-shape" "slanted") tmtextsl)
  (("font-shape" "italic") tmtextit)
  (("font-shape" "small-caps") tmtextsc)
  (("math-font" "cal") mathcal)
  (("math-font" "cal*") mathscr)
  (("math-font" "cal**") EuScript)
  (("math-font" "Euler") mathfrak)
  (("math-font" "Bbb") mathbb)
  (("math-font" "Bbb*") mathbbm)
  (("math-font" "Bbb**") mathbbmss)
  (("math-font" "Bbb***") mathbb)
  (("math-font" "Bbb****") mathds)
  (("math-font-family" "mr") mathrm)
  (("math-font-family" "ms") mathsf)
  (("math-font-family" "mt") mathtt)
  (("math-font-family" "normal") mathnormal)
  (("math-font-family" "rm") mathrm)
  (("math-font-family" "ss") mathsf)
  (("math-font-family" "tt") mathtt)
  (("math-font-family" "bf") mathbf)
  (("math-font-family" "it") mathit)
  (("math-font-series" "bold") tmmathbf)
  (("par-columns" "2") (!begin "multicols" "2"))
  (("par-columns" "3") (!begin "multicols" "3"))
  (("par-mode" "center") (!begin "center"))
  (("par-mode" "left") (!begin "flushleft"))
  (("par-mode" "right") (!begin "flushright")))

(drd-table tex-assign-cmd%
  (("font-family" "rm") rmfamily)
  (("font-family" "ss") ssfamily)
  (("font-family" "tt") ttfamily)
  (("font-series" "medium") mdseries)
  (("font-series" "bold") bfseries)
  (("font-shape" "right") upshape)
  (("font-shape" "slanted") slshape)
  (("font-shape" "italic") itshape)
  (("font-shape" "small-caps") scshape))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Manipulation of the environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-env-list var)
  (let ((r (ahash-ref tmtex-env var)))
    (if r r '())))

(define (tmtex-env-get var)
  (let ((val (tmtex-env-list var)))
    (if (null? val) #f
	(car val))))

(define (tmtex-env-get-previous var)
  (let ((val (tmtex-env-list var)))
    (if (or (null? val) (null? (cdr val))) #f
	(cadr val))))

(define (tmtex-math-mode?)
  (== (tmtex-env-get "mode") "math"))

(tm-define (tmtex-env-set var val)
  (ahash-set! tmtex-env var (cons val (tmtex-env-list var))))

(tm-define (tmtex-env-reset var)
  (let ((val (tmtex-env-list var)))
    (if (nnull? val)
	(ahash-set! tmtex-env var (cdr val)))))

(tm-define (tmtex-env-assign var val)
  (tmtex-env-reset var)
  (tmtex-env-set var val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used TeX construction subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tex-concat-similar l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((r (tex-concat-similar (cdr l))))
	(cond ((and (func? (car l) '!sub) (func? (car r) '!sub))
	       (cons (list '!sub (tex-concat (list (cadar l) (cadar r))))
		     (cdr r)))
	      ((and (func? (car l) '!sup) (func? (car r) '!sup))
	       (cons (list '!sup (tex-concat (list (cadar l) (cadar r))))
		     (cdr r)))
	      (else (cons (car l) r))))))

(define (tex-concat-list l)
  (cond ((null? l) l)
	((== (car l) "") (tex-concat-list (cdr l)))
	((func? (car l) '!concat) (append (cdar l) (tex-concat-list (cdr l))))
	(else (cons (car l) (tex-concat-list (cdr l))))))

(tm-define (tex-concat l)
  (:synopsis "Horizontal concatenation of list of LaTeX expressions")
  (let ((r (tex-concat-similar (tex-concat-list l))))
    (if (null? r) ""
	(if (null? (cdr r)) (car r)
	    (cons '!concat r)))))

(define (tex-concat-strings l)
  (cond ((< (length l) 2) l)
	((and (string? (car l)) (string? (cadr l)))
	 (tex-concat-strings (cons (string-append (car l) (cadr l)) (cddr l))))
	(else (cons (car l) (tex-concat-strings (cdr l))))))

(tm-define (tex-concat* l)
  (:synopsis "Variant of tex-concat which concatenates adjacent strings")
  (tex-concat (tex-concat-strings l)))

(define tex-apply
  (lambda l
    (if (or (tmtex-math-mode?) (drd-in? (car l) tmpre-sectional%)) l
	(list '!group l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-starts? s r)
  (and (>= (string-length s) (string-length r))
       (== (substring s 0 (string-length r)) r)))

(define (tmtex-modified-token op s i)
  (tex-apply op
   (if (= (string-length s) (+ i 1))
       (substring s i (string-length s))
       (tex-apply (string->symbol (substring s i (string-length s)))))))

(define (tmtex-token-sub s group?)
  (cond ((== s "less") #\<)
	((== s "gtr") #\>)
	((== s "box") (list 'Box))
	((== s "||") (list '|))
	((string-starts? s "cal-") (tmtex-modified-token 'mathcal s 4))
	((string-starts? s "frak-") (tmtex-modified-token 'mathfrak s 5))
	((string-starts? s "bbb-") (tmtex-modified-token 'mathbbm s 4))
	((string-starts? s "b-cal-")
	 (tex-apply 'tmmathbf (tmtex-modified-token 'mathcal s 6)))
	((string-starts? s "b-") (tmtex-modified-token 'tmmathbf s 2))
	((and (string-starts? s "#") tmtex-oriental?)
	 (cond (tmtex-japanese?
		(let* ((qs (string-append "<" s ">"))
		       (cv (string-convert qs "Cork" "ISO-2022-JP"))
		       (ex (list->string (list #\33 #\50 #\102))))
		  (set! cv (string-append cv ex))
		  (list '!widechar (string->symbol cv))))
	       (tmtex-korean?
		(let* ((qs (string-append "<" s ">"))
		       (cv (string-convert qs "Cork" "UTF-8")))
		  (list '!widechar (string->symbol cv))))
	       ((or tmtex-chinese? tmtex-taiwanese?)
		(let* ((qs (string-append "<" s ">"))
		       ;;(cv (string-convert qs "Cork" "cp936")) ; Chinese?
		       ;;(cv (string-convert qs "Cork" "cp950")) ; Taiwanese ?
		       (cv (string-convert qs "Cork" "UTF-8")))
		  (list '!widechar (string->symbol cv))))))
	(else (let ((ss (list (string->symbol s))))
		(cond ((not (drd-in? (car ss) latex-symbol%))
		       (display* "TeXmacs] non converted symbol: " s "\n")
		       "")
		      (group? (list '!group ss))
		      (else (list '!symbol ss)))))))

(define (tmtex-token l routine group?)
  (receive (p1 p2) (list-break (cdr l) (lambda (x) (== x #\>)))
    (let* ((s (list->string p1))
	   (q (if (null? p2) '() (cdr p2)))
	   (r (routine q)))
      (cons (tmtex-token-sub s group?) r))))

(define (tmtex-text-sub head l)
  (append (string->list head) (tmtex-text-list (cdr l))))

(define (tmtex-special-char? c)
  (string-index "#$%&_{}" c))

(define (tmtex-break-char? c)
  (string-index "+ -:=,?;()[]{}<>/" c))

(define (tmtex-text-list-space l)
  (cond ((null? l) l)
	((== (car l) #\space)
	 (cons (list (string->symbol " ")) (tmtex-text-list-space (cdr l))))
	(else (tmtex-text-list l))))

(define (tmtex-text-list l)
  (if (null? l) l
      (let ((c (car l)))
	(cond ((== c #\<) (tmtex-token l tmtex-text-list #t))
	      ((== c #\space) (cons c (tmtex-text-list-space (cdr l))))
	      ((tmtex-special-char? c)
	       (cons (list (string->symbol (char->string c)))
		     (tmtex-text-list (cdr l))))
	      ((== c #\~) (cons (list '~ " ") (tmtex-text-list (cdr l))))
	      ((== c #\^) (cons (list '^ " ") (tmtex-text-list (cdr l))))
	      ((== c #\\) (cons (list 'tmbsl) (tmtex-text-list (cdr l))))
	      ((== c #\21) (tmtex-text-sub "''" l))
	      ((== c #\22) (tmtex-text-sub ",," l))
	      ((== c #\25) (tmtex-text-sub "--" l))
	      ((== c #\26) (tmtex-text-sub "---" l))
	      (else (cons c (tmtex-text-list (cdr l))))))))

(define (tmtex-math-operator l)
  (receive (p q) (list-break l (lambda (c) (not (char-alphabetic? c))))
    (let* ((op (list->string p))
	   (tail (tmtex-math-list q)))
      (if (drd-in? (string->symbol op) latex-operator%)
	  (cons (list '!symbol (tex-apply (string->symbol op))) tail)
	  (cons (tex-apply 'tmop op) tail)))))

(define (tmtex-math-list l)
  (if (null? l) l
      (let ((c (car l)))
	(cond ((== c #\<) (tmtex-token l tmtex-math-list #f))
	      ((tmtex-special-char? c)
	       (cons (list (string->symbol (char->string c)))
		     (tmtex-math-list (cdr l))))
	      ((== c #\~) (tmtex-math-list (cdr l)))
	      ((== c #\^) (tmtex-math-list (cdr l)))
	      ((== c #\\)
	       (cons (list 'backslash) (tmtex-math-list (cdr l))))
;;	      ((== c #\*) (cons '(*) (tmtex-math-list (cdr l))))
	      ((== c #\*) (tmtex-math-list (cdr l)))
;;	      ((== c #\space) (tmtex-math-list (cdr l)))
	      ((and (char-alphabetic? c)
		    (nnull? (cdr l))
		    (char-alphabetic? (cadr l)))
	       (tmtex-math-operator l))
	      (else (cons c (tmtex-math-list (cdr l))))))))

(define (tmtex-verb-list l)
  (if (null? l) l
      (let ((c (car l)))
	(if (== c #\<)
	    (let ((r (tmtex-token l tmtex-verb-list #t)))
	      (if (char? (car r)) r (cdr r)))
	    (cons c (tmtex-verb-list (cdr l)))))))
            
(define (tmtex-string-break? x start)
  (or (not (char? x))
      (and (tmtex-math-mode?) 
	   (or (tmtex-break-char? x)
	       (and (char-alphabetic? x) (char-numeric? start))
	       (and (char-alphabetic? start) (char-numeric? x))))))

(define (tmtex-string-produce l)
  (if (null? l) l
      (if (not (tmtex-string-break? (car l) (car l)))
	  (receive (p q)
            (list-break l (lambda (x) (tmtex-string-break? x (car l))))
	    (cons (list->string p) (tmtex-string-produce q)))
	  (if (equal? (car l) #\space)
	      (tmtex-string-produce (cdr l))
	      (cons (if (char? (car l)) (char->string (car l)) (car l))
		(tmtex-string-produce (cdr l)))))))

(define (tmtex-string s)
  (let* ((l (string->list s))
	 (t (if (tmtex-math-mode?)
		(tmtex-math-list l)
		(tmtex-text-list l)))
	 (r (tmtex-string-produce t)))
    (tex-concat r)))

(define (tmtex-verb-string s)
  (let* ((l (string->list s))
	 (t (tmtex-verb-list l))
	 (r (tmtex-string-produce t)))
    (tex-concat r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entire files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-transform-style x)
  (cond ((in? x '("article" "book" "letter")) x)
	((in? x '("generic" "exam")) "letter")
	((== x "seminar") "slides")
	((in? x '("tmarticle" "tmdoc" "mmxdoc")) "article")
	((in? x '("tmbook" "tmmanual")) "book")
	;;((in? x '("acmconf" "amsart" "svjour")) x)
	((in? x '("elsart" "jsc")) "elsart")
	((in? x '("acmconf" "amsart")) x)
	((in? x '("svjour" "elsart" "jsc")) "article")
	((not tmtex-replace-style?) x)
	(else #f)))

(define (tmtex-filter-styles l)
  (if (null? l) l
      (let* ((next (tmtex-transform-style (car l)))
	     (tail (tmtex-filter-styles (cdr l))))
	(if next (cons next tail) tail))))

(define (macro-definition? x)
  (and (func? x 'assign 2)
       (string? (cadr x))
       (func? (caddr x) 'macro)))

(define (tmtex-filter-preamble l)
  (cond ((or (nlist? l) (null? l)) '())
	((macro-definition? l) (list l))
	((== (car l) 'hide-preamble) (cdadr l))
	(else (append-map tmtex-filter-preamble (cdr l)))))

(define (tmtex-filter-body l)
  (cond ((or (nlist? l) (null? l)) l)
	((== (car l) 'assign) "")
	((== (car l) 'hide-preamble) "")
	(else (cons (car l) (map tmtex-filter-body (cdr l))))))

(define (tmtex-apply-init body init)
  ;;(display* "init= " init "\n")
  (cond ((== (assoc-ref init "language") "verbatim")
	 (with init* (assoc-remove! init "language")
	   (tmtex-apply-init `(verbatim ,body) init*)))
	(else body)))

(define (tmtex-file l)
  (let* ((doc (car l))
	 (styles (cadr l))
	 (lang (caddr l))
	 (init (cadddr l))
	 (init-bis (map (lambda (x) (cons (cadr x) (caddr x))) (cdr init)))
	 (doc-preamble (tmtex-filter-preamble doc))
	 (doc-body-pre (tmtex-filter-body doc))
	 (doc-body (tmtex-apply-init doc-body-pre init-bis)))
    (if (== (get-preference "texmacs->latex:expand-user-macros") "on")
	(set! doc-preamble '()))
    (if (null? styles) (tmtex doc)
	(let* ((body* (tmtex doc-body))
	       (styles* (tmtex-filter-styles styles))
	       (preamble* (ahash-with tmtex-env :preamble #t
			    (map-in-order tmtex doc-preamble))))
	  (list '!file body* styles* lang init preamble*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-noop l) "")
(define (tmtex-default s l) (cons (string->symbol s) (tmtex-list l)))
(define (tmtex-id l) (tmtex (car l)))
(define (tmtex-second l) (tmtex (cadr l)))
(define (tmtex-hide-part s l) "")
(define (tmtex-show-part s l) (tmtex (cadr l)))

(define (tmtex-document l)
  (cons '!document (tmtex-list l)))

(define (tmtex-para l)
  (cons '!paragraph (tmtex-list l)))

(define (tmtex-surround-sub l z)
  (if (null? (cdr l))
      (list (tex-concat (list (car l) z)))
      (cons (car l) (tmtex-surround-sub (cdr l) z))))

(define (tmtex-surround l)
  (let* ((ll (tmtex-list l))
	 (x (car ll))
	 (y (caddr ll))
	 (z (cadr ll)))
    (if (func? y '!document)
	(let* ((a (cadr y))
	       (b (cddr y)))
	  (cons '!document
		(tmtex-surround-sub
		 (cons (tex-concat (list x a)) b) z)))
	(tex-concat (list x y z)))))

(define (tmtex-script? x)
  (or (func? x '!sub)
      (func? x '!sup)
      (and (string? x) (!= x "") (in? (string-ref x 0) '(#\' #\, #\) #\])))
      (and (func? x '!concat) (tmtex-script? (cadr x)))))

(define (tmtex-math-concat-spaces l)
  (if (or (null? l) (null? (cdr l))) l
      (let* ((head (car l))
	     (tail (tmtex-math-concat-spaces (cdr l))))
	(if (tmtex-script? (car tail))
	    (cons head tail)
	    (cons* head " " tail)))))

(define (tmtex-rewrite-no-break l)
  (cond ((null? l) l)
	((and (string? (car l)) (string-ends? (car l) " ")
	      (nnull? (cdr l)) (== (cadr l) '(no-break)))
	 (let* ((s (substring (car l) 0 (- (string-length (car l)) 1)))
		(r (tmtex-rewrite-no-break (cddr l))))
	   (if (== s "") (cons '(!nbsp) r) (cons* s '(!nbsp) r))))
	(else (cons (car l) (tmtex-rewrite-no-break (cdr l))))))

(define (tmtex-concat l)
  (if (tmtex-math-mode?)
      (tex-concat (tmtex-math-concat-spaces (tmtex-list l)))
      (tex-concat (tmtex-list (tmtex-rewrite-no-break l)))))

(define (tmtex-rigid l)
  (tmtex-function '!group l))

(define (tmtex-no-first-indentation l) (tex-apply 'noindent))
(define (tmtex-line-break l) (tex-apply 'linebreak))
(define (tmtex-page-break l) (tex-apply 'pagebreak))
(define (tmtex-new-page l) (tex-apply 'newpage))
(define (tmtex-new-line l) (tex-apply '!newline))
(define (tmtex-next-line l) (list '!nextline))
(define (tmtex-no-break l) '(!group (nobreak)))

(define (tmtex-decode-length s)
  ;; FIXME: should be completed
  (cond ((string-ends? s "fn") (string-replace s "fn" "em"))
	((string-ends? s "spc") (string-replace s "spc" "em"))
	((string-ends? s "par") (string-replace s "par" "\\columnwidth"))
	(else s)))

(define (tmtex-hspace l)
  (let ((s (if (= (length l) 1) (car l) (cadr l))))
    (cond ((== s "1fn") (list 'quad))
	  ((== s "-0.6spc") '(!concat (!) (!) (!)))
	  ((== s "-0.4spc") '(!concat (!) (!)))
	  ((== s "-0.2spc") '(!concat (!)))
	  ((== s "0.2spc") (list (string->symbol ",")))
	  ((== s "0.4spc") (list (string->symbol ":")))
	  ((== s "0.6spc") (list (string->symbol ";")))
	  (else (tex-apply 'hspace (tmtex-decode-length s))))))

(define (tmtex-vspace l)
  (let ((s (if (= (length l) 1) (car l) (cadr l))))
    (cond ((== s "0.5fn") (tex-apply 'smallskip))
	  ((== s "1fn") (tex-apply 'medskip))
	  ((== s "2fn") (tex-apply 'bigskip))
	  (else (tex-apply 'vspace (tmtex-decode-length s))))))

(define (tmtex-space l)
  (tmtex-hspace (list (car l))))

(define (tmtex-float-make size type position x capt)
  (let* ((body (tmtex x))
	 (caption (tmtex capt))
	 (body* `(!paragraph ,body (caption ,caption))))
    (cond ((and (== size "big") (== type "figure"))
	   `((!begin "figure" (!option ,position)) ,body*))
	  ((and (== size "big") (== type "table"))
	   `((!begin "table" (!option ,position)) ,body*))
	  (else (list 'tmfloat position size type body caption)))))

(define (tmtex-float-table? x)
  (or (func? x 'small-table 2) (func? x 'big-table 2)))

(define (tmtex-float-figure? x)
  (or (func? x 'small-figure 2) (func? x 'big-figure 2)))

(define (tmtex-float-size l)
  (if (list? l)
      (if (or (func? l 'small-table) (func? l 'small-figure)) "small" "big")
      "big"))

(define (tmtex-float-sub position l)
  (cond ((func? l 'document 1) (tmtex-float-sub position (cadr l)))
	((tmtex-float-figure? l)
	 (tmtex-float-make (tmtex-float-size l) "figure" position (cadr l)
	   (caddr l)))
	((tmtex-float-table? l)
	 (tmtex-float-make (tmtex-float-size l) "table" position (cadr l)
	   (caddr l)))
	(else (tmtex-float-make "big" "figure" position l ""))))

(define (tmtex-float l)
  (tmtex-float-sub (force-string (cadr l)) (caddr l)))

(define (tmtex-htab l)
  (tex-apply 'hspace* (list 'fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-around l)
  (tmtex-concat (cdr (downgrade-brackets (cons 'around l)))))

(define (tmtex-around* l)
  (tmtex-concat (cdr (downgrade-brackets (cons 'around* l)))))

(define (tmtex-big-around l)
  (tmtex-concat (cdr (downgrade-brackets (cons 'big-around l)))))

(define (tmtex-large-decode s)
  (cond ((nstring? s) ".")
        ((in? s '("(" ")" "[" "]" "|" "/" ".")) s)
	((== s "||") "\\|")
	((== s "\\") "\\backslash")
	(else (string-append "\\" s))))

(define (tmtex-left l)
  (let* ((s (tmtex-large-decode (car l)))
	 (n (if (> (length l) 1) (string->number (cadr l)) 0))
	 (b (cond ((= n 1) "bigl")
		  ((= n 2) "Bigl")
		  ((= n 3) "biggl")
		  ((= n 4) "Biggl")
		  (else "left"))))
    (list (string->symbol (string-append b s)))))

(define (tmtex-mid l)
  (let ((s (tmtex-large-decode (car l))))
    (if (== (string-ref s 0) #\\)
	(list (string->symbol (substring s 1 (string-length s))))
	s)))

(define (tmtex-right l)
  (let* ((s (tmtex-large-decode (car l)))
	 (n (if (> (length l) 1) (string->number (cadr l)) 0))
	 (b (cond ((= n 1) "bigr")
		  ((= n 2) "Bigr")
		  ((= n 3) "biggr")
		  ((= n 4) "Biggr")
		  (else "right"))))
    (list (string->symbol (string-append b s)))))

(define (tmtex-big-decode s)
  (cond ((nstring? s) "bignone")
        ((in? s '("sum" "prod" "int" "oint" "coprod")) s)
	((== s "amalg") "coprod")
	((== s "pluscup") "uplus")
	((== s ".") "bignone")
	(else (string-append "big" s))))

(define (tmtex-big l)
  (list (string->symbol (tmtex-big-decode (car l)))))

(define (tmtex-prime-list l)
  (if (null? l) l
      (cond ((== (car l) #\<)
	     (receive (p q) (list-break (cdr l) (lambda (c) (== c #\>)))
	       (let ((next (if (null? q) '() (cdr q))))
		 (cons (list '!sup (list (string->symbol (list->string p))))
		       (tmtex-prime-list next)))))
	    ((== (car l) #\') (cons "'" (tmtex-prime-list (cdr l))))
	    ((== (car l) #\`)
	     (cons (list '!sup (list 'backprime))
		   (tmtex-prime-list (cdr l))))
	    (else (cons (list '!sup (char->string (car l)))
			(tmtex-prime-list (cdr l)))))))

(define (tmtex-lprime l)
  (tmtex (list 'concat (list 'text "") (list 'rprime (car l)))))

(define (tmtex-rprime l)
  (tex-concat (tmtex-prime-list (string->list (car l)))))

(define (tmtex-below l)
  (list 'underset (tmtex (cadr l)) (tmtex (car l))))

(define (tmtex-above l)
  (list 'overset (tmtex (cadr l)) (tmtex (car l))))

(define (tmtex-lsub l)
  (tmtex (list 'concat (list 'text "") (list 'rsub (car l)))))

(define (tmtex-lsup l)
  (tmtex (list 'concat (list 'text "") (list 'rsup (car l)))))

(define (tmtex-contains-table? x)
  (cond ((nlist? x) #f)
	((and (>= (length x) 2) (== (car x) '!table)) #t)
	(else (list-or (map-in-order tmtex-contains-table? (cdr x))))))

(define (tmtex-script which script)
  (with r (tmtex script)
    (if (tmtex-contains-table? r)
	(list which (list 'tmscript r))
	(list which r))))

(define (tmtex-rsub l)
  (tmtex-script '!sub (car l)))

(define (tmtex-rsup l)
  (tmtex-script '!sup (car l)))

(define (tmtex-frac l)
  (tmtex-function 'frac l))

(define (tmtex-sqrt l)
  (if (= (length l) 1)
      (tmtex-function 'sqrt l)
      (list 'sqrt
	    (list '!option (tmtex (cadr l)))
	    (tmtex (car l)))))

(define (tmtex-token? s)
  (or (= (string-length s) 1)
      (and (!= s "")
	   (== (string-ref s 0) #\<)
	   (== (string-index s #\>) (- (string-length s) 1)))))
       
(define (tmtex-wide-star? x)
  (cond ((func? x 'wide* 1) (tmtex-wide-star? (cadr x)))
	((nstring? x) #t)
	(else (not (tmtex-token? x)))))

(define (tmtex-wide-star l)
  (let ((wide (tmtex-wide-star? (car l)))
	(arg (tmtex (car l)))
	(acc (cadr l)))
    (if (and (string? acc) (string-starts? acc "<wide-"))
	(set! acc (string-append "<" (substring acc 6 (string-length acc)))))
    (cond ((nstring? acc) arg)
	  ((== acc "~")
	   (tmtex-below (list (car l) (list 'mbox (list 'textasciitilde)))))
	  ((== acc "<bar>") (list 'underline arg))
	  ((in? acc '("<underbrace>" "<underbrace*>"))
	   (list 'underbrace arg))
	  ((in? acc '("<overbrace>" "<overbrace*>"))
	   (tmtex-below `(,(car l) (text (downbracefill)))))
	  ;; imperfect translations
	  ((in? acc '("<squnderbrace>" "<squnderbrace*>"))
	   (list 'underbrace arg))
	  ((in? acc '("<sqoverbrace>" "<sqoverbrace*>"))
	   (tmtex-below `(,(car l) (text (downbracefill)))))
	  (else
	   (display* "TeXmacs] non converted accent below: " acc "\n")
	   arg))))

(define (tmtex-wide? x)
  (cond ((func? x 'wide 1) (tmtex-wide? (cadr x)))
	((nstring? x) #t)
	(else (not (tmtex-token? x)))))

(define (tmtex-wide l)
  (let ((wide (tmtex-wide? (car l)))
	(arg (tmtex (car l)))
	(acc (cadr l)))
    (if (and (string? acc) (string-starts? acc "<wide-"))
	(set! acc (string-append "<" (substring acc 6 (string-length acc)))))
    (cond ((nstring? acc) arg)
	  ((in? acc '("<hat>" "^")) (list (if wide 'widehat 'hat) arg))
	  ((in? acc '("<tilde>" "~")) (list (if wide 'widetilde 'tilde) arg))
	  ((== (cadr l) "<wide-bar>") (list 'overline arg))
	  ((== acc "<bar>") (list (if wide 'overline 'bar) arg))
	  ((== acc "<vect>") (list (if wide 'overrightarrow 'vec) arg))
	  ((== acc "<breve>") (list 'breve arg))
	  ((== acc "<check>") (list 'check arg))
	  ((== acc "<acute>") (list 'acute arg))
	  ((== acc "<grave>") (list 'grave arg))
	  ((== acc "<dot>") (list 'dot arg))
	  ((== acc "<ddot>") (list 'ddot arg))
	  ((== acc "<dddot>") (list 'dddot arg))
	  ((== acc "<ddddot>") (list 'ddddot arg))
	  ((in? acc '("<overbrace>" "<overbrace*>"))
	   (list 'overbrace arg))
	  ((in? acc '("<underbrace>" "<underbrace*>"))
	   (tmtex-above `(,(car l) (text (upbracefill)))))
	  ;; FIXME: imperfect translations
	  ((== acc "<abovering>") (list 'dot arg))
	  ((in? acc '("<sqoverbrace>" "<sqoverbrace*>"))
	   (list 'overbrace arg))
	  ((in? acc '("<squnderbrace>" "<squnderbrace*>"))
	   (tmtex-above `(,(car l) (text (upbracefill)))))
	  (else
	   (display* "TeXmacs] non converted accent: " acc "\n")
	   arg))))

(define (tmtex-neg l)
  (tmtex-function 'not l))

(define (tmtex-tree l)
  (let* ((root (list '!begin "bundle" (tmtex (car l))))
	 (children (map (lambda (x) (list 'chunk (tmtex x))) (cdr l))))
    (list root (tex-concat children))))

(define (tmtex-tree-eps l)
  (tmtex-eps (cons 'tree l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-table-rows-assemble tb bb rows)
  (cond ((null? rows)
	 (if (null? bb) '() (if (car bb) (list (list 'hline)) '())))
	(else (append (if (or (car tb) (car bb)) (list (list 'hline)) '()) 
		      (cons (cons '!row (map tmtex (car rows)))
			    (tmtex-table-rows-assemble 
			     (cdr tb) (cdr bb) (cdr rows)))))))

(define (tmtex-table-make p)
  (let ((tb (p 'rows 'tborder))
	(bb (p 'rows 'bborder))
	(l (p 'rows 'content)))
    (cons '!table (tmtex-table-rows-assemble tb (cons (car tb) bb) l))))

(define (tmtex-table-args-assemble lb rb ha)
  (cond
    ((null? ha) (if (null? rb) '() (list (if (car rb) "|" ""))))
    (else (cons (if (or (car lb) (car rb)) "|" "")
		(cons (car ha) (tmtex-table-args-assemble 
				(cdr lb) (cdr rb) (cdr ha)))))))
 
(define (tmtex-table-args p)
  (let ((lb (p 'cols 'lborder))
	(rb (p 'cols 'rborder))
	(l (p 'cols 'halign)))
    (apply string-append 
	   (tmtex-table-args-assemble lb (cons (car lb) rb) l))))

(define (tmtex-table-apply key x)
  (let* ((props (drd-ref tmtex-table-props% key)))
    (if props
	(let* ((env (if (tmtex-math-mode?) 'array 'tabular))
	       (before (car props))
	       (after (caddr props))
	       (defaults (append (tmtable-cell-halign (cadr props))
				 (tmtable-block-borders (cadddr props))))
	       (p (tmtable-parser `(tformat ,@defaults ,x)))
	       (e (list '!begin (symbol->string env) (tmtex-table-args p)))
	       (r (tmtex-table-make p)))
	  (tex-concat (list before (list e r) after)))
	(list (list '!begin (symbol->string key))
	      (tmtex-table-make (tmtable-parser x))))))

(define (tmtex-tformat l)
  (tmtex-table-apply 'tabular (cons 'tformat l)))

(define (tmtex-table l)
  (tmtex-table-apply 'tabular (cons 'table l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local and global environment changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-get-with-cmd var val)
  (drd-ref tex-with-cmd% (list var val)))

(define (tmtex-get-assign-cmd var val)
  (if (== var "font-size")
      (let ((x (* (string->number val) 10)))
	(cond ((< x 1) #f)
	      ((< x 5.5) 'tiny)
	      ((< x 6.5) 'scriptsize)
	      ((< x 7.5) 'footnotesize)
	      ((< x 9.5) 'small)
	      ((< x 11.5) 'normalsize)
	      ((< x 13.5) 'large)
	      ((< x 15.5) 'Large)
	      ((< x 18.5) 'LARGE)
	      ((< x 22.5) 'huge)
	      ((< x 50) 'Huge)
	      (else #f)))
      (drd-ref tex-assign-cmd% (list var val))))

(define (tmlength->texlength len)
  ;; TODO: rewrite (quote x) -> x and (tmlen ...) -> ...pt
  (with tmlen (string->tmlength (force-string len))
    (if (tmlength-null? tmlen) "0pt"
	(let* ((val (tmlength-value tmlen))
	       (unit (symbol->string (tmlength-unit tmlen)))
	       (val-string (number->string val)))
	  (cond ((== unit "fn") (string-append val-string "em"))
		(else len))))))

(define (tmtex-make-parmod x y z arg)
  (set! x (tmlength->texlength x))
  (set! y (tmlength->texlength y))
  (set! z (tmlength->texlength z))
  (if (and (tmlength-zero? (string->tmlength x))
	   (tmlength-zero? (string->tmlength y))
	   (tmlength-zero? (string->tmlength z)))
      arg
      (list (list '!begin "tmparmod" x y z) arg)))

(define (tmtex-make-parsep x arg)
  (set! x (tmlength->texlength x))
  (list (list '!begin "tmparsep" x) arg))

(define (tmtex-with-one var val arg)
  (if (== var "mode")
      (let ((old (tmtex-env-get-previous "mode")))
	(cond ((and (== val "text") (!= old "text"))
	       (list 'text arg))
	      ((and (== val "math") (!= old "math")
		    (ahash-ref tmtex-env :preamble))
	       (list 'ensuremath arg))
	      ((and (== val "math") (!= old "math"))
	       (list '!math arg))
	      (else arg)))
      (let ((w (tmtex-get-with-cmd var val))
	    (a (tmtex-get-assign-cmd var val)))
	(cond (w (list w arg))
	      (a (list '!group (tex-concat (list (list a) " " arg))))
	      ((== "par-left" var) (tmtex-make-parmod val "0pt" "0pt" arg))
	      ((== "par-right" var) (tmtex-make-parmod "0pt" val "0pt" arg))
	      ((== "par-first" var) (tmtex-make-parmod "0pt" "0pt" val arg))
	      ((== "par-par-sep" var) (tmtex-make-parsep val arg))
	      ((== var "color")
	        (if (and (= (string-length val) 7) (char=? (string-ref val 0) #\#))
		  (let* ((r (quotient (* (string->number (substring val 1 3) 16) 1000) 255))
			 (g (quotient (* (string->number (substring val 3 5) 16) 1000) 255))
			 (b (quotient (* (string->number (substring val 5 7) 16) 1000) 255))
			 (rgb (format #f "~,,-3f,~,,-3f,~,,-3f" r g b)))
		    (list '!group (tex-concat (list (list 'color (list '!option "rgb") rgb) " " arg))))
		  (list '!group (tex-concat (list (list 'color val) " " arg)))))
	      (else arg)))))

(define (tmtex-with l)
  (cond ((null? l) "")
	((null? (cdr l)) (tmtex (car l)))
	((func? (cAr l) 'graphics) (tmtex-eps (cons 'with l)))
	(else (let ((var (force-string (car l)))
		    (val (force-string (cadr l)))
		    (next (cddr l)))
		(tmtex-env-set var val)
		(let ((r (tmtex-with-one var val (tmtex-with next))))
		  (tmtex-env-reset var)
		  r)))))

(define (tmtex-var-name-sub l)
  (if (null? l) l
      (let ((c (car l)) (r (tmtex-var-name-sub (cdr l))))
	(cond ((char-alphabetic? c) (cons c r))
	      ((char=? c #\0) (cons* #\z #\e #\r #\o r))
	      ((char=? c #\1) (cons* #\o #\n #\e r))
	      ((char=? c #\2) (cons* #\t #\w #\o r))
	      ((and (char=? c #\*) (null? (cdr l))) (list c))
	      (else r)))))

(define (tmtex-var-name var)
  (cond ((nstring? var) "")
	((drd-in? (string->symbol var) tmtex-protected%)
	 (string-append "tm" var))
	((<= (string-length var) 1) var)
	(else (list->string (tmtex-var-name-sub (string->list var))))))

(define (tmtex-tex-arg l)
  (cons '!arg l))

(define (tmtex-args-search x args)
  (cond ((null? args) #f)
	((== x (car args)) 1)
	(else
	 (let ((n (tmtex-args-search x (cdr args))))
	   (if n (+ 1 n) #f)))))

(define (tmtex-args-sub l args)
  (if (null? l) l
      (cons (tmtex-args (car l) args)
	    (tmtex-args-sub (cdr l) args))))

(define (tmtex-args x args)
  (cond ((nlist? x) x)
	((or (func? x 'arg) (func? x 'value))
	 (let ((n (tmtex-args-search (cadr x) args)))
	   (if n (list '!arg (number->string n)) (tmtex-args-sub x args))))
	(else (tmtex-args-sub x args))))

(define (tmtex-assign l)
  (let ((var (tmtex-var-name (car l)))
	(val (cadr l)))
    (while (func? val 'quote 1) (set! val (cadr val)))
    (if (!= var "")
	(begin
	  (tmtex-env-assign var val)
	  (cond ((string? val)
		 (let ((a (tmtex-get-assign-cmd var val)))
		   (if a (list a)
		       (list 'newcommand (string-append "\\" var)
			     (tmtex val)))))
		((or (func? val 'macro) (func? val 'func))
		 (if (null? (cddr val))
		     (list 'newcommand (string-append "\\" var)
			   (tmtex (cAr val)))
		     (list 'newcommand (string-append "\\" var)
			   (list '!option (number->string (- (length val) 2)))
			   (tmtex (tmtex-args (cAr val) (cDdr val))))))
		(else (list 'newcommand (string-append "\\" var)
			    (tmtex val)))))
	"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-quote l)
  (tmtex (car l)))

(define (tmtex-label l)
  (list 'label (force-string (car l))))

(define (tmtex-reference l)
  (list 'ref (force-string (car l))))

(define (tmtex-pageref l)
  (list 'pageref (force-string (car l))))

(define (tmtex-specific l)
  (cond ((== (car l) "latex") (tmtex-tt (cadr l)))
	((== (car l) "image") (tmtex-eps (cadr l)))
	(else "")))

(define (tmtex-eps-names)
  (set! tmtex-serial (+ tmtex-serial 1))
  (let* ((postfix (string-append "-" (number->string tmtex-serial) ".eps"))
	 (name-url (url-glue tmtex-image-root-url postfix))
	 (name-string (string-append tmtex-image-root-string postfix)))
    (values name-url name-string)))

(define (tmtex-eps x)
  (if (tmtex-math-mode?) (set! x `(with "mode" "math" ,x)))
  (receive (name-url name-string) (tmtex-eps-names)
    (print-snippet name-url x)
    (list 'includegraphics name-string)))

(define (tmtex-graphics l)
  (tmtex-eps (cons 'graphics l)))

(define (tmtex-as-eps name)
  (with u (url-relative current-save-target (string->url name))
    (if (or (string-ends? name ".ps")
	    (string-ends? name ".eps")
	    (not (url-exists? u)))
	(list 'includegraphics name)
	(let* ((suffix (url-suffix u))
	       (fm (string-append (format-from-suffix suffix) "-file")))
	  (receive (name-url name-string) (tmtex-eps-names)
	    (convert-to-file u fm "postscript-file" name-url)
	    (list 'includegraphics name-string))))))

(define (tmtex-postscript l)
  (let* ((fig (tmtex-as-eps (force-string (car l))))
	 (hor (if (== (cadr l) "") "!" (tmtex-decode-length (cadr l))))
	 (ver (if (== (caddr l) "") "!" (tmtex-decode-length (caddr l)))))
    (if (or (string-starts? hor "*") (string-starts? hor "/")) (set! hor "!"))
    (if (or (string-starts? ver "*") (string-starts? ver "/")) (set! ver "!"))
    (if (and (== hor "!") (== ver "!")) fig
	(list 'resizebox hor ver fig))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Titles of documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-compressed sep l)
  (cond ((null? l) l)
	((null? (cdr l)) (list (tmtex (car l))))
	(else (cons* (tmtex (car l))
		     sep
		     (tmtex-compressed sep (cdr l))))))

(define (tmtex-data-assemble sep l)
  (cond ((null? l) l)
	((null? (cdr l)) (car l))
	(else (with r (tmtex-data-assemble sep (cdr l))
		(cond ((null? (car l)) r)
		      ((null? r) (car l))
		      (else (append (car l) (list sep) r)))))))

(tm-define (tmtex-select-data expr tag)
  (:synopsis "Get data matching @tag in @expr with nice separators")
  (let* ((data (select expr (list tag)))
	 (sep (if (== tag 'author-address) '(!nextline) "; "))
	 (fun (lambda (x)
		(cond ((func? x 'document)
		       (list (tex-concat* (tmtex-compressed sep (cdr x)))))
		      (else (list (tmtex x)))))))
    (if (null? data) '()
	(with l (cdar data)
	  (tmtex-data-assemble ", " (map fun l))))))

(define (tmtex-data-apply tag l)
  (if (null? l) l
      (list (list tag (tex-concat* l)))))

(define (tmtex-make-author tag)
  (let* ((name (tmtex-select-data tag 'author-name))
	 (address (tmtex-select-data tag 'author-address))
	 (note (tmtex-select-data tag 'author-note))
	 (email (tmtex-select-data tag 'author-email))
	 (homepage (tmtex-select-data tag 'author-homepage))
	 (email* (tmtex-data-apply 'email email))
	 (homepage* (tmtex-data-apply 'homepage homepage))
	 (note* (tmtex-data-assemble "; " (list note email* homepage*)))
	 (name* (append name (tmtex-data-apply 'thanks note*))))
    (tex-concat* (tmtex-data-assemble '(!nextline)
				      (list name* address)))))

(tm-define (tmtex-doc-data s l)
  (let* ((tag (cons s l))
	 (title (tmtex-select-data tag 'doc-title))
	 (authors (map tmtex-make-author (select tag '(doc-author-data))))
	 (date (tmtex-select-data tag 'doc-date))
	 (note (tmtex-select-data tag 'doc-note))
	 (keywords (tmtex-select-data tag 'doc-keywords))
	 (AMS-class (tmtex-select-data tag 'doc-AMS-class))
	 (keywords* (tmtex-data-apply 'keywords keywords))
	 (AMS-class* (tmtex-data-apply 'AMSclass AMS-class))
	 (note* (tmtex-data-assemble "; " (list note keywords* AMS-class*)))
	 (title* (append title (tmtex-data-apply 'thanks note*)))
	 (author* (tmtex-data-assemble '(and) (map list authors))))
    (tex-concat `((title ,(tex-concat title*))
		  (author ,(tex-concat author*))
		  (maketitle)))))

(define (tmtex-doc-data-wrapper s l)
  (tmtex-doc-data s l))

(tm-define (tmtex-abstract s l)
  (tmtex-std-env s l))

(define (tmtex-abstract-wrapper s l)
  (tmtex-abstract s l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs style primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-std-env s l)
  (if (== s "quote-env") (set! s "quote"))
  (list (list '!begin s) (tmtex (car l))))

(define (tmtex-appendix s l)
  (with app (list (if (latex-book-style?) 'chapter 'section) (tmtex (car l)))
    (if tmtex-appendices? app
      (begin
	(set! tmtex-appendices? #t)
	(list '!concat '(appendix) app)))))

(define (tmtex-tt-document l)
  (cond ((null? l) "")
	((null? (cdr l)) (tmtex-tt (car l)))
	(else (string-append (tmtex-tt (car l)) "\n"
			     (tmtex-tt-document (cdr l))))))

(define (tmtex-tt x)
  (cond ((string? x) (tmtex-verb-string x))
	((== x '(next-line)) "\n")
	((func? x 'document) (tmtex-tt-document (cdr x)))
	((func? x 'para) (tmtex-tt-document (cdr x)))
	((func? x 'concat)
	 (apply string-append (map-in-order tmtex-tt (cdr x))))
	(else "")))

(define (tmtex-verbatim s l)
  (if (func? (car l) 'document)
      (list '!verbatim (tmtex-tt (car l)))
      (list 'tmtexttt (tmtex (car l)))))
;;(list '!verb (tmtex-tt (car l)))))

(define (tmtex-indent s l)
  (list (list '!begin "tmindent") (tmtex (car l))))

(define (tmtex-list-env s l)
  (let* ((r (string-replace s "-" ""))
	 (t (cond ((== r "enumerateRoman") "enumerateromancap")
		  ((== r "enumerateAlpha") "enumeratealphacap")
		  (else r))))
    (list (list '!begin t) (tmtex (car l)))))

(define (tmtex-tiny s l)
  (tex-apply 'tiny (tmtex (car l))))

(define (tmtex-scriptsize s l)
  (tex-apply 'scriptsize (tmtex (car l))))

(define (tmtex-footnotesize s l)
  (tex-apply 'footnotesize (tmtex (car l))))

(define (tmtex-normalsize s l)
  (tex-apply 'normalsize (tmtex (car l))))

(define (tmtex-large s l)
  (tex-apply 'large (tmtex (car l))))

(define (tmtex-Large s l)
  (tex-apply 'Large (tmtex (car l))))

(define (tmtex-LARGE s l)
  (tex-apply 'LARGE (tmtex (car l))))

(define (tmtex-Huge s l)
  (list 'Huge (tmtex (car l))))

(tm-define (tmtex-equation s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex (car l))))
    (tmtex-env-reset "mode")
    (if (== s "equation")
	(list (list '!begin s) r)
	(list '!eqn r))))

(define (tmtex-equation-wrapper s l)
  (tmtex-equation s l))

(define (tmtex-eqnarray s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex-table-apply (string->symbol s) (car l))))
    (tmtex-env-reset "mode")
    r))

(define (tmtex-math s l)
  (tmtex `(with "mode" "math" ,(car l))))

(define (tmtex-dummy s l)
  "")

(define (tmtex-toc s l)
  (tex-apply 'tableofcontents))

(define (tmtex-bib-sub doc)
  (cond ((nlist? doc) doc)
	((match? doc '(concat (bibitem* :%1) (label :string?) :*))
	 (let* ((l (cadr (caddr doc)))
		(s (if (string-starts? l "bib-") (string-drop l 4) l)))
	   (cons* 'concat (list 'bibitem* (cadadr doc) s) (cdddr doc))))
	((func? doc 'bib-list 2) (tmtex-bib-sub (cAr doc)))
	(else (map tmtex-bib-sub doc))))

(define (tmtex-bib-max l)
  (cond ((npair? l) "")
	((match? l '(bibitem* :string? :%1)) (cadr l))
	(else (let* ((s1 (tmtex-bib-max (car l)))
		     (s2 (tmtex-bib-max (cdr l))))
		(if (< (string-length s1) (string-length s2)) s2 s1)))))

(define (tmtex-bib s l)
  (if tmtex-indirect-bib?
      (tex-concat (list (list 'bibliographystyle (force-string (cadr l)))
			(list 'bibliography (force-string (caddr l)))))
      (let* ((doc (tmtex-bib-sub (cadddr l)))
	     (max (tmtex-bib-max doc)))
	(tmtex (list 'thebibliography max doc)))))

(define (tmtex-thebibliography s l)
  (list (list '!begin s (car l)) (tmtex (cadr l))))

(define (tmtex-bibitem* s l)
  (cond ((= (length l) 1)
	 `(bibitem ,(tmtex (car l))))
	((= (length l) 2)
	 `(bibitem (!option ,(tmtex (car l))) ,(tmtex (cadr l))))
	(else "")))

(define (tmtex-figure s l)
  (tmtex-float-sub "h" (cons (string->symbol s) l)))

(define (tmtex-item s l)
  (tex-concat (list (list 'item) " ")))

(define (tmtex-item-arg s l)
  (tex-concat (list (list 'item (list '!option (tmtex (car l)))) " ")))

(define (tmtex-render-proof s l)
  (list (list '!begin "proof*" (tmtex (car l))) (tmtex (cadr l))))

(define (tmtex-nbsp s l)
  '(!nbsp))

(define (tmtex-nbhyph s l)
  '(!nbhyph))

(define (tmtex-session s l)
  (tmtex (cAr l)))

(define (tmtex-input s l)
  (let ((prompt (car l)) (x (cadr l)))
    (tex-concat
     (list '(noindent)
	   `(!group (!concat (color "red") (ttfamily ,(tmtex prompt))))
	   (cond ((func? x 'math 1)
		  (tmtex-env-set "mode" "math")
		  (let ((r (tmtex (cadr x))))
		    (tmtex-env-reset "mode")
		    `(!math (!group (!concat (color "blue") ,r)))))
		 (else `(!group (!concat (color "blue")
					 (!verb ,(tmtex-tt x))))))
	   '(smallskip)))))

(define (tmtex-output s l)
  (tex-concat
   (list '(noindent)
	 (list '!group (list 'ttfamily (tmtex (car l))))
	 '(medskip))))

(define (tmtex-hlink s l)
  (list 'href (tmtex (cadr l)) (tmtex (car l))))

(define (tmtex-href s l)
  (tmtex-function 'url l))

(define (tmtex-action s l)
  (list 'tmaction (tmtex (car l)) (tmtex (cadr l))))

(define (tmtex-choose s l)
  (list 'binom (tmtex (car l)) (tmtex (cadr l))))

(define (tmtex-modifier s l)
  (tex-apply (string->symbol (string-append "tm" s)) (tmtex (car l))))

(define (tmtex-menu-one x)
  (tmtex (list 'samp x)))

(define (tmtex-menu-list l)
  (if (null? l) l
      (cons* (list '!math (list 'rightarrow))
	     (tmtex-menu-one (car l))
	     (tmtex-menu-list (cdr l)))))

(define (tmtex-menu s l)
  (tex-concat (cons (tmtex-menu-one (car l)) (tmtex-menu-list (cdr l)))))

(define ((tmtex-rename into) s l)
  (tmtex-apply into (tmtex-list l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Citations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-cite-list l)
  (cond ((null? l) "")
	((nstring? (car l)) (tmtex-cite-list (cdr l)))
	((null? (cdr l)) (car l))
	(else (string-append (car l) "," (tmtex-cite-list (cdr l))))))

(tm-define (tmtex-cite s l)
  (tex-apply 'cite (tmtex-cite-list l)))

(tm-define (tmtex-cite s l)
  (:mode natbib-package?)
  (tex-apply 'citep (tmtex-cite-list l)))

(define (tmtex-nocite s l)
  (tex-apply 'nocite (tmtex-cite-list l)))

(tm-define (tmtex-cite-detail s l)
  (tex-apply 'cite `(!option ,(tmtex (cadr l))) (tmtex (car l))))

(tm-define (tmtex-cite-detail s l)
  (:mode natbib-package?)
  (tex-apply 'citetext `(!concat (citealp ,(tmtex (car l))) ", "
				 ,(tmtex (cadr l)))))

(define (tmtex-cite-raw s l)
  (tex-apply 'citealp (tmtex-cite-list l)))

(define (tmtex-cite-raw* s l)
  (tex-apply 'citealp* (tmtex-cite-list l)))

(define (tmtex-cite-textual s l)
  (tex-apply 'citet (tmtex-cite-list l)))

(define (tmtex-cite-textual* s l)
  (tex-apply 'citet* (tmtex-cite-list l)))

(define (tmtex-cite-parenthesized s l)
  (tex-apply 'citep (tmtex-cite-list l)))

(define (tmtex-cite-parenthesized* s l)
  (tex-apply 'citep* (tmtex-cite-list l)))

(define (tmtex-render-cite s l)
  (tex-apply 'citetext (tmtex (car l))))

(define (tmtex-cite-author s l)
  (tex-apply 'citeauthor (tmtex (car l))))

(define (tmtex-cite-author* s l)
  (tex-apply 'citeauthor* (tmtex (car l))))

(define (tmtex-cite-year s l)
  (tex-apply 'citeyear (tmtex (car l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glossaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-glossary s l)
  (with nr (+ tmtex-auto-produce 1)
    (set! tmtex-auto-produce nr)
    `(label ,(string-append "autolab" (number->string nr)))))

(define (tmtex-glossary-entry s l)
  (with nr (+ tmtex-auto-consume 1)
    (with lab (string-append "autolab" (number->string nr))
      (set! tmtex-auto-consume nr)
      `(glossaryentry ,(tmtex (car l)) ,(tmtex (cadr l)) (pageref ,lab)))))

(define (tmtex-the-glossary s l)
  `(!document
      (,(if (latex-book-style?) 'chapter* 'section*) "Glossary")
      ((!begin "theglossary" ,(car l)) ,(tmtex (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-apply key args)
  (let ((n (length args))
	(r (drd-ref tmtex-methods% key)))
    (if (in? key '(quote quasiquote unquote)) (set! r tmtex-noop))
    (if r (r args)
	(let ((p (drd-ref tmtex-tmstyle% key)))
	  (if (and p (or (= (cadr p) -1) (= (cadr p) n)))
	      ((car p) (symbol->string key) args)
	      (if (and (= n 1)
		       (or (func? (car args) 'tformat)
			   (func? (car args) 'table)))
		  (tmtex-table-apply key (car args))
		  (tmtex-function key args)))))))

(define (tmtex-function f l)
  (if (== (string-ref (symbol->string f) 0) #\!)
      (cons f (map-in-order tmtex l))
      (let ((v (tmtex-var-name (symbol->string f))))
	(if (== v "") ""
	    (apply tex-apply
		   (cons (string->symbol v)
			 (map-in-order tmtex l)))))))

(define (tmtex-compound l)
  (if (string? (car l))
      (tmtex-apply (string->symbol (car l)) (cdr l))
      ""))

(define (tmtex-list l)
  (map-in-order tmtex l))

(tm-define (tmtex x)
  (if (string? x) (tmtex-string x)
      (tmtex-apply (car x) (cdr x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmtex-methods%
  ((:or unknown uninit error raw-data) tmtex-noop)
  (document tmtex-document)
  (para tmtex-para)
  (surround tmtex-surround)
  (concat tmtex-concat)
  (rigid tmtex-rigid)
  (hidden tmtex-noop)
  (hspace tmtex-hspace)
  (vspace* tmtex-noop)
  (vspace tmtex-vspace)
  (space tmtex-space)
  (htab tmtex-htab)
  (move tmtex-noop)
  (resize tmtex-noop)
  (repeat tmtex-noop)
  (float tmtex-float)
  ((:or datoms dlines dpages dbox) tmtex-noop)

  (with-limits tmtex-noop)
  (line-break tmtex-line-break)
  (new-line tmtex-new-line)
  (next-line tmtex-next-line)
  (no-break tmtex-no-break)
  (no-indent tmtex-no-first-indentation)
  (yes-indent tmtex-noop)
  (no-indent* tmtex-noop)
  (yes-indent* tmtex-noop)
  (page-break* tmtex-noop)
  (page-break tmtex-page-break)
  (no-page-break* tmtex-noop)
  (no-page-break tmtex-noop)
  (new-page* tmtex-noop)
  (new-page tmtex-new-page)
  (new-dpage* tmtex-noop)
  (new-dpage tmtex-noop)

  (around tmtex-around)
  (around* tmtex-around*)
  (big-around tmtex-big-around)
  (left tmtex-left)
  (mid tmtex-mid)
  (right tmtex-right)
  (big tmtex-big)
  (lprime tmtex-lprime)
  (rprime tmtex-rprime)
  (below tmtex-below)
  (above tmtex-above)
  (lsub tmtex-lsub)
  (lsup tmtex-lsup)
  (rsub tmtex-rsub)
  (rsup tmtex-rsup)
  (frac tmtex-frac)
  (sqrt tmtex-sqrt)
  (wide tmtex-wide)
  (neg tmtex-neg)
  (wide* tmtex-wide-star)
  ;;(tree tmtex-tree)
  (tree tmtex-tree-eps)

  (tformat tmtex-tformat)
  ((:or twith cwith tmarker) tmtex-noop)
  (table tmtex-table)
  ((:or row cell subtable) tmtex-noop)

  (assign tmtex-assign)
  (with tmtex-with)
  (provides tmtex-noop)
  (value tmtex-compound)
  (quote-value tmtex-noop)
  ((:or quote-value drd-props arg quote-arg) tmtex-noop)
  (compound tmtex-compound)
  ((:or xmacro get-label get-arity map-args eval-args mark eval) tmtex-noop)
  ;; quote missing
  (quasi tmtex-noop)
  ;; quasiquote missing
  ;; unquote missing
  ((:or unquote* copy
	if if* case while for-each
	extern include use-package) tmtex-noop)

  ((:or or xor and not plus minus times over div mod
	merge length range number date translate change-case find-file
	is-tuple look-up
	equal unequal less lesseq greater greatereq) tmtex-noop)

  ((:or cm-length mm-length in-length pt-length
	bp-length dd-length pc-length cc-length
	fs-length fbs-length em-length
	ln-length sep-length yfrac-length ex-length
	fn-length fns-length bls-length
	spc-length xspc-length par-length pag-length
	gm-length gh-length) tmtex-noop)

  ((:or style-with style-with* style-only style-only*
	active active* inactive inactive*
	rewrite-inactive inline-tag open-tag middle-tag close-tag
	symbol latex hybrid) tmtex-noop)

  ((:or tuple attr tmlen collection associate backup) tmtex-noop)
  (label tmtex-label)
  (reference tmtex-reference)
  (pageref tmtex-pageref)
  (write tmtex-noop)
  (specific tmtex-specific)
  ((:or tag meaning flag) tmtex-noop)

  ((:or anim-compose anim-repeat anim-constant
	anim-translate anim-progressive video sound) tmtex-noop)

  (graphics tmtex-graphics)
  (superpose tmtex-noop)
  ((:or gr-group gr-linear-transform
	text-at cline arc carc spline spine* cspline fill) tmtex-noop)
  (postscript tmtex-postscript)
  ((:or box-info frame-direct frame-inverse) tmtex-noop)

  ((:or format line-sep split delay hold release
	old-matrix old-table old-mosaic old-mosaic-item
	set reset expand expand* hide-expand
	apply begin end func env) tmtex-noop)
  
  (shown tmtex-id)
  (!file tmtex-file)
  (!arg tmtex-tex-arg))

(drd-table tmtex-tmstyle%
  ((:or hide-preamble show-preamble) (,tmtex-default -1))
  (hide-part (,tmtex-hide-part -1))
  (show-part (,tmtex-show-part -1))
  (doc-data (,tmtex-doc-data-wrapper -1))
  ((:or doc-title doc-author-data doc-date doc-note
	doc-keywords doc-AMS-class) (,tmtex-default -1))
  ((:or author-name author-address author-note
	author-email author-homepage) (,tmtex-default -1))
  (abstract (,tmtex-abstract-wrapper 1))
  (appendix (,tmtex-appendix 1))
  ((:or theorem proposition lemma corollary proof axiom definition
	notation conjecture remark note example exercise problem warning
	convention quote-env quotation verse)
   (,tmtex-std-env 1))
  ((:or verbatim code) (,tmtex-verbatim 1))
  (center (,tmtex-std-env 1))
  (indent (,tmtex-indent 1))
  ((:or description description-compact description-aligned
	description-dash description-long
	itemize itemize-minus itemize-dot itemize-arrow
	enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   (,tmtex-list-env 1))
  (really-tiny (,tmtex-tiny 1))
  (very-tiny (,tmtex-tiny 1))
  (really-small (,tmtex-scriptsize 1))
  (very-small (,tmtex-scriptsize 1))
  (smaller (,tmtex-footnotesize 1))
  (flat-size (,tmtex-footnotesize 1))
  (normal-size (,tmtex-normalsize 1))
  (sharp-size (,tmtex-large 1))
  (larger (,tmtex-Large 1))
  (very-large (,tmtex-LARGE 1))
  (really-large (,tmtex-LARGE 1))
  (really-huge (,tmtex-Huge 1))

  (math (,tmtex-math 1))
  ((:or equation equation*) (,tmtex-equation-wrapper 1))
  ((:or eqnarray eqnarray* leqnarray*) (,tmtex-eqnarray 1))
  (eq-number (,tmtex-default -1))
  (the-index (,tmtex-dummy -1))
  (glossary (,tmtex-glossary 1))
  (glossary-explain (,tmtex-glossary 2))
  (glossary-2 (,tmtex-glossary-entry 3))
  (the-glossary (,tmtex-the-glossary 2))
  ((:or table-of-contents) (,tmtex-toc 2))
  (bibliography (,tmtex-bib 4))
  (thebibliography (,tmtex-thebibliography 2))
  (bib-list (,tmtex-second 2))
  (bibitem* (,tmtex-bibitem* -1))
  ((:or small-figure big-figure small-table big-table) (,tmtex-figure 2))
  (item (,tmtex-item 0))
  (item* (,tmtex-item-arg 1))
  (render-proof (,tmtex-render-proof 2))
  (nbsp (,tmtex-nbsp 0))
  (nbhyph (,tmtex-nbhyph 0))
  (session (,tmtex-session -1))
  (input (,tmtex-input 2))
  (output (,tmtex-output 1))
  (hlink (,tmtex-hlink 2))
  (action (,tmtex-action -1))
  (href (,tmtex-href 1))
  (choose (,tmtex-choose 2))
  ((:or strong em tt name samp abbr dfn kbd var acronym person)
   (,tmtex-modifier 1))
  (menu (,tmtex-menu -1))
  (with-TeXmacs-text (,(tmtex-rename 'withTeXmacstext) 0))
  (made-by-TeXmacs (,(tmtex-rename 'madebyTeXmacs) 0))
  (cite (,tmtex-cite -1))
  (nocite (,tmtex-nocite -1))
  (cite-detail (,tmtex-cite-detail 2))
  (cite-raw (,tmtex-cite-raw -1))
  (cite-raw* (,tmtex-cite-raw* -1))
  (cite-textual (,tmtex-cite-textual -1))
  (cite-textual* (,tmtex-cite-textual* -1))
  (cite-parenthesized (,tmtex-cite-parenthesized -1))
  (cite-parenthesized* (,tmtex-cite-parenthesized* -1))
  (render-cite (,tmtex-render-cite 1))
  ((:or cite-author cite-author-link) (,tmtex-cite-author 1))
  ((:or cite-author* cite-author*-link) (,tmtex-cite-author* 1))
  ((:or cite-year cite-year-link) (,tmtex-cite-year 1)))

(drd-group tmtex-protected%
  a b c d i j k l o r t u v H L O P S
  aa ae bf cr dh dj dp em fi ge gg ht if in it le lg ll lu lq mp mu
  ne ng ni nu oe or pi pm rm rq sb sc sf sl sp ss th to tt wd wp wr xi
  AA AE DH DJ Im NG OE Pi Pr Re SS TH Xi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion of all macros which are not recognized by LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmtex-user-defs-table (make-ahash-table))

(define (user-definition? x)
  (and (func? x 'assign 2)
       (string? (cadr x))))

(define (collect-user-defs-sub t)
  (cond ((npair? t) (noop))
	((user-definition? t)
	 (ahash-set! tmtex-user-defs-table (string->symbol (cadr t)) #t))
	(else (for-each collect-user-defs-sub (cdr t)))))

(define (collect-user-defs t)
  (if (== (get-preference "texmacs->latex:expand-user-macros") "on") '()
      (begin
	(set! tmtex-user-defs-table (make-ahash-table))
	(collect-user-defs-sub (cons 'document (tmtex-filter-preamble t)))
	(ahash-set->list tmtex-user-defs-table))))

(define (as-string sym)
  (with s (symbol->string sym)
    (if (string-starts? s "begin-")
	(substring s 6 (string-length s))
	s)))

(define (drd-first-list name)
  (let* ((l1 (query (cons name '('first 'second))))
	 (l2 (map (cut assoc-ref <> 'first) l1)))
    (map as-string l2)))

(define (tmtex-env-macro name)
  `(associate ,name (xmacro "x" (eval-args "x"))))

(tm-define (tmtex-env-patch t)
  (let* ((l1 (drd-first-list 'tmtex-methods%))
	 (l2 (drd-first-list 'tmtex-tmstyle%))
	 (l3 (map as-string (drd-apply-list '(latex-tag%))))
	 (l4 (map as-string (drd-apply-list '(latex-symbol%))))
	 (l5 (list-difference l3 l4))
	 (l6 (map as-string (collect-user-defs (tree->stree t))))
	 (l7 (list-difference (list-union l2 (list-union l5 l6)) l1)))
    `(collection ,@(map tmtex-env-macro l7))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-get-style sty)
  (cond ((not sty) (set! sty (list "generic")))
	((string? sty) (set! sty (list sty)))
	((func? sty 'tuple) (set! sty (cdr sty)))
	((null? sty) (set! sty '("letter"))))
  (if (== (car sty) "generic") (set! sty (cons "letter" (cdr sty))))
  sty)

(tm-define (texmacs->latex x opts)
  ;;(display* "texmacs->latex [" opts "], " x "\n")
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
	     (style (tmtex-get-style (tmfile-extract x 'style)))
	     (main-style (or (tmtex-transform-style (car style)) "letter"))
	     (lan (tmfile-init x "language"))
	     (init (tmfile-extract x 'initial))
	     (doc (list '!file body style lan init (get-texmacs-path))))
	(latex-set-style main-style)
	(latex-set-packages '())
	(latex-set-language lan)
	(set! tmtex-style (car style))
	(set! tmtex-packages (cdr style))
	(when (elsevier-style?)
	  (import-from (convert latex tmtex-elsevier)))
	(tmtex-style-init body)
	(tmtex-set-language lan)
	(with result (texmacs->latex doc opts)
	  (set! tmtex-style "generic")
	  (set! tmtex-packages '())
	  (tmtex-set-language "english")
	  result))
      (let* ((x2 (tmtm-eqnumber->nonumber x))
	     (x3 (tmtm-match-brackets x2)))
	(tmtex-initialize opts)
	(with r (tmtex (tmpre-produce x3))
	  (if (not tmtex-use-macros?)
	      (set! r (latex-expand-macros r)))
	  (if (not tmtex-use-catcodes?)
	      (set! r (latex-expand-catcodes r)))
	  r))))
