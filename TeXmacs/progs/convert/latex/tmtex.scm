
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtex.scm
;; DESCRIPTION : conversion of TeXmacs trees into TeX/LaTeX trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex tmtex)
  (:use
    (convert tools tmpre) (convert tools tmtable)
    (convert rewrite tmtm-eqns) (convert rewrite tmtm-brackets)
    (convert latex texout))
  (:export
    texmacs->latex
    tmtex tmtex-initialize)) ;; for tmtex-test

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmtex-env (make-ahash-table))
(define tmtex-appendices? #f)
(define tmtex-faithful-style? #f)
(define tmtex-indirect-bib? #f)

(define (tmtex-initialize opts)
  (set! tmtex-appendices? #f)
  (set! tmtex-faithful-style?
	(== (assoc-ref opts "texmacs->latex:faithful-style") "on"))
  (set! tmtex-indirect-bib?
	(== (assoc-ref opts "texmacs->latex:indirect-bib") "on"))
  (set! tmtex-env (make-ahash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-table tmtex-table-props%
  (block ("" "l" "" #t))
  (block* ("" "c" "" #t))
  (tabular ("" "l" "" #f))
  (tabular* ("" "c" "" #f))
  (matrix ((#{left\(}#) "c" (#{right\)}#) #f))
  (det ((left|) "c" (right|) #f))
  (choice ((left\{) "l" (right.) #f)))

(drd-group tex-mathops%
  arccos arcsin arctan cos cosh cot coth csc deg det dim exp gcd
  hom inf ker lg lim liminf limsup ln log max min Pr sec sin
  sinh sup tan tanh)

(drd-table tex-with-cmd%
  (("font-family" "rm") textrm)
  (("font-family" "ss") textsf)
  (("font-family" "tt") texttt)
  (("font-series" "medium") textmd)
  (("font-series" "bold") textbf)
  (("font-shape" "right") textup)
  (("font-shape" "slanted") textsl)
  (("font-shape" "italic") textit)
  (("font-shape" "small-caps") textsc)
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

(define (tmtex-env-set var val)
  (ahash-set! tmtex-env var (cons val (tmtex-env-list var))))

(define (tmtex-env-reset var)
  (let ((val (tmtex-env-list var)))
    (if (nnull? val)
	(ahash-set! tmtex-env var (cdr val)))))

(define (tmtex-env-assign var val)
  (tmtex-env-reset var)
  (tmtex-env-set var val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frequently used TeX construction subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tex-concat-similar l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((r (tex-concat-similar (cdr l))))
	    (if (and (func? (car l) '!sup) (func? (car r) '!sup))
		(cons (list '!sup (tex-concat (list (cadar l) (cadar r))))
		      (cdr r))
		(cons (car l) r)))))

(define (tex-concat-list l)
  (cond ((null? l) l)
	((== (car l) "") (tex-concat-list (cdr l)))
	((func? (car l) '!concat) (append (cdar l) (tex-concat-list (cdr l))))
	(else (cons (car l) (tex-concat-list (cdr l))))))

(define (tex-concat l)
  (let ((r (tex-concat-similar (tex-concat-list l))))
    (if (null? r) ""
	(if (null? (cdr r)) (car r)
	    (cons '!concat r)))))

(define (tex-concat-strings l)
  (cond ((< (length l) 2) l)
	((and (string? (car l)) (string? (cadr l)))
	 (tex-concat-strings (cons (string-append (car l) (cadr l)) (cddr l))))
	(else (cons (car l) (tex-concat-strings (cdr l))))))

(define (tex-concat* l)
  "Variant of tex-concat for which adjecent strings are concatenated"
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
	(else (let ((ss (list (string->symbol s))))
		(if group? (list '!group ss) ss)))))

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

(define (tmtex-text-list l)
  (if (null? l) l
      (let ((c (car l)))
	(cond ((== c #\<) (tmtex-token l tmtex-text-list #t))
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
	      ((== c #\337) (tmtex-text-sub "SS" l))
	      ((== c #\377) (tmtex-text-sub "ß" l))
	      (else (cons c (tmtex-text-list (cdr l))))))))

(define (tmtex-math-operator l)
  (receive (p q) (list-break l (lambda (c) (not (char-alphabetic? c))))
    (let* ((op (list->string p))
	   (tail (tmtex-math-list q)))
      (if (drd-in? (string->symbol op) tex-mathops%)
	  (cons (tex-apply (string->symbol op)) tail)
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

(define (tmtex-no-math-space c1 c2)
  (or (== c2 #\,)
      (and (== c1 c2) (in? c1 '(#\( #\) #\[ #\])))
      (and (char? c1) (char? c2)
	   (char-numeric? c1) (char-numeric? c2))))

(define (tmtex-math-spaces l)
  (if (or (null? l) (null? (cdr l))) l
      (let ((tail (tmtex-math-spaces (cdr l))))
	(if (tmtex-no-math-space (car l) (cadr l))
	    (cons (car l) tail)
	    (cons* (car l) #\space tail)))))

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
	((in? x '("acmconf" "amsart" "jsc" "svjour")) x)
	(tmtex-faithful-style? x)
	(else #f)))

(define (tmtex-filter-styles l)
  (if (null? l) l
      (let* ((next (tmtex-transform-style (car l)))
	     (tail (tmtex-filter-styles (cdr l))))
	(if next (cons next tail) tail))))

(define (tmtex-filter-preamble l)
  (define (append-lists l)
    (if (null? l) l (append (car l) (append-lists (cdr l)))))
  (cond ((or (nlist? l) (null? l)) '())
	((== (car l) 'assign) (list l))
	(else (append-lists (map tmtex-filter-preamble (cdr l))))))

(define (tmtex-filter-body l)
  (cond ((or (nlist? l) (null? l)) l)
	((== (car l) 'assign) "")
	(else (cons (car l) (map tmtex-filter-body (cdr l))))))

(define (tmtex-file l)
  (let* ((doc (car l))
	 (styles (cdadr l))
	 (lang (caddr l))
	 (init (cadddr l))
	 (doc-preamble (tmtex-filter-preamble doc))
	 (doc-body (tmtex-filter-body doc)))
    (if (null? styles) (tmtex doc)
	(begin
	  (list '!file
		(tmtex doc-body)
		(tmtex-filter-styles styles)
		lang
		init
		(map-in-order tmtex doc-preamble))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-noop l) "")

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
      (and (string? x) (!= x "") (in? (string-ref x 0) '(#\' #\,)))
      (and (func? x '!concat) (tmtex-script? (cadr x)))))

(define (tmtex-math-concat-spaces l)
  (if (or (null? l) (null? (cdr l))) l
      (let* ((head (car l))
	     (tail (tmtex-math-concat-spaces (cdr l))))
	(if (tmtex-script? (car tail))
	    (cons head tail)
	    (cons* head " " tail)))))

(define (tmtex-concat l)
  (if (tmtex-math-mode?)
      (tex-concat (tmtex-math-concat-spaces (tmtex-list l)))
      (tex-concat (tmtex-list l))))

(define (tmtex-no-first-indentation l) (tex-apply 'noindent))
(define (tmtex-line-break l) (tex-apply 'linebreak))
(define (tmtex-page-break l) (tex-apply 'pagebreak))
(define (tmtex-new-page l) (tex-apply 'newpage))
(define (tmtex-new-line l) (tex-apply '!newline))
(define (tmtex-next-line l) (list '!nextline))

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

(define (tmtex-float-make size type position x caption)
  (list 'tmfloat position size type (tmtex x) (tmtex caption)))

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

(define (tmtex-group l)
  (tmtex-function '!group l))

(define (tmtex-large-decode s)
  (cond ((nstring? s) ".")
        ((in? s '("(" ")" "[" "]" "|" "/" ".")) s)
	((== s "||") "\\|")
	((== s "\\") "\\backslash")
	(else (string-append "\\" s))))

(define (tmtex-left l)
  (list (string->symbol (string-append "left" (tmtex-large-decode (car l))))))

(define (tmtex-mid l)
  (let ((s (tmtex-large-decode (car l))))
    (if (== (string-ref s 0) #\\)
	(list (string->symbol (substring s 1 (string-length s))))
	s)))

(define (tmtex-right l)
  (list (string->symbol (string-append "right" (tmtex-large-decode (car l))))))

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

(define (tmtex-rsub l)
  (tmtex-function '!sub l))

(define (tmtex-rsup l)
  (tmtex-function '!sup l))

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
  (let ((wide (tmtex-wide-star? (car l))) (arg (tmtex (car l))) (acc (cadr l)))
    (cond ((nstring? acc) arg)
	  ((== acc "<bar>") (list 'underline arg))
	  ((== acc "<wide-bar>") (list 'underline arg))
	  ((== acc "<wide-underbrace>") (list 'underbrace arg))
	  ((== acc "<wide-overbrace*>")
	    (tmtex-below (list (car l) (list 'text (list 'downbracefill)))))
	  ((== acc "~")
	   (tmtex-below (list (car l) (list 'mbox (list 'textasciitilde)))))
	  (else (list 'wide arg)))))

(define (tmtex-wide? x)
  (cond ((func? x 'wide 1) (tmtex-wide? (cadr x)))
	((nstring? x) #t)
	(else (not (tmtex-token? x)))))

(define (tmtex-wide l)
  (let ((wide (tmtex-wide? (car l))) (arg (tmtex (car l))) (acc (cadr l)))
    (cond ((nstring? acc) arg)
	  ((== acc "<check>") (list 'check arg))
	  ((== acc "<vect>") (list (if wide 'overrightarrow 'vec) arg))
	  ((== acc "<acute>") (list 'acute arg))
	  ((== acc "<grave>") (list 'grave arg))
	  ((== acc "<breve>") (list 'breve arg))
	  ((== acc "<bar>") (list (if wide 'overline 'bar) arg))
	  ((== acc "<dot>") (list 'dot arg))
	  ((== acc "<ddot>") (list 'ddot arg))
	  ((== acc "<dddot>") (list 'dddot arg))
	  ((== acc "<ddddot>") (list 'ddddot arg))
	  ((== acc "<wide-overbrace>") (list 'overbrace arg))
	  ((== acc "<wide-underbrace*>")
	    (tmtex-above (list (car l) (list 'text (list 'upbracefill)))))
	  ((== acc "~") (list (if wide 'widetilde 'tilde) arg))
	  ((== acc "^") (list (if wide 'widehat 'hat) arg))
	  (else arg))))

(define (tmtex-neg l)
  (tmtex-function 'not l))

(define (tmtex-tree l)
  (let* ((root (list '!begin "bundle" (tmtex (car l))))
	 (children (map (lambda (x) (list 'chunk (tmtex x))) (cdr l))))
    (list root (tex-concat children))))

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

(define (tmtex-with-one var val arg)
  (if (== var "mode")
      (let ((old (tmtex-env-get-previous "mode")))
	(cond ((and (== val "text") (!= old "text"))
	       (list 'text arg))
	      ((and (== val "math") (!= old "math"))
	       (list '!math arg))
	      (else arg)))
      (let ((w (tmtex-get-with-cmd var val))
	    (a (tmtex-get-assign-cmd var val)))
	(cond (w (list w arg))
	      (a (list '!group (tex-concat (list (list a) " " arg))))
	      ((== "par-left" var)
               (list (list '!begin "tmparmod" val "0pt" "0pt") arg))
	      ((== "par-right" var)
               (list (list '!begin "tmparmod" "0pt" val "0pt") arg))
	      ((== "par-first" var)
               (list (list '!begin "tmparmod" "0pt" "0pt" val) arg))
	      ((== var "color")
	      	(list '!group (tex-concat (list (list 'color val) " " arg))))
	      (else arg)))))

(define (tmtex-with l)
  (if (null? l) ""
      (if (null? (cdr l)) (tmtex (car l))
	  (let ((var (force-string (car l)))
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
  (let ((var (tmtex-var-name (car l))) (val (cadr l)))
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

(define (tmtex-label l)
  (list 'label (force-string (car l))))

(define (tmtex-reference l)
  (list 'ref (force-string (car l))))

(define (tmtex-pageref l)
  (list 'pageref (force-string (car l))))

(define (tmtex-specific l)
  (if (== (car l) "latex") (tmtex (cadr l)) ""))

(define (tmtex-hyperlink l)
  (tmtex-function 'tmhlink l))

(define (tmtex-action l)
  (tmtex-function 'tmaction l))

(define (tmtex-postscript l)
  (let* ((fig (list 'epsfig (string-append "file=" (force-string (car l)))))
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

(define (tmtex-select-data expr tag)
  (let* ((data (tm-select expr (list tag)))
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

(define (tmtex-doc-data s l)
  (let* ((tag (cons s l))
	 (title (tmtex-select-data tag 'doc-title))
	 (authors (map tmtex-make-author (tm-select tag '(doc-author-data))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs style primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-std-env s l)
  (list (list '!begin s) (tmtex (car l))))

(define (tmtex-appendix s l)
  (if tmtex-appendices?
      (list 'chapter (tmtex (car l)))
      (begin
	(set! tmtex-appendices? #t)
	(list '!concat '(appendix) (list 'chapter (tmtex (car l)))))))

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
      (list '!verb (tmtex-tt (car l)))))

(define (tmtex-list-env s l)
  (let* ((r (string-replace s "-" ""))
	 (t (cond ((== r "enumerateRoman") "enumerateromancap")
		  ((== r "enumerateAlpha") "enumeratealphacap")
		  (else r))))
    (list (list '!begin t) (tmtex (car l)))))

(define (tmtex-equation s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex (car l))))
    (tmtex-env-reset "mode")
    (if (== s "equation")
	(list (list '!begin s) r)
	(list '!eqn r))))

(define (tmtex-eqnarray s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex-table-apply (string->symbol s) (car l))))
    (tmtex-env-reset "mode")
    r))

(define (tmtex-dummy s l)
  "")

(define (tmtex-toc s l)
  (tex-apply 'tableofcontents))

(define (tmtex-bib-sub doc)
  (cond ((nlist? doc) doc)
	((match? doc '(concat (bibitem* :1) (label :string?) :*))
	 (let* ((l (cadr (caddr doc)))
		(s (if (string-starts? l "bib-") (string-drop l 4) l)))
	   (cons* 'concat (list 'bibitem* (cadadr doc) s) (cdddr doc))))
	(else (map tmtex-bib-sub doc))))

(define (tmtex-bib-max l)
  (cond ((npair? l) "")
	((match? l '(bibitem* :string? :1)) (cadr l))
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

(define (tmtex-session s l)
  (tmtex (car l)))

(define (tmtex-input s l)
  (let ((prompt (car l)) (x (cadr l)))
    (tex-concat
     (list `(!group (!concat (color "red") (ttfamily ,(tmtex prompt))))
	   (cond ((func? x 'math 1)
		  (tmtex-env-set "mode" "math")
		  (let ((r (tmtex (cadr x))))
		    (tmtex-env-reset "mode")
		    `(!math (!group (!concat (color "blue") ,r)))))
		 (else `(!group (!concat (color "blue")
					 (!verb ,(tmtex-tt x))))))))))

(define (tmtex-output s l)
  (list '!group (list 'ttfamily (tmtex (car l)))))

(define (tmtex-cite-list l)
  (cond ((null? l) "")
	((nstring? (car l)) (tmtex-cite-list (cdr l)))
	((null? (cdr l)) (car l))
	(else (string-append (car l) "," (tmtex-cite-list (cdr l))))))

(define (tmtex-cite s l)
  (tex-apply (string->symbol s) (tmtex-cite-list l)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-apply key args)
  (let ((n (length args))
	(r (drd-ref tmtex-methods% key)))
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
  (tmtex-apply (string->symbol (car l)) (cdr l)))

(define (tmtex-list l)
  (map-in-order tmtex l))

(define (tmtex x)
  (if (string? x) (tmtex-string x)
      (tmtex-apply (car x) (cdr x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmtex-methods%
  (document tmtex-document)
  (para tmtex-para)
  (surround tmtex-surround)
  (concat tmtex-concat)
  (format tmtex-noop)
  (hspace tmtex-hspace)
  (vspace* tmtex-noop)
  (vspace tmtex-vspace)
  (space tmtex-space)
  (htab tmtex-htab)
  (split tmtex-noop)
  (move tmtex-noop)
  (resize tmtex-noop)
  (float tmtex-float)
  ((:or repeat datoms dlines dpages dbox) tmtex-noop)
  (with-limits tmtex-noop)
  (line-break tmtex-line-break)
  (new-line tmtex-new-line)
  (line-sep tmtex-noop)
  (next-line tmtex-next-line)
  (no_break tmtex-noop)
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
  (group tmtex-group)
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
  (tree tmtex-noop)
  ((:or old-matrix old-table old-mosaic old-mosaic-item) tmtex-noop)
  (tformat tmtex-tformat)
  ((:or twith cwith tmarker) tmtex-noop)
  (table tmtex-table)
  ((:or row cell subtable) tmtex-noop)
  (assign tmtex-assign)
  (with tmtex-with)
  ((:or set reset) tmtex-noop)
  (compound tmtex-compound)
  ((:or begin end) tmtex-noop)
  (include tmtex-noop)
  ((:or macro func env eval) tmtex-noop)
  (value tmtex-compound)
  (arg tmtex-noop)
  ((:or backup quote delay hold release) tmtex-noop)
  ((:or or xor and not plus minus times over div mod merge length range
	number date translate is-tuple look-up equal unequal less lesseq
	greater greatereq if case while extern authorize)
   tmtex-noop)
  ((:or inactive symbol latex hybrid tuple collection associate) tmtex-noop)
  (label tmtex-label)
  (reference tmtex-reference)
  (pageref tmtex-pageref)
  (write tmtex-noop)
  (specific tmtex-specific)
  (hlink tmtex-hyperlink)
  (action tmtex-action)
  ((:or tag meaning) tmtex-noop)
  ((:or switch fold exclusive progressive superposed) tmtex-noop)
  ((:or graphics point line arc bezier) tmtex-noop)
  (postscript tmtex-postscript)
  
  (!file tmtex-file)
  (!arg tmtex-tex-arg))

(drd-table tmtex-tmstyle%
  (doc-data (,tmtex-doc-data -1))
  (abstract (,tmtex-std-env 1))
  (appendix (,tmtex-appendix 1))
  ((:or theorem proposition lemma corollary proof axiom definition
	notation conjecture remark note example exercise warning
	convention quote quotation verse)
   (,tmtex-std-env 1))
  ((:or verbatim code) (,tmtex-verbatim 1))
  ((:or center indent body) (,tmtex-std-env 1))
  ((:or description itemize itemize-minus itemize-dot itemize-arrow
	enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   (,tmtex-list-env 1))
  ((:or equation equation*) (,tmtex-equation 1))
  ((:or eqnarray eqnarray* leqnarray*) (,tmtex-eqnarray 1))
  ((:or the-index the-glossary) (,tmtex-dummy -1))
  ((:or table-of-contents) (,tmtex-toc 2))
  (bibliography (,tmtex-bib 4))
  (thebibliography (,tmtex-thebibliography 2))
  (bibitem* (,tmtex-bibitem* -1))
  ((:or small-figure big-figure small-table big-table) (,tmtex-figure 2))
  (item (,tmtex-item 0))
  (item* (,tmtex-item-arg 1))
  (render-proof (,tmtex-render-proof 2))
  (nbsp (,tmtex-nbsp 0))
  (session (,tmtex-session 1))
  (input (,tmtex-input 2))
  (output (,tmtex-output 1))
  ((:or cite nocite) (,tmtex-cite -1))
  (choose (,tmtex-choose 2))
  ((:or strong em tt name samp abbr dfn kbd var acronym person)
   (,tmtex-modifier 1))
  (menu (,tmtex-menu -1)))

(drd-group tmtex-protected%
  a b c d i j k l o r t u v H L O P S
  aa ae bf cr dh dj dp em fi ge gg ht if in it le lg ll lu lq mp mu
  ne ng ni nu oe or pi pm rm rq sb sc sf sl sp ss th to tt wd wp wr xi
  AA AE DH DJ Im NG OE Pi Pr Re SS TH Xi)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs->latex x opts)
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
	     (style* (tmfile-extract x 'style))
	     (style (if (list? style*) style* (list style*)))
	     (lan (tmfile-init x "language"))
	     (init (tmfile-extract x 'initial))
	     (doc (list '!file body style lan init (get-texmacs-path))))
	(texmacs->latex doc opts))
      (let* ((x2 (tmtm-eqnumber->nonumber x))
	     (x3 (tmtm-match-brackets x2)))
	(tmtex-initialize opts)
	(tmtex (tmpre-produce x3)))))
