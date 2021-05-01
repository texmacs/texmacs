
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
	(convert rewrite tmtm-brackets)
	(convert latex texout)
        (doc tmdoc-markup)
	(convert latex latex-tools)))

(use-modules (ice-9 format))

(tm-define tmtex-debug-mode? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define tmtex-style "generic")
(tm-define tmtex-packages '())
(tm-define tmtex-replace-style? #t)
(define tmtex-languages '())
(define tmtex-colors '())
(define tmtex-colormaps '())
(define tmtex-env (make-ahash-table))
(define tmtex-macros (make-ahash-table))
(define tmtex-dynamic (make-ahash-table))
(define tmtex-serial 0)
(define tmtex-ref-cnt 1)
(define tmtex-auto-produce 0)
(define tmtex-auto-consume 0)
(define tmtex-image-root-url (unix->url "image"))
(define tmtex-image-root-string "image")
(define tmtex-appendices? #f)
(define tmtex-indirect-bib? #f)
(define tmtex-mathjax? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  ;;; Elsevier styles
  (elsevier-style%      (in? tmtex-style '("elsart" "jsc" "elsarticle"
                                           "ifac")))
  (jsc-style%           (in? tmtex-style '("jsc"))        elsevier-style%)
  (elsarticle-style%    (in? tmtex-style '("elsarticle")) elsevier-style%)
  (elsart-style%        (in? tmtex-style '("elsart"))     elsevier-style%)
  (ifac-style%          (in? tmtex-style '("ifac"))       elsevier-style%)

  ;;; ACM styles
  (acm-style%           (in? tmtex-style '("acmconf" "sig-alternate"
                                           "acm_proc_article-sp"
                                           "acmsmall" "acmlarge" "acmtog"
                                           "sigconf" "sigchi" "sigplan"
                                           "acmart")))
  (acm-art-style%       (in? tmtex-style '("acmsmall" "acmlarge" "acmtog"
                                           "sigconf" "sigchi" "sigplan"
                                           "acmart")) acm-style%)
  (sig-alternate-style% (in? tmtex-style '("sig-alternate")) acm-style%)
  (acm-conf-style%      (in? tmtex-style '("acmconf" "sig-alternate"
                                           "acm_proc_article-sp")) acm-style%)
  (acm-small-style%     (in? tmtex-style '("acmsmall")) acm-art-style%)
  (acm-large-style%     (in? tmtex-style '("acmlarge")) acm-art-style%)
  (acm-tog-style%       (in? tmtex-style '("acmtog")) acm-art-style%)
  (acm-sigconf-style%   (in? tmtex-style '("sigconf")) acm-art-style%)
  (acm-sigchi-style%    (in? tmtex-style '("sigchi")) acm-art-style%)
  (acm-sigplan-style%   (in? tmtex-style '("sigplan")) acm-art-style%)

  ;; AMS styles
  (ams-style%           (in? tmtex-style '("amsart")))

  ;; Revtex styles
  (revtex-style%        (in? tmtex-style '("aip" "aps")))
  (aip-style%           (in? tmtex-style '("aip")) revtex-style%)
  (aps-style%           (in? tmtex-style '("aps")) revtex-style%)
  (sv-style%            (in? tmtex-style '("svjour" "svjour3"
                                           "llncs" "svmono")))

  ;; Springer styles
  (springer-style%      (in? tmtex-style '("svjour" "svjour3"
                                           "llncs" sv-style%)))
  (svjour-style%        (in? tmtex-style '("svjour"
                                           "svjour3")) springer-style%)
  (llncs-style%         (in? tmtex-style '("llncs"))  springer-style%)
  (svmono-style%        (in? tmtex-style '("svmono")) sv-style%)

  ;; IEEE styles
  (ieee-style%          (in? tmtex-style '("ieeeconf" "ieeetran")))
  (ieee-conf-style%     (in? tmtex-style '("ieeeconf")) ieee-style%)
  (ieee-tran-style%     (in? tmtex-style '("ieeetran")) ieee-style%)

  ;; Other styles
  (beamer-style%        (in? tmtex-style '("beamer" "old-beamer")))
  (natbib-package%      (in? "natbib" tmtex-packages)))

(tm-define (tmtex-style-init body)
  (noop))

(tm-define (tmtex-style-preprocess doc) doc)

(define (import-tmtex-styles)
  (cond ((elsevier-style?) (import-from (convert latex tmtex-elsevier)))
        ((acm-style?)      (import-from (convert latex tmtex-acm)))
        ((ams-style?)      (import-from (convert latex tmtex-ams)))
        ((revtex-style?)   (import-from (convert latex tmtex-revtex)))
        ((ieee-style?)     (import-from (convert latex tmtex-ieee)))
        ((beamer-style?)   (import-from (convert latex tmtex-beamer)))
        ((or (springer-style?) (svmono-style?))
         (import-from (convert latex tmtex-springer)))
         (else (noop))))

(tm-define (tmtex-provided-packages) '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization from options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-initialize opts)
  (set! tmtex-ref-cnt 1)
  (set! tmtex-env (make-ahash-table))
  (set! tmtex-macros (make-ahash-table))
  (set! tmtex-dynamic (make-ahash-table))
  (set! tmtex-serial 0)
  (set! tmtex-auto-produce 0)
  (set! tmtex-auto-consume 0)
  (set! tmtex-mathjax? #f)
  (if (== (url-suffix current-save-target) "tex")
      (begin
	(set! tmtex-image-root-url (url-unglue current-save-target 4))
	(set! tmtex-image-root-string
	      (url->unix (url-tail tmtex-image-root-url))))
      (begin
	(set! tmtex-image-root-url (unix->url "image"))
	(set! tmtex-image-root-string "image")))
  (set! tmtex-appendices? #f)
  (set! tmtex-replace-style?
    (== (assoc-ref opts "texmacs->latex:replace-style") "on"))
  (set! tmtex-indirect-bib?
    (== (assoc-ref opts "texmacs->latex:indirect-bib") "on"))
  (set! tmtex-use-macros?
    (== (assoc-ref opts "texmacs->latex:use-macros") "on"))
  (when (== (assoc-ref opts "texmacs->latex:mathjax") "on")
    (tmtex-env-set "mode" "math")
    (set! tmtex-mathjax? #t))
  (with charset (assoc-ref opts "texmacs->latex:encoding")
    (if tmtex-cjk-document? (set! charset "utf-8"))
    (cond ((== charset "utf-8")
           (set! tmtex-use-catcodes? #f)
           (set! tmtex-use-ascii?    #f)
           (set! tmtex-use-unicode?  #t))
          ((== charset "cork")
           (set! tmtex-use-catcodes? #t)
           (set! tmtex-use-ascii?    #f)
           (set! tmtex-use-unicode?  #f))
          ((== charset "ascii")
           (set! tmtex-use-catcodes? #f)
           (set! tmtex-use-ascii?    #t)
           (set! tmtex-use-unicode?  #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Determination of the mode in which commands are used
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define command-text-uses (make-ahash-table))
(define command-math-uses (make-ahash-table))

(define (compute-mode-stats t mode)
  (when (tree-compound? t)
    (let* ((h (if (== mode (tree "math"))
                  command-math-uses
                  command-text-uses))
           (n (or (ahash-ref h (tree-label t)) 0)))
      (ahash-set! h (tree-label t) (+ n 1))
      (for-each (lambda (i)
                  (with nmode (tree-child-env t i "mode" mode)
                    (compute-mode-stats (tree-ref t i) nmode)))
                (.. 0 (tree-arity t))))))

(define (init-mode-stats t)
  (set! command-text-uses (make-ahash-table))
  (set! command-math-uses (make-ahash-table))
  (compute-mode-stats (tm->tree t) "text"))

(define (mode-protect t)
  (cond ((and (pair? t) (symbol? (car t))
              (string-starts? (symbol->string (car t)) "tmtext"))
         `(text ,t))
        ((and (pair? t) (symbol? (car t))
              (or (string-starts? (symbol->string (car t)) "tmmath")
                  (string-starts? (symbol->string (car t)) "math")))
         `(ensuremath ,t))
        ((func? t '!concat)
         `(!concat ,@(map mode-protect (cdr t))))
        (else t)))

(define (tmtex-pre t)
  (cond ((tm-func? t 'para)
         (cons '!paragraph (map-in-order tmtex-pre (tm-children t))))
        ((tm-func? t 'concat)
         (cons '!paragraph (map-in-order tmtex-pre (tm-children t))))
        ((and (tm-func? t 'assign 2) (tm-atomic? (tm-ref t 0)))
         (let* ((name (tm-ref t 0))
                (tag (string->symbol name))
                (tnr (or (ahash-ref command-text-uses tag) 0))
                (mnr (or (ahash-ref command-math-uses tag) 0)))
           ;;(display* tag ", " tnr ", " mnr "\n")
           (cond ((and (string-ends? name "*")
                       (or (string-starts? name "itemize")
                           (string-starts? name "enumerate")
                           (string-starts? name "description")))
                  "")
                 ((>= tnr mnr)
                  (with r (tmtex t)
                    ;;(display* t " -> " r "\n")
                    (when (and (> mnr 0) (func? r 'newcommand 2))
                      (with val (mode-protect (caddr r))
                        (set! r (list (car r) (cadr r) val))))
                    r))
                 (else
                   (tmtex-env-set "mode" "math")
                   (with r (tmtex t)
                     (tmtex-env-reset "mode")
                     ;;(display* t " -> " r "\n")
                     (when (and (> tnr 0) (func? r 'newcommand 2))
                      (with val (mode-protect (caddr r))
                        (set! r (list (car r) (cadr r) val))))
                     r)))))
        (else (tmtex t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table tmtex-table-props%
  (block ("" "l" "" #t))
  (block* ("" "c" "" #t))
  (wide-block ("{\\noindent}" "@{}X@{}" "" #t))
  (tabular ("" "l" "" #f))
  (tabular* ("" "c" "" #f))
  (wide-tabular ("{\\noindent}" "@{}X@{}" "" #f))
  (matrix ((,(string->symbol "left(")) "c" (,(string->symbol "right)")) #f))
  (det ((left|) "c" (right|) #f))
  (bmatrix ((,(string->symbol "left[")) "c" (,(string->symbol "right]")) #f))
  (stack ("" "c" "" #f))
  (choice ((left\{) "l" (right.) #f))
  (tabbed ("" "l" "" #f))
  (tabbed* ("" "l" "" #f)))

(logic-table tex-with-cmd%
  (("font-family" "rm") tmtextrm)
  (("font-family" "ss") tmtextsf)
  (("font-family" "tt") tmtexttt)
  (("font-series" "medium") tmtextmd)
  (("font-series" "bold") tmtextbf)
  (("font-shape" "right") tmtextup)
  (("font-shape" "slanted") tmtextsl)
  (("font-shape" "italic") tmtextit)
  (("font-shape" "small-caps") tmtextsc)
  (("par-columns" "2") (!begin "multicols" "2"))
  (("par-columns" "3") (!begin "multicols" "3"))
  (("par-mode" "center") (!begin "center"))
  (("par-mode" "left") (!begin "flushleft"))
  (("par-mode" "right") (!begin "flushright")))

(logic-table tex-with-cmd-math%
  (("font" "cal") mathcal)
  (("font" "cal*") mathscr)
  (("font" "cal**") EuScript)
  (("font" "Euler") mathfrak)
  (("font" "Bbb") mathbb)
  (("font" "Bbb*") mathbbm)
  (("font" "Bbb**") mathbbmss)
  (("font" "Bbb***") mathbb)
  (("font" "Bbb****") mathds)
  (("font-family" "rm") mathrm)
  (("font-family" "ss") mathsf)
  (("font-family" "tt") mathtt)
  (("font-series" "medium") tmmathmd)
  (("font-series" "bold") tmmathbf)
  (("font-shape" "right") mathrm)
  (("font-shape" "slanted") mathit)
  (("font-shape" "italic") mathit)
  (("font-shape" "small-caps") mathrm)
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
  (("math-font-series" "bold") tmmathbf))

(logic-table tex-assign-cmd%
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
    (and (pair? val) (car val))))

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

(tm-define (tmtex-concat-sep l)
  (set! l (list-intersperse l '(!concat (tmsep) " ")))
  (if (null? l) '() `((!concat ,@l))))

(tm-define (tmtex-concat-Sep l)
  (set! l (list-intersperse l '(!concat (tmSep) " ")))
  (if (null? l) '() `((!concat ,@l))))

(define (tex-concat-similar l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((> (length l) 1000)
         (let* ((s (quotient (length l) 2))
                (h (list-head l s))
                (t (list-tail l s)))
           (tex-concat-similar `((!concat ,@h) (!concat ,@t)))))
        (else
          (let ((r (tex-concat-similar (cdr l))))
            (cond ((and (func? (car l) '!sub) (func? (car r) '!sub))
                   (cons (list '!sub (tex-concat (list (cadar l) (cadar r))))
                         (cdr r)))
                  ((and (func? (car l) '!sup) (func? (car r) '!sup))
                   (cons (list '!sup (tex-concat (list (cadar l) (cadar r))))
                         (cdr r)))
                  (else (cons (car l) r)))))))

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

(tm-define (tex-apply . l)
  (if (or (tmtex-math-mode?) (logic-in? (car l) tmpre-sectional%)) l
      (list '!group l)))

(tm-define (tex-math-apply . l)
  (if (tmtex-math-mode?) l
      (list 'ensuremath l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-starts? s r)
  (and (>= (string-length s) (string-length r))
       (== (substring s 0 (string-length r)) r)))

(define (tmtex-modified-token op s i)
  (tex-math-apply op
    (if (= (string-length s) (+ i 1))
        (substring s i (string-length s))
        (tex-apply (string->symbol (substring s i (string-length s)))))))

(logic-table latex-special-symbols%
  ("less"          #\<)
  ("gtr"           #\>)
  ("box"           (Box))
  ("over"          #\:)
  ("||"            (|)) ;; |
  ("precdot"       (tmprecdot)))

(logic-table latex-text-symbols%
  ("#20AC"         euro)
  ("cent"          textcent)
  ("circledR"      textregistered)
  ("copyright"     textcopyright)
  ("currency"      textcurrency)
  ("degree"        textdegree)
  ("mu"            textmu)
  ("onehalf"       textonehalf)
  ("onequarter"    textonequarter)
  ("onesuperior"   textonesuperior)
  ("paragraph"     P)
  ("threequarters" textthreequarters)
  ("threesuperior" textthreesuperior)
  ("trademark"     texttrademark)
  ("twosuperior"   texttwosuperior)
  ("yen"           textyen))

(tm-define (tmtex-token-sub s group?)
  (cond ((logic-ref latex-special-symbols% s)
         (logic-ref latex-special-symbols% s))
        ((string-starts? s "up-") (tmtex-modified-token 'mathrm s 3))
        ;;((string-starts? s "bbb-") (tmtex-modified-token 'mathbbm s 4))
        ((string-starts? s "bbb-") (tmtex-modified-token 'mathbb s 4))
        ((string-starts? s "cal-") (tmtex-modified-token 'mathcal s 4))
        ((string-starts? s "frak-") (tmtex-modified-token 'mathfrak s 5))
        ((string-starts? s "b-cal-")
         (tex-math-apply 'tmmathbf (tmtex-modified-token 'mathcal s 6)))
        ((string-starts? s "b-up-") (tmtex-modified-token 'mathbf s 5))
        ((string-starts? s "b-") (tmtex-modified-token 'tmmathbf s 2))
        ((and (not (tmtex-math-mode?)) (logic-ref latex-text-symbols% s))
         (list '!group (list (logic-ref latex-text-symbols% s))))
        ((and (string-starts? s "#") (not tmtex-use-catcodes?))
         (let* ((qs (string-append "<" s ">"))
                (cv (string-convert qs "Cork" "UTF-8")))
           (list '!widechar (string->symbol cv))))
        ((and (string-starts? s "#") tmtex-use-catcodes?)
         (let* ((qs (string-append "<" s ">"))
                (us (string-convert qs "Cork" "UTF-8"))
                (cv (string-convert us "UTF-8" "LaTeX")))
           (list '!widechar (string->symbol cv))))
        (else (let* ((s2 (string-replace s "-" ""))
                     (ss (list (string->symbol s2))))
                (cond ((logic-in? (car ss) tmtex-protected-symbol%)
                       (with sy (string->symbol (string-append "tmx" s2))
                         (list '!symbol (list sy))))
                      ((not (logic-in? (car ss) latex-symbol%))
                       (display* "TeXmacs] non converted symbol: " s "\n")
                       (list '!symbol (list 'nonconverted s2)))
                      (group? (list '!group ss))
                      (else (list '!symbol ss)))))))

(define (tmtex-token l routine group?)
  (receive (p1 p2) (list-break (cdr l) (lambda (x) (== x #\>)))
    (let* ((s (list->string p1))
	   (q (if (null? p2) '() (cdr p2)))
	   (r (routine q)))
      (cons (tmtex-token-sub s group?) r))))

(define (tmtex-text-sub head l)
  (if (string? head)
    (append (string->list head) (tmtex-text-list (cdr l)))
    (append (list head) (tmtex-text-list (cdr l)))))

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
	      ((== c #\~)  (tmtex-text-sub "\\~{}" l))
	      ((== c #\^)  (tmtex-text-sub "\\^{}" l))
	      ((== c #\\)  (tmtex-text-sub '(textbackslash) l))
	      ((== c #\`)  (tmtex-text-sub "`" l))
	      ((== c #\00) (tmtex-text-sub "\\`{}" l))
	      ((== c #\01) (tmtex-text-sub "\\'{}" l))
	      ((== c #\04) (tmtex-text-sub "\\\"{}" l))
	      ((== c #\05) (tmtex-text-sub "\\H{}" l))
	      ((== c #\06) (tmtex-text-sub "\\r{}" l))
	      ((== c #\07) (tmtex-text-sub "\\v{}" l))
	      ((== c #\10) (tmtex-text-sub "\\u{}" l))
	      ((== c #\11) (tmtex-text-sub "\\={}" l))
	      ((== c #\12) (tmtex-text-sub "\\.{}" l))
	      ((== c #\14) (tmtex-text-sub "\\k{}" l))
	      ((== c #\20) (tmtex-text-sub "``" l))
	      ((== c #\21) (tmtex-text-sub "''" l))
	      ((== c #\22) (tmtex-text-sub ",," l))
	      ((== c #\25) (tmtex-text-sub "--" l))
	      ((== c #\26) (tmtex-text-sub "---" l))
	      ((== c #\27) (tmtex-text-sub "{}" l))
	      ((== c #\33) (tmtex-text-sub "ff" l))
	      ((== c #\34) (tmtex-text-sub '(textbackslash) l))
	      ((== c #\35) (tmtex-text-sub "fl" l))
	      ((== c #\36) (tmtex-text-sub "ffi" l))
	      ((== c #\37) (tmtex-text-sub "ffl" l))
	      ((== c #\174) (tmtex-text-sub '(textbar) l))
	      (else
		(append
                  (if (or tmtex-use-unicode? tmtex-use-ascii?)
                      (string->list (string-convert (char->string c)
                                                    "Cork" "UTF-8"))
                      (list c))
                  (tmtex-text-list (cdr l))))))))

(define (tmtex-math-operator l)
  (receive (p q) (list-break l (lambda (c) (not (char-alphabetic? c))))
    (let* ((op (list->string p))
	   (tail (tmtex-math-list q)))
      (if (logic-in? (string->symbol op) latex-operator%)
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
	      ((== c #\') (append (list '(prime)) (tmtex-math-list (cdr l))))
	      ((== c #\`) (append (list '(backprime)) (tmtex-math-list (cdr l))))
;;	      ((== c #\space) (tmtex-math-list (cdr l)))
	      ((and (char-alphabetic? c)
		    (nnull? (cdr l))
		    (char-alphabetic? (cadr l)))
	       (tmtex-math-operator l))
	      (else
                (with c
                  (if (or tmtex-use-unicode? tmtex-use-ascii?)
                      (string->list (string-convert (char->string c)
                                                    "Cork" "UTF-8"))
                      (list c))
                  (append c (tmtex-math-list (cdr l)))))))))

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
  (if (> (string-length s) 1000)
    `(!concat ,@(map tmtex (tmstring-split s)))
    (let* ((l (string->list s))
           (t (if (tmtex-math-mode?)
                (tmtex-math-list l)
                (tmtex-text-list l)))
           (r (tmtex-string-produce t)))
      (tex-concat r))))

(define (string-convert* what from to)
  (with c (string->list what)
    (apply string-append
           (map (lambda (x) (string-convert (char->string x) from to)) c))))

(define (tmtex-verb-string s)
  (when (nstring? s)
    (set! s (texmacs->verbatim (tm->tree s))))
  (let* ((l (string->list s))
         (t (tmtex-verb-list l))
         (r (tmtex-string-produce t)))
    (if (or tmtex-use-unicode? tmtex-use-ascii?)
        (set! r (map (lambda (x) (string-convert* x "Cork" "UTF-8")) r))
        (set! r (map unescape-angles r)))
    (tex-concat r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entire files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-transform-style x)
  (cond ((in? x '("generic" "exam"
                  "old-generic" "old-article"
                  "tmarticle" "tmdoc" "mmxdoc"))           "article")
        ((in? x '("book" "old-book" "tmbook" "tmmanual"))  "book")
        ((in? x '("letter"  "old-letter"))                 "letter")
        ((in? x '("beamer"  "old-beamer"))                 "beamer")
        ((in? x '("seminar" "old-seminar"))                "slides")
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

(define (tmtex-filter-style-macro t)
  (letrec ((ndef-style? (lambda (x env) (or (not (macro-definition? x))
                                            (nin? (cadr x) env))))
           (filter-style-macro
             (lambda (t env)
               (cond ((nlist? t) t)
                     (else (map (cut filter-style-macro <> env)
                                (filter (cut ndef-style? <> env) t)))))))
    (with env (append (logic-first-list 'tmtex-methods%)
                      (logic-first-list 'tmtex-tmstyle%))
      (filter-style-macro t env))))

(define (comment-preamble t)
  (cond ((string? t) `(!comment ,t))
        ((or (func? t 'para)
             (func? t 'concat)
             (func? t 'document)) (map comment-preamble t))
        (else t)))

(define (tmtex-filter-preamble l)
  (cond ((or (nlist? l) (null? l)) '())
	((macro-definition? l) (list l))
	((and (func? l 'hide-preamble 1)
              (list>0? (cadr l))) (map comment-preamble (cdadr l)))
	(else (append-map tmtex-filter-preamble (cdr l)))))

(define (tmtex-non-preamble-statement? l)
  (cond ((or (nlist? l) (null? l)) #t)
        ((== (car l) 'assign) #f)
        ((== (car l) 'hide-preamble) #f)
        ((func? l 'mtm 2) (tmtex-non-preamble-statement? (caddr l)))
        (else #t)))

(define (tmtex-filter-body l)
  (cond ((or (nlist? l) (null? l)) l)
        ((== (car l) 'assign) "")
        ((== (car l) 'hide-preamble) "")
        ((in? (car l) '(concat document))
         (with a (list-filter (cdr l) tmtex-non-preamble-statement?)
           (if (null? a)
               (if (== (car l) 'concat) "" '(document ""))
               (cons (car l) (map tmtex-filter-body a)))))
        (else (cons (car l) (map tmtex-filter-body (cdr l))))))

(define (tmtex-filter-duplicates* l t)
  (cond ((null? l) l)
        ((func? (car l) 'assign 2)
         (let* ((var (cadr (car l)))
                (r (tmtex-filter-duplicates* (cdr l) t))
                (dup? (ahash-ref t var)))
           (ahash-set! t var #t)
           (if dup? r (cons (car l) r))))
        ((or (func? (car l) 'concat)
             (func? (car l) 'para)
             (func? (car l) 'document))
         (with r (tmtex-filter-duplicates* (cdr l) t)
           (cons (cons (caar l) (tmtex-filter-duplicates* (cdar l) t)) r)))
        (else (cons (car l) (tmtex-filter-duplicates* (cdr l) t)))))

(define (tmtex-filter-duplicates l)
  (with t (make-ahash-table)
    (tmtex-filter-duplicates* l t)))

(define (tmtex-apply-init body init)
  ;;(display* "init= " init "\n")
  (cond ((== (assoc-ref init "language") "verbatim")
	 (with init* (assoc-remove! init "language")
	   (tmtex-apply-init `(verbatim ,body) init*)))
	(else body)))

(define (tmtex-clean-body b)
  (when (and (func? b '!document)
             (> (length b) 1)
             (== (cadr b) `(!document "")))
    (set! b (cons (car b) (cddr b))))
  b)

(define (tmtex-file l)
  (let* ((doc (car l))
         (styles (cadr l))
         (init* (cadddr l))
         (init (or (and (!= init* "#f") init*) '(collection)))
         (init-bis (if (list>1? init)
                     (map (lambda (x) (cons (cadr x) (caddr x))) (cdr init))
                     '()))
         (att (or (cadddr (cdr l)) '()))
         (doc-pre (tmtex-filter-preamble (tmtex-filter-style-macro doc)))
         (doc-preamble (tmtex-filter-duplicates doc-pre))
         (doc-body-pre (tmtex-filter-body doc))
         (doc-body (tmtex-apply-init doc-body-pre init-bis)))
    (init-mode-stats doc-body-pre)
    (latex-set-texmacs-style (if (pair? styles) (car styles) "none"))
    (latex-set-texmacs-packages (if (pair? styles) (cdr styles) (list)))
    (if (== (get-preference "texmacs->latex:expand-user-macros") "on")
      (set! doc-preamble '()))
    (if (null? styles) (tmtex doc)
      (let* ((styles* (tmtex-filter-styles styles))
             (styles** (if (and (== styles* (list "article"))
                                (in? `(associate "par-columns" "2") init))
                           (list `("twocolumn" "article"))
                           styles*))
             (preamble* (ahash-with tmtex-env :preamble #t
                          (map-in-order tmtex-pre doc-preamble)))
             (body* (tmtex-postprocess-body (tmtex doc-body)))
             (body** (tmtex-clean-body body*))
             (needs (list tmtex-languages tmtex-colors tmtex-colormaps)))
        (list '!file body** styles** needs init preamble*)))))

(define (convert-charset t)
  (cond ((string? t) (unescape-angles (utf8->cork t)))
        ((list>0? t) `(,(car t) ,@(map convert-charset (cdr t))))))

(define (tmtex-ilx l)
  `(!invariant ,(car l)))

(define (tmtex-mtm l)
  (cond ((null? l) "")
        ((null? (cdr l)) (tmtex (car l)))
        (else
          (with lab (car l)
            (when (func? lab 'mtm 1) (set! lab (cadr lab)))
            `(!concat (!marker btm ,lab)
                      ,(tmtex (cadr l))
                      (!marker etm ,lab))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-noop l) "")
(define (tmtex-default s l) (cons (string->symbol s) (tmtex-list l)))
(define (tmtex-id l) (tmtex (car l)))
(define (tmtex-first l) (tmtex (car l)))
(define (tmtex-style-first s l) (tmtex (car l)))
(define (tmtex-second l) (tmtex (cadr l)))
(define (tmtex-style-second s l) (tmtex (cadr l)))
(define (tmtex-hide-part s l) "")
(define (tmtex-show-part s l) (tmtex (cadr l)))

(define (tmtex-noop l) "")

(define (tmtex-error l)
  (display* "TeXmacs] error in conversion: " l "\n")
  (if tmtex-debug-mode? "(error)" ""))

(define (tmtex-line-note l)
  `(tmlinenote ,(tmtex (car l))
               ,(tmtex-decode-length (cadr l))
               ,(tmtex-decode-length (caddr l))))

(define (tmtex-marginal-left-note l)
  `(marginpar (!option ,(tmtex (cAr l))) ,(tmtex '())))

(define (tmtex-marginal-right-note l)
  `(marginpar (!option "") ,(tmtex (cAr l))))

(define (tmtex-marginal-note l)
  (cond ((== (car l) "left") (tmtex-marginal-left-note (cdr l)))
        ((== (car l) "right") (tmtex-marginal-right-note (cdr l)))
        (else `(marginpar ,(tmtex (cAr l))))))

(define (tmtex-document l)
  (cons '!document (tmtex-list l)))

(define (tmtex-date l)
  (tmtex-default "tmdate" l))

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

(define (tmtex-no-space-before? x)
  (or (func? x '!sub)
      (func? x '!sup)
      (and (string? x) (!= x "")
           (in? (string-ref x 0) '(#\' #\, #\) #\])))
      (and (func? x '!concat) (tmtex-no-space-before? (cadr x)))))

(define (tmtex-no-space-after? x)
  (or (and (string? x) (!= x "")
           (in? (string-ref x 0) '(#\( #\[)))
      (and (func? x '!concat) (tmtex-no-space-after? (cAr x)))))

(define (tmtex-math-concat-spaces l)
  (if (or (null? l) (null? (cdr l))) l
      (let* ((head (car l))
	     (tail (tmtex-math-concat-spaces (cdr l))))
	(if (or (tmtex-no-space-after? head)
                (tmtex-no-space-before? (car tail)))
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
  ;;(display* "l= " l "\n")
  (if (> (length l) 50)
    (with s (quotient (length l) 2)
      (let ((h (list-head l s))
            (t (list-tail l s)))
        (tmtex-concat `((concat ,@h) (concat ,@t)))))
    (if (tmtex-math-mode?)
        (begin
          ;;(display* "l1= " l "\n")
          ;;(display* "l2= " (pre-brackets-recurse l) "\n")
          ;;(display* "l3= " (tmtex-list (pre-brackets-recurse l)) "\n")
          (tex-concat (tmtex-math-concat-spaces
                       (tmtex-list (pre-brackets-recurse l)))))
        (tex-concat (tmtex-list (tmtex-rewrite-no-break l))))))

(define (tmtex-rigid l)
  (tmtex-function '!group l))

(define (tmtex-no-first-indentation l) (tex-apply 'noindent))
(define (tmtex-line-break l) (tex-apply 'linebreak))
(define (tmtex-page-break l) (tex-apply 'pagebreak))
(define (tmtex-new-page l) (tex-apply 'newpage))
(define (tmtex-no-page-break l) (tex-apply 'nopagebreak))
(define (tmtex-next-line l) (list '!nextline))
(define (tmtex-no-break l) '(!group (nobreak)))
(define (tmtex-emdash l) "---")

(define (tmtex-new-line l)
  (if (tmtex-math-mode?) (tmtex-next-line l) (tex-apply '!newline)))

(tm-define (tmtex-decode-length len)
  ;; FIXME: should be completed
  (with s (force-string len)
    (cond ((string-ends? s "fn")   (string-replace s "fn"   "em"))
	  ((string-ends? s "tab")  (string-replace s "tab"  "em"))
	  ((string-ends? s "spc")  (string-replace s "spc"  "em"))
	  ((string-ends? s "sep")  (string-replace s "sep"  "ex"))
	  ((string-ends? s "par")  (string-replace s "par"  "\\columnwidth"))
	  ((string-ends? s "pag")  (string-replace s "pag"  "\\textheight"))
	  (else s))))

(define (tmtex-hrule s l) (list 'hrulefill))

(define (tmtex-hspace l)
  (let ((s (if (= (length l) 1) (car l) (cadr l))))
    (cond ((== s "0.5fn") (list 'enspace))
	  ((== s "1fn") (list 'quad))
	  ((== s "2fn") (list 'qquad))
	  ((== s "0.5em") (list 'enspace))
	  ((== s "1em") (list 'quad))
	  ((== s "2em") (list 'qquad))
	  ((== s "0.2spc") (list (string->symbol ",")))
          ((not (tmtex-math-mode?))
           (cond ((== s "0.4spc") (list (string->symbol ",")))
                 ((== s "0.6spc") (list (string->symbol ",")))
                 ((== s "0.16667em") (list (string->symbol ",")))
                 (else (tex-apply 'hspace (tmtex-decode-length s)))))
	  ((== s "0.4spc") (list (string->symbol ":")))
	  ((== s "0.6spc") (list (string->symbol ";")))
	  ((== s "-0.6spc") '(!concat (!) (!) (!)))
	  ((== s "-0.4spc") '(!concat (!) (!)))
	  ((== s "-0.2spc") '(!concat (!)))
	  (else (tex-apply 'hspace (tmtex-decode-length s))))))

(define (tmtex-hspace* s l)
  (tmtex-hspace l))

(define (tmtex-vspace l)
  (let ((s (if (= (length l) 1) (car l) (cadr l))))
    (cond ((== s "0.5fn") (tex-apply 'smallskip))
	  ((== s "1fn") (tex-apply 'medskip))
	  ((== s "2fn") (tex-apply 'bigskip))
	  (else (tex-apply 'vspace (tmtex-decode-length s))))))

(define (tmtex-space l)
  (tmtex-hspace (list (car l))))

(define (into-single-paragraph t)
  (set! t (tm-replace t (lambda (x) (tm-in? x '(equation equation*)))
                        (lambda (x)
                          (if (and (== (length x) 2)
                                   (tm-func? (cadr x) 'document 1))
                              `(math ,(cadr (cadr x)))
                              `(math ,@(cdr x))))))
  (set! t (tm-replace t (lambda (x) (tm-func? x 'document))
                        (lambda (x) `(para ,@(cdr x)))))
  t)

(define (tmtex-float-make wide? size type position x capt)
  (let* ((pos (string-replace position "f" ""))
         (type* (if wide? (string-append type "*") type))
         (body (tmtex x))
	 (caption (tmtex (into-single-paragraph capt)))
	 (body* `(!paragraph ,body (caption ,caption))))
    (cond ((and (== size "big") (== type "figure"))
           (if (== pos "")
               `((!begin ,type) ,body*)
               `((!begin ,type* (!option ,pos)) ,body*)))
	  ((and (== size "big") (== type "table"))
           (if (== pos "")
               `((!begin ,type) ,body*)
               `((!begin ,type* (!option ,pos)) ,body*)))
	  (else (list 'tmfloat pos size type* body caption)))))

(define (tmtex-float-table? x)
  (or (func? x 'small-table 2) (func? x 'big-table 2)))

(define (tmtex-float-figure? x)
  (or (func? x 'small-figure 2) (func? x 'big-figure 2)))

(define (tmtex-float-size l)
  (if (list? l)
      (if (or (func? l 'small-table) (func? l 'small-figure)) "small" "big")
      "big"))

(define (tmtex-float-sub wide? position l)
  (with pos (string-replace position "f" "")
    (cond ((func? l 'document 1)
           (tmtex-float-sub wide? pos (cadr l)))
          ((tmtex-float-figure? l)
           (tmtex-float-make wide? (tmtex-float-size l) "figure"
                             pos (cadr l) (caddr l)))
          ((tmtex-float-table? l)
           (tmtex-float-make wide? (tmtex-float-size l) "table"
                             pos (cadr l) (caddr l)))
          (else
            (tmtex-float-make wide? "big" "figure"
                              pos l "")))))

(define (tmtex-float l)
  (tmtex-float-sub #f (force-string (cadr l)) (caddr l)))

(define (tmtex-wide-float l)
  (tmtex-float-sub #t (force-string (cadr l)) (caddr l)))

(define (tmtex-htab l)
  (tex-apply 'hspace* (list 'fill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make brackets small when necessary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (disable-large? x level)
  (cond ((string? x) #t)
        ((func? x 'concat)
         (list-and (map (cut disable-large? <> level) (cdr x))))
        ((tm-in? x '(left mid right)) #t)
        ((tm-in? x '(lsub lsup rsub rsup))
         (and (> level 0) (disable-large? (cadr x) (- level 1))))
        ((tm-in? x '(lprime rprime)) #t)
        ((tm-in? x '(wide wide*))
         (disable-large? (cadr x) (- level 1)))
        ((tm-in? x '(with rigid locus))
         (disable-large? (cAr x) level))
        (else #f)))

(define (make-small s)
  (cond ((nstring? s) "<nobracket>")
	((== s ".") "<nobracket>")
	((<= (string-length s) 1) s)
        ((and (string-starts? s "<") (string-ends? s ">")) s)
	(else (string-append "<" s ">"))))

(define (make-small-bracket x)
  (if (tm-in? x '(left mid right)) (make-small (cadr x)) x))

(define (find-right l)
  (cond ((null? l) #f)
        ((func? (car l) 'left) #f)
        ((func? (car l) 'right) 2)
        (else (with i (find-right (cdr l)) (and i (+ i 1))))))

(define (pre-brackets l)
  (cond ((null? l) l)
        ((func? (car l) 'left)
         (with n (find-right (cdr l))
           (if (not n) (cons (car l) (pre-brackets (cdr l)))
               (let* ((r (pre-brackets (sublist l n (length l))))
                      (m (sublist l 0 n)))
                 (if (disable-large? `(concat ,@m) 2)
                     (begin
                       ;;(display* "< " m "\n")
                       ;;(display* "> " (map make-small-bracket m) "\n")
                       (append (map make-small-bracket m) r))
                     (append m r))))))
        (else (cons (car l) (pre-brackets (cdr l))))))

(define (pre-brackets-recurse l)
  (with r (pre-brackets l)
    (if (== r l) r
        (pre-brackets-recurse r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-around x)
  (with d (downgrade-brackets x)
    (tmtex-concat (if (pair? d) (cdr d) (list d)))))

(define (tmtex-around l)
  (convert-around (cons 'around l)))

(define (tmtex-around* l)
  (convert-around (cons 'around* l)))

(define (tmtex-big-around l)
  (convert-around (cons 'big-around l)))

(define (tmtex-large-decode s)
  (cond ((nstring? s) ".")
        ((in? s '("(" ")" "[" "]" "|" "/" ".")) s)
	((== s "||") "\\|")
	((== s "\\") "\\backslash")
	((and (string-starts? s "<") (string-ends? s ">"))
	 (string-append "\\" (substring s 1 (- (string-length s) 1))))
	(else (string-append "\\" s))))

(define (tmtex-large-decode-text s)
  (cond ((nstring? s) "")
        ((== s ".") "")
        ((in? s '("(" ")" "[" "]" "|" "/")) s)
        ((in? s '("{" "}")) (string-append "\\" s))
        (else
         (display* "TeXmacs] non converted bracket: " s "\n")
         "")))

(define (tmtex-left l)
  (if (tmtex-math-mode?)
      (let* ((s (tmtex-large-decode (car l)))
             (n (if (= (length l) 2) (string->number (cadr l)) 0))
             (b (cond ((not n) "left")
                      ((= n 1) "bigl")
                      ((= n 2) "Bigl")
                      ((= n 3) "biggl")
                      ((= n 4) "Biggl")
                      (else "left"))))
        (list (string->symbol (string-append b s))))
      (tmtex-large-decode-text (car l))))

(define (tmtex-mid l)
  (if (tmtex-math-mode?)
      (with s (tmtex-large-decode (car l))
        (if (== s ".") "" s))
      (tmtex-large-decode-text (car l))))

(define (tmtex-right l)
  (if (tmtex-math-mode?)
      (let* ((s (tmtex-large-decode (car l)))
             (n (if (= (length l) 2) (string->number (cadr l)) 0))
             (b (cond ((not n) "right")
                      ((= n 1) "bigr")
                      ((= n 2) "Bigr")
                      ((= n 3) "biggr")
                      ((= n 4) "Biggr")
                      (else "right"))))
        (list (string->symbol (string-append b s))))
      (tmtex-large-decode-text (car l))))

(define (tmtex-big-decode s)
  (cond ((nstring? s) "bignone")
        ((in? s '("sum" "prod" "int" "oint" "coprod")) s)
        ((in? s '("iint" "iiint" "iiiint" "idotsint")) s)
        ((in? s '("oiint" "oiiint")) s)
	((== s "amalg") "coprod")
	((== s "pluscup") "uplus")
	((== s ".") "bignone")
	(else (string-append "big" s))))

(define (tmtex-big l)
  (list (string->symbol (tmtex-big-decode (car l)))))

(define (tmtex-decode-long-arrow s)
  (cond ((nstring? s) #f)
        ((and (string-starts? s "<rubber-") (string-ends? s ">"))
         (tmtex-decode-long-arrow (substring s 8 (- (string-length s) 1))))
        ((in? s '("minus" "leftarrow" "rightarrow" "leftrightarrow"
                  "equal" "Leftarrow" "Rightarrow" "Leftrightarrow"
                  "mapsto" "mapsfrom"))
         (string->symbol (string-append "x" s)))
        ((in? s '("leftrightarrows" "leftleftarrows"
                  "threeleftarrows" "fourleftarrows"
                  "rightleftarrows" "rightrightarrows"
                  "threerightarrows" "fourrightarrows"))
         (string-append "<long" s ">"))
        ((== s "Lleftarrow") "<Llongleftarrow>")
        ((== s "Rrightarrow") "<Llongrightarrow>")
        ((== s "LRleftrightarrow") "<Llongleftrightarrow>")
        (else (string-append "<" s ">"))))

(define (tmtex-long-arrow l)
  (with cmd (tmtex-decode-long-arrow (car l))
    (cond ((and (symbol? cmd) (== (length l) 2))
           (list cmd (tmtex (cadr l))))
          ((symbol? cmd)
           (list cmd (list '!option (tmtex (caddr l))) (tmtex (cadr l))))
          ((== (length l) 2)
           (list 'overset (tmtex (cadr l)) (tmtex cmd)))
          ((== (cadr l) "")
           (list 'underset (tmtex (caddr l)) (tmtex cmd)))
          (else
           (list 'underset (tmtex (caddr l))
                 (list 'overset (tmtex (cadr l)) (tmtex cmd)))))))

(define (tmtex-below l)
  (list 'underset (tmtex (cadr l)) (tmtex (car l))))

(define (tmtex-above l)
  (list 'overset (tmtex (cadr l)) (tmtex (car l))))

(define (tmtex-lsub l)
  (cond ((== (car l) "") "")
        ((tmtex-math-mode?) (tmtex `(concat (!group) (rsub ,(car l)))))
        (else (tmtex `(rsub ,(car l))))))

(define (tmtex-lsup l)
  (cond ((== (car l) "") "")
        ((tmtex-math-mode?) (tmtex `(concat (!group) (rsup ,(car l)))))
        (else (tmtex `(rsup ,(car l))))))

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
  (cond ((== (car l) "") "")
        ((tmtex-math-mode?) (tmtex-script '!sub (car l)))
        (else (list 'tmrsub (tmtex (car l))))))

(define (tmtex-rsup l)
  (cond ((== (car l) "") "")
        ((tmtex-math-mode?) (tmtex-script '!sup (car l)))
        (else (list 'tmrsup (tmtex (car l))))))

(define (tmtex-modulo l)
      (tmtex-script 'mod (car l)))

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
  (let ((wide? (tmtex-wide-star? (car l)))
	(arg (tmtex (car l)))
	(acc (cadr l))
        (text? (not (tmtex-math-mode?))))
    (if (and (string? acc) (string-starts? acc "<wide-"))
	(set! acc (string-append "<" (substring acc 6 (string-length acc)))))
    (cond ((nstring? acc) arg)
	  ((in? acc '("<hat>" "^")) (list (if wide? 'uwidehat 'uhat) arg))
          ((in? acc '("<tilde>" "~")) (list (if wide? 'uwidetilde 'utilde) arg))
	  ((== acc "<bar>") (list 'underline arg))
	  ((== acc "<vect>") (list (if wide? 'underrightarrow 'uvec) arg))
	  ((== acc "<breve>") (list 'ubreve arg))
	  ((== acc "<invbreve>") (list 'uinvbreve arg))
	  ((== acc "<check>") (list 'ucheck arg))
	  ((== acc "<abovering>") (list 'uring arg))
	  ((== acc "<acute>") (list 'uacute arg))
	  ((== acc "<grave>") (list 'ugrave arg))
	  ((== acc "<dot>") (list 'underdot arg))
	  ((== acc "<ddot>") (list 'uddot arg))
	  ((== acc "<dddot>") (list 'udddot arg))
	  ((== acc "<ddddot>") (list 'uddddot arg))
	  ((== acc "<rightarrow>") (list 'underrightarrow arg))
	  ((== acc "<leftarrow>") (list 'underleftarrow arg))
	  ((== acc "<leftrightarrow>") (list 'underleftrightarrow arg))
	  ((== acc "<varrightarrow>") (list 'underrightarrow arg))
	  ((== acc "<varleftarrow>") (list 'underleftarrow arg))
	  ((== acc "<varleftrightarrow>") (list 'underleftrightarrow arg))
	  ((in? acc '("<underbrace>" "<underbrace*>"))
	   (list 'underbrace arg))
	  ((in? acc '("<overbrace>" "<overbrace*>"))
	   (tmtex-below `(,(car l) (text (downbracefill)))))
	  ((in? acc '("<punderbrace>" "<punderbrace*>"))
	   (list 'underbrace arg))
	  ((in? acc '("<poverbrace>" "<poverbrace*>"))
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
  (let ((wide? (tmtex-wide? (car l)))
	(arg (tmtex (car l)))
	(acc (cadr l))
        (text? (not (tmtex-math-mode?))))
    (if (and (string? acc) (string-starts? acc "<wide-"))
	(set! acc (string-append "<" (substring acc 6 (string-length acc)))))
    (cond ((nstring? acc) arg)
	  ((in? acc '("<hat>" "^"))
           (list (if text? '^ (if wide? 'widehat 'hat)) arg))
          ((in? acc '("<tilde>" "~"))
           (list (if text? '~ (if wide? 'widetilde 'tilde)) arg))
	  ((== (cadr l) "<wide-bar>")
           (list (if text? '= 'overline) arg))
	  ((== acc "<bar>")
           (list (if text? '= (if wide? 'overline 'bar)) arg))
	  ((== acc "<vect>") (list (if wide? 'overrightarrow 'vec) arg))
	  ((== acc "<breve>") (list (if text? 'u 'breve) arg))
	  ((== acc "<invbreve>") (list 'invbreve arg))
	  ((== acc "<check>") (list (if text? 'v 'check) arg))
	  ((== acc "<abovering>") (list (if text? 'r 'ring) arg))
	  ((== acc "<acute>")
           (list (if text? (string->symbol "'") 'acute) arg))
	  ((== acc "<grave>")
           (list (if text? (string->symbol "`") 'grave) arg))
	  ((== acc "<dot>")
           (list (if text? (string->symbol ".") 'dot) arg))
	  ((== acc "<ddot>")
           (list (if text? (string->symbol "\"") 'ddot) arg))
	  ((== acc "<dddot>") (list 'dddot arg))
	  ((== acc "<ddddot>") (list 'ddddot arg))
	  ((== acc "<rightarrow>") (list 'overrightarrow arg))
	  ((== acc "<leftarrow>") (list 'overleftarrow arg))
	  ((== acc "<leftrightarrow>") (list 'overleftrightarrow arg))
	  ((== acc "<varrightarrow>") (list 'overrightarrow arg))
	  ((== acc "<varleftarrow>") (list 'overleftarrow arg))
	  ((== acc "<varleftrightarrow>") (list 'overleftrightarrow arg))
	  ((in? acc '("<overbrace>" "<overbrace*>"))
	   (list 'overbrace arg))
	  ((in? acc '("<underbrace>" "<underbrace*>"))
	   (tmtex-above `(,(car l) (text (upbracefill)))))
	  ((in? acc '("<poverbrace>" "<poverbrace*>"))
	   (list 'overbrace arg))
	  ((in? acc '("<punderbrace>" "<punderbrace*>"))
	   (tmtex-above `(,(car l) (text (upbracefill)))))
	  ;; FIXME: imperfect translations
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
;; Hacks for tables with multi-paragraph cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-or l1 l2)
  (if (or (null? l1) (null? l2)) (list)
      (cons (or (car l1) (car l2)) (map-or (cdr l1) (cdr l2)))))

(define (tmtex-block-columns t)
  (cond ((tm-func? t 'tformat) (tmtex-block-columns (cAr t)))
        ((tm-func? t 'table 1) (tmtex-block-columns (cAr t)))
        ((tm-func? t 'table)
         (let* ((b1 (tmtex-block-columns `(table ,(cadr t))))
                (b2 (tmtex-block-columns `(table ,@(cddr t)))))
           (map-or b1 b2)))
        ((tm-func? t 'row) (map tmtex-block-columns (cdr t)))
        ((tm-func? t 'cell) (tmtex-block-columns (cAr t)))
        (else (tm-func? t 'document))))

(define (column-numbers l i)
  (cond ((null? l) (list))
        ((car l) (cons i (column-numbers (cdr l) (+ i 1))))
        (else (column-numbers (cdr l) (+ i 1)))))

(define (block-align nr out-of)
  (let* ((c (number->string nr))
         (p (string-append "p{" (number->string (/ 12.0 out-of)) "cm}")))
    `(cwith "1" "-1" ,c ,c "cell-halign" ,p)))

(define (tmtex-block-adjust t)
  (cond ((tm-func? t 'tformat)
         (append (cDr t) (list (tmtex-block-adjust (cAr t)))))
        ((tm-func? t 'table)
         (let* ((b (tmtex-block-columns t))
                (n (column-numbers b 1)))
           (if (null? n) t
               `(tformat ,@(map (cut block-align <> (length n)) n) ,t))))
        (else t)))

(define (tm-big-figure? t)
  (tm-in? t '(big-figure big-table)))

(define (tm-replace-figure t)
  (cond ((tm-func? t 'big-figure)
         (list 'tmfloat "h" "big" "figure" (cadr t) (caddr t)))
        ((tm-func? t 'big-table)
         (list 'tmfloat "h" "big" "table" (cadr t) (caddr t)))
        (else t)))

(define (tmtex-figure-adjust t)
  (tm-replace t tm-big-figure? tm-replace-figure))

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

(define (tmtex-table-apply key args x)
  (let* ((props (logic-ref tmtex-table-props% key))
         (wide? (and props (string-contains? (cadr props) "X"))))
    (when (and (not (tmtex-math-mode?)) (not wide?))
      (set! x (tmtex-block-adjust x))
      (set! x (tmtex-figure-adjust x)))
    (if props
	(let* ((env (if (tmtex-math-mode?) "array" "tabular"))
               (env* (if wide? (list "tabularx" "1.0\\textwidth") (list env)))
	       (before (car props))
	       (after (caddr props))
	       (defaults (append (tmtable-cell-halign (cadr props))
				 (tmtable-block-borders (cadddr props))))
	       (p (tmtable-parser `(tformat ,@defaults ,x)))
	       (e `(!begin ,@env* ,(tmtex-table-args p)))
	       (r (tmtex-table-make p)))
	  (tex-concat (list before (list e r) after)))
        (begin
          (list `(!begin ,(symbol->string key) ,@args)
                (tmtex-table-make (tmtable-parser x)))))))

(define (tmtex-tformat l)
  (tmtex-table-apply 'tabular '() (cons 'tformat l)))

(define (tmtex-table l)
  (tmtex-table-apply 'tabular '() (cons 'table l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local and global environment changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-get-with-cmd var val)
  (if (tmtex-math-mode?)
      (or (logic-ref tex-with-cmd-math% (list var val))
          (logic-ref tex-with-cmd% (list var val)))
      (logic-ref tex-with-cmd% (list var val))))

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
      (logic-ref tex-assign-cmd% (list var val))))

(define (tmlength->texlength len)
  ;; TODO: rewrite (quote x) -> x and (tmlen ...) -> ...pt
  (with tmlen (string->tmlength (force-string len))
    (if (tmlength-null? tmlen) "0pt"
	(let* ((val (tmlength-value tmlen))
	       (unit (symbol->string (tmlength-unit tmlen)))
	       (val-string (number->string val)))
	  (cond ((== unit "fn") (string-append val-string "em"))
		(else len))))))

(define (tmtex-make-parmod x y z arg flag?)
  (set! x (tmlength->texlength x))
  (set! y (tmlength->texlength y))
  (set! z (tmlength->texlength z))
  (if (and (tmlength-zero? (string->tmlength x))
	   (tmlength-zero? (string->tmlength y))
	   (tmlength-zero? (string->tmlength z))
           flag?)
      arg
      (list (list '!begin "tmparmod" x y z) arg)))

(define (tmtex-make-parsep x arg)
  (set! x (tmlength->texlength x))
  (list (list '!begin "tmparsep" x) arg))

(define (tmtex-make-lang val arg)
  (if (== val "verbatim")
    `(tt ,arg)
    (begin
      (if (nin? val tmtex-languages)
        (set! tmtex-languages (append (list val) tmtex-languages)))
      (if (texout-multiline? arg)
        `((!begin "otherlanguage" ,val) ,arg)
        `(foreignlanguage ,val ,arg)))))

(define (tmtex-decode-color s . force-html)
  (with cm (if (string-starts? s "#") "HTML" (named-color->xcolormap s))
    (cond ((and (== cm "none") (nnull? force-html))
           (tmtex-decode-color (get-hex-color s) force-html))
          ((and (== cm "HTML") (nnull? force-html))
           `((!option "HTML") ,(html-color->latex-xcolor s)))
          ((== cm "texmacs")
           (when (nin? s tmtex-colors)
             (set! tmtex-colors (append (list s) tmtex-colors)))
           (string-replace s " " ""))
          ((in? cm (list "x11names"))
           (tmtex-decode-color (get-hex-color s) #t))
          (else
            (when (and (nin? cm tmtex-colormaps)
                       (!= cm "xcolor") (!= cm "none"))
              (set! tmtex-colormaps (append (list cm) tmtex-colormaps)))
            (string-replace s " " "")))))

(define (tmtex-make-color val arg)
  (with ltxcolor (tmtex-decode-color val #t)
    (if (list? ltxcolor)
        `(!group (!append (color ,@ltxcolor) ,arg))
        `(tmcolor ,ltxcolor ,arg))))

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
	      ((and (== val "prog") (== old "text"))
	       `(tt ,arg))
	      ((and (== val "prog") (== old "math"))
	       `(text (tt ,arg)))
	      (else arg)))
      (let ((w (tmtex-get-with-cmd var val))
	    (a (tmtex-get-assign-cmd var val)))
	(cond ((and w (tm-func? arg w 1)) arg)
              (w (list w arg))
	      (a (list '!group (tex-concat (list (list a) " " arg))))
	      ((== "par-left" var)  (tmtex-make-parmod val "0pt" "0pt" arg #t))
	      ((== "par-right" var) (tmtex-make-parmod "0pt" val "0pt" arg #t))
	      ((== "par-first" var) (tmtex-make-parmod "0pt" "0pt" val arg #f))
	      ((== "par-par-sep" var) (tmtex-make-parsep val arg))
              ((== var "language")    (tmtex-make-lang   val arg))
	      ((== var "color")       (tmtex-make-color  val arg))
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

(define (tmtex-with-wrapped l)
  (if (and (== (length l) 3)
           (== (car l) "par-columns")
           (== (cadr l) "1")
           (tm-in? (caddr l) '(small-figure big-figure
                               small-table big-table)))
      (tmtex-float-sub #t "h" (caddr l))
      (tmtex-with l)))

(define (tmtex-var-name-sub l)
  (if (null? l) l
      (let ((c (car l)) (r (tmtex-var-name-sub (cdr l))))
	(cond ((char-alphabetic? c) (cons c r))
              ((char-numeric? c)
               (cond ((char=? c #\0) (cons* #\z #\e #\r #\o r))
                     ((char=? c #\1) (cons* #\o #\n #\e r))
                     ((char=? c #\2) (cons* #\t #\w #\o r))
                     ((char=? c #\3) (cons* #\t #\h #\r #\e #\e r))
                     ((char=? c #\4) (cons* #\f #\o #\u #\r r))
                     ((char=? c #\5) (cons* #\f #\i #\v #\e r))
                     ((char=? c #\6) (cons* #\s #\i #\x r))
                     ((char=? c #\7) (cons* #\s #\e #\v #\e #\n r))
                     ((char=? c #\8) (cons* #\e #\i #\g #\h #\t r))
                     ((char=? c #\9) (cons* #\n #\i #\n #\e r))
                     (else r)))
	      ((and (char=? c #\*) (null? (cdr l))) (list c))
	      (else r)))))

(define (tmtex-var-name var)
  (cond ((nstring? var) "")
	((logic-in? (string->symbol var) tmtex-protected%)
	 (string-append "tm" var))
	((<= (string-length var) 1) var)
	(else
          (with r (list->string (tmtex-var-name-sub (string->list var)))
            (if (and (string-occurs? "*" r)
                     (== (latex-type r) "undefined"))
                (string-replace r "*" "star")
                r)))))

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
  (let* ((var (tmtex-var-name (car l)))
         (bsvar (string-append "\\" var))
         (type (latex-type var))
         (def (if (== type "undefined") 'newcommand 'providecommand))
         (val (cadr l)))
    (while (func? val 'quote 1) (set! val (cadr val)))
    (if (!= var "")
	(begin
	  (tmtex-env-assign var val)
	  (cond ((string? val)
		 (let ((a (tmtex-get-assign-cmd var val)))
		   (if a (list a) (list def bsvar (tmtex val)))))
		((or (func? val 'macro) (func? val 'func))
		 (if (null? (cddr val))
		     (list def bsvar (tmtex (cAr val)))
		     (list def bsvar
			   (list '!option (number->string (- (length val) 2)))
			   (tmtex (tmtex-args (cAr val) (cDdr val))))))
		(else (list def bsvar (tmtex val)))))
	"")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-quote l)
  (tmtex (car l)))

(define (tmtex-hidden-binding l)
  (if (and (== (length l) 2) (string->number (force-string (cAr l))))
      (list 'custombinding (force-string (cAr l)))
      ""))

(define (tmtex-label l)
  (list 'label (force-string (car l))))

(define (tmtex-reference l)
  (list 'ref (force-string (car l))))

(define (tmtex-pageref l)
  (list 'pageref (force-string (car l))))

(define (tmtex-eqref s l)
  (list 'eqref (force-string (car l))))

(define (tmtex-smart-ref s l)
  (let* ((ss (map force-string l))
         (key (string-recompose ss ",")))
    (list 'cref key)))

(define (tmtex-specific l)
  (cond ((== (car l) "latex") (tmtex-tt (cadr l)))
	((== (car l) "image") (tmtex-eps (cadr l)))
	((== (car l) "printer") (tmtex (cadr l)))
	((== (car l) "odd") `(ifthispageodd ,(tmtex (cadr l)) ""))
	((== (car l) "even") `(ifthispageodd "" ,(tmtex (cadr l))))
	(else "")))

(define (tmtex-eps-names)
  (set! tmtex-serial (+ tmtex-serial 1))
  (let* ((suffix (if (get-boolean-preference "native pdf") ".pdf" ".eps"))
         (postfix (string-append "-" (number->string tmtex-serial) suffix))
	 (name-url (url-glue tmtex-image-root-url postfix))
	 (name-string (string-append tmtex-image-root-string postfix)))
    (values name-url name-string)))

(define (tmtex-eps x)
  (if (tmtex-math-mode?) (set! x `(with "mode" "math" ,x)))
  (receive (name-url name-string) (tmtex-eps-names)
    (let* ((extents (print-snippet name-url x #t))
           (unit (* (/ 1.0 60984.0) (/ 600.0 (tenth extents))))
           (x3 (* unit (first extents)))
           (y3 (* unit (second extents)))
           (x4 (* unit (third extents)))
           (y4 (* unit (fourth extents)))
           (x1 (* unit (fifth extents)))
           (y1 (* unit (sixth extents)))
           (x2 (* unit (seventh extents)))
           (y2 (* unit (eighth extents)))
           (lm (string-append (number->string (- x3 x1)) "cm"))
           (rm (string-append (number->string (- x2 x4)) "cm"))
           (ww (string-append (number->string (- x4 x3)) "cm"))
           (hh (string-append (number->string (- y4 y3)) "cm"))
           (opt `(!option ,(string-append "width=" ww ",height=" hh)))
           (rat (/ y3 (- y4 y3)))
           (dy `(!concat ,(number->string rat) (height)))
           (rb `(raisebox ,dy (includegraphics ,opt ,name-string))))
      ;; TODO: top and bottom margins
      ;;(display* name-url ": " x1 ", " y1 "; " x2 ", " y2 "\n")
      ;;(display* name-url ": " x3 ", " y3 "; " x4 ", " y4 "\n")
      (if (and (< (abs (- x3 x1)) 0.01) (< (abs (- x2 x4)) 0.01)) rb
          `(!concat (hspace ,lm) ,rb (hspace ,rm))))))

(define (tmtex-make-eps s l)
  (tmtex-eps (cons (string->symbol s) l)))

(define (tmtex-graphics l)
  (tmtex-eps (cons 'graphics l)))

(define (tmtex-as-eps name)
  (let* ((u (url-relative current-save-target (unix->url name)))
         (suffix (url-suffix u))
         (fm (string-append (format-from-suffix suffix) "-file")))
    (if (and (url-exists? u) (in? suffix (list "eps" "pdf" "png" "jpg")))
	(list 'includegraphics name)
        (receive (name-url name-string) (tmtex-eps-names)
          (convert-to-file u fm "postscript-file" name-url)
          (list 'includegraphics name-string)))))

(define (tmtex-image-length len)
  (let* ((s (force-string len))
	 (unit (and (tm-length? s) (tm-length-unit len))))
    (cond ((== s "") "!")
	  ((string-ends? s "%") "!")
	  ((in? unit '("w" "h")) "!")
	  (else (tmtex-decode-length len)))))

(define (tmtex-image-mag len)
  (let* ((s (force-string len))
	 (val (and (tm-length? s) (tm-length-value len)))
	 (unit (and (tm-length? s) (tm-length-unit len))))
    (cond ((== s "") 0.0)
	  ((string-ends? s "%")
	   (with x (string->number (string-drop-right s 1))
	     (if x (/ x 100.0) 0)))
	  ((in? unit '("w" "h")) (or val 0))
	  (else #f))))

(define (tmtex-image l)
  (if (nstring? (car l))
      (tmtex-eps (cons 'image l))
      (let* ((fig (tmtex-as-eps (force-string (car l))))
             (hor (tmtex-image-length (cadr l)))
             (ver (tmtex-image-length (caddr l)))
             (mhor (tmtex-image-mag (cadr l)))
             (mver (tmtex-image-mag (caddr l))))
        (cond ((or (not mhor) (not mver)) (list 'resizebox hor ver fig))
              ((and (== mhor 0.0) (== mver 0.0)) fig)
              ((or (== mhor 1.0) (== mver 1.0)) fig)
              ((== mhor 0.0) (list 'scalebox (number->string mver) fig))
              (else (list 'scalebox (number->string mhor) fig))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metadata for documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-make-inline t)
  (tm-replace t '(new-line) '(next-line)))

(tm-define (tmtex-inline t)
  (tmtex (tmtex-make-inline t)))

(tm-define (tmtex-doc-title t)
  `(title ,(tmtex-inline (cadr t))))

(tm-define (tmtex-doc-running-title t)
  `(tmrunningtitle ,(tmtex-inline (cadr t))))

(tm-define (tmtex-doc-subtitle t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmsubtitle ,(tmtex-inline (cadr t))))

(tm-define (tmtex-doc-note t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-doc-misc t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

(tm-define (tmtex-doc-date t)
  `(date ,(tmtex-inline (cadr t))))

(tm-define (tmtex-doc-running-author t)
  `(tmrunningauthor ,(tmtex-inline (cadr t))))

(tm-define (tmtex-author-name t)
  `(author ,(tmtex-inline (cadr t))))

(tm-define (tmtex-author-affiliation t)
  ;;(set! t (tmtex-remove-line-feeds t))
  `(tmaffiliation ,(tmtex (cadr t))))

(tm-define (tmtex-author-email t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmemail ,(tmtex-inline (cadr t))))

(tm-define (tmtex-author-homepage t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmhomepage ,(tmtex-inline (cadr t))))

(tm-define (tmtex-author-note t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmnote ,(tmtex (cadr t))))

(tm-define (tmtex-author-misc t)
  (set! t (tmtex-remove-line-feeds t))
  `(tmmisc ,(tmtex (cadr t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful macros for metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-select-args-by-func n l)
  (filter (lambda (x) (func? x n)) l))

(define (tmtex-get-transform l tag)
  (let ((transform (symbol-append 'tmtex- tag))
        (l*        (tmtex-select-args-by-func tag l)))
    (map tmtex l*)))

(tm-define (tmtex-remove-line-feeds t)
  (if (npair? t) t
    (with (r s) (list (car t) (map tmtex-remove-line-feeds (cdr t)))
      (if (== r 'next-line) '(!concat (tmSep) (!linefeed)) `(,r ,@s)))))

(tm-define (tmtex-replace-documents t)
  (if (npair? t) t
    (with (r s) (list (car t) (map tmtex-replace-documents (cdr t)))
      (if (!= r 'document) `(,r ,@s)
        `(concat ,@(list-intersperse s '(next-line)))))))

(tm-define (contains-tags? t l)
  (cond ((or (nlist? t) (null? t)) #f)
        ((in? (car t) l) #t)
        (else
          (with found? #f
            (for-each (lambda (x)
                        (set! found? (or found? (contains-tags? x l))))
                      t)
            found?))))

(tm-define (contains-stree? t u)
  (cond ((== t u) #t)
        ((or (null? t) (nlist? t)) #f)
        (else
          (with found? #f
            (for-each (lambda (x)
                        (set! found? (or found? (contains-stree? x u))))
                      t)
            found?))))

;; Metadata clustering

(define (stree-replace l what by)
  (cond ((or (null? l) (nlist? l)) l)
        ((== l what) by)
        (else
          (map (lambda (x) (stree-replace x what by)) l))))

(define (next-stree-occurence l tag)
  (cond ((or (null? l) (nlist? l)) #f)
        ((== (car l) tag) l)
        (else
          (with found? #f
            (map-in-order
              (lambda (x)
                (if (not found?)
                  (set! found? (next-stree-occurence x tag)))) l)
            found?))))

(define (add-refs l n tag tr tl global-counter?)
  (with streetag (next-stree-occurence (car l) tag)
    (if (not streetag)
      (begin
        (if global-counter? (set! tmtex-ref-cnt n))
        l)
      (let* ((n*      (number->string n))
             (tagref  (list tr n*))
             (authors (stree-replace (car l) streetag tagref))
             (taglist (if (null? (cdr l)) '() (cadr l)))
             (taglist `(,@taglist (,tl ,n* ,(cadr streetag))))
             (l*      (list authors taglist)))
        (add-refs l* (1+ n) tag tr tl global-counter?)))))

(tm-define (make-references l tag author? global-counter?)
  (let* ((tag-ref      (symbol-append tag '- 'ref))
         (tag-label    (symbol-append tag '- 'label))
         (cnt          (if global-counter? tmtex-ref-cnt 1))
         (tmp          (add-refs `(,l) cnt tag tag-ref tag-label
                                 global-counter?))
         (data-refs    (car tmp))
         (data-labels  (if (null? (cdr tmp)) '() (cadr tmp))))
    (if author?
      (set! data-labels `((doc-author (author-data ,@data-labels)))))
    `(,@data-refs ,@data-labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-prepare-author-data l) l)

(tm-define (tmtex-make-author names affiliations emails urls miscs notes
                              affs* emails* urls* miscs* notes*)
  (let* ((names  (tmtex-concat-Sep (map cadr names)))
         (result `(,@names ,@notes ,@miscs ,@affiliations ,@emails ,@urls)))
    (if (null? result) '()
      `(author (!paragraph ,@result)))))

(tm-define (tmtex-doc-author t)
  (if (or (npair? t) (npair? (cdr t)) (not (func? (cadr t) 'author-data))) '()
    (let* ((l        (tmtex-prepare-author-data (cdadr t)))
           (names    (tmtex-get-transform l 'author-name))
           (emails   (tmtex-get-transform l 'author-email))
           (urls     (tmtex-get-transform l 'author-homepage))
           (affs     (tmtex-get-transform l 'author-affiliation))
           (miscs    (tmtex-get-transform l 'author-misc))
           (notes    (tmtex-get-transform l 'author-note))
           (emails*  (tmtex-get-transform l 'author-email-ref))
           (urls*    (tmtex-get-transform l 'author-homepage-ref))
           (affs*    (tmtex-get-transform l 'author-affiliation-ref))
           (miscs*   (tmtex-get-transform l 'author-misc-ref))
           (notes*   (tmtex-get-transform l 'author-note-ref))
           (affs     (append affs   (tmtex-get-transform
                                      l 'author-affiliation-label)))
           (urls     (append urls   (tmtex-get-transform
                                      l 'author-homepage-label)))
           (miscs    (append miscs  (tmtex-get-transform
                                      l 'author-misc-label)))
           (notes    (append notes  (tmtex-get-transform
                                      l 'author-note-label)))
           (emails   (append emails (tmtex-get-transform
                                      l 'author-email-label))))
      (tmtex-make-author names affs emails urls miscs notes
                         affs* emails* urls* miscs* notes*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-prepare-doc-data l)
  (set! l (map tmtex-replace-documents l))
  l)

(define (tmtex-make-title titles subtitles notes miscs tr)
  (let* ((titles (tmtex-concat-Sep (map cadr titles)))
         (content `(,@titles ,@subtitles ,@notes ,@miscs)))
    (if (null? content) '()
      `((title (!indent (!paragraph ,@content)))))))

(tm-define (tmtex-append-authors l)
  (set! l (filter nnull? l))
  (cond ((null? l) '())
        ((== (length l) 1) `((author (!indent (!concat ,@(cdar l))))))
        (else
          (with lf '(!concat (!linefeed) (and) (!linefeed))
            `((author
                (!indent (!concat ,@(list-intersperse (map cadr l) lf)))))))))

(tm-define (tmtex-make-doc-data titles subtitles authors dates miscs notes
                                subtits-l dates-l miscs-l notes-l tr ar)
  `(!document
     ,@(tmtex-make-title titles subtitles notes miscs tr)
     ,@(tmtex-append-authors authors)
     ,@dates
     (maketitle)))

(tm-define (tmtex-get-title-option l)
  (apply append (map cdr (tmtex-select-args-by-func 'doc-title-options l))))

(tm-define (tmtex-doc-data s l)
  (set! l (tmtex-prepare-doc-data l))
  (let* ((titles    (tmtex-get-transform l 'doc-title))
         (tr        (tmtex-get-transform l 'doc-running-title))
         (subtits   (tmtex-get-transform l 'doc-subtitle))
         (authors   (tmtex-get-transform l 'doc-author))
         (ar        (tmtex-get-transform l 'doc-running-author))
         (dates     (tmtex-get-transform l 'doc-date))
         (miscs     (tmtex-get-transform l 'doc-misc))
         (notes     (tmtex-get-transform l 'doc-note))
         (subtits-l (tmtex-get-transform l 'doc-subtitle-label))
         (dates-l   (tmtex-get-transform l 'doc-date-label))
         (miscs-l   (tmtex-get-transform l 'doc-misc-label))
         (notes-l   (tmtex-get-transform l 'doc-note-label))
         (subtits   (append subtits (tmtex-get-transform l 'doc-subtitle-ref)))
         (dates     (append dates  (tmtex-get-transform l 'doc-date-ref)))
         (miscs     (append miscs  (tmtex-get-transform l 'doc-misc-ref)))
         (notes     (append notes  (tmtex-get-transform l 'doc-note-ref))))
    (tmtex-make-doc-data titles subtits authors dates miscs notes
                         subtits-l dates-l miscs-l notes-l tr ar)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract metadata presentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmtex-abstract t)
  (tmtex-std-env "abstract" (cdr t)))

(tm-define (tmtex-abstract-keywords t)
  (with args (list-intersperse (map tmtex (cdr t)) '(tmsep))
    `(!concat (tmkeywords) ,@(map (lambda (x) `(!group ,x)) args))))

(tm-define (tmtex-abstract-acm t)
  (with args (list-intersperse (map tmtex (cdr t)) '(tmsep))
    `(!concat (tmacm) ,@(map (lambda (x) `(!group ,x)) args))))

(tm-define (tmtex-abstract-arxiv t)
  (with args (list-intersperse (map tmtex (cdr t)) '(tmsep))
    `(!concat (tmarxiv) ,@(map (lambda (x) `(!group ,x)) args))))

(tm-define (tmtex-abstract-msc t)
  (with args (list-intersperse (map tmtex (cdr t)) '(tmsep))
    `(!concat (tmmsc) ,@(map (lambda (x) `(!group ,x)) args))))

(tm-define (tmtex-abstract-pacs t)
  (with args (list-intersperse (map tmtex (cdr t)) '(tmsep))
    `(!concat (tmpacs) ,@(map (lambda (x) `(!group ,x)) args))))

(tm-define (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)
  (with result `(,@abstract ,@acm ,@arxiv ,@msc ,@pacs ,@keywords)
    (if (null? result) "" `(!document ,@result))))

(tm-define (tmtex-abstract-data s l)
  (let* ((acm      (map tmtex-abstract-acm
                        (tmtex-select-args-by-func 'abstract-acm l)))
         (arxiv    (map tmtex-abstract-arxiv
                        (tmtex-select-args-by-func 'abstract-arxiv l)))
         (msc      (map tmtex-abstract-msc
                        (tmtex-select-args-by-func 'abstract-msc l)))
         (pacs     (map tmtex-abstract-pacs
                        (tmtex-select-args-by-func 'abstract-pacs l)))
         (keywords (map tmtex-abstract-keywords
                        (tmtex-select-args-by-func 'abstract-keywords l)))
         (abstract (map tmtex-abstract
                        (tmtex-select-args-by-func 'abstract l))))
    (tmtex-make-abstract-data keywords acm arxiv msc pacs abstract)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs style primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-std-env s l)
  (if (== s "quote-env") (set! s "quote"))
  (list (list '!begin s) (tmtex (car l))))

(define (tmtex-footnote s l)
  `(footnote ,(tmtex (car l))))

(define (tmtex-footnotemark s l)
  `(footnotemark (!option ,(tmtex (car l)))))

(define (filter-enunciation-due-to l)
  (cond ((func? l 'dueto) (list l))
        ((nlist>0? l) '())
        (else (append-map filter-enunciation-due-to l))))

(define (filter-enunciation-body l)
  (cond ((func? l 'dueto) '())
        ((nlist>0? l) l)
        (else (filter nnull? (map filter-enunciation-body l)))))

(define (tmtex-enunciation s l)
  (let* ((t       (car l))
         (option  (filter-enunciation-due-to t))
         (option* (map (lambda (x) `(!option ,(tmtex (cadr x)))) option))
         (body    (filter-enunciation-body t)))
  `((!begin ,s ,@option*) ,(tmtex body))))

(define (find-label x)
  (cond ((npair? x) #f)
        ((func? x 'label) x)
        (else (or (find-label (car x)) (find-label (cdr x))))))

(define (remove-labels x)
  (cond ((npair? x) x)
        ((func? x 'label) "")
        (else (cons (remove-labels (car x)) (remove-labels (cdr x))))))

(define (tmtex-sectional s l)
  (let* ((lab (find-label (car l)))
         (tit (if lab (remove-labels (car l)) (car l)))
         (sec (list (string->symbol s) (tmtex tit))))
    (if lab (list '!concat sec lab) sec)))

(define (tmtex-appendix s l)
  (with app (list (if (latex-book-style?) 'chapter 'section) (tmtex (car l)))
    (if tmtex-appendices? app
      (begin
	(set! tmtex-appendices? #t)
	(list '!concat '(appendix) app)))))

(define (tmtex-appendix* s l)
  (with app (list (if (latex-book-style?) 'chapter* 'section*) (tmtex (car l)))
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
        ((func? x 'mtm 2) (tmtex-tt (cAr x)))
        ((func? x 'surround 3)
         (string-append (tmtex-tt (cadr x))
                        (tmtex-tt (cadddr x))
                        (tmtex-tt (caddr x))))
        ((or (func? x 'hgroup 1) (func? x 'vgroup 1))
         (tmtex-tt (cadr x)))
        ((func? x 'with)
         (begin
           (display* "TeXmacs] lost <with> in verbatim content: " (cDr x) "\n")
           (tmtex-tt (cAr x))))
        ((func? x 'math)
         (begin
           (display* "TeXmacs] lost <math> in verbatim content: " (cDr x) "\n")
           (tmtex-tt (cAr x))))
	(else
          (begin
	    (display* "TeXmacs] non converted verbatim content: " x "\n")
            ""))))

(define (unescape-angles l)
  (cond ((string? l)
         (string-replace (string-replace l "<less>" "<") "<gtr>" ">"))
        ((symbol? l) l)
        (else (map unescape-angles l))))

(define (escape-braces l)
  (cond ((string? l) (string-replace (string-replace l "{" "\\{") "}" "\\}"))
        ((symbol? l) l)
        (else (map escape-braces l))))

(define (escape-backslashes l)
  (cond ((string? l) (string-replace l "\\" "\\textbackslash "))
        ((symbol? l) l)
        (else (map escape-backslashes l))))

(define (tmtex-new-theorem s l)
  (ahash-set! tmtex-dynamic (string->symbol (car l)) 'environment)
  `(newtheorem ,@l))

(define (tmtex-verbatim s l)
  (if (func? (car l) 'document)
      (list '!verbatim (tmtex-tt (escape-braces (escape-backslashes (car l)))))
      (list 'tmverbatim (tmtex (car l)))))

(define (sharp-fix t)
  (cond ((and (func? t '!document) (nnull? (cdr t)))
         `(!document ,(sharp-fix (cadr t)) ,@(cddr t)))
        ((and (func? t '!concat) (nnull? (cdr t)))
         `(!concat ,(sharp-fix (cadr t)) ,@(cddr t)))
        ((and (string? t) (string-starts? t "#"))
         (string-append "\\" t))
        (else t)))

(define (tmtex-verbatim* s l)
  (if (func? (car l) 'document)
      (list '!verbatim* (sharp-fix (tmtex-tt (car l))))
      (list 'tmverbatim (tmtex (car l)))))

(define (tmtex-code-inline s l)
  (with lang `((!option ,s))
    `(tmcodeinline ,@lang ,(tmtex (car l)))))

(define (tmtex-code-block s l)
  (set! l (escape-backslashes l))
  (set! l (escape-braces l))
  (set! s (car (string-decompose s "-")))
  (with lang (if (or (== s "verbatim") (== s "code")) '() `((!option ,s)))
    `((!begin* "tmcode" ,@lang) ,(tmtex-verbatim* "" l))))

(define (tmtex-add-preview-packages x)
  (cond ((list? x) (for-each tmtex-add-preview-packages x))
        ((nstring? x) (noop))
        ((string-occurs? "tikzpicture" x) (latex-add-extra "tikz"))))

(define (tmtex-mixed s l)
  (if (func? (cadr l) 'text) (set! l `("" ,(cadadr l))))
  ;; (set! l (unescape-angles l))
  ;; NOTE: instead, we now unescape in tmtex-verb-string
  (tmtex-env-set "mode" "text")
  (with src (list '!verbatim* (tmtex-tt (cadr l)))
    (tmtex-add-preview-packages src)
    (tmtex-env-reset "mode")
    (list '!unindent src)))

(define (tmtex-listing s l)
  (list (list '!begin "tmlisting") (tmtex (car l))))
  ;;(list (list '!begin "linenumbers") (tmtex (car l))))

(define (tmtex-minipage s l)
  (let*
    ((pos  (car l))
     (opt  (if (== pos "f") '() `((!option ,pos))))
     (size (cadr l))
     (body (caddr l)))
     `((!begin "minipage" ,@opt ,(tmtex-decode-length size)) ,(tmtex body))))

(define (tmtex-number-renderer l)
  (let ((r (cond ((string? l) l)
                 ((list? l) (tmtex-number-renderer (car l)))
                 (else ""))))
    (cond
      ((== r "alpha") "alph")
      ((== r "Alpha") "Alph")
      (else      r))))

(define (tmtex-number-counter l)
  (cond ((func? l 'value) (tmtex-number-counter (cdr l)))
        ((and (list? l) (== 1 (length l))) (tmtex-number-counter (car l)))
        ((symbol? l) (tmtex-number-counter (symbol->string l)))
        ((string? l) (if (string-ends? l "-nr") (string-drop-right l 3) l))
        (else "")))

(define (tmtex-number l)
  (tmtex-default
    (tmtex-number-renderer (cdr l))
    (list (tmtex-number-counter (car l)))))

(define (tmtex-change-case l)
  (cond
    ((== (cadr l) "UPCASE") (tex-apply 'MakeUppercase (tmtex (car l))))
    ((== (cadr l) "locase") (tex-apply 'MakeLowercase (tmtex (car l))))
    (else (tmtex (car l)))))

(define (tmtex-frame s l)
  `(fbox ,(car l)))

(define (tmtex-colored-frame s l)
  `(colorbox ,(tmtex-decode-color (car l)) ,(tmtex (cadr l))))

(define (tmtex-fcolorbox s l)
  `(fcolorbox ,@(map tmtex-decode-color (cDr l)) ,(tmtex (cAr l))))

(define (tmtex-translate s l)
  (let ((from (cadr l))
        (to   (caddr l))
        (body (car l)))
    (tmtex (translate-from-to body from to))))

(define (tmtex-localize s l)
  (with lan (if (list>0? tmtex-languages) (cAr tmtex-languages) "english")
    (tmtex `(translate ,(car l) "english" ,lan))))

(define (tmtex-render-key s l)
  (with body (tmtex (car l))
    (if (func? body '!concat)
      (set! body `(!append ,@(cdr body))))
  `(key ,body)))

(define (tmtex-key s l)
  (tmtex (tm->stree (tmdoc-key (car l)))))

(define (tmtex-key* s l)
  (tmtex (tm->stree (tmdoc-key* (car l)))))

(define (tmtex-padded-center s l)
  (list (list '!begin "center") (tmtex (car l))))

(define (tmtex-padded-left-aligned s l)
  (list (list '!begin "flushleft") (tmtex (car l))))

(define (tmtex-padded-right-aligned s l)
  (list (list '!begin "flushright") (tmtex (car l))))

(define (tmtex-compact s l)
  (list (list '!begin "tmcompact") (tmtex (car l))))

(define (tmtex-compressed s l)
  (list (list '!begin "tmcompressed") (tmtex (car l))))

(define (tmtex-amplified s l)
  (list (list '!begin "tmamplified") (tmtex (car l))))

(define (tmtex-indent s l)
  (list (list '!begin "tmindent") (tmtex (car l))))

(define (tmtex-jump-in s l)
  (list (list '!begin "tmjumpin") (tmtex (car l))))

(define (tmtex-script-inout s l)
  (let ((name  (string->symbol (string-append "tm" (string-replace s "-" ""))))
        (lang  (car l))
        (lang* (session-name (car l)))
        (in    (tmtex (caddr l)))
        (out   (tmtex (cadddr l))))
    `(,name ,lang ,lang* ,in ,out)))

(define (tmtex-converter s l)
  (let ((name  (string->symbol (string-append "tm" (string-replace s "-" ""))))
        (lang  (car l))
        (lang* (format-get-name (car l)))
        (in    (tmtex (cadr l)))
        (out   (tmtex (caddr l))))
    `(,name ,lang ,lang* ,in ,out)))

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

(define (tmtex-small s l)
  (tex-apply 'small (tmtex (car l))))

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

(define (tmtex-specific-language s l)
  (tmtex `(with "language" ,s ,(car l))))

(tm-define (tmtex-equation s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex (car l))))
    (tmtex-env-reset "mode")
    (if (== s "equation")
	(list (list '!begin s) r)
	(list '!eqn r))))

(define (tmtex-eqnarray s l)
  (tmtex-env-set "mode" "math")
  (let ((r (tmtex-table-apply (string->symbol s) '() (car l))))
    (tmtex-env-reset "mode")
    r))

(define (tmtex-math s l)
  (cond ((tm-in? (car l) '(equation equation* eqnarray eqnarray*))
         (tmtex (car l)))
        ((not (tm-func? (car l) 'document))
         (tmtex `(with "mode" "math" ,(car l))))
        ((tm-func? (car l) 'document 1)
         (tmtex `(math ,(cadr (car l)))))
        (else
          (with ps (map (lambda (x) `(math ,x)) (cdar l))
            (tmtex `(document ,@ps))))))

(define (tmtex-textual x)
  (tmtex-env-set "mode" "text")
  (with r (tmtex x)
    (tmtex-env-reset "mode")
    r))

(define (tmtex-text s l)
  (list 'text (tmtex-textual (car l))))

(define (tmtex-math-up s l)
  (list 'mathrm (tmtex-textual (car l))))

(define (tmtex-math-ss s l)
  (list 'mathsf (tmtex-textual (car l))))

(define (tmtex-math-tt s l)
  (list 'mathtt (tmtex-textual (car l))))

(define (tmtex-math-bf s l)
  (list 'mathbf (tmtex-textual (car l))))

(define (tmtex-math-sl s l)
  (list 'mathsl (tmtex-textual (car l))))

(define (tmtex-math-it s l)
  (list 'mathit (tmtex-textual (car l))))

(define (tmtex-mathord s l)
  (list 'mathord (tmtex (car l))))

(define (tmtex-mathbin s l)
  (list 'mathbin (tmtex (car l))))

(define (tmtex-mathrel s l)
  (list 'mathrel (tmtex (car l))))

(define (tmtex-mathopen s l)
  (list 'mathopen (tmtex (car l))))

(define (tmtex-mathclose s l)
  (list 'mathclose (tmtex (car l))))

(define (tmtex-mathpunct s l)
  (list 'mathpunct (tmtex (car l))))

(define (tmtex-mathop s l)
  (list 'mathop (tmtex (car l))))

(define (tmtex-syntax l)
  (tmtex (car l)))

(define (tmtex-theindex s l)
  (list 'printindex))

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

(tm-define (tmtex-biblio s l titled?)
  (if tmtex-indirect-bib?
      (tex-concat (list (list 'bibliographystyle (force-string (cadr l)))
			(list 'bibliography (force-string (caddr l)))))
      (let* ((doc (tmtex-bib-sub (cadddr l)))
	     (max (tmtex-bib-max doc))
             (tls tmtex-languages)
             (lan (or (and (pair? tls) (car tls)) "english"))
             (txt (translate-from-to "References" "english" lan))
             (bib (tmtex (list 'thebibliography max doc))))
        (if titled?
            `(!document (section* ,(tmtex txt)) ,bib)
            bib))))

(tm-define (tmtex-bib t)
  (tmtex-biblio (car t) (cdr t) #f))

(define (tmtex-thebibliography s l)
  (list (list '!begin s (car l)) (tmtex (cadr l))))

(define (tmtex-bibitem*-std s l)
  (cond ((= (length l) 1)
	 `(bibitem ,(car l)))
	((= (length l) 2)
	 `(bibitem (!option ,(tmtex (car l))) ,(cadr l)))
	(else
          (begin
	    (display* "TeXmacs] non converted bibitem content: "
                      (list s l) "\n")
            ""))))

(tm-define (tmtex-bibitem* s l)
  (tmtex-bibitem*-std s l))

(define (split-year s pos)
  (if (and (> pos 0)
           (string>=? (substring s (- pos 1) pos) "0")
           (string<=? (substring s (- pos 1) pos) "9"))
      (split-year s (- pos 1))
      pos))

(define (natbibify s)
  (let* ((pos  (split-year s (string-length s)))
         (auth (substring s 0 pos))
         (year (substring s pos (string-length s))))
    (when (== (string-length year) 2)
      (set! year (string-append (if (string>=? year "30") "19" "20") year)))
    (string-append auth "(" year ")")))

(tm-define (tmtex-bibitem* s l)
  (:mode natbib-package?)
  (if (and (== (length l) 2)
           (string? (cadr l))
           (not (string-occurs? "(" (cadr l))))
      (tmtex-bibitem*-std s (list (natbibify (cadr l)) (cadr l)))
      (tmtex-bibitem*-std s l)))

(define (tmtex-figure s l)
  (tmtex-float-sub #f "h" (cons (string->symbol s) l)))

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

(define (tmtex-frac* s l)
  (tex-concat (list (tmtex (car l)) "/" (tmtex (cadr l)))))

(define (tmtex-ornament-shape s)
  (if (== s "rounded") "1.7ex" "0pt"))

(define (assign-ornament-env l)
  (let* ((keys* (car  l))
         (val   (cadr l))
         (keys  (cDr keys*))
         (fun   (cAr keys*)))
    (apply string-append
           (list-intersperse
             (map (lambda (key)
                    (with arg (fun val)
                      (if (nstring? arg) ""
                        (string-append key "=" arg)))) keys) ","))))

(define (get-ornament-env)
  (let* ((l1  (ahash-set->list tmtex-env))
         (l21 (map (cut logic-ref tex-ornament-opts% <>) l1))
         (l22 (map (cut tmtex-env-get <>) l1))
         (l3  (map (lambda (x y) (if (and x y) (list x y) '())) l21 l22))
         (l4  (filter nnull? l3))
         (l5  (map assign-ornament-env l4)))
  (apply string-append (list-intersperse l5 ","))))

(define (tmtex-ornamented s l)
  (let* ((env     (string-append "tm" s))
         (option  (get-ornament-env))
         (option* (if (!= option "") `((!option ,option)) '())))
  `((!begin ,env ,@option*) ,(tmtex (car  l)))))

(logic-table tex-ornament-opts%
  ("padding-above"     ("skipabove" ,tmtex-decode-length))
  ("padding-below"     ("skipbelow" ,tmtex-decode-length))
  ("overlined-sep"     ("innertopmargin" ,tmtex-decode-length))
  ("underlined-sep"    ("innerbottommargin" ,tmtex-decode-length))
  ("framed-hsep"       ("innerleftmargin" "innerrightmargin"
                        ,tmtex-decode-length))
  ("framed-vsep"       ("innertopmargin"  "innerbottommargin"
                        ,tmtex-decode-length))
  ("ornament-vpadding" ("innertopmargin"  "innerbottommargin"
                        ,tmtex-decode-length))
  ("ornament-hpadding" ("innerleftmargin" "innerrightmargin"
                        ,tmtex-decode-length))
  ("ornament-color"    ("backgroundcolor" ,tmtex-decode-color))
  ("ornament-shape"    ("roundcorner" ,tmtex-ornament-shape)))

(define (tmtex-tm s l)
  (with tag (string->symbol (string-append "tm" (string-replace s "-" "")))
  `(,tag ,@(map tmtex l))))

(define (tmtex-input-math s l)
  (let ((tag (string->symbol (string-append "tm" (string-replace s "-" ""))))
        (a1  (tmtex (car l)))
        (a2  (with r (begin
                       (tmtex-env-set "mode" "math")
                       (tmtex (cadr l)))
               (tmtex-env-reset "mode") r)))
  (list tag a1 a2)))

(define (tmtex-fold-io-math s l)
  (let ((tag (string->symbol (string-append "tm" (string-replace s "-" ""))))
        (a1  (tmtex (car l)))
        (a2  (with r (begin
                       (tmtex-env-set "mode" "math")
                       (tmtex (cadr l)))
               (tmtex-env-reset "mode") r))
        (a3  (tmtex (caddr l))))
  (list tag a1 a2 a3)))

(define (tmtex-session s l)
  (let* ((tag (string->symbol (string-append "tm" (string-replace s "-" ""))))
         (arg (tmtex (car l)))
         (lan (tmtex (cadr l)))
         (lst (tmtex (caddr l))))
    (if (func? lst '!document)
      (set! lst `(!indent (!paragraph ,@(cdr lst)))))
    `(!document (,tag ,arg ,lan ,lst))))

(define (escape-backslashes-in-url l)
  (cond ((string? l) (string-replace l "\\" "\\\\"))
        ((symbol? l) l)
        (else (map escape-backslashes-in-url l))))

(define (tmtex-hyperref u)
  (tmtex-tt (escape-backslashes-in-url u)))

(define (tmtex-hlink s l)
  (list 'href (tmtex-hyperref (cadr l)) (tmtex (car l))))

(define (tmtex-href s l)
  (list 'url (tmtex-verb-string (car l))))

(define (tmtex-action s l)
  (list 'tmaction (tmtex (car l)) (tmtex (cadr l))))

(define (tmtex-choose s l)
  (list 'binom (tmtex (car l)) (tmtex (cadr l))))

(define (tmtex-text-tt s l)
  (if (tmtex-math-mode?)
      (tmtex-math-tt s l)
      (tmtex-modifier s l)))

(define (tmtex-modifier s l)
  (tex-apply (string->symbol (string-append "tm" s)) (tmtex (car l))))

(define (tmtex-render-line-number s l)
  (list 'tmlinenumber (tmtex (car l)) (tmtex-decode-length (tmtex (cadr l)))))

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
        ((nstring? (car l))
         (display* "TeXmacs] non converted citation: " (car l) "\n")
         (tmtex-cite-list (cdr l)))
	((null? (cdr l)) (car l))
	(else (string-append (car l) "," (tmtex-cite-list (cdr l))))))

(tm-define (tmtex-cite s l)
  (tex-apply 'cite (tmtex-cite-list l)))

(tm-define (tmtex-cite s l)
  (:mode natbib-package?)
  (tex-apply 'citep (tmtex-cite-list l)))

(define (tmtex-nocite s l)
  (tex-apply 'nocite (tmtex-cite-list l)))

(define (tmtex-cite-TeXmacs s l)
  (tex-apply 'citetexmacs (tmtex-cite-list l)))

(tm-define (tmtex-cite-detail s l)
  (with c (tmtex-cite-list (list (car l)))
    (tex-apply 'cite `(!option ,(tmtex (cadr l))) c)))

(tm-define (tmtex-cite-detail s l)
  (:mode natbib-package?)
  (with c (tmtex-cite-list (list (car l)))
    (tex-apply 'citetext `(!concat (citealp ,c) ", " ,(tmtex (cadr l))))))

(tm-define (tmtex-cite-detail-poor s l)
  (with c (tmtex-cite-list (list (car l)))
    `(!concat ,(tex-apply 'cite c) " (" ,(tmtex (cadr l)) ")")))

(define (tmtex-cite-detail-hook s l)
  (tmtex-cite-detail s l))

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

(define (tmtex-natbib-triple s l)
  `(protect (citeauthoryear ,@(map tmtex l))))

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

(define (tmtex-glossary-line t)
  (with r (tmtex t)
    (if (func? r 'glossaryentry) r
        `(listpart ,r))))

(define (tmtex-glossary-body b)
  (if (not (tm-func? b 'document))
      (tmtex b)
      (cons '!document (map-in-order tmtex-glossary-line (cdr b)))))

(define (tmtex-the-glossary s l)
  `(!document
      (,(if (latex-book-style?) 'chapter* 'section*) "Glossary")
      ((!begin "theglossary" ,(car l)) ,(tmtex-glossary-body (cadr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-apply key args)
  (let ((n (length args))
        (r (or (ahash-ref tmtex-dynamic key) (logic-ref tmtex-methods% key))))
    (if (in? key '(quote quasiquote unquote)) (set! r tmtex-noop))
    (cond ((== r 'environment)
           (tmtex-std-env (symbol->string key) args))
          (r (r args))
          (else
            (let ((p (logic-ref tmtex-tmstyle% key)))
              (cond ((and p (or (= (cadr p) -1) (= (cadr p) n)))
                     ((car p) (symbol->string key) args))
                    ((and p (= (cadr p) -2)) ((car p) `(,key ,@args)))
                    ((and (= n 1)
                          (or (func? (car args) 'tformat)
                              (func? (car args) 'table)))
                     (tmtex-table-apply key '() (car args)))
                    ((and (= n 2)
                          (or (func? (cAr args) 'tformat)
                              (func? (cAr args) 'table)))
                     (tmtex-table-apply key (cDr args) (cAr args)))
                    (else (tmtex-function key args))))))))

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
  (cond ((string? x) (tmtex-string x))
        ((list>0? x) (tmtex-apply (car x) (cdr x)))
        (else "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-dispatcher tmtex-primitives%
  ((:or unknown uninit error raw-data) tmtex-error)
  (document tmtex-document)
  (para tmtex-para)
  (surround tmtex-surround)
  (concat tmtex-concat)
  (rigid tmtex-rigid)
  (hgroup tmtex-rigid)
  (vgroup tmtex-id)
  (hidden tmtex-noop)
  (hspace tmtex-hspace)
  (vspace* tmtex-noop)
  (vspace tmtex-vspace)
  (space tmtex-space)
  (htab tmtex-htab)
  (move tmtex-first)
  (shift tmtex-first)
  (resize tmtex-first)
  (clipped tmtex-first)
  (repeat tmtex-noop)
  (float tmtex-float)
  (datoms tmtex-second)
  ((:or dlines dpages dbox) tmtex-noop)
  (line-note tmtex-line-note)

  (with-limits tmtex-noop)
  (line-break tmtex-line-break)
  (new-line tmtex-new-line)
  (next-line tmtex-next-line)
  (emdash tmtex-emdash)
  (no-break tmtex-no-break)
  (no-indent tmtex-no-first-indentation)
  (yes-indent tmtex-noop)
  (no-indent* tmtex-noop)
  (yes-indent* tmtex-noop)
  (page-break* tmtex-noop)
  (page-break tmtex-page-break)
  (no-page-break* tmtex-noop)
  (no-page-break tmtex-no-page-break)
  (no-break-here* tmtex-noop)
  (no-break-here tmtex-no-page-break)
  (no-break-start tmtex-no-page-break)
  (no-break-end tmtex-noop)
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
  (long-arrow tmtex-long-arrow)
  (lprime tmtex-lsup)
  (rprime tmtex-rsup)
  (below tmtex-below)
  (above tmtex-above)
  (lsub tmtex-lsub)
  (lsup tmtex-lsup)
  (rsub tmtex-rsub)
  (rsup tmtex-rsup)
  (modulo tmtex-modulo)
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
  (with tmtex-with-wrapped)
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
  (syntax tmtex-syntax)

  ((:or or xor and not plus minus times over div mod
	merge length range find-file
	is-tuple look-up
	equal unequal less lesseq greater greatereq) tmtex-noop)

  (number tmtex-number)
  (change-case tmtex-change-case)
  (date tmtex-date)

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
  (set-binding tmtex-noop)
  (get-binding tmtex-noop)
  (hidden-binding tmtex-hidden-binding)
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
  ((:or gr-group gr-transform
	text-at cline arc carc spline spine* cspline fill) tmtex-noop)
  (image tmtex-image)
  ((:or box-info frame-direct frame-inverse) tmtex-noop)

  ((:or format line-sep split delay hold release
	old-matrix old-table old-mosaic old-mosaic-item
	set reset expand expand* hide-expand display-baloon
	apply begin end func env) tmtex-noop)

  (shown tmtex-id)
  (mtm tmtex-mtm)
  (!file tmtex-file)
  (!arg tmtex-tex-arg))


(logic-dispatcher tmtex-extra-methods%
  (wide-float tmtex-wide-float)
  (phantom-float tmtex-noop)
  ((:or marginal-note marginal-normal-note) tmtex-marginal-note)
  ((:or marginal-left-note marginal-even-left-note) tmtex-marginal-left-note)
  ((:or marginal-right-note marginal-even-right-note)tmtex-marginal-right-note)
  (!ilx tmtex-ilx))

(logic-rules
  ((tmtex-methods% 'x 'y) (tmtex-primitives% 'x 'y))
  ((tmtex-methods% 'x 'y) (tmtex-extra-methods% 'x 'y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion of all macros which are not recognized by LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-table tmtex-tmstyle%
  ((:or section subsection subsubsection paragraph subparagraph part chapter)
   (,tmtex-sectional 1))
  ((:or hide-preamble show-preamble) (,tmtex-default -1))
  (hide-part (,tmtex-hide-part -1))
  (show-part (,tmtex-show-part -1))
  ((:or doc-title-options author-data) (,tmtex-default -1))
  (appendix (,tmtex-appendix 1))
  (appendix* (,tmtex-appendix* 1))
  ((:or theorem proposition lemma corollary proof axiom definition
	notation conjecture remark note example convention warning
        acknowledgments
        exercise problem question solution answer
        quote-env quotation verse
        theorem* proposition* lemma* corollary* axiom* definition*
	notation* conjecture* remark* note* example* convention* warning*
        acknowledgments*
        exercise* problem* question* solution* answer*)
   (,tmtex-enunciation 1))
  (new-theorem (,tmtex-new-theorem 2))
  (verbatim (,tmtex-verbatim 1))
  (padded-center (,tmtex-padded-center 1))
  (padded-left-aligned (,tmtex-padded-left-aligned 1))
  (padded-right-aligned (,tmtex-padded-right-aligned 1))
  (compact (,tmtex-compact 1))
  (compressed (,tmtex-compressed 1))
  (amplified (,tmtex-amplified 1))
  (indent (,tmtex-indent 1))
  (jump-in (,tmtex-jump-in 1))
  (algorithm-indent (,tmtex-indent 1))
  ((:or footnote wide-footnote) (,tmtex-footnote 1))
  (footnotemark (,tmtex-default 0))
  (footnotemark* (,tmtex-footnotemark 1))
  ((:or description description-compact description-aligned
	description-dash description-long description-paragraphs
	itemize itemize-minus itemize-dot itemize-arrow
	enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   (,tmtex-list-env 1))
  ((:or folded unfolded folded-plain unfolded-plain folded-std unfolded-std
        folded-explain unfolded-explain folded-env unfolded-env
        folded-documentation unfolded-documentation folded-grouped
        unfolded-grouped summarized detailed summarized-plain summarized-std
        summarized-env summarized-documentation summarized-grouped
        summarized-raw summarized-tiny detailed-plain detailed-std detailed-env
        detailed-documentation detailed-grouped detailed-raw detailed-tiny
        unfolded-subsession folded-subsession folded-io unfolded-io
        input output errput timing)
   (,tmtex-tm -1))
  ((:or padded underlined overlined bothlined framed ornamented)
   (,tmtex-ornamented 1))
  ((:or folded-io-math unfolded-io-math) (,tmtex-fold-io-math 3))
  (input-math (,tmtex-input-math 2))
  (session (,tmtex-session 3))
  ((:or converter-input converter-output) (,tmtex-converter 3))
  ((:or script-input script-output) (,tmtex-script-inout 4))
  (really-tiny (,tmtex-tiny 1))
  (very-tiny (,tmtex-tiny 1))
  (tiny (,tmtex-tiny 1))
  (really-small (,tmtex-scriptsize 1))
  (very-small (,tmtex-scriptsize 1))
  (smaller (,tmtex-footnotesize 1))
  (small (,tmtex-small 1))
  (flat-size (,tmtex-small 1))
  (normal-size (,tmtex-normalsize 1))
  (sharp-size (,tmtex-large 1))
  (large (,tmtex-large 1))
  (larger (,tmtex-Large 1))
  (very-large (,tmtex-LARGE 1))
  (really-large (,tmtex-LARGE 1))
  (really-huge (,tmtex-Huge 1))
  ((:or british bulgarian chinese croatian czech danish dutch english
	esperanto finnish french german greek hungarian italian japanese
	korean polish portuguese romanian russian slovak slovene spanish
	swedish taiwanese ukrainian)
   (,tmtex-specific-language 1))

  (math (,tmtex-math 1))
  (text (,tmtex-text 1))
  (math-up (,tmtex-math-up 1))
  (math-ss (,tmtex-math-ss 1))
  (math-tt (,tmtex-math-tt 1))
  (math-bf (,tmtex-math-bf 1))
  (math-sl (,tmtex-math-sl 1))
  (math-it (,tmtex-math-it 1))
  (math-separator (,tmtex-mathpunct 1))
  (math-quantifier (,tmtex-mathord 1))
  (math-imply (,tmtex-mathbin 1))
  (math-or (,tmtex-mathbin 1))
  (math-and (,tmtex-mathbin 1))
  (math-not (,tmtex-mathord 1))
  (math-relation (,tmtex-mathrel 1))
  (math-union (,tmtex-mathbin 1))
  (math-intersection (,tmtex-mathbin 1))
  (math-exclude (,tmtex-mathbin 1))
  (math-plus (,tmtex-mathbin 1))
  (math-minus (,tmtex-mathbin 1))
  (math-times (,tmtex-mathbin 1))
  (math-over (,tmtex-mathbin 1))
  (math-big (,tmtex-mathop 1))
  (math-prefix (,tmtex-mathord 1))
  (math-postfix (,tmtex-mathord 1))
  (math-open (,tmtex-mathopen 1))
  (math-close (,tmtex-mathclose 1))
  (math-ordinary (,tmtex-mathord 1))
  (math-ignore (,tmtex-mathord 1))
  ((:or eqnarray eqnarray* leqnarray*
        gather multline gather* multline* align
        flalign alignat align* flalign* alignat*) (,tmtex-eqnarray 1))

  (eq-number (,tmtex-default -1))
  (application-space (,tmtex-hspace* 1))

  ((:or code cpp-code mmx-code scm-code shell-code scilab-code verbatim-code)
   (,tmtex-code-block 1))
  ((:or mmx cpp scm shell scilab) (,tmtex-code-inline 1))

  (frame    (,tmtex-frame 1))
  (colored-frame (,tmtex-colored-frame 2))
  (fcolorbox (,tmtex-fcolorbox 3))
  (condensed (,tmtex-style-first 1))
  (translate (,tmtex-translate 3))
  (localize (,tmtex-localize 1))
  (render-key (,tmtex-render-key 1))
  (key  (,tmtex-key 1))
  (key* (,tmtex-key* 1))
  (minipage (,tmtex-minipage 3))
  (latex_preview (,tmtex-mixed 2))
  (picture-mixed (,tmtex-mixed 2))
  (source-mixed (,tmtex-mixed 2))
  (listing (,tmtex-listing 1))
  (draw-over (,tmtex-make-eps 3))
  (draw-under (,tmtex-make-eps 3))
  (version-old (,tmtex-style-first 2))
  (version-both (,tmtex-style-second 2))
  (version-new (,tmtex-style-second 2))
  (the-index (,tmtex-theindex -1))
  (glossary (,tmtex-glossary 1))
  (glossary-explain (,tmtex-glossary 2))
  (glossary-2 (,tmtex-glossary-entry 3))
  (the-glossary (,tmtex-the-glossary 2))
  ((:or table-of-contents) (,tmtex-toc 2))
  (thebibliography (,tmtex-thebibliography 2))
  (bib-list (,tmtex-style-second 2))
  (bibitem* (,tmtex-bibitem* -1))
  ((:or small-figure big-figure small-table big-table) (,tmtex-figure 2))
  (item (,tmtex-item 0))
  (item* (,tmtex-item-arg 1))
  (render-proof (,tmtex-render-proof 2))
  (nbsp (,tmtex-nbsp 0))
  (nbhyph (,tmtex-nbhyph 0))
  (hrule (,tmtex-hrule 0))
  (frac* (,tmtex-frac* 2))
  (hlink (,tmtex-hlink 2))
  (action (,tmtex-action -1))
  (href (,tmtex-href 1))
  (slink (,tmtex-href 1))
  (eqref (,tmtex-eqref 1))
  (smart-ref (,tmtex-smart-ref -1))
  (choose (,tmtex-choose 2))
  (tt (,tmtex-text-tt 1))
  ((:or strong em name samp abbr dfn kbd var acronym person)
   (,tmtex-modifier 1))
  (render-line-number (,tmtex-render-line-number 2))
  (menu (,tmtex-menu -1))
  (with-TeXmacs-text (,(tmtex-rename 'withTeXmacstext) 0))
  (made-by-TeXmacs (,(tmtex-rename 'madebyTeXmacs) 0))
  (cite-website (,(tmtex-rename 'citewebsite) 0))
  (tm-made (,(tmtex-rename 'tmmade) 0))
  (cite (,tmtex-cite -1))
  (nocite (,tmtex-nocite -1))
  (cite-TeXmacs (,tmtex-cite-TeXmacs -1))
  (cite-detail (,tmtex-cite-detail-hook 2))
  (cite-raw (,tmtex-cite-raw -1))
  (cite-raw* (,tmtex-cite-raw* -1))
  (cite-textual (,tmtex-cite-textual -1))
  (cite-textual* (,tmtex-cite-textual* -1))
  (cite-parenthesized (,tmtex-cite-parenthesized -1))
  (cite-parenthesized* (,tmtex-cite-parenthesized* -1))
  (render-cite (,tmtex-render-cite 1))
  ((:or cite-author cite-author-link) (,tmtex-cite-author 1))
  ((:or cite-author* cite-author*-link) (,tmtex-cite-author* 1))
  ((:or cite-year cite-year-link) (,tmtex-cite-year 1))
  (natbib-triple (,tmtex-natbib-triple 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags which are customized in particular style files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-dependent-declare x)
  (with (tag fun narg) x
    (with fun+bis (symbol-append fun '+bis)
      (if (== narg 2)
        `(begin
           (when (not (defined? ',fun))
             (tm-define (,fun s l) (tmtex-function (string->symbol s) l)))
           (when (not (defined? ',fun+bis))
             (tm-define (,fun+bis s l) (,fun s l))))
        `(begin
           (when (not (defined? ',fun))
             (tm-define (,fun t)
               (tmtex-function (string->symbol (car t)) (cdr t))))
           (when (not (defined? ',fun+bis))
             (tm-define (,fun+bis s l)
               (,fun (append (list (string->symbol s)) l)))))))))

(tm-define (style-dependent-transform x)
  (with (tag fun narg) x
    (with fun+bis (symbol-append fun '+bis)
      `(,tag (,(list 'unquote fun+bis) -1)))))

(define-macro (tmtex-style-dependent . l)
  `(begin
     ,@(map style-dependent-declare l)
     (logic-table tmtex-tmstyle% ,@(map style-dependent-transform l))))

(tmtex-style-dependent
  ;; to be removed
  (doc-data                 tmtex-doc-data 2)
  (abstract-data            tmtex-abstract-data 2)
  ;; abstract markup
  (abstract                 tmtex-abstract 1)
  (abstract-acm             tmtex-abstract-acm 1)
  (abstract-arxiv           tmtex-abstract-arxiv 1)
  (abstract-msc             tmtex-abstract-msc 1)
  (abstract-pacs            tmtex-abstract-pacs 1)
  (abstract-keywords        tmtex-abstract-keywords 1)
  ;; metadata markup
  (doc-title                tmtex-doc-title 1)
  (doc-running-title        tmtex-doc-running-title 1)
  (doc-subtitle             tmtex-doc-subtitle 1)
  (doc-note                 tmtex-doc-note 1)
  (doc-misc                 tmtex-doc-misc 1)
  (doc-date                 tmtex-doc-date 1)
  (doc-running-author       tmtex-doc-running-author 1)
  (doc-author               tmtex-doc-author 1)
  (author-name              tmtex-author-name 1)
  (author-affiliation       tmtex-author-affiliation 1)
  (author-misc              tmtex-author-misc 1)
  (author-note              tmtex-author-note 1)
  (author-email             tmtex-author-email 1)
  (author-homepage          tmtex-author-homepage 1)
  ;; references
  (doc-subtitle-ref         tmtex-doc-subtitle-ref 2)
  (doc-date-ref             tmtex-doc-date-ref 2)
  (doc-note-ref             tmtex-doc-note-ref 2)
  (doc-misc-ref             tmtex-doc-misc-ref 2)
  (author-affiliation-ref   tmtex-author-affiliation-ref 2)
  (author-email-ref         tmtex-author-email-ref 2)
  (author-homepage-ref      tmtex-author-homepage-ref 2)
  (author-note-ref          tmtex-author-note-ref 2)
  (author-misc-ref          tmtex-author-misc-ref 2)
  ;; labels
  (doc-subtitle-label       tmtex-doc-subtitle-label 2)
  (doc-date-label           tmtex-doc-date-label 2)
  (doc-note-label           tmtex-doc-note-label 2)
  (doc-misc-label           tmtex-doc-misc-label 2)
  (author-affiliation-label tmtex-author-affiliation-label 2)
  (author-email-label       tmtex-author-email-label 2)
  (author-homepage-label    tmtex-author-homepage-label 2)
  (author-note-label        tmtex-author-note-label 2)
  (author-misc-label        tmtex-author-misc-label 2)
  ;; misc
  ((:or equation equation*) tmtex-equation 2)
  (bibliography             tmtex-bib 4)
  (elsevier-frontmatter     tmtex-elsevier-frontmatter 2)
  (conferenceinfo           tmtex-acm-conferenceinfo 2)
  (CopyrightYear            tmtex-acm-copyright-year 2)
  (slide                    tmtex-beamer-slide 2)
  (tit                      tmtex-beamer-tit 2)
  (crdata                   tmtex-acm-crdata 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protected tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-group tmtex-protected%
  a b c d i j k l o r t u v H L O P S
  aa ae bf cr dh dj dp em fi ge gg ht if in it le lg ll lu lq mp mu
  ne ng ni nu oe or pi pm rm rq sb sc sf sl sp ss th to tt wd wp wr xi
  AA AE DH DJ Im NG OE Pi Pr Re SS TH Xi)

(logic-group tmtex-protected-symbol%
  space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expansion of all macros which are not recognized by LaTeX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmtex-user-defs-table (make-ahash-table))

(define (user-definition? x)
  (or (and (func? x 'new-theorem 2) (string? (cadr x)))
      (and (func? x 'assign 2) (string? (cadr x)))))

(define (collect-user-defs-sub t)
  (cond ((npair? t) (noop))
	((user-definition? t)
	 (ahash-set! tmtex-user-defs-table (string->symbol (cadr t)) #t))
	(else (for-each collect-user-defs-sub (cdr t)))))

(define (collect-user-defs t)
  (set! tmtex-user-defs-table (make-ahash-table))
  (collect-user-defs-sub (cons 'document (tmtex-filter-preamble t)))
  (ahash-set->list tmtex-user-defs-table))

(define (as-string sym)
  (with s (symbol->string sym)
    (if (string-starts? s "begin-")
	(substring s 6 (string-length s))
	s)))

(define (logic-first-list name)
  (let* ((l1 (query (cons name '('first 'second))))
	 (l2 (map (cut assoc-ref <> 'first) l1)))
    (map as-string l2)))

(define (collect-user-macros-in t h)
  (when (tm-compound? t)
    (when (tree-label-extension? (tm-label t))
      (ahash-set! h (symbol->string (tm-label t)) #t))
    (for-each (cut collect-user-macros-in <> h) (tm-children t))))

(define (collect-user-macros t)
  (with h (make-ahash-table)
    (collect-user-macros-in t h)
    (ahash-set->list h)))

(define (tmtex-env-macro name)
  `(associate ,name (xmacro "x" (eval-args "x"))))

(define tmtex-always-expand
  ;; FIXME: find a cleaner way to handle these environments
  (list "render-theorem" "render-remark" "render-exercise" "render-proof"
        "algorithm" "algorithm*" "named-algorithm" "named-algorithm-old"
        "specified-algorithm" "specified-algorithm*"
        "named-specified-algorithm" "algorithm-body" "numbered"

        "short-item" "short-question"
        "question-arabic" "question-alpha" "question-Alpha"
        "question-roman" "question-Roman" "question-item"
        "answer-arabic" "answer-alpha" "answer-Alpha"
        "answer-roman" "answer-Roman" "answer-item"

        "gap" "gap-dots" "gap-underlined" "gap-box"
        "gap-wide" "gap-dots-wide" "gap-underlined-wide" "gap-box-wide"
        "gap-long" "gap-dots-long" "gap-underlined-long" "gap-box-long"

        "with-button-box" "with-button-box*"
        "with-button-circle" "with-button-circle*"
        "with-button-arabic" "with-button-alpha" "with-button-Alpha"
        "with-button-roman" "with-button-Roman"
        "mc-field" "mc-wide-field" "show-reply" "hide-reply"
        "mc" "mc-monospaced" "mc-horizontal" "mc-vertical"))

(tm-define (tmtex-env-patch t l0)
  (let* ((st (tree->stree t))
         (l0 (logic-first-list 'tmtex-primitives%))
         (l1 (logic-first-list 'tmtex-extra-methods%))
	 (l2 (logic-first-list 'tmtex-tmstyle%))
	 (l3 (map as-string (logic-apply-list '(latex-tag%))))
	 (l4 (map as-string (logic-apply-list '(latex-symbol%))))
	 (l5 (list-difference l3 (list-union l4 tmtex-always-expand)))
	 (l6 (map as-string (collect-user-defs st)))
	 (l7 (if (preference-on? "texmacs->latex:expand-user-macros") '() l6))
         (l8 (list-difference (collect-user-macros st)
                              (list-union l0 l6 tmtex-always-expand)))
	 (l9 (list-difference (list-union l1 l2 l5 l7 l8) l0))
         (l10 (list-filter l0 (lambda (s) (and (string? s)
                                               (<= (string-length s) 2)))))
         (l11 (list-difference l10 (list "tt" "em" "op")))
         (l12 (list-difference l9 l11)))
    `(collection ,@(map tmtex-env-macro l12))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtex-get-style sty)
  (cond ((not sty) (set! sty (list "article")))
        ((string? sty) (set! sty (list sty)))
        ((func? sty 'tuple) (set! sty (cdr sty)))
        ((null? sty) (set! sty '("article"))))
  sty)

(tm-define (tmtex-postprocess x) x)
(tm-define (tmtex-postprocess-body x) x)

(tm-define (texmacs->latex x opts)
  ;;(display* "texmacs->latex [" opts "], " x "\n")
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
             (style (tmtex-get-style (tmfile-extract x 'style)))
             (main-style (or (tmtex-transform-style (car style)) "article"))
             (lan (tmfile-language x))
             (init (tmfile-extract x 'initial))
             (att (tmfile-extract x 'attachments))
             (doc (list '!file body style lan init att
                        (url->string (get-texmacs-path)))))
        (set! tmtex-cjk-document?
              (in? lan '("chinese" "taiwanese" "japanese" "korean")))
        (latex-set-style main-style)
        (latex-set-packages '())
        (latex-set-extra '())
        (set! tmtex-style (car style))
        (set! tmtex-packages (cdr style))
        (set! tmtex-languages (list lan))
        (set! tmtex-colors '())
        (set! tmtex-colormaps '())
        (import-tmtex-styles)
        (tmtex-style-init body)
        (set! doc (tmtex-style-preprocess doc))
        (with result (tmtex-postprocess (texmacs->latex doc opts))
          (set! tmtex-style "generic")
          (set! tmtex-packages '())
          result))
      (let* ((x2 (tree->stree (tmtm-eqnumber->nonumber (stree->tree x))))
             (x3 (tmtm-match-brackets x2)))
        (tmtex-initialize opts)
        (with r (tmtex (tmpre-produce x3))
          (if tmtex-mathjax?
              (set! r (latex-mathjax-pre r)))
          (if (not tmtex-use-macros?)
              (set! r (latex-expand-macros r)))
          (if tmtex-mathjax?
              (set! r (latex-mathjax r)))
          r))))
