
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathtm.scm
;; DESCRIPTION : conversion of MathML trees to TeXmacs trees
;; COPYRIGHT   : (C) 2002, 2005 Joris van der Hoeven and David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -- MathML notes --
;;
;; . Linking is defined by XLink.
;; . mtr and mtd may be inferred for compat with MathML1
;; . transfinite symbols are U+2135-U+2138
;; . how to render empty tokens?
;; . how to render whitespace in tokens?

;; . mathvariant affect comparison of chars corresponding to SMP conterparts

;; . slanting rules of <mi> match rules of texmacs
;; . <mi>...</mi> might be rendered as ldots, check if it is a MathML char
;; . only a very reduced subset of <mn> can be natively supported
;; . <mo> containing text cannot be supported
;; . nesting of <mrow> can denote range of delimiters in <mo>
;; . texmacs cannot do \widearrow (script under extensible arrow)
;; . <mo> containing text may be rendered as nested text delimited by spaces
;;   (see 3.2.6.4)

;; !!! Check semantic of mathvariant on multiple symbols !!!

(texmacs-module (convert mathml mathtm)
  (:use (convert tools tmtable)
	(convert tools sxml)
	(convert tools xmltm)
	(convert mathml mathml-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-math env a c)
  `((with "mode" "math" ,(mathtm-args-serial env c))))

(define (mathtm-none env a c)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-string env s)
  ;; FIXME: use translators or parser for this!!!
  ;; TODO: learn when the trailing ';' is optional
  (cond ((drd-ref mathml-symbol->tm% s) => identity)
	(else (xmltm-text s))))

(define (mathtm-mo env a c)
  (cond ((null? c) '())
	((drd-ref mathml-left->tm% (car c)) => (lambda (x) `((left ,x))))
	((drd-ref mathml-right->tm% (car c)) => (lambda (x) `((right ,x))))
	((drd-ref mathml-big->tm% (car c)) => (lambda (x) `((big ,x))))
	(else (list (mathtm-args-serial env c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple mathematical constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-mfrac env a c)
  (if (== (length c) 2)
      `((frac
	 ,(mathtm-as-serial env (first c))
	 ,(mathtm-as-serial env (second c))))
      (mathtm-error "bad mfrac")))

(define (mathtm-msqrt env a c)
  (if (== (length c) 1)
      `((sqrt ,(mathtm-as-serial env (first c))))
      (mathtm-error "bad msqrt")))

(define (mathtm-mroot env a c)
  (if (== (length c) 2)
      `((sqrt
	 ,(mathtm-as-serial env (first c))
	 ,(mathtm-as-serial env (second c))))
      (mathtm-error "bad mroot")))

(define (mathtm-error message)
  `((with "color" "red" ,message)))

(define (mathtm-merror env a c)
  (matthtm-error (mathtm-mrow env a c)))

(define (mathtm-mtext env a c)
  `((with "mode" "text" ,(mathtm-args-serial env c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-prime-sub? s)
  (or (== s "'") (== s "`") (== s "<dagger>") ))

(define (mathtm-prime? s)
  (and (string? s)
       (list-every mathtm-prime-sub? (tmconcat-tokenize-math s))))

(define (mathtm-superscript type1 type2 sup)
  (if (mathtm-prime? sup)
      (list type2 sup)
      (list type1 sup)))

(define (mathtm-scripts base lsub lsup rsub rsup)
  (if lsub (set! base (cons `(lsub ,lsub) base)))
  (if lsup (set! base (cons (mathtm-superscript `lsup `lprime lsup) base)))
  (if rsub (set! base (rcons base `(rsub ,rsub))))
  (if rsup (set! base (rcons base (mathtm-superscript `rsup `rprime rsup))))
  base)

(define (mathtm-msub env a c)
  (if (== (length c) 2)
      (let ((base (mathtm env (first c)))
	    (sub (mathtm-as-serial env (second c))))
	(mathtm-scripts base #f #f sub #f))
      (mathtm-error "bad msub")))

(define (mathtm-msup env a c)
  (if (== (length c) 2)
      (let ((base (mathtm env (first c)))
	    (sup (mathtm-as-serial env (second c))))
	(mathtm-scripts base #f #f #f sup))
      (mathtm-error "bad msup")))

(define (mathtm-msubsup env a c)
  (if (== (length c) 3)
      (let ((base (mathtm env (first c)))
	    (sub (mathtm-as-serial env (second c)))
	    (sup (mathtm-as-serial env (third c))))
	(mathtm-scripts base #f #f sub sup))
      (mathtm-error "bad msubsup")))

(define (mathtm-mmultiscripts-sub env l right?)
  (cond ((or (null? l) (null? (cdr l))) (values '() '() '() '()))
	((or (func? (car l) 'mprescripts) (func? (car l) 'm:mprescripts))
	 (mathtm-mmultiscripts-sub env (cdr l) #f))
	(else (receive (lsub lsup rsub rsup)
		  (mathtm-mmultiscripts-sub env (cddr l) right?)
		(let ((sub (mathtm env (car l)))
		      (sup (mathtm env (cadr l))))
		  (if right?
		      (values lsub lsup
			      (append rsub sub) (append rsup sup))
		      (values (append sub lsub)
			      (append sup lsup) rsub rsup)))))))

(define (mathtm-multiscript env l)
  (if (null? l) #f
      (mathtm-serial env l)))

(define (mathtm-mmultiscripts env a c)
  (if (> (length c) 0)
      (with base (mathtm env (first c))
	(receive (lsub lsup rsub rsup)
	    (mathtm-mmultiscripts-sub env (cdr c) #t)
	  (mathtm-scripts base
			  (mathtm-multiscript env lsub)
			  (mathtm-multiscript env lsup)
			  (mathtm-multiscript env rsub)
			  (mathtm-multiscript env rsup))))
      (mathtm-error "bad mmultiscripts")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Under and over scripts and wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-below base sub)
  (cond ((drd-ref mathml-below->tm% sub) =>
	 (lambda (x) `((wide* ,base ,x))))
	(else `((below ,base ,sub)))))

(define (mathtm-above base sup)
  (cond ((drd-ref mathml-above->tm% sup) =>
	 (lambda (x) `((wide ,base ,x))))
	(else `((above ,base ,sup)))))

(define (mathtm-munder env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c))))
	(mathtm-below base sub))
      (mathtm-error "bad munder")))

(define (mathtm-mover env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sup (mathtm-as-serial env (second c))))
	(mathtm-above base sup))
      (mathtm-error "bad mover")))

(define (mathtm-munderover env a c)
  (if (== (length c) 3)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c)))
	    (sup (mathtm-as-serial env (third c))))
	(mathtm-above (car (mathtm-below base sub)) sup))
      (mathtm-error "bad munderover")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathml-func? x y)
  (and (list? x)
       (or (== (car x) y)
	   (== (car x) (symbol-append 'm:' y)))))

(define (mathml-func-in? x l)
  (list-or (map (cut mathml-func? x <>) l)))

(define (mathtm-mtd env a c)
  `((cell ,(mathtm-serial env (mathtm-pass env a c)))))

(define (mathtm-mtr env a c)
  (let* ((cell? (lambda (x) (mathml-func? x 'mtd)))
	 (c2 (map (lambda (x) (if (cell? x) x `(m:mtd ,x))) c))
	 (l (map (cut mathtm-as-serial env <>) c)))
    `((row ,@l))))

(define (mathtm-mlabeledtr env a c)
  ;; FIXME: label is still ignored
  (if (null? c) '((row))
      (mathtm-mtr env a (cdr c))))

(define (mathtm-mtable env a c)
  ;; TODO: rows of unequal lengths
  (let* ((row? (lambda (x) (mathml-func-in? x '(mtr mlabeledtr))))
	 (c2 (map (lambda (x) (if (row? x) x `(m:mtr ,x))) c))
	 (l (map (cut mathtm-as-serial env <>) c)))
    `((tformat (table ,@l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-drop env a c) '())

(define (mathtm-pass env a c)
  ;; TODO: consolidate with htmltm-pass
  (let ((l (mathtm-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate a 'id (mathtm-serial env l))))))

(define (mathtm-args env l)
  ;; TODO: consolidate with htmltm-args
  (append-map (lambda (x) (mathtm env x)) l))

(define (mathtm-args-serial env l)
  ;; FIXME: mathml must not be affected by xml:space (html <pre> mode)
  (mathtm-serial env (mathtm-args env l)))

(define (mathtm env t)
  (sxml-dispatch (lambda (env t) (list (mathtm-string env t)))
		 mathtm-pass env t))

(tm-define mathtm-as-serial
  ;; TODO: replace #f by 'environment' object, see htmltm-as-serial
  (case-lambda
    ((t) (mathtm-as-serial #f t))
    ((env t) (mathtm-serial env (mathtm env t)))))

(drd-dispatcher mathtm-methods%
  ;;; Interface
  (math (mathtm-handler :element mathtm-math))
  (none (mathtm-handler :mixed mathtm-none))
  ;;; Presentation
  ;; Token
  ;; presentation tokens contain CDATA, MathML entities, align marks, or glyphs
  (mi (mathtm-handler :mixed mathtm-pass))
  (mn (mathtm-handler :mixed mathtm-pass))
  (mo (mathtm-handler :mixed mathtm-mo))
  (mtext (mathtm-handler :mixed mathtm-mtext))
  (mspace (mathtm-handler :mixed mathtm-drop))
  (ms (mathtm-handler :mixed mathtm-mtext))
  (mglyph (mathtm-handler :empty mathtm-drop))
  ;; General layout
  (mrow (mathtm-handler :element mathtm-pass))
  (mfrac (mathtm-handler :element mathtm-mfrac))
  (msqrt (mathtm-handler :element mathtm-msqrt))
  (mroot (mathtm-handler :element mathtm-mroot))
  (mstyle (mathtm-handler :element mathtm-pass))
  (merror (mathtm-handler :element mathtm-merror))
  (mpadded (mathtm-handler :element mathtm-pass))
  (mphantom (mathtm-handler :element mathtm-pass))
  (mfenced (mathtm-handler :element mathtm-pass))
  (menclose (mathtm-handler :element mathtm-pass))
  ;; Script and limits
  (msub (mathtm-handler :element mathtm-msub))
  (msup (mathtm-handler :element mathtm-msup))
  (msubsup (mathtm-handler :element mathtm-msubsup))
  (munder (mathtm-handler :element mathtm-munder))
  (mover (mathtm-handler :element mathtm-mover))
  (munderover (mathtm-handler :element mathtm-munderover))
  (mmultiscripts (mathtm-handler :element mathtm-mmultiscripts))
  ;; Tables
  (mtable (mathtm-handler :element mathtm-mtable))
  (mtr (mathtm-handler :element mathtm-mtr))
  (mlabeledtr (mathtm-handler :element mathtm-mlabeledtr))
  (mtd (mathtm-handler :element mathtm-mtd))
  ;; Actions
  (maction (mathtm-handler :element mathtm-pass)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mathml->tree s)
  (:synopsis "Convert the MathML @s into a document fragment.")
  (mathtm-as-serial (parse-xml s)))
