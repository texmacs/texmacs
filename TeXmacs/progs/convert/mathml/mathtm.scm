
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mathtm.scm
;; DESCRIPTION : conversion of MathML trees to TeXmacs trees
;; COPYRIGHT   : (C) 2002, 2005 Joris van der Hoeven and David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  (let* ((m (mathtm-args-serial env c))
	 (r (tree->stree (upgrade-mathml m))))
    `((math ,r))))

(define (mathtm-none env a c)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-string env s)
  ;; FIXME: use translators or parser for this!!!
  ;; TODO: learn when the trailing ';' is optional
  (cond ((logic-ref mathml-symbol->tm% s) => identity)
	(else (xmltm-text s))))

(define (mathtm-mo env a c)
  (cond ((null? c) '())
	((or (nnull? (cdr c)) (nstring? (car c)))
	 (list (mathtm-args-serial env c)))
	(else
	 (let* ((s (car c))
		(r (xmltm-text s)))
	   (cond ((logic-ref mathml-left->tm% s) => (lambda (x) `((left ,x))))
		 ((logic-ref mathml-right->tm% s) => (lambda (x) `((right ,x))))
		 ((logic-ref mathml-big->tm% s) => (lambda (x) `((big ,x))))
		 ((logic-ref mathml-symbol->tm% s) => (lambda (x) `(,x)))
		 ((logic-ref tmtm-left% r) => (lambda (x) `((left ,x))))
		 ((logic-ref tmtm-right% r) => (lambda (x) `((right ,x))))
		 ((logic-ref tmtm-big% r) => (lambda (x) `((big ,x))))
		 (else (list r)))))))

(define (mathtm-mtext env a c)
  `((with "mode" "text" ,(mathtm-args-serial env c))))

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
  `((sqrt ,(mathtm-args-serial env c))))

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

(define (mathtm-style l)
  (if (null? l) l
      (let* ((h (car l))
	     (r (mathtm-style (cdr l))))
	(cond ((or (func? h 'mathcolor) (func? h 'color))
	       (cons* "color" (cadr h) r))
	      ((func? h 'displaystyle)
	       (cons* "math-display" (cadr h) r))
	      ((or (== h '(mathvariant "bold"))
		   (== h '(mathvariant "bold-italic")))
	       (cons* "math-font-series" "bold" r))
	      ((or (== h '(mathvariant "sans-serif"))
		   (== h '(mathvariant "sans-serif--italic")))
	       (cons* "math-font-family" "ms" r))
	      ((== h '(mathvariant "monospace"))
	       (cons* "math-font-family" "mt" r))
	      (else r)))))

(define (mathtm-mstyle env a c)
  (let* ((attrs (mathtm-style a))
	 (l (mathtm-args env c)))
    (if (null? attrs) l `((with ,@attrs ,(mathtm-serial env l))))))

(define (mathtm-mphantom env a c)
  `((phantom ,(mathtm-args-serial env c))))

(define (mathtm-sep-list l seps)
  (cond ((null? l) l)
	((null? seps) l)
	(else (cons* (car l) `(m:mo ,(car seps))
		     (mathtm-sep-list (cdr l) (cdr seps))))))

(define (mathtm-mfenced env a c)
  (let* ((open (car (or (assoc-ref a 'open) '("("))))
	 (close (car (or (assoc-ref a 'close) '(")"))))
	 (seps (string-tokenize-by-char
		(car (or (assoc-ref a 'separators) '("")))
		#\space)))
    (if (== seps '("")) (set! seps '()))
    (mathtm env `(m:mrow (m:mo ,open)
			 ,@(mathtm-sep-list c seps)
			 (m:mo ,close)))))

(define (mathtm-menclose env a c)
  (let* ((args (mathtm-args env c))
	 (notation (car (or (assoc-ref a 'notation) '(""))))
	 (l (if (== notation "") '()
		(string-tokenize-by-char notation #\space))))
    (if (in? "updiagonalstrike" l)
	`((neg ,(mathtm-serial env args)))
	args)))

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
  (cond ((logic-ref mathml-below->tm% sub) =>
	 (lambda (x) `((wide* ,base ,x))))
	(else `((below ,base ,sub)))))

(define (mathtm-above base sup)
  (cond ((logic-ref mathml-above->tm% sup) =>
	 (lambda (x) `((wide ,base ,x))))
	(else `((above ,base ,sup)))))

(define (stretchy? src dest)
  (and (string? dest)
       (string-starts? dest "<")
       (string-ends? dest ">")
       (or (func? src 'm:mo) (func? src 'mo))
       (>= (length src) 3)
       (func? (cadr src) '@)
       (>= (length (cadr src)) 2)
       (func? (cadr (cadr src)) 'stretchy 1)))

(define (rubberify arrow)
  (string-append "<rubber-" (substring arrow 1 (string-length arrow))))

(define (mathtm-munder env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c))))
        (if (stretchy? (first c) base)
            `((long-arrow ,(ruzbberify base) "" ,sub))
            (mathtm-below base sub)))
      (mathtm-error "bad munder")))

(define (mathtm-mover env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sup (mathtm-as-serial env (second c))))
        (if (stretchy? (first c) base)
            `((long-arrow ,(rubberify base) ,sup))
            (mathtm-above base sup)))
      (mathtm-error "bad mover")))

(define (mathtm-munderover env a c)
  (if (== (length c) 3)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c)))
	    (sup (mathtm-as-serial env (third c))))
        (if (stretchy? (first c) base)
            `((long-arrow ,(rubberify base) ,sub ,sup))
            (mathtm-above (car (mathtm-below base sub)) sup)))
      (mathtm-error "bad munderover")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathml-func? x y)
  (and (list? x)
       (or (== (car x) y)
	   (== (car x) (symbol-append 'm: y)))))

(define (mathml-func-in? x l)
  (list-or (map (cut mathml-func? x <>) l)))

(define (mathtm-halign s)
  (cond ((== s "left") "l")
	((== s "center") "c")
	((== s "right") "r")
	(else #f)))

(define (mathtm-valign s)
  (cond ((== s "bottom") "b")
	((== s "baseline") "B")
	((== s "axis") "f")
	((== s "center") "c")
	((== s "top") "t")
	(else #f)))

(define (mathtm-cell-format a)
  (cond ((and (func? a 'columnalign) (mathtm-halign (cadr a)))
	 `((cwith "cell-halign" ,(mathtm-halign (cadr a)))))
	((and (func? a 'rowalign) (mathtm-valign (cadr a)))
	 `((cwith "cell-valign" ,(mathtm-valign (cadr a)))))
	(else '())))

(define (mathtm-mtd env a c)
  (let ((fm (append-map mathtm-cell-format a))
	(c `(cell ,(mathtm-serial env (mathtm-pass env a c)))))
    (if (null? fm) `(,c) `((tformat ,@fm ,c)))))

(define (mathtm-row-halign l nr)
  (if (null? l) '()
      (let* ((h (mathtm-halign (car l)))
	     (r (mathtm-row-halign (cdr l) (+ nr 1)))
	     (s (number->string nr))
	     (c `(cwith ,s ,s "cell-halign" ,h)))
	(if h (cons c r) r))))

(define (mathtm-row-format a)
  (cond ((func? a 'columnalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-row-halign l 1)))
	((and (func? a 'rowalign) (mathtm-valign (cadr a)))
	 `((cwith "1" "-1" "cell-valign" ,(mathtm-valign (cadr a)))))
	(else '())))

(define (mathtm-mtr env a c)
  (let* ((cell? (lambda (x) (mathml-func? x 'mtd)))
	 (c2 (map (lambda (x) (if (cell? x) x `(m:mtd ,x))) c))
	 (r `(row ,@(map (cut mathtm-as-serial env <>) c2)))
	 (fm (append-map mathtm-row-format a)))
    (if (null? fm) `(,r) `((tformat ,@fm ,r)))))

(define (mathtm-mlabeledtr env a c)
  ;; FIXME: label is still ignored
  (if (null? c) '((row))
      (mathtm-mtr env a (cdr c))))

(define (mathtm-table-halign l nr)
  (if (null? l) '()
      (let* ((h (mathtm-halign (car l)))
	     (r (mathtm-table-halign (cdr l) (+ nr 1)))
	     (s (number->string nr))
	     (c `(cwith "1" "-1" ,s ,s "cell-halign" ,h)))
	(if h (cons c r) r))))

(define (mathtm-table-valign l nr)
  (if (null? l) '()
      (let* ((h (mathtm-valign (car l)))
	     (r (mathtm-table-valign (cdr l) (+ nr 1)))
	     (s (number->string nr))
	     (c `(cwith ,s ,s "1" "-1" "cell-valign" ,h)))
	(if h (cons c r) r))))

(define (mathtm-table-format a)
  (cond ((func? a 'columnalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-table-halign l 1)))
	((func? a 'rowalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-table-valign l 1)))
	(else '())))

(define (mathtm-mtable env a c)
  (let* ((row? (lambda (x) (mathml-func-in? x '(mtr mlabeledtr))))
	 (c2 (map (lambda (x) (if (row? x) x `(m:mtr ,x))) c))
	 (l (map (cut mathtm-as-serial env <>) c2))
	 (fm (append-map mathtm-table-format a))
	 (t (tmtable-complete `(tformat ,@fm (table ,@l)))))
    (set! t (tmtable-format-up t))
    (if (func? t 'tformat 1) (set! t (cAr t)))
    `((tabular ,t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further features used by wikipedia
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-semantics env a c)
  (cond ((and (list-2? c)
              (func? (second c) 'm:annotation 2)
              (func? (second (second c)) '@)
              (== (shtml-attr-non-null (cdr (second (second c))) 'encoding)
                  "application/x-tex")
              (string? (third (second c))))
         (if (== (get-preference "mathml->texmacs:latex-annotations") "on")
             (let* ((s (third (second c)))
                    (l (parse-latex (string-append "$" s "$")))
                    (r (latex->texmacs l)))
               (list r))
             (mathtm env (first c))))
        (else (mathtm-pass env a c))))

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

(logic-dispatcher mathtm-methods%
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
  (mstyle (mathtm-handler :element mathtm-mstyle))
  (merror (mathtm-handler :element mathtm-merror))
  (mpadded (mathtm-handler :element mathtm-pass))
  (mphantom (mathtm-handler :element mathtm-mphantom))
  (mfenced (mathtm-handler :element mathtm-mfenced))
  (menclose (mathtm-handler :element mathtm-menclose))
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
  (maction (mathtm-handler :element mathtm-pass))
  ;; Further features used by wikipedia
  (semantics (mathtm-handler :element mathtm-semantics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mathml->tree s)
  (:synopsis "Convert the MathML @s into a document fragment.")
  (mathtm-as-serial (parse-xml s)))
