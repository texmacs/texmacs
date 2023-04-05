
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
	 (r (tree->stree (upgrade-mathml m)))
   (r (replace-symbol-in-stree r 'around 'around*))
   (displayed? (attribute-is? a 'display "block")))
       ; according to https://developer.mozilla.org/en-US/docs/Web/MathML/Element/math
    (if displayed?
       `((document (equation* ,r)))
       `((math ,r)))))

(define (attribute-is? a key value)
  (if (null? a) 
    #f
    (if (and (pair? (car a))
       (func? (car a) key 1)
       (== (cadar a) value))
       #t 
       (attribute-is? (cdr a) key value))))

(define (attribute-val a key)
  (if (null? a) 
    #f
    (if (and (pair? (car a))
       (func? (car a) key 1))
       (cadar a)
       (attribute-val (cdr a) key))))
       
;copied from htmltm.scm
(define (replace-symbol-in-stree st from to)
  (cond ((== st from) to)
        ((list? st) (map (lambda (x) (replace-symbol-in-stree x from to)) st))
        (else st)))

    
(define (mathtm-none env a c)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Literals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-string env s)
  ;; FIXME: use translators or parser for this!!!
  ;; TODO: learn when the trailing ';' is optional
  (cond ((logic-ref mathml-symbol->tm% s) => identity)
  ((string-starts? s "&") (entity->tm s))
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
		 ((string-starts? s "&") `(,(entity->tm s)))
		 (else (list r)))))))
     
(define (entity->tm s)
  (let* ((l (string-length s))
         (typ (cond 
               ((and (== l 6) (== (string-take-right s 4) "opf;")) "<bbb-" )
               ((and (== l 6) (== (string-take-right s 4) "scr;")) "<cal-" )
               ((and (== l 5) (== (string-take-right s 3) "fr;")) "<frak-" )
               (else #f))))
        (if typ 
          (string-append typ (substring s 1 2) ">")
          s)))  
  
(define (mathtm-mtext env a c)
  `((with "mode" "text" ,(mathtm-args-serial env c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple mathematical constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-mfrac env a c)
  (if (== (length c) 2)
    (with lt (attribute-val a 'linethickness)
      (if (and lt  (length-zero? lt)) 
        `((stack (tformat (table
           (row (cell ,(mathtm-as-serial env (first c))))
           (row (cell ,(mathtm-as-serial env (second c)))))))) 
        `((frac
     ,(mathtm-as-serial env (first c))
     ,(mathtm-as-serial env (second c))))))
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

;(define (mathtm-mstyle env a c)
;  (let* ((attrs (mathtm-style a))
;	 (l (mathtm-args env c)))
;    (if (null? attrs) l `((with ,@attrs ,(mathtm-serial env l))))))

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
       (attribute-is? (cdadr src) 'stretchy "true")))

(define (rubberify arrow)
  (string-append "<rubber-" (substring arrow 1 (string-length arrow))))

(define (mathtm-munder env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c))))
        (cond 
             ((and (list? sub) (== (first sub) 'below) (logic-ref mathml-below->tm% (second sub) )) 
             ;widenable-decoration, but inverted order 
             `((below ,(car (mathtm-below base (second sub))) ,(third sub))))
            ((stretchy? (first c) base)
            `((long-arrow ,(rubberify base) "" ,sub)))
            (else (mathtm-below base sub))))
      (mathtm-error "bad munder")))

(define (mathtm-mover env a c)
  (if (== (length c) 2)
      (let ((base (mathtm-as-serial env (first c)))
	    (sup (mathtm-as-serial env (second c))))
        (cond 
           ((and (list? sup) (== (first sup) 'above) (logic-ref mathml-above->tm% (second sup) ))
             ;inverted over
             `((above ,(car (mathtm-above base (second sup))) ,(third sup))))
           ((stretchy? (first c) base)
            `((long-arrow ,(rubberify base) ,sup)))
           (else (mathtm-above base sup))))
      (mathtm-error "bad mover")))

(define (mathtm-munderover env a c)
  (if (== (length c) 3)
      (let ((base (mathtm-as-serial env (first c)))
	    (sub (mathtm-as-serial env (second c)))
	    (sup (mathtm-as-serial env (third c))))
        (if (stretchy? (first c) base)
            `((long-arrow ,(rubberify base) ,sup ,sub))
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
 (with sa (mathtm-style (list a))
   (if (nnull? sa) ;styling attributes?
      (map (lambda (x) `(cwith ,(first x) ,(second x))) 
        (split-by sa 2))
  (cond ((and (func? a 'columnalign) (mathtm-halign (cadr a)))
	 `((cwith "cell-halign" ,(mathtm-halign (cadr a)))))
	((and (func? a 'rowalign) (mathtm-valign (cadr a)))
	 `((cwith "cell-valign" ,(mathtm-valign (cadr a)))))
	(else '()))
  )))

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
 (with sa (mathtm-style (list a))
   (if (nnull? sa) ;styling attributes?
      (map (lambda (x) `(cwith "1" "-1" ,(first x) ,(second x))) 
        (split-by sa 2))
  (cond ((func? a 'columnalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-row-halign l 1)))
	((and (func? a 'rowalign) (mathtm-valign (cadr a)))
	 `((cwith "1" "-1" "cell-valign" ,(mathtm-valign (cadr a)))))
	(else '()))
  )))

(define (split-by lst n)
   (if (not (null? lst))
       (cons (list-take lst n) (split-by (list-drop lst n) n))
       '() ))  

(define (mathtm-mtr env a c)
  (let* ((cell? (lambda (x) (mathml-func? x 'mtd)))
	 (c2 (map (lambda (x) (if (cell? x) x `(m:mtd ,x))) c))
	 (r `(row ,@(map (cut mathtm-as-serial env <>) c2)))
	 (fm (append-map mathtm-row-format a)))
    (if (null? fm) `(,r) `((tformat ,@fm ,r)))))

(define (mathtm-mlabeledtr env a c)
  ;; row label is ignored (not MathML Core spec)
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
 (with sa (mathtm-style (list a))
   (if (nnull? sa) ;styling attributes?
      (map (lambda (x) `(cwith "1" "-1" "1" "-1" ,(first x) ,(second x))) 
        (split-by sa 2))
  (cond ((func? a 'columnalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-table-halign l 1)))
	((func? a 'rowalign)
	 (with l (string-tokenize-by-char (cadr a) #\space)
	   (mathtm-table-valign l 1)))
	(else '()))
  )))

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
;; Further features used by wikipedia & LibreOffice Math
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathtm-semantics env a c)
  (or (and (list>1? c)
       (== (get-preference "mathml->texmacs:latex-annotations") "on")
       (mathtm-annotation env a (cdr c)))
    (mathtm env (first c))))
      
(define (mathtm-annotation env a l) ;
;there may be more than one annotation, scan them all
  (with  r (and (list>1? l) (mathtm-annotation env a cdr (l)))
    (or  r
      (let* ((an (car l))
        (enc (and (func? an 'm:annotation 2)
              (func? (second an) '@)  (shtml-attr-non-null (cdr (second an)) 'encoding))))
        (cond 
          ((and enc (in? enc '("application/x-tex" "TeX")))
            (let* ((s (third an))
                    (lat (parse-latex (string-append "$" s "$")))
                    (str (latex->texmacs lat)))
               (list str)))
          ((and enc (string-starts? enc "StarMath")) ;ignore
            #f)
          (else
            (debug-message "debug-convert" (string-append "Mathml contains an unknown annotation type \"" enc "\"\n with value: \n" (third an) "\nTeXmacs is not using it\n"))
            #f)          
           )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mathml tags can all handle the same set of (so-called "global") styling attributes
;; -> handle them with a single function (except for table tags, treated differently)
;; If attributes can be interpreted, wrap the result of the tag procedure
;; in the appropriate '(with ...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(define (mathtm-globattr env a c tagproc)
  (with res (tagproc env a c)
    (cond 
      ((null? res) res)
      ((null? a) res)
      (else 
        (with attrs (mathtm-style a)
          (if (null? attrs) res
         `((with ,@attrs ,(car res)))))))))

(define (mathtm-style l)
;note that it does not hurt handling tag-specific attributes here
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
        ((func? h 'mathsize)
	       (cons* "font-base-size" (cadr h) r))
        ((func? h 'scriptlevel)
          (with sl (string->number (cadr h))
            (cond 
              ((and sl (>= sl 0))
	              (cons* "math-level" (cadr h) r))
              ((== sl -1)
                (cons* "font-size" "1.189" r)) ;std large
              ((== sl -2)
                (cons* "font-size" "1.414" r)) ;std very-large
              ((== sl -3)
                (cons* "font-size" "1.682" r)) ;std huge
              ((< sl -3)
                (cons* "font-size" "2" r)) ;std really huge
              (else r))))
        ((func? h 'style) ;css styling string, expand to attributes 
         (append 
           (mathtm-style 
           (map (lambda (l) (list (string->symbol (car l)) (cAr l))) 
            (filter list-2? 
              (map (lambda (x)  (map string-trim (string-tokenize-by-char x #\:))) 
                (string-tokenize-by-char (cadr h) #\;)))))
           r))
	      (else r)))))

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
  (math (mathtm-handler :element mathtm-math mathtm-globattr))
  (none (mathtm-handler :mixed mathtm-none mathtm-globattr))
  ;;; Presentation
  ;; Token
  ;; presentation tokens contain CDATA, MathML entities, align marks, or glyphs
  (mi (mathtm-handler :mixed mathtm-pass mathtm-globattr))
  (mn (mathtm-handler :mixed mathtm-pass mathtm-globattr))
  (mo (mathtm-handler :mixed mathtm-mo mathtm-globattr))
  (mtext (mathtm-handler :mixed mathtm-mtext mathtm-globattr))
  (mspace (mathtm-handler :mixed mathtm-drop mathtm-globattr))
  (ms (mathtm-handler :mixed mathtm-mtext mathtm-globattr))
  (mglyph (mathtm-handler :empty mathtm-drop mathtm-globattr))
  ;; General layout
  (mrow (mathtm-handler :mixed mathtm-pass mathtm-globattr)) ;was :element, now more tolerant with malformed xml  
  (mfrac (mathtm-handler :element mathtm-mfrac mathtm-globattr))
  (msqrt (mathtm-handler :element mathtm-msqrt mathtm-globattr))
  (mroot (mathtm-handler :element mathtm-mroot mathtm-globattr))
  ;(mstyle (mathtm-handler :element mathtm-mstyle))
  (mstyle (mathtm-handler :element mathtm-pass mathtm-globattr))
  ; <mstyle> is now just equivalent to an <mrow>
  (merror (mathtm-handler :element mathtm-merror mathtm-globattr))
  (mpadded (mathtm-handler :element mathtm-pass mathtm-globattr))
  (mphantom (mathtm-handler :element mathtm-mphantom mathtm-globattr))
  (mfenced (mathtm-handler :element mathtm-mfenced mathtm-globattr))
  (menclose (mathtm-handler :element mathtm-menclose mathtm-globattr))
  ;; Script and limits
  (msub (mathtm-handler :element mathtm-msub mathtm-globattr))
  (msup (mathtm-handler :element mathtm-msup mathtm-globattr))
  (msubsup (mathtm-handler :element mathtm-msubsup mathtm-globattr))
  (munder (mathtm-handler :element mathtm-munder mathtm-globattr))
  (mover (mathtm-handler :element mathtm-mover mathtm-globattr))
  (munderover (mathtm-handler :element mathtm-munderover mathtm-globattr))
  (mmultiscripts (mathtm-handler :element mathtm-mmultiscripts mathtm-globattr))
  ;; Tables
  (mtable (mathtm-handler :element mathtm-mtable))
  (mtr (mathtm-handler :element mathtm-mtr))
  (mlabeledtr (mathtm-handler :element mathtm-mlabeledtr))
  (mtd (mathtm-handler :element mathtm-mtd))
  ;; Actions
  (maction (mathtm-handler :element mathtm-pass mathtm-globattr))
  ;; Further features used by wikipedia
  (semantics (mathtm-handler :element mathtm-semantics mathtm-globattr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mathml->tree s)
  (:synopsis "Convert the MathML @s into a document fragment.")
  (mathtm-as-serial (parse-xml s)))
