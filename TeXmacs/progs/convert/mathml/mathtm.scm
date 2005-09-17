
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

(define (mathtm-style l)
  (if (null? l) l
      (let* ((h (car l))
	     (r (mathtm-style (cdr l))))
	(cond ((or (func? h 'mathcolor) (func? h 'color))
	       (cons* "color" (cadr h) r))
	      ((or (== h '(mathvariant "bold"))
		   (== h '(mathvariant "bold-italic")))
	       (cons* "math-font-series" "bold" r))
	      (else r)))))

(define (mathtm-mstyle env a c)
  (let* ((attrs (mathtm-style a))
	 (l (mathtm-args env c)))
    (if (null? attrs) l `((with ,@attrs ,(mathtm-serial env l))))))

(define (mathtm-mphantom env a c)
  `((phantom ,(mathtm-args-serial env c))))

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
;; Helper routine on TeXmacs tables (should go into other file)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmcell-format-up c)
  (cond ((func? c 'tformat) c)
	((func? c 'cell) `(tformat ,c))
	(else (texmacs-error "tmcell-format-up" "~S is not a cell" c))))

(define (tmrow-append r1 r2)
  `(tformat ,@(cDdr r1) ,@(cDdr r2) (row ,@(cdAr r1) ,@(cdAr r2))))

(define (tmrow-format-up-sub l nr)
  (if (null? l) `(tmformat (row))
      (let* ((cell (tmcell-format-up (car l)))
	     (snr (number->string nr))
	     (fun (lambda (x) `(cwith ,snr ,snr ,@(cdr x))))
	     (head `(tformat ,@(map fun (cDdr cell)) (row ,(cAr cell)))))
	(tmrow-append head (tmrow-format-up-sub (cdr l) (+ nr 1))))))

(define (tmrow-format-up r)
  "Raise tformat tags in cells and cells of @r to the upmost level"
  (cond ((func? r 'tformat)
	 (with s (tmrow-raise-format (cAr r))
	   (append (cDr r) (cdr s))))
	((func? r 'row) (tmrow-format-up-sub (cdr r) 1))
	(else (texmacs-error "tmrow-format-up" "~S is not a row" r))))

(define (tmtable-append t1 t2)
  `(tformat ,@(cDdr t1) ,@(cDdr t2) (table ,@(cdAr t1) ,@(cdAr t2))))

(define (tmtable-format-up-sub l nr)
  (if (null? l) `(tmformat (table))
      (let* ((row (tmrow-format-up (car l)))
	     (snr (number->string nr))
	     (fun (lambda (x) `(cwith ,snr ,snr ,@(cdr x))))
	     (head `(tformat ,@(map fun (cDdr row)) (table ,(cAr row)))))
	(tmtable-append head (tmtable-format-up-sub (cdr l) (+ nr 1))))))

(define (tmtable-format-up t)
  "Raise tformat tags in rows and cells of @t to the upmost level"
  (cond ((func? t 'tformat)
	 (with u (tmtable-format-up (cAr t))
	   (append (cDr t) (cdr u))))
	((func? t 'table) (tmtable-format-up-sub (cdr t) 1))
	(else (texmacs-error "tmtable-format-up" "~S is not a table" t))))

(define (tmrow-cols r)
  "Return the number of columns of the row @r"
  (cond ((func? r 'tformat) (tmrow-cols (cAr r)))
	((func? r 'row) (- (length r) 1))
	(else (texmacs-error "tmrow-cols" "~S is not a row" r))))

(define (tmrow-complete r n)
  "Complete the row @r with empty strings to become at least @n columns wide"
  (cond ((func? r 'tformat) (rcons (cDr r) (tmrow-complete (cAr r) n)))
	((== r '(row)) (cons 'row (make-list (max n 0) '(cell ""))))
	((func? r 'row)
	 (with next (tmrow-complete (cons 'row (cddr r)) (- n 1))
	   (cons* 'row (cadr r) (cdr next))))
	(else (texmacs-error "tmrow-complete" "~S is not a row" r))))

(define (tmtable-complete t)
  "Completes missing cells on rows of @t with empty strings"
  (cond ((func? t 'tformat) (rcons (cDr t) (tmtable-complete (cAr t))))
	((func? t 'table)
	 (with cols (apply max (map tmrow-cols (cdr t)))
	   (cons 'table (map (cut tmrow-complete <> cols) (cdr t)))))
	(else (texmacs-error "tmtable-complete" "~S is not a table" t))))

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
	 (with l (string-tokenize (cadr a) #\space)
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
	 (with l (string-tokenize (cadr a) #\space)
	   (mathtm-table-halign l 1)))
	((func? a 'rowalign)
	 (with l (string-tokenize (cadr a) #\space)
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
  (mstyle (mathtm-handler :element mathtm-mstyle))
  (merror (mathtm-handler :element mathtm-merror))
  (mpadded (mathtm-handler :element mathtm-pass))
  (mphantom (mathtm-handler :element mathtm-mphantom))
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
