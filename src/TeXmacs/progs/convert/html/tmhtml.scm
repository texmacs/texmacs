
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml.scm
;; DESCRIPTION : conversion of TeXmacs trees into Html trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml)
  (:use (convert tools tmlength) (convert tools tmtable)
	(convert tools tmconcat) (convert tools sxml)
	(convert tools stm) (convert tools sxhtml)
	(convert html htmlout))
  (:export texmacs->html tmhtml))

;; FIXME: should go into environment
(define tmhtml-math-mode? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Empty handler and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-noop l) '())

(define (tmhtml-sub-token s pos)
  (substring s pos (- (string-length s) 1)))

(define (tmhtml-math-token s)
  (cond ((= (string-length s) 1)
	 (cond ((== s "&") "&amp;")
	       ((== s "*") " ")
	       ((== s "+") " + ")
	       ((== s "-") " - ")
	       ((char-alphabetic? (string-ref s 0)) `(h:i ,s))
	       (else s)))
	((string-starts? s "<b-") `(h:b (h:i ,(tmhtml-sub-token s 3))))
	((string-starts? s "<bbb-") `(h:u (h:b ,(tmhtml-sub-token s 5))))
	((string-starts? s "<cal-") `(h:u (h:i ,(tmhtml-sub-token s 5))))
	((string-starts? s "<frak-") `(h:u ,(tmhtml-sub-token s 6)))
	((== s "<less>") "&lt;")
	((string-starts? s "<")
	 (with encoded (cork->utf8 s)
	   (if (or (== s encoded) (> (string-length encoded) 3))
	       (tm->xml-cdata s)
	       encoded)))
	(else s)))

(define (tmhtml-string s)
  (if tmhtml-math-mode?
      (tmhtml-post-simplify-nodes
       (map tmhtml-math-token (tmconcat-tokenize-math s)))
      (let* ((r1 (string-replace s "&" "&amp;"))
	     (r2 (string-replace r1 "<less>" "&lt;"))
	     (r3 (cork->utf8 r2)))
	(list r3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entire documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-find-title doc)
  (cond ((func? doc 'tmdoc-title 1) (cadr doc))
	((func? doc 'tmdoc-title* 2) (cadr doc))
	((func? doc 'tmdoc-title** 3) (caddr doc))
	((not (pair? doc)) #f)
	(else (with title (tmhtml-find-title (car doc))
		(if title title
		    (tmhtml-find-title (cdr doc)))))))

(define (tmhtml-file l)
  ;; This handler is special:
  ;; Since !file is a special node used only at the top of trees
  ;; it produces a single node, and not a nodeset like other handlers.
  (let* ((doc (car l))
	 (styles (cdadr l))
	 (lang (caddr l))
	 (tmpath (cadddr l))
	 (title (tmhtml-find-title doc))
	 (css '(h:style (@ (type "text/css")) "body { text-align: justify }"))
	 (body (tmhtml doc)))
    (set! title (if title `(concat ,title " (FSF GNU project)") "No title"))
    (if (or (in? "tmdoc" styles) (in? "tmweb" styles))
	(begin
	  (set! css '(h:link (@ (rel "stylesheet")
				(href "http://www.texmacs.org/css/tmdoc.css")
				(type "text/css"))))
	  (set! body (tmhtml-tmdoc-post body))))
    `(h:html
      (h:head
       (h:title ,@(tmhtml title))
       (h:meta (@ (name "generator")
		  (content ,(string-append "TeXmacs " (texmacs-version)))))
       ,css)
      (h:body ,@body))))

(define (tmhtml-finalize-document top)
  ;; @top must be a node produced by tmhtml-file
  "Prepare a XML document for serialization"
  (define xmlns-attrs
    '((xmlns "http://www.w3.org/1999/xhtml")
      (xmlns:m "http://www.w3.org/1998/Math/MathML")
      (xmlns:x "http://www.texmacs.org/2002/extensions")))
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
	  ,((cut sxml-set-attrs <> xmlns-attrs)
	    (sxml-strip-ns-prefix "h" top))))

(define (tmhtml-finalize-selection l)
  ;; @l is a nodeset produced by any handler _but_ tmhtml-file
  "Prepare a HTML node-set for serialization."
  `(*TOP* ,@(map (cut sxml-strip-ns-prefix "h" <>) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-document l)
  (tmhtml-post-paragraphs (map (lambda (x) (cons 'h:p (tmhtml x)))
			       l)))

(define (tmhtml-paragraph l)
  (let rec ((l l))
    (if (null? l) '()
	(let ((h (tmhtml (car l)))
	      (r (rec (cdr l))))
	  (cond ((null? h) r)		; correct when r is null too
		((null? r) h)
		(else `(,@h (h:br) ,@r)))))))

(define (tmhtml-post-paragraphs l)
  ;; Post process a collection of h:p elements
  ;;
  ;; If a h:p contains a h:hN, remove the h:p node and prepend the rest of the
  ;; contents to the next h:p. If the next element, after post processing is
  ;; not a h:p, create an intermediate h:p to hold the data.
  ;;
  ;; If a h:p contains a list element, remove the enclosing h:p. The TeXmacs
  ;; editor ensures that an <item-list> or <desc-list> is the only element
  ;; contained in its enclosing <doc-item>.
  ;;
  ;; NOTE: assumes the heading is at the start of a paragraph. That is
  ;; consistent with the fact that (as of 2003-02-04) the only converted
  ;; invisible markup is <label> and correct usage requires it to be after the
  ;; section title.
  (let rec ((in l) (out '()) (trail #f))
    (let* ((para (and (pair? in) (car in)))
	   (cont (and para (sxml-content para)))
	   (first (and cont (pair? cont) (car cont)))
	   (next (lambda (o t) (rec (cdr in) o t)))
	   (flush (lambda () (if trail `((h:p ,@trail) ,@out) out)))
	   (accept (lambda () (if trail (sxml-prepend para trail) para)))
	   (give (lambda () (and (pair? (cdr cont)) (cdr cont)))))
      ;; invariant: (xor prev prev-trail)
      (cond ((null? in) (reverse (flush)))
	    ((or (null? cont) (string? first))
	     (next (cons (accept) out) #f))
	    ((sxhtml-heading? first)
	     ;; tmhtml-post-heading should be called by concat handler
	     (next (cons first (flush)) (give)))
	    ((sxhtml-list? first)
	     ;; texmacs editor ensures there is not trail after a list
	     (next (append cont (flush)) #f))
	    ((and (sxhtml-table? first) (null? (cdr cont)))
	     ;; if table is not alone, we cannot help but produce bad html
	     ;; if table is alone, drop the enclosing <h:p>
	     (next (append cont (flush)) #f))
	    (else (next (cons (accept) out) #f))))))

(define (tmhtml-post-heading l)
  ;; Post-process the converted result of a concat containing a section title.
  ;;
  ;; The first label after the section is changed to an 'id' attribute in the
  ;; heading element, if it has not already an 'id' attribute.
  ;;
  ;; NOTE: assumes the heading is the first node of the set.
  (let ((heading (car l)))
    (if (sxml-attr heading 'id) l
	(receive (labels rest) (list-partition (cdr l) sxhtml-label?)
	  (if (null? labels) l
	      (cons (sxml-prepend (sxhtml-glue-label heading (car labels))
				  (cdr labels))
		    rest))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-force-document l)
  (cond ((func? l 'document) l)
	((func? l 'with)
	 (let* ((args (cDdr l))
		(r (cdr (tmhtml-force-document (cAr l)))))
	   (cond ((null? r) r)
		 ((null? (cdr r))
		  `(document (with ,@args ,(car r))))
		 ((null? (cddr r))
		  `(document (with ,@args ,(car r))
			     (with ,@args ,(cAr r))))
		 (else
		  `(document (with ,@args ,(car r))
			     (with ,@args (document ,@(cDdr r)))
			     (with ,@args ,(cAr r)))))))
	((func? l 'surround 3)
	 (let* ((left (cadr l))
		(right (caddr l))
		(r (cdr (tmhtml-force-document (cadddr l)))))
	   (cond ((null? r) r)
		 ((null? (cdr r))
		  `(document (concat ,left ,(car r) ,right)))
		 (else
		  `(document (concat ,left ,(car r))
			     ,@(cDdr r)
			     (concat ,(cAr r) ,right))))))
	(else `(document ,l))))

(define (tmhtml-surround l)
  (let* ((r1 (tmhtml-force-document `(surround ,@l)))
	 (r2 (tree->object (tree-simplify (object->tree r1)))))
    ;(display* "r0= " `(surround ,@l) "\n")
    ;(display* "r1= " r1 "\n")
    ;(display* "r2= " r2 "\n")
    (tmhtml r2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-concat l)
  (set! l (tmconcat-structure-tabs l))
  (tmhtml-post-simplify-nodes
   (let ((l (tmhtml-list l)))
     (cond ((null? l) '())
	   ((string? (car l)) l)
	   ((sxhtml-heading? (car l)) (tmhtml-post-heading l))
	   ((list-any sxhtml-table? l) (tmhtml-post-table l))
	   (else l)))))

(define (tmhtml-align-left l)
  (if (in? l '(() (""))) '()
      `((h:div (@ (style "text-align: left"))
	       ,@(tmhtml-concat l)))))

(define (tmhtml-align-middle l)
  (if (in? l '(() (""))) '()
      `((h:div (@ (style "text-align: center"))
	       ,@(tmhtml-concat l)))))

(define (tmhtml-align-right l)
  (if (in? l '(() (""))) '()
      `((h:div (@ (style "text-align: right"))
	       ,@(tmhtml-concat l)))))

(define (tmhtml-post-simplify-nodes l)
  ;; Catenate adjacent string nodes and remove empty string nodes
  (let rec ((l l))
    (cond ((null? l) '())
	  ((and (string? (car l)) (string-null? (car l)))
	   (rec (cdr l)))
	  ((null? (cdr l)) l)
	  ((and (string? (car l)) (string? (cadr l)))
	   (rec (cons (string-append (car l) (cadr l)) (cddr l))))
	  (else (cons (car l) (rec (cdr l)))))))

(define (tmhtml-post-simplify-element e)
  ;; Simplify the nodes of the element content
  (list (append (sxml-element-head e)
		(tmhtml-post-simplify-nodes (sxml-content e)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-hspace l)
  '(" "))

(define (tmhtml-vspace l)
  '())

(define (tmhtml-move l)
  (tmhtml (car l)))

(define (tmhtml-resize l)
  (tmhtml (car l)))

(define (tmhtml-float l)
  (tmhtml (cAr l)))

(define (tmhtml-repeat l)
  (tmhtml (car l)))

(define (tmhtml-datoms l)
  (tmhtml (cAr l)))

(define (tmhtml-new-line l)
  '((h:br)))

(define (tmhtml-next-line l)
  '((h:br)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-id l)
  (tmhtml (car l)))

(define (tmhtml-big l)
  (cond ((in? (car l) '("sum" "prod" "int" "oint" "amalg"))
	 (tmhtml (string-append "<" (car l) ">")))
	((in? (car l) '("<cap>" "<cup>" "<vee>" "<wedge>"))
	 (with s (substring (car l) 1 (- (string-length (car l)) 1))
	   (tmhtml (string-append "<big" s ">"))))
	((== (car l) ".") '())
	(else (tmhtml (car l)))))

(define (tmhtml-below l)
  `("below (" ,@(tmhtml (car l)) ", " ,@(tmhtml (cadr l)) ")"))

(define (tmhtml-above l)
  `("above (" ,@(tmhtml (car l)) ", " ,@(tmhtml (cadr l)) ")"))

(define (tmhtml-sub l)
  `((h:sub ,@(tmhtml (car l)))))

(define (tmhtml-sup l)
  `((h:sup ,@(tmhtml (car l)))))

(define (tmhtml-frac l)
  (let* ((num (tmhtml (car l)))
	 (den (tmhtml (cadr l))))
    `("frac (" ,@num ", " ,@den ")")))

;    `((h:table (@ (style "display: inline; margin-top: -10px; padding-top: -10px")
;		  (valign "center"))
;	       (h:tr (h:td (@ (align "center")
;			      (style "border-bottom: solid 1px")) ,@num))
;	       (h:tr (h:td (@ (align "center")) ,@den))))))

(define (tmhtml-sqrt l)
  (if (= (length l) 1)
      `("sqrt (" ,@(tmhtml (car l)) ")")
      `("sqrt" (h:sub ,@(tmhtml (cadr l))) " (" ,@(tmhtml (car l)) ")")))

(define (tmhtml-wide l)
  `("(" ,@(tmhtml (car l)) ")" (h:sup ,@(tmhtml (cadr l)))))

(define (tmhtml-neg l)
  `("not(" ,@(tmhtml (car l)) ")"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local and global environment changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-with-font-size val)
  (let* ((x (* (string->number val) 100))
	 (s (cond ((< x 1) #f) ((< x 55) "-4") ((< x 65) "-3")
		  ((< x 75) "-2") ((< x 95) "-1") ((< x 115) "0")
		  ((< x 135) "+1") ((< x 155) "+2") ((< x 185) "+3")
		  ((< x 225) "+4") ((< x 500) "+5") (else #f))))
    (and s `(h:font (@ (size ,s))))))

(define (tmhtml-with-one var val arg)
  (if (== var "mode")
      ;; FIXME: should go into the DRD modulo some reorganization
      (with old-mode? tmhtml-math-mode?
	(set! tmhtml-math-mode? (== val "math"))
	(with r (tmhtml arg)
	  (set! tmhtml-math-mode? old-mode?)
	  r)) ;`((h:class (@ ("style" "font-style: normal")) ,@r))))
      (let ((w (or (drd-ref tmhtml-with-cmd% (list var val))
		   (let ((h (drd-ref tmhtml-with-cmd% var)))
		     (and h (h val))))))
	(if w
	    (list (append w (tmhtml arg)))
	    (tmhtml arg)))))

(define (tmhtml-with l)
  (cond ((null? l) '())
	((null? (cdr l)) (tmhtml (car l)))
	((null? (cddr l)) '())
	(else
	 (let* ((var (force-string (car l)))
		(val (force-string (cadr l)))
		(next (cddr l)))
	   (tmhtml-with-one var val `(with ,@next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-compound l)
  ;; Explicit expansions are converted and handled as implicit expansions.
  (tmhtml-implicit-compound (cons (string->symbol (car l)) (cdr l))))

(define (tmhtml-label l)
  ;; WARNING: bad conversion if ID is not a string.
  `((h:a (@ (id ,(cork->utf8 (force-string (car l))))))))

;(define (tmhtml-reference l)
;  (list 'ref (cork->utf8 (force-string (car l)))))

;(define (tmhtml-pageref l)
;  (list 'pageref (cork->utf8 (force-string (car l)))))

(define (tmhtml-suffix s)
  ;; Change .html suffix to .tm suffix for local files for correct
  ;; conversion of entire web-sites. We might create an option
  ;; in order to disable this suffix change
  (let* ((sdir (string-rindex s #\/))
	 (sep (string-rindex s #\#)))
    (cond ((or (string-starts? s "http:") (string-starts? s "ftp:")) s)
          ((and sep (or (not sdir) (< sdir sep)))
	   (string-append (tmhtml-suffix (substring s 0 sep))
			  (string-drop s sep)))
	  ((string-ends? s ".tm")
	   (string-append (string-drop-right s 3) ".html"))
	  (else s))))

(define (tmhtml-hyperlink l)
  ;; WARNING: bad conversion if URI is not a string.
  ;; TODO: change label at start of content into ID attribute, move other
  ;; labels out (A elements cannot be nested!).
  (let* ((body (tmhtml (first l)))
	 (to (cork->utf8 (force-string (second l)))))
    (if (string-starts? to "$")
	body ;; temporary fix for URLs like $TEXMACS_PATH/...
	`((h:a (@ (href ,(tmhtml-suffix to))) ,@body)))))

(define (tmhtml-specific l)
  (if (== (car l) "html") (tmhtml (cadr l)) '()))

(define (tmhtml-action l)
  `((h:u ,@(tmhtml (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-table-cols p)
  (map (lambda (x)
	 `(h:col (@ (align ,(cadr (assoc x '(("l" "left")
					     ("c" "center")
					     ("r" "right"))))))))
       (p 'cols 'halign)))

(define (tmhtml-table-contents p)
  (define (cell x)
    `(h:td ,@(tmhtml x)))
  (define (row l)
    `(h:tr ,@(map cell l)))
  (map row (p 'rows 'content)))

(define (tmhtml-table-make p)
  `((h:table
     ,@(if (p 'global 'border) '((@ (border "1"))) '())
     ,@(tmhtml-table-cols p)
     (h:tbody ,@(tmhtml-table-contents p)))))

(define (tmhtml-table halign border x)
  ;; assert (= 1 (length x))
  ;; assert (or (func? (car x) 'tformat) (func? (car x) 'table))
  (let ((p (tmtable-parser `(tformat ,@(tmtable-cell-halign halign)
				     ,@(tmtable-block-borders border)
				     ,(car x)))))
    (if p (tmhtml-table-make p) '())))

(define (tmhtml-post-table l)
  ;; Post process the converted result of a concat containing a table.
  ;;
  ;; If a label is adjacent to the table, use it to set the table id. If there
  ;; are several labels adjacent to the table, leave all but one label
  ;; untouched. There is no guarantee on which label is glued.
  (define (glue-label-to-table x knil)
    (cond ((null? knil) (list x))
	  ((and (sxhtml-label? x)
		(sxhtml-table? (car knil))
		(not (sxml-attr (car knil) 'id)))
	   (cons (sxhtml-glue-label (car knil) x)
		 (cdr knil)))
	  ((and (sxhtml-table? x)
		(not (sxml-attr x 'id))
		(sxhtml-label? (car knil)))
	   (cons (sxhtml-glue-label x (car knil))
		 (cdr knil)))
	  (else (cons x knil))))
  (list-fold-right glue-label-to-table '() l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: Should also test that width and height are not magnifications.
;; Currently, magnifications make string->tmlength return #f.

(define (tmhtml-dimension-attr s name handlers)
  (let ((w (and-let* ((tmlen (string->tmlength s))
		      ((not (tmlength-null? tmlen)))
		      (proc (assoc (tmlength-unit tmlen) handlers)))
		     ((second proc) (tmlength-value tmlen)))))
    (if w `((,name ,w)) '())))

(define (exact-number->string x)
  (number->string (inexact->exact x)))
(define (number->percent x)
  (string-append (exact-number->string (* 100 x)) "%"))

(define (tmhtml-postscript l)
  (let ((s (first l)) (w (second l)) (h (third l)))
    (if (not (string? s)) '()	; only convert linked images
	`((h:img (@ (src ,(cork->utf8 s))
		    ,@(tmhtml-dimension-attr
		       w 'width `((par ,number->percent)
				  (px ,exact-number->string)))
		    ,@(tmhtml-dimension-attr
		       h 'height `((px ,exact-number->string)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-list-document list-doc)
  ;; Convert a list-document to a list of <h:li> elements.
  (define (item->li mark item)
    (cond ((null? item) '(h:li))
	  ((null? (cdr item)) `(h:li ,@(tmhtml (car item))))
	  (else `(h:li ,@(tmhtml `(document ,@item))))))
  (if (null? (cdr list-doc)) '((h:li))
      (stm-list-map item->li
		    (lambda (x) (== x '(item)))
		    (cdr list-doc))))

;; TODO: when the first data of the list is a label, it must be used to set the
;; ID attribute of the resulting xhtml element. When that is done, remove the
;; warning comment from htmltm-handler.

(define (tmhtml-itemize args)
  `((h:ul ,@(tmhtml-list-document (car args)))))

(define (tmhtml-enumerate args)
  `((h:ol ,@(tmhtml-list-document (car args)))))

(define (tmhtml-desc-document desc-doc)
  (define (item->dt-dd mark item)
    (let ((html-item (if (null? (cdr item))
			 (tmhtml (car item))
			 (tmhtml `(document ,@item)))))
      (append
       (if mark `((h:dt ,@(tmhtml (cadr mark)))) '())
       (cond ((and (null? html-item) mark) '())
	     ((null? html-item) '((h:dd)))
	     (else `((h:dd ,@html-item)))))))
  (if (null? (cdr desc-doc)) '((h:dd))
      (apply append (stm-list-map item->dt-dd
				  (lambda (x) (func? x 'item* 1))
				  (cdr desc-doc)))))

(define (tmhtml-description args)
  `((h:dl ,@(tmhtml-desc-document (car args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: 'verb' should really become an environment property

(define (tmhtml-verb x)
  (cond ((string? x) (tmhtml-string x))
	((func? x 'document 1) (tmhtml-verb (cadr x)))
	((func? x 'document)
	 (append (tmhtml-verb (cadr x))
		 (list "\n")
		 (tmhtml-verb `(document ,@(cddr x)))))
	((func? x 'concat)
	 (apply append (map tmhtml-verb (cdr x))))
	((== x '(next_line)) (list "\n"))
	((func? x 'with) (tmhtml-verb (cAr x)))
	((func? x 'surround 3)
	 (append (tmhtml-verb (cadr x))
		 (tmhtml-verb (cadddr x))
		 (tmhtml-verb (caddr x))))
	((func? x 'verbatim) (tmhtml-verb (cadr x)))
	((func? x 'em) `((h:em ,@(tmhtml-verb (cadr x)))))
	((func? x 'hlink) (tmhtml-hyperlink (cdr x)))
	((func? x 'key) (tmhtml-key (cdr x)))
	(else
	 ;(display* "Rejected " x "\n")
	 '())))

(define (tmhtml-verbatim l)
  (if (func? (car l) 'document)
      `((h:pre ,@(tmhtml-verb (car l))))
      `((h:tt ,@(tmhtml (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmdoc tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-make-block content)
  (let* ((l '(h:td
	      (@ (align "left"))
	      (h:img (@ (src "http://www.texmacs.org/Images/tm_gnu1b.png")))))
	 (c `(h:td
	      (@ (align "center") (width "100%"))
	      ,@(tmhtml content)))
	 (r '(h:td
	      (@ (align "right"))
	      (h:img (@ (src "http://www.texmacs.org/Images/tm_gnu2b.png")))))
	 (row `(h:tr ,l ,c ,r)))
    `(h:table (@ (width "100%") (cellspacing "0") (cellpadding "3")) ,row)))

(define (tmhtml-tmdoc-title l)
  (list `(h:div (@ (class "tmdoc-title-1")) ,(tmhtml-make-block (car l)))))

(define (tmhtml-tmdoc-title* l)
  (list `(h:div (@ (class "tmdoc-title-2")) ,(tmhtml-make-block (car l)))
	`(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (cadr l)))))

(define (tmhtml-tmdoc-title** l)
  (list `(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (car l)))
	`(h:div (@ (class "tmdoc-title-3")) ,(tmhtml-make-block (cadr l)))
	`(h:div (@ (class "tmdoc-navbar")) ,@(tmhtml (caddr l)))))

(define (tmhtml-tmdoc-flag l)
  ;(tmhtml (car l)))
  (list `(h:div (@ (class "tmdoc-flag")) ,@(tmhtml (car l)))))

(define (tmhtml-tmdoc-copyright* l)
  (if (null? l) l
      `(", " ,@(tmhtml (car l)) ,@(tmhtml-tmdoc-copyright* (cdr l)))))

(define (tmhtml-tmdoc-copyright l)
  (with content
      `("&copy; " ,@(tmhtml (car l))
	" " ,@(tmhtml (cadr l)) ,@(tmhtml-tmdoc-copyright* (cddr l)))
    (list `(h:div (@ (class "tmdoc-copyright")) ,@content))))

(define (tmhtml-tmdoc-license l)
  (list `(h:div (@ (class "tmdoc-license")) ,@(tmhtml (car l)))))

(define (tmhtml-key l)
  `((h:u (h:tt ,@(tmhtml (car l))))))

(define (tmhtml-tmdoc-bar? y)
  (or (func? y 'h:h1)
      (func? y 'h:h2)
      (and (func? y 'h:div)
	   (not (null? (cdr y)))
	   (func? (cadr y) '@ 1)
	   (== (first (cadadr y)) 'class)
	   (string-starts? (second (cadadr y)) "tmdoc"))))

(define (tmhtml-tmdoc-post-sub x)
  ;; FIXME: these rewritings are quite hacky;
  ;; better simplification would be nice...
  (cond ((and (func? x 'h:p) (list-find (cdr x) tmhtml-tmdoc-bar?)) (cdr x))
	((func? x 'h:p)
	 (with r (append-map tmhtml-tmdoc-post-sub (cdr x))
	   (if (== (cdr x) r) (list x) r)))
	(else (list x))))

(define (tmhtml-tmdoc-post body)
  (with r (append-map tmhtml-tmdoc-post-sub body)
    `((h:div (@ (class "tmdoc-body")) ,@r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-list l)
  (append-map tmhtml l))

(define (tmhtml-dispatch htable l)
  (let ((x (drd-ref ,htable (car l))))
    (cond ((not x) #f)
	  ((procedure? x) (x (cdr l)))
	  (else (tmhtml-post-simplify-element
		 (append x (tmhtml-list (cdr l))))))))

(define (tmhtml-implicit-compound l)
  (or (tmhtml-dispatch 'tmhtml-stdmarkup% l)
      (tmhtml-dispatch 'tmhtml-tables% l)
      '()))

(define (tmhtml x)
  ;; Main conversion function.
  ;; Takes a TeXmacs tree in Scheme notation and produce a SXML node-set.
  ;; All handler functions have a similar prototype.
  (if (string? x)
      (if (string-null? x) '() (tmhtml-string x)) ; handle string nodes
      (or (tmhtml-dispatch 'tmhtml-primitives% x)
	  (tmhtml-implicit-compound x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(drd-dispatcher tmhtml-primitives%
  (document tmhtml-document)
  (para tmhtml-paragraph)
  (surround tmhtml-surround)
  (concat tmhtml-concat)
  (format tmhtml-noop)
  (hspace tmhtml-vspace)
  (vspace* tmhtml-vspace)
  (vspace tmhtml-vspace)
  (space tmhtml-hspace)
  (htab tmhtml-hspace)
  (split tmhtml-noop)
  (move tmhtml-move)
  (resize tmhtml-resize)
  (float tmhtml-float)
  (repeat tmhtml-repeat)
  (datoms tmhtml-datoms)
  (dlines tmhtml-datoms)
  (dpages tmhtml-datoms)
  (dbox tmhtml-datoms)

  (with_limits tmhtml-noop)
  (line_break tmhtml-noop)
  (new_line tmhtml-new-line)
  (line_separator tmhtml-noop)
  (next_line tmhtml-next-line)
  (no_break tmhtml-noop)
  (no_first_indentation tmhtml-noop)
  (enable_first_indentation tmhtml-noop)
  (no_indentation_after tmhtml-noop)
  (enable_indentation_after tmhtml-noop)
  (page_break_before tmhtml-noop)
  (page_break tmhtml-noop)
  (no_page_break_before tmhtml-noop)
  (no_page_break_after tmhtml-noop)
  (new_page_before tmhtml-noop)
  (new_page tmhtml-noop)
  (new_double_page_before tmhtml-noop)
  (new_double_page tmhtml-noop)

  (group tmhtml-id)
  (left tmhtml-id)
  (mid tmhtml-id)
  (right tmhtml-id)
  (big tmhtml-big)
  (lprime tmhtml-id)
  (rprime tmhtml-id)
  (below tmhtml-below)
  (above tmhtml-above)
  (lsub tmhtml-sub)
  (lsup tmhtml-sup)
  (rsub tmhtml-sub)
  (rsup tmhtml-sup)
  (frac tmhtml-frac)
  (sqrt tmhtml-sqrt)
  (wide tmhtml-wide)
  (neg tmhtml-neg)
  ((:or tree old_matrix old_table old_mosaic old_mosaic_item)
   tmhtml-noop)
  (table (lambda (l) (tmhtml `(tabular (table ,@l)))))
  (tformat (lambda (l) (tmhtml `(tabular (tformat ,@l)))))
  ((:or twith cwith tmarker row cell sub_table) tmhtml-noop)

  (assign tmhtml-noop)
  (with tmhtml-with)
  ((:or set reset) tmhtml-noop)
  (compound tmhtml-compound)
  ((:or begin end include macro func env eval) tmhtml-noop)
  (value tmhtml-compound)
  (arg tmhtml-noop)
  ((:or backup quote delay hold release) tmhtml-noop)
  
  ((:or or xor and not plus minus times over div mod merge length range
	number date translate is_tuple look_up equal unequal less lesseq
	greater greatereq if case while extern authorize)
   tmhtml-noop)
  ((:or inactive symbol latex hybrid tuple collection associate) tmhtml-noop)
  (label tmhtml-label)
  (reference tmhtml-noop)
  (pageref tmhtml-noop)
  (write tmhtml-noop)
  (specific tmhtml-specific)
  (hlink tmhtml-hyperlink)
  (action tmhtml-action)
  ((:or tag meaning) tmhtml-noop)
  ((:or switch fold exclusive progressive superposed) tmhtml-noop)
  ((:or graphics point line arc bezier) tmhtml-noop)
  (postscript tmhtml-postscript)

  (!file tmhtml-file))

(drd-table tmhtml-stdmarkup%
  ;; special auxiliary tags
  (!left ,tmhtml-align-left)
  (!middle ,tmhtml-align-middle)
  (!right ,tmhtml-align-right)
  ;; Sectioning
  (chapter* (h:h1))
  (section* (h:h2))
  (subsection* (h:h3))
  (subsubsection* (h:h4))
  ;; paragraph and subparagraph are intented to be used at the start
  ;; of a paragraph. So they cannot be converted to 'h5' and 'h6
  (paragraph* (h:strong (@(class "paragraph"))))
  (subparagraph* (h:strong (@(class "subparagraph"))))
  ;; Lists
  ((:or itemize itemize-minus itemize-dot itemize-arrow)
   ,tmhtml-itemize)
  ((:or enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   ,tmhtml-enumerate)
  ((:or description description-compact description-dash
	description-align description-long)
   ,tmhtml-description)
  ;; Phrase elements
  (strong (h:strong))
  (em (h:em))
  (dfn (h:dfn))
  (code* (h:code))
  (samp (h:samp)) ; WARNING: semantic documentation does not match HTML4
  (kbd (h:kbd))
  (var (h:var))
  (abbr (h:abbr))
  (acronym (h:acronym))
  (verbatim ,tmhtml-verbatim)
  (code ,tmhtml-verbatim)
  ;; Presentation
  (tt (h:tt))
  ;; Names
  (TeXmacs ,(lambda x '("TeXmacs")))
  (TeX ,(lambda x '("TeX")))
  (LaTeX ,(lambda x '("LaTeX")))
  ;; tmdoc tags
  (tmdoc-title ,tmhtml-tmdoc-title)
  (tmdoc-title* ,tmhtml-tmdoc-title*)
  (tmdoc-title** ,tmhtml-tmdoc-title**)
  (tmdoc-flag ,tmhtml-tmdoc-flag)
  (tmdoc-copyright ,tmhtml-tmdoc-copyright)
  (tmdoc-license ,tmhtml-tmdoc-license)
  (key ,tmhtml-key)
  (hyper-link ,tmhtml-hyperlink))

;;    (name (h:name)) ; not in HTML4
;;    (person (h:person)))) ; not in HTML4

(drd-table tmhtml-with-cmd%
  (("font family" "tt") (h:tt))
  (("font family" "ss") (h:class (@ ("style" "font-family: sans-serif"))))
  (("font series" "bold") (h:b))
  (("font shape" "italic") (h:i))
  (("font" "roman") (h:class (@ ("style" "font-family: Times New Roman"))))
  (("font" "times") (h:class (@ ("style" "font-family: Times New Roman"))))
  (("font" "helvetica") (h:class (@ ("style" "font-family: Helvetica"))))
  (("font" "courier") (h:class (@ ("style" "font-family: Coutier"))))
  (("math font" "cal") (h:class (@ ("style" "font-family: Flemish Script"))))
  (("math font" "frak") (h:class (@ ("style" "font-family: Bernhard Modern"))))
  (("font" "roman") (h:class (@ ("style" "font-family: times"))))
  (("font series" "medium") (h:class (@ ("style" "font-weight: normal"))))
  (("font shape" "right") (h:class (@ ("style" "font-style: normal"))))
  (("font shape" "small-caps")
   (h:class (@ ("style" "font-variant: small-caps")))))

(drd-table tmhtml-with-cmd% ; deprecated
  ("font size" ,tmhtml-with-font-size)
  (("color" "black") (h:font (@ ("color" "black"))))
  (("color" "grey") (h:font (@ ("color" "grey"))))
  (("color" "white") (h:font (@ ("color" "white"))))
  (("color" "red") (h:font (@ ("color" "red"))))
  (("color" "blue") (h:font (@ ("color" "blue"))))
  (("color" "yellow") (h:font (@ ("color" "black"))))
  (("color" "magenta") (h:font (@ ("color" "magenta"))))
  (("color" "orange") (h:font (@ ("color" "orange"))))
  (("color" "green") (h:font (@ ("color" "green"))))
  (("color" "brown") (h:font (@ ("color" "brown"))))
  (("color" "dark magenta") (h:font (@ ("color" "#800080"))))
  (("color" "dark green") (h:font (@ ("color" "#008000"))))
  (("paragraph mode" "left") (h:div (@ ("align" "left"))))
  (("paragraph mode" "justify") (h:div (@ ("align" "justify"))))
  (("paragraph mode" "center") (h:center)))

(drd-table tmhtml-with-cmd% ; netscape4
  (("nr columns" "1") (h:multicol (@ ("cols" "1"))))
  (("nr columns" "2") (h:multicol (@ ("cols" "2"))))
  (("nr columns" "3") (h:multicol (@ ("cols" "3"))))
  (("nr columns" "4") (h:multicol (@ ("cols" "4"))))
  (("nr columns" "5") (h:multicol (@ ("cols" "5")))))

(drd-dispatcher tmhtml-tables%
  (block (lambda (l) (tmhtml-table "l" #t l)))
  (block* (lambda (l) (tmhtml-table "c" #t l)))
  (tabular (lambda (l) (tmhtml-table "l" #f l)))
  (tabular* (lambda (l) (tmhtml-table "c" #f l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs->html x)
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
	     (style* (tmfile-extract x 'style))
	     (style (if (list? style*) style* (list style*)))
	     (lan (tmfile-init x "language"))
	     (doc (list '!file body style lan (get-texmacs-path))))
	(texmacs->html doc))
      (begin
	(set! tmhtml-math-mode? #f)
	((if (func? x '!file)
	     tmhtml-finalize-document
	     tmhtml-finalize-selection)
	 (tmhtml x)))))
