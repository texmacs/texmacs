
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmhtml.scm
;; DESCRIPTION : conversion of TeXmacs trees into Html trees
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert html tmhtml)
  (:use (convert tools tmconcat)
	(convert mathml tmmath)
	(convert tools stm)
	(convert tools tmlength)
	(convert tools tmtable)
	(convert tools old-tmtable)
	(convert tools sxml)
	(convert tools sxhtml)
	(convert tools css)
	(convert html htmlout)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmhtml-env (make-ahash-table))
(define tmhtml-css? #t)
(define tmhtml-mathjax? #f)
(define tmhtml-mathml? #f)
(define tmhtml-images? #f)
(define tmhtml-image-serial 0)
(define tmhtml-image-cache (make-ahash-table))
(define tmhtml-image-root-url (unix->url "image"))
(define tmhtml-image-root-string "image")
(define tmhtml-site-version #f)
(define tmhtml-web-formats (list "gif" "jpg" "jpeg" "png" "bmp" "svg"))

(tm-define (tmhtml-initialize opts)
  (set! tmhtml-env (make-ahash-table))
  (set! tmhtml-css?
	(== (assoc-ref opts "texmacs->html:css") "on"))
  (set! tmhtml-mathjax?
	(== (assoc-ref opts "texmacs->html:mathjax") "on"))
  (set! tmhtml-mathml?
	(== (assoc-ref opts "texmacs->html:mathml") "on"))
  (set! tmhtml-images?
	(== (assoc-ref opts "texmacs->html:images") "on"))
  (set! tmhtml-image-cache (make-ahash-table))
  (let* ((suffix (url-suffix current-save-target))
	 (n (+ (string-length suffix) 1)))
    (if (in? suffix '("html" "xhtml" "htm"))
	(begin
	  (set! tmhtml-image-serial 0)
	  (set! tmhtml-image-root-url (url-unglue current-save-target n))
	  (set! tmhtml-image-root-string
		(url->unix (url-tail tmhtml-image-root-url))))
	(begin
	  (set! tmhtml-image-serial 0)
	  (set! tmhtml-image-root-url (unix->url "image"))
	  (set! tmhtml-image-root-string "image")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Empty handler and strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-noop l) '())

(define (cork->html s)
  (utf8->html (cork->utf8 s)))

(define (tmhtml-sub-token s pos)
  (with ss (substring s pos (- (string-length s) 1))
    (if (= (string-length ss) 1) ss
	(tmhtml-math-token (string-append "<" ss ">")))))

(define (tmhtml-math-token s)
  (cond ((= (string-length s) 1)
	 (cond ((== s "*") " ")
	       ((in? s '("+" "-" "=")) (string-append " " s " "))
	       ((char-alphabetic? (string-ref s 0)) `(h:var ,s))
	       (else s)))
	((string-starts? s "<cal-")
	 `(h:font (@ (face "Zapf Chancery")) ,(tmhtml-sub-token s 5)))
	((string-starts? s "<b-cal-")
	 `(h:u (h:font (@ (face "Zapf Chancery")) ,(tmhtml-sub-token s 7))))
	((string-starts? s "<frak-")
	 `(h:u ,(tmhtml-sub-token s 6)))
	((string-starts? s "<bbb-") `(h:u (h:b ,(tmhtml-sub-token s 5))))
	((string-starts? s "<up-") (tmhtml-sub-token s 4))
	((string-starts? s "<b-up-") `(h:b ,(tmhtml-sub-token s 6)))
	((string-starts? s "<b-") `(h:b (h:var ,(tmhtml-sub-token s 3))))
	((string-starts? s "<")
	 (with encoded (cork->utf8 s)
           (if (== s encoded)
             (utf8->html (old-tm->xml-cdata s))
             `(h:var ,(utf8->html encoded)))))
	(else s)))

(define (tmhtml-string s)
  (if (ahash-ref tmhtml-env :math)
      (tmhtml-post-simplify-nodes
       (map tmhtml-math-token (tmconcat-tokenize-math s)))
      (list (cork->html s))))

(define (tmhtml-text s)
  (if (or (ahash-ref tmhtml-env :math) (ahash-ref tmhtml-env :preformatted))
      (tmhtml-string s)
      (tmhtml-string (make-ligatures s))))

(define cork-endash (char->string (integer->char 21)))
(define cork-ldquo (char->string (integer->char 16)))
(define cork-rdquo (char->string (integer->char 17)))

(define (make-ligatures s)
  ;; Make texmacs ligatures in Cork encoding
  (string-replace
   (string-replace
    (string-replace s "--" cork-endash) "``" cork-ldquo) "''" cork-rdquo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entire documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-find-title doc)
  (cond ((npair? doc) #f)
	((func? doc 'doc-title 1) (cadr doc))
	((func? doc 'web-title 1) (cadr doc))
	((func? doc 'tmdoc-title 1) (cadr doc))
	((func? doc 'tmdoc-title* 2) (cadr doc))
	((func? doc 'tmdoc-title** 3) (caddr doc))
	((func? doc 'hidden-title 1) (cadr doc))
	(else (with title (tmhtml-find-title (car doc))
		(if title title
		    (tmhtml-find-title (cdr doc)))))))

(define (tmhtml-css-header)
  ;; TODO: return only used CSS properties
  (let ((html
	 (string-append
	  "body { text-align: justify } "
	  "h5 { display: inline; padding-right: 1em } "
	  "h6 { display: inline; padding-right: 1em } "
	  "table { border-collapse: collapse } "
	  "td { padding: 0.2em; vertical-align: baseline } "
          "dt { float: left; min-width: 1.75em; text-align: right; "
          "padding-right: 0.75em; font-weight: bold; } "
          "dd { margin-left: 2.75em; padding-bottom: 0.25em; } "
          "dd p { padding-top: 0em; } "
	  ".subsup { display: inline; vertical-align: -0.2em } "
	  ".subsup td { padding: 0px; text-align: left} "
	  ".fraction { display: inline; vertical-align: -0.8em } "
	  ".fraction td { padding: 0px; text-align: center } "
	  ".wide { position: relative; margin-left: -0.4em } "
	  ".accent { position: relative; margin-left: -0.4em; top: -0.1em } "
	  ".title-block { width: 100%; text-align: center } "
	  ".title-block p { margin: 0px } "
	  ".compact-block p { margin-top: 0px; margin-bottom: 0px } "
	  ".left-tab { text-align: left } "
	  ".center-tab { text-align: center } "
          ".balloon-anchor { border-bottom: 1px dotted #000000; outline: none;"
          " cursor: help; position: relative; } "
          ".balloon-anchor [hidden] { margin-left: -999em; position: absolute;"
	  " display: none; } "
          ".balloon-anchor: hover [hidden] { position: absolute; left: 1em;"
	  " top: 2em; z-index: 99; margin-left: 0;"
	  " width: 500px; display: inline-block; } "
          ".balloon-body { } "
	  ".ornament { border-width: 1px; border-style: solid;"
	  " border-color: black; display: inline-block; padding: 0.2em; } "
	  ".right-tab { float: right; position: relative; top: -1em; } "
	  ".no-breaks { white-space: nowrap; } "
	  ".underline { text-decoration: underline; } "
	  ".overline { text-decoration: overline; } "
	  ".strike-through { text-decoration: line-through; } "
	  "del { text-decoration: line-through wavy red; } "
	  ".fill-out { text-decoration: underline dotted; } "))
	(mathml "math { font-family: cmr, times, verdana } "))
    (if tmhtml-mathml? (string-append html mathml) html)))

(define (with-extract-sub w var post)
  (cond ((and (pair? w) (== (car w) 'with)
	      (pair? (cdr w)) (== (cadr w) var)
	      (pair? (cddr w)))
	 (post (caddr w)))
	((and (pair? w) (== (car w) 'with)
	      (pair? (cdr w)) (pair? (cddr w)))
	 (with-extract-sub `(with ,@(cdddr w)) var post))
	(else #f)))

(define (with-extract w var)
  (with-extract-sub w var tmhtml-force-string))

(define (with-extract* w var)
  (with post (lambda (x) (if (tm-func? x 'quote 1) (cadr x) x))
    (with-extract-sub w var post)))

(define (tmhtml-file l)
  ;; This handler is special:
  ;; Since !file is a special node used only at the top of trees
  ;; it produces a single node, and not a nodeset like other handlers.
  (let* ((doc (car l))
	 (styles (cdadr l))
	 (lang (caddr l))
	 (tmpath (cadddr l))
	 (title (tmhtml-find-title doc))
	 (css `(h:style (@ (type "text/css")) ,(tmhtml-css-header)))
	 (xhead '())
	 (body (tmhtml doc)))
    (set! tmhtml-site-version
          (with-extract doc "html-site-version"))
    (set! title
	  (cond ((with-extract doc "html-title")
                 (with-extract doc "html-title"))
		((and (not title) (with-extract doc "html-doc-title"))
                 (with-extract doc "html-doc-title"))
		((not title) "No title")
		((or (in? "tmdoc" styles)
                     (in? "tmweb" styles) (in? "tmweb2" styles))
		 `(concat ,(tmhtml-force-string title)
			  " (FSF GNU project)"))
		(else (tmhtml-force-string title))))
    (set! css
	  (cond ((with-extract doc "html-css")
		 `(h:link (@ (rel "stylesheet")
			     (href ,(with-extract doc "html-css"))
			     (type "text/css"))))
		(else css)))
    (if (with-extract doc "html-head-javascript-src")
	(let* ((src (with-extract doc "html-head-javascript-src"))
	       (script `(h:script (@ (language "javascript")
                                     (src ,src)))))
	  (set! xhead (append xhead (list script)))))
    (if (with-extract doc "html-head-javascript")
	(let* ((code (with-extract doc "html-head-javascript"))
	       (script `(h:script (@ (language "javascript")) ,code)))
	  (set! xhead (append xhead (list script)))))
    (if (with-extract doc "html-head-favicon")
	(let* ((code (with-extract doc "html-head-favicon"))
	       (icon `(h:link (@ (rel "icon") (href ,code)))))
	  (set! xhead (append xhead (list icon)))))
    (if (and (not (func? css 'h:link))
             (string-ends? (get-preference "texmacs->html:css-stylesheet")
                           ".css"))
        (with src (get-preference "texmacs->html:css-stylesheet")
          (with link-css `(h:link (@ (rel "stylesheet")
                                     (href ,src)
                                     (type "text/css")))
            (set! xhead (append xhead (list link-css))))))
    (if (tm-func? (with-extract* doc "html-extra-css") 'tuple)
        (for (src (cdr (with-extract* doc "html-extra-css")))
          (with link-css `(h:link (@ (rel "stylesheet")
                                     (href ,src)
                                     (type "text/css")))
            (set! xhead (append xhead (list link-css))))))
    (if (tm-func? (with-extract* doc "html-extra-javascript-src") 'tuple)
        (for (src (cdr (with-extract* doc "html-extra-javascript-src")))
          (with script `(h:script (@ (language "javascript")
                                     (src ,src)
                                     (defer "<implicit>")))
            (set! xhead (append xhead (list script))))))
    (if (tm-func? (with-extract* doc "html-extra-javascript") 'tuple)
        (for (code (cdr (with-extract* doc "html-extra-javascript")))
          (with script `(h:script (@ (language "javascript")
                                     (defer "<implicit>")) ,code)
            (set! xhead (append xhead (list script))))))
    (if tmhtml-mathjax?
	(let* ((site "https://cdn.jsdelivr.net/")
               (loc "npm/mathjax@3/es5/tex-mml-chtml.js")
               (src (string-append site loc))
	       (script `(h:script (@ (language "javascript") (src ,src)))))
	  (set! xhead (append xhead (list script)))))
    (if (or (in? "tmdoc" styles)
            (in? "tmweb" styles) (in? "tmweb2" styles)
            (in? "mmxdoc" styles) (in? "magix-web" styles)
            (in? "max-web" styles) (in? "node-web" styles))
	(set! body (tmhtml-tmdoc-post body)))
    (if tmhtml-css?
        (set! body (tmhtml-css-post body)))
    `(h:html
      (h:head
       (h:title ,@(tmhtml title))
       (h:meta (@ (charset "utf-8") (name "generator") 
		  (content ,(string-append "TeXmacs " (texmacs-version)))))
       ,css
       ,@xhead)
      (h:body ,@body))))

(define (tmhtml-finalize-document top)
  ;; @top must be a node produced by tmhtml-file
  "Prepare a XML document for serialization"
  (define xmlns-attrs
    '((xmlns "http://www.w3.org/1999/xhtml")
      (xmlns:m "http://www.w3.org/1998/Math/MathML")
      (xmlns:x "https://www.texmacs.org/2002/extensions")))
  (define doctype-list
    (let ((html       "-//W3C//DTD XHTML 1.1//EN")
          (mathml     "-//W3C//DTD XHTML 1.1 plus MathML 2.0//EN")
	  (html-drd   "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")
	  (mathml-drd (string-append
                        "http://www.w3.org/2002/04/xhtml-math-svg/"
                        "xhtml-math-svg.dtd")))
      (if tmhtml-mathml? (list mathml mathml-drd) (list html html-drd))))
  `(*TOP* (*PI* xml "version=\"1.0\" encoding=\"UTF-8\"")
	  (*DOCTYPE* html PUBLIC ,@doctype-list)
	  ,((cut sxml-set-attrs <> xmlns-attrs)
	    (sxml-strip-ns-prefix "h" (sxml-strip-ns-prefix "m" top)))))

(define (tmhtml-finalize-selection l)
  ;; @l is a nodeset produced by any handler _but_ tmhtml-file
  "Prepare a HTML node-set for serialization."
  `(*TOP* ,@(map (cut sxml-strip-ns-prefix "h" <>) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-document-elem x)
  ;; NOTE: this should not really be necessary, but it improves
  ;; the layout of verbatim environments with a missing block structure
  (if (and (list-2? x)
	   (or (== (car x) 'verbatim) (== (car x) 'code))
	   (not (func? (cadr x) 'document)))
      (tmhtml (list (car x) (list 'document (cadr x))))
      (tmhtml x)))

(define (tmhtml-compute-max-vspace l after?)
  (and (nnull? l)
    (with s1 (tmhtml-compute-vspace (car l) after?)
      (with s2 (tmhtml-compute-max-vspace (cdr l) after?)
	(cond ((not s1) s2)
	      ((not s2) s1)
	      (else
		(with l1 (string->tmlength s1)
		  (with l2 (string->tmlength s2)
		    (if (== (tmlength-unit l1) (tmlength-unit l2))
			(if (>= (tmlength-value l1) (tmlength-value l2)) s1 s2)
			s1 ;; FIXME: do something more subtle here
			)))))))))

(define (tmhtml-compute-vspace x after?)
  (cond ((and (not after?) (func? x 'vspace* 1)) (tmhtml-force-string (cadr x)))
	((and after? (func? x 'vspace 1)) (tmhtml-force-string (cadr x)))
	;;((and (not after?) (func? x 'document)) (tmhtml-compute-vspace (cadr x) #f))
	;;((and after? (func? x 'document)) (tmhtml-compute-vspace (cAr x) #t))
	((func? x 'concat) (tmhtml-compute-max-vspace (cdr x) after?))
	((func? x 'with) (tmhtml-compute-vspace (cAr x) after?))
	;;((func? x 'surround) (tmhtml-compute-max-vspace (cDdr x) after?))
	;;((func? x 'surround) (tmhtml-compute-max-vspace (cdr x) after?))
	(else #f)))

(define (tmhtml-div-merged-attrs x1 x2)
  (let* ((c1 (sxml-attr x1 'class))
         (c2 (sxml-attr x2 'class))
         (c  (if (and c1 c2) (string-append c1 " " c2) (or c1 c2)))
         (s1 (sxml-attr x1 'style))
         (s2 (sxml-attr x2 'style))
         (s  (if (and s1 s2) (css-merge-styles s1 s2) (or s1 s2)))
         (o? (lambda (x) (or (npair? x) (nin? (car x) '(style class)))))
         (other (list-filter (append (sxml-attr-list x1)
                                     (sxml-attr-list x2)) o?)))
    (append (if c (list (list 'class c)) (list))
            (if s (list (list 'style s)) (list))
            other)))

(define (tmhtml-simplify-div x)
  (or (and (func? x 'h:div)
           (with l1 (or (sxml-content x) (list))
             (and (== (length l1) 1)
                  (func? (car l1) 'h:div)
                  (let* ((l2 (or (sxml-content (car l1)) (list)))
                         (a  (tmhtml-div-merged-attrs x (car l1)))
                         (c1 (sxml-attr x 'class))
                         (c2 (sxml-attr (car l1) 'class)))
                    (and (not (and c1 c2))
                         `(h:div (@ ,@a) ,@l2))))))
      x))

(define (xhtml-block? x)
  (tm-in? x '(h:p h:div h:pre h:h1 h:h2 h:h3 h:h4 h:h5 h:h6
              h:ol h:ul h:dl h:table)))

(define (mixed-block l)
  (cond ((null? l) l)
        ((xhtml-block? (car l)) (cons (car l) (mixed-block (cdr l))))
        (else
          (let* ((i (or (list-find-index l xhtml-block?) (length l)))
                 (s (sublist l 0 i))
                 (r (mixed-block (sublist l i (length l)))))
            (cons `(h:div (@ (style "display: inline")) ,@s) r)))))

(define (as-block x)
  ;;(display* "--- " x "\n")
  (cond ((func? x 'h:div) (tmhtml-simplify-div x))
        ((and (func? x 'h:p)
              (not (forall? (negate xhtml-block?) (sxml-content x)))
              (not (forall? xhtml-block? (sxml-content x))))
         (let* ((a (sxml-attr-list x))
                (m (mixed-block (sxml-content x))))
           (if (null? a) `(h:p ,@m) `(h:p (@ ,@a) ,@m))))
        (else x)))

(define (force-block? x)
  (or (and (tm-in? x '(h:p h:div h:pre h:h1 h:h2 h:h3 h:h4
                       h:ol h:ul h:dl h:table))
           (not (and-with style (sxml-attr x 'style)
                  (string-contains? style "display: inline")))
           (not (and-with class (sxml-attr x 'class)
                  (string-contains? class "-license"))))
      (and (tm-in? x '(h:i h:b h:u h:var h:font h:class))
           ;; FIXME: we should really restructure this kind of Html output
           ;; such that these tags never contain block content
           (exists? force-block? (sxml-content x)))))

(define (as-blocks l)
  (cond ((null? l) l)
        ((and (func? (car l) 'h:p) (list-find (cdar l) force-block?))
         (let* ((sl (cdar l))
                (i  (list-find-index sl force-block?))
                (l1 (sublist sl 0 i))
                (x2 (list-ref sl i))
                (l2 (sublist sl (+ i 1) (length sl)))
                (h  (if (null? l1) (list x2) (list `(h:p ,@l1) x2)))
                (t  (if (null? l2) (cdr l) (cons `(h:p ,@l2) (cdr l)))))
           (append (map as-block h) (as-blocks t))))
        (else (cons (as-block (car l))
                    (as-blocks (cdr l))))))

(define (add-style-attr x s1 s2)
  (with s (or (and s1 s2 (string-append s1 "; " s2)) s1 s2)
    (cond ((not s) x)
          ((and (xhtml-block? x) (null? (sxml-attr-list x)))
           `(,(car x) (@ (style ,s)) ,@(cdr x)))
          ((and (xhtml-block? x) (not (sxml-attr x 'style)))
           `(,(car x) (@ (style ,s) ,@(sxml-attr-list x)) ,@(sxml-content x)))
          ((func? x 'h:p)
           `(h:p (@ (style ,s)) (h:div ,(cdr x))))
          (else `(h:div (@ (style ,s)) ,x)))))

(define (tmhtml-p x)
  (let* ((body (tmhtml-document-elem x))
         (bl (as-blocks (list `(h:p ,@body))))
	 (l1 (tmhtml-compute-vspace x #f))
	 (l2 (tmhtml-compute-vspace x #t))
	 (h1 (and l1 (tmlength->htmllength l1 #t)))
	 (h2 (and l2 (tmlength->htmllength l2 #t)))
	 (s1 (and h1 (string-append "margin-top: " h1)))
	 (s2 (and h2 (string-append "margin-bottom: " h2))))
    ;;(display* "  <<<<< " body "\n")
    ;;(display* "  >>>>> " bl "\n")
    (cond ((null? bl) bl)
          ((null? (cdr bl)) (list (add-style-attr (car bl) s1 s2)))
          (else (append (list (add-style-attr (car bl) s1 #f))
                        (cdr (cDr bl))
                        (list (add-style-attr (cAr bl) #f s2)))))))

(define (tmhtml-document l)
  (cond ((null? l) '())
	((ahash-ref tmhtml-env :preformatted)
	 (tmhtml-post-simplify-nodes
	  (list-concatenate
	   ((cut list-intersperse <> '("\n"))
	    (map tmhtml l)))))
	(else
          (with pars (map tmhtml-p l)
            ;;(display* "===== Got  " l "\n")
            ;;(display* "----- Post " (apply append pars) "\n\n")
            (apply append pars)))))

(define (tmhtml-paragraph l)
  (let rec ((l l))
    (if (null? l) '()
	(let ((h (tmhtml (car l)))
	      (r (rec (cdr l))))
	  (cond ((null? h) r)		; correct when r is null too
		((null? r) h)
		(else `(,@h (h:br) ,@r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Surrounding block structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define document-done '())
(define concat-done '())

(define (serialize-print x)
  (set! concat-done (cons x concat-done)))

(define (serialize-paragraph x)
  (serialize-concat x)
  (with l (tmconcat-simplify (reverse concat-done))
    (set! document-done (cons (cons 'concat l) document-done))
    (set! concat-done '())))

(define (serialize-concat x)
  (cond ((in? x '("" (document) (concat))) (noop))
	((func? x 'document)
	 (for-each serialize-paragraph (cDdr x))
	 (serialize-concat (cAr x)))
	((func? x 'concat)
	 (for-each serialize-concat (cdr x)))
	((func? x 'surround 3)
	 (serialize-concat (cadr x))
	 (serialize-concat (cadddr x))
	 (serialize-concat (caddr x)))
        ((func? x 'with 1) (serialize-concat (cadr x)))
        ((and (func? x 'with)
              (in? cadr (list "locus-color" "visited-color")))
         (serialize-concat `(with ,@(cdddr x))))
	((func? x 'with)
	 (let* ((r (simplify-document (cAr x)))
		(w (lambda (y) `(with ,@(cDdr x) ,y))))
	   (if (not (func? r 'document))
	       (serialize-print (w r))
	       (let* ((head (cadr r))
		      (body `(document ,@(cDr (cddr r))))
		      (tail (cAr r)))
		 (serialize-paragraph (w head))
                 (for (x (cDr (cddr r)))
                   (serialize-paragraph (w x)))
                 ;;(when (nnull? (cdr body))
                 ;;  (set! document-done (cons (w body) document-done)))
		 (serialize-concat (w tail))))))
        ((func? x 'locus) (serialize-concat (cAr x)))
	(else (serialize-print x))))

(define (simplify-document x)
  (with-global document-done '()
    (with-global concat-done '()
      (serialize-paragraph x)
      (if (list-1? document-done)
	  (car document-done)
	  (cons 'document (reverse document-done))))))

(define (block-document? x)
  (cond ((func? x 'document) #t)
	((func? x 'concat) (list-any block-document? (cdr x)))
	((func? x 'surround 3) (block-document? (cAr x)))
	((func? x 'with) (block-document? (cAr x)))
	(else #f)))

(define (blockify x)
  (cond ((func? x 'document) x)
	((or (func? x 'surround 3) (func? x 'with))
	 (rcons (cDr x) (blockify (cAr x))))
	(else `(document ,x))))

(define (tmhtml-surround l)
  (let* ((r1 `(surround ,@l))
	 (r2 (simplify-document r1))
	 (f? (and (block-document? r1) (not (func? r2 'document))))
	 (r3 (if f? (blockify r2) r2)))
    (tmhtml r3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-glue-scripts l)
  (cond ((or (null? l) (null? (cdr l))) l)
	((and (func? (car l) 'rsub 1) (func? (cadr l) 'rsup 1))
	 (cons `(rsubsup ,(cadar l) ,(cadadr l))
	       (tmhtml-glue-scripts (cddr l))))
	(else (cons (car l) (tmhtml-glue-scripts (cdr l))))))

(define (heading? l)
  (cond ((null? l) #f)
	((sxhtml-label? (car l)) (heading? (cdr l)))
	((sxhtml-heading? (car l)) #t)
	(else #f)))

(define (tmhtml-post-heading l)
  ;; Post-process the converted result of a concat containing a section title.
  ;;
  ;; Any label preceding the section is moved after it.
  ;;
  ;; The first label after the section is changed to an 'id' attribute in the
  ;; heading element, if it has not already an 'id' attribute.
  ;;
  ;; NOTE: assumes the heading is the first node (not counting labels)
  (receive (labels-before rest) (list-span l sxhtml-label?)
    (receive (heading rest) (car+cdr rest)
      (if (sxml-attr heading 'id)
	  `(,heading ,@labels-before ,@rest)
	  (receive (labels-after rest) (list-partition rest sxhtml-label?)
	    (let ((labels (append labels-before labels-after)))
	      (if (null? labels) l
		  (cons (sxml-prepend (sxhtml-glue-label heading (car labels))
				      (cdr labels))
			rest))))))))

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

(define (tmhtml-concat l)
  (set! l (tmhtml-glue-scripts l))
  ;;(display* "l << " l "\n")
  (set! l (tmconcat-structure-tabs l))
  ;;(display* "l >> " l "\n")
  (tmhtml-post-simplify-nodes
   (let ((l (tmhtml-list l)))
     (cond ((null? l) '())
	   ((string? (car l)) l)
	   ((heading? l) (tmhtml-post-heading l))
	   ((list-any sxhtml-table? l) (tmhtml-post-table l))
	   ((and (null? (cdr l)) (pair? (car l))
		 (== (caar l) 'h:div) (== (cadar l) '(@ (class "left-tab"))))
	    (cddar l))
	   (else l)))))

(define (tmhtml-align-left l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "left-tab")) ,@r)))))

(define (tmhtml-align-middle l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "center-tab")) ,@r)))))

(define (tmhtml-align-right l)
  (with r (tmhtml-concat l)
    (if (in? r '(() (""))) '()
	`((h:div (@ (class "right-tab")) ,@r)))))

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

(define (tmhtml-hidden l)
  ;; FIXME: distinguish inline and block?
  (with r (tmhtml (car l))
    `((div (@ (class "toggle") (style "display: none")) ,@r))))

(define (tmhtml-shown l)
  ;; FIXME: distinguish inline and block?
  (with r (tmhtml (car l))
    `((div (@ (class "toggle") (style "display: block")) ,@r))))

(define (tmhtml-hspace l)
  (with len (tmlength->htmllength (if (list-1? l) (car l) (cadr l)) #t)
    (if (not len) '()
	`((h:span (@ (style ,(string-append "margin-left: " len))))))))

(define (tmhtml-vspace l)
  '())

(define (tmhtml-move l)
  (tmhtml (car l)))

(define (tmhtml-resize l)
  (tmhtml (car l)))

(define (tmhtml-float l)
  (let* ((type (tmhtml-force-string (car l)))
         (cl (if (== type "") "unknown-float" type))
         (r (tmhtml (cAr l))))
    `((div (@ (class ,cl)) ,@r))))

(define (tmhtml-repeat l)
  (tmhtml (car l)))

(define (tmhtml-underline l)
   `((h:span (@ (class "underline")) ,@(tmhtml (cAr l)) )))

(define (tmhtml-overline l)
   `((h:span (@ (class "overline")) ,@(tmhtml (cAr l)) )))

(define (tmhtml-strike-through l)
   `((h:span (@ (class "strike-through")) ,@(tmhtml (cAr l)) )))
   
(define (tmhtml-deleted l)
   `((h:del ,@(tmhtml (cAr l)) )))

(define (tmhtml-marked l)
   `((h:mark ,@(tmhtml (cAr l)) )))
   
(define (tmhtml-fill-out l)
   `((h:span (@ (class "fill-out")) ,@(tmhtml (cAr l)) )))

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

(define (tmhtml-subsup l)
  (let* ((sub (tmhtml (car l)))
	 (sup (tmhtml (cadr l)))
	 (r1 `(h:tr (h:td ,@sup)))
	 (r2 `(h:tr (h:td ,@sub))))
    `((h:sub (h:table (@ (class "subsup")) ,r1 ,r2)))))

;;(define (tmhtml-frac l)
;;  (let* ((num (tmhtml (car l)))
;;	 (den (tmhtml (cadr l))))
;;    `("frac (" ,@num ", " ,@den ")")))

(define (tmhtml-frac l)
  (let* ((num (tmhtml (car l)))
	 (den (tmhtml (cadr l)))
	 (n `(h:tr (h:td (@ (style "border-bottom: solid 1px")) ,@num)))
	 (d `(h:tr (h:td ,@den))))
    `((h:table (@ (class "fraction")) ,n ,d))))

(define (tmhtml-sqrt l)
  (if (= (length l) 1)
      `("sqrt (" ,@(tmhtml (car l)) ")")
      `("sqrt" (h:sub ,@(tmhtml (cadr l)))
	" (" ,@(tmhtml (car l)) ")")))

(define (tmhtml-short? l)
  (and (list-1? l)
       (or (string? (car l))
	   (and (func? (car l) 'h:i) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:b) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:u) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:var) (tmhtml-short? (cdar l)))
	   (and (func? (car l) 'h:font) (tmhtml-short? (cdar l))))))

(define (tmhtml-wide l)
  (let* ((body (tmhtml (car l)))
	 (acc (tmhtml (cadr l)))
	 (class (if (in? acc '(("^") ("~"))) "accent" "wide")))
    (if (tmhtml-short? body)
	`(,@body (h:sup (@ (class ,class)) ,@acc))
	`("(" ,@body ")" (h:sup ,@acc)))))

(define (tmhtml-neg l)
  `("not(" ,@(tmhtml (car l)) ")"))

(define (tmhtml-tree l)
  (tmhtml-png `(tree ,@l)))

(define (tmhtml-syntax l)
  (tmhtml (car l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shape conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmshape->htmllength x)
  (if (== (tmhtml-force-string x) "rounded") "15px" "0px"))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmcolor->htmlcolor x)
  (with s (tmhtml-force-string x)
    (cond ((== s "light grey") "#d0d0d0")
	  ((== s "pastel grey") "#dfdfdf")
	  ((== s "dark grey") "#707070")
	  ((== s "dark red") "#800000")
	  ((== s "dark green") "#008000")
	  ((== s "dark blue") "#000080")
	  ((== s "dark yellow") "#808000")
	  ((== s "dark magenta") "#800080")
	  ((== s "dark cyan") "#008080")
	  ((== s "dark orange") "#804000")
	  ((== s "dark brown") "#401000")
	  ((== s "broken white") "#ffffdf")
	  ((== s "pastel red") "#ffdfdf")
	  ((== s "pastel green") "#dfffdf")
	  ((== s "pastel blue") "#dfdfff")
	  ((== s "pastel yellow") "#ffffdf")
	  ((== s "pastel magenta") "#ffdfff")
	  ((== s "pastel cyan") "#dfffff")
	  ((== s "pastel orange") "#ffdfbf")
	  ((== s "pastel brown") "#dfbfbf")
	  (else s))))

(define (tmbgcolor->htmlbgcolor x)
  (with s (tmhtml-force-string x)
    (cond ((== s "light grey") "#80808060")
	  ((== s "pastel grey") "#80808040")
	  ((== s "broken white") "#ffff0020")
	  ((== s "pastel red") "#ff000020")
	  ((== s "pastel green") "#00ff0020")
	  ((== s "pastel blue") "#0000ff20")
	  ((== s "pastel yellow") "#ffff0020")
	  ((== s "pastel magenta") "#ff00ff20")
	  ((== s "pastel cyan") "#00ffff20")
	  ((== s "pastel orange") "#ff800040")
	  ((== s "pastel brown") "#80000040")
	  ((== s "#c0c0ff") "#8080ff80")
	  ((== s "#c0ffc0") "#80ff8080")
	  ((== s "#ffc0c0") "#ff808080")
	  ((== s "#aaf") "#0000ff55")
	  ((== s "#afa") "#00ff0055")
	  ((== s "#faa") "#ff000055")
	  ((== s "#ffa") "#ffff0055")
	  ((== s "#faf") "#ff00ff55")
	  ((== s "#aff") "#00ffff55")
	  ((== s "#fa6") "#ff800099")
          ((== s "") "#00000000")
	  (else (tmcolor->htmlcolor x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Length conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table tmhtml-length-table
  ("mm" . 0.1)
  ("cm" . 1.0)
  ("in" . 2.54)
  ("pt" . 3.514598e-2)
  ("tmpt" . 2.7457797e-5)
  ("fn" . 0.4)
  ("em" . 0.4)
  ("ex" . 0.2)
  ("spc" . 0.2)
  ("pc" . 0.42175)
  ("par" . 16)
  ("pag" . 12)
  ("px" . 0.025)
  ("ln" . 0.025)
  ("%" . 1)
  ("@" . 1))

(define (make-exact x)
  (number->string (inexact->exact x)))

(define (number->htmlstring x)
  (number->string (if (exact? x)
		      (if (integer? x) x (exact->inexact x))
		      (if (and (integer? (inexact->exact x))
			       (= x (exact->inexact (inexact->exact x))))
			  (inexact->exact x) x))))

(define (tmlength->htmllength len . css?)
  (if (list>0? css?) (set! css? (car css?)) (set! css? #t))
  (and-let* ((s (tmhtml-force-string len))
             (len-str (if (string->number s) (string-append s "tmpt") s))
	     (tmlen (string->tmlength len-str))
	     (dummy2? (not (tmlength-null? tmlen)))
	     (val (tmlength-value tmlen))
	     (unit (symbol->string (tmlength-unit tmlen)))
	     (incm (ahash-ref tmhtml-length-table unit))
	     (cmpx (/ 1 (ahash-ref tmhtml-length-table "px"))))
    (cond ((== unit "px") (number->htmlstring val))
	  ((in? unit '("par" "pag"))
	   (string-append (number->htmlstring (/ (round (* val 10000)) 100)) "%"))
	  ((and css? (== unit "tmpt"))
	   (string-append (number->htmlstring (* cmpx val incm)) "px"))
	  ((and css? (== unit "fn"))
	   (string-append (number->htmlstring val) "em"))
	  ((and css? (== unit "spc"))
	   (string-append (number->htmlstring (/ val 2)) "em"))
	  ((and css? (== unit "ln"))
	   (string-append (number->htmlstring val) "px"))
	  ((and css? (== unit "@")) "auto")
	  (css? len)
	  (else (number->htmlstring (* cmpx val incm))))))

(define (tmlength->px len)
  (and-let* ((tmlen (string->tmlength len))
	     (dummy? (not (tmlength-null? tmlen)))
	     (val (tmlength-value tmlen))
	     (unit (symbol->string (tmlength-unit tmlen)))
	     (incm (ahash-ref tmhtml-length-table unit))
	     (cmpx (/ 1 (ahash-ref tmhtml-length-table "px"))))
    (* cmpx val incm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local environment changes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-with-mode val arg)
  (ahash-with tmhtml-env :math (== val "math")
    (tmhtml (if (== val "prog") `(verbatim ,arg) arg))))

(define (tmhtml-with-math-display val arg)
  (ahash-with tmhtml-env :math-display (== val "true")
    (tmhtml arg)))

(define (tmhtml-with-color val arg)
  `((h:font (@ (color ,(tmcolor->htmlcolor val))) ,@(tmhtml arg))))

(define (tmhtml-with-font-size val arg)
  (ahash-with tmhtml-env :mag val
    (let* ((x (* (string->number val) 100))
           (c (string-append "font-size: " (number->string x) "%"))
	   (s (cond ((< x 1) "-4") ((< x 55) "-4") ((< x 65) "-3")
		    ((< x 75) "-2") ((< x 95) "-1") ((< x 115) "0")
		    ((< x 135) "+1") ((< x 155) "+2") ((< x 185) "+3")
		    ((< x 225) "+4") ((< x 500) "+5") (else "+5"))))
      (cond (tmhtml-css? `((h:font (@ (style ,c)) ,@(tmhtml arg))))
            (s `((h:font (@ (size ,s)) ,@(tmhtml arg))))
            (else (tmhtml arg))))))

(define (tmhtml-with-block style arg)
  (with r (tmhtml (blockify arg))
    (if (in? r '(() ("") ((h:p)) ((h:p "")))) '()
	`((h:div (@ (style ,style)) ,@r)))))

(define (tmhtml-with-par-left val arg)
  (with x (tmlength->px val)
    (if (not x) (tmhtml arg)
	(with d (- x (ahash-ref tmhtml-env :left-margin))
	  (with s (string-append "margin-left: " (number->htmlstring d) "px")
	    (ahash-with tmhtml-env :left-margin x
	      (tmhtml-with-block s arg)))))))

(define (tmhtml-with-par-right val arg)
  (with x (tmlength->px val)
    (if (not x) (tmhtml arg)
	(with d (- x (ahash-ref tmhtml-env :right-margin))
	  (with s (string-append "margin-right: " (number->htmlstring d) "px")
	    (ahash-with tmhtml-env :right-margin x
	      (tmhtml-with-block s arg)))))))

(define (tmhtml-with-par-first val arg)
  (with x (tmlength->htmllength val #t)
    (if (not x) (tmhtml arg)
	(with s (string-append "text-indent: " x)
	  (tmhtml-with-block s arg)))))

(define (tmhtml-with-par-par-sep val arg)
  (with x (tmlength->px val)
    (if (and x (== (inexact->exact x) 0))
	`((h:div (@ (class "compact-block")) ,@(tmhtml arg)))
	(tmhtml arg))))

(define (tmhtml-with-one var val arg)
  (cond ((logic-ref tmhtml-with-cmd% (list var val)) =>
	 (lambda (w) (list (append w (tmhtml arg)))))
	((logic-ref tmhtml-with-cmd% (list var)) =>
	 (lambda (x) (ahash-with tmhtml-env x val (tmhtml arg))))
	((logic-ref tmhtml-with-cmd% var) =>
	 (lambda (h) (h val arg)))
	(else (tmhtml arg))))

(define (tmhtml-force-string x)
  (cond ((string? x) x)
	((func? x 'quote 1) (tmhtml-force-string (cadr x)))
	((func? x 'tmlen 1)
	 (string-append (tmhtml-force-string (cadr x)) "tmpt"))
	((func? x 'tmlen 3)
	 (string-append (tmhtml-force-string (caddr x)) "tmpt"))
	((func? x 'tuple)
         (apply string-append (list-intersperse
                                (map tmhtml-force-string (cdr x)) ";")))
	;;(else (force-string x))))
	(else (texmacs->code x "utf-8"))))

(define (tmhtml-with l)
  (cond ((null? l) '())
	((null? (cdr l)) (tmhtml (car l)))
	((null? (cddr l)) '())
	((func? (cAr l) 'graphics) (tmhtml-png (cons 'with l)))
	(else
	 (let* ((var (tmhtml-force-string (car l)))
		(val (tmhtml-force-string (cadr l)))
		(next (cddr l)))
	   (tmhtml-with-one var val `(with ,@next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other macro-related primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-compound l)
  ;; Explicit expansions are converted and handled as implicit expansions.
  (tmhtml-implicit-compound (cons (string->symbol (car l)) (cdr l))))

(define (tmhtml-mark l)
  ;; Explicit expansions are converted and handled as implicit expansions.
  (tmhtml (cadr l)))

(define (tmhtml-include l)
  (if (or (npair? l) (nstring? (car l))) '()
      (with u (url-relative current-save-source (unix->url (car l)))
	(if (not (url-exists? u)) '()
	    (with-global current-save-source u
	      (with t (tree-inclusion u)
		(tmhtml (tm->stree t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (blue sym)
  `(h:font (@ (color "blue")) ,sym))

(define (tmhtml-src-args l)
  (if (null? l) l
      `(,(blue "|")
	,@(tmhtml (car l))
	,@(tmhtml-src-args (cdr l)))))

(define (tmhtml-inline-tag l)
  `(,(blue "&lt;")
    ,@(tmhtml (car l))
    ,@(tmhtml-src-args (cdr l))
    ,(blue "&gt;")))

(define (tmhtml-open-tag l)
  `(,(blue "&lt;\\")
    ,@(tmhtml (car l))
    ,@(tmhtml-src-args (cdr l))
    ,(blue "|")))

(define (tmhtml-middle-tag l)
  `(,@(tmhtml-src-args (cdr l))
    ,(blue "|")))

(define (tmhtml-close-tag l)
  `(,@(tmhtml-src-args (cdr l))
    ,(blue "&gt;")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-label l)
  ;; WARNING: bad conversion if ID is not a string.
  `((h:a (@ (id ,(cork->html (force-string (car l))))))))

;(define (tmhtml-reference l)
;  (list 'ref (cork->html (force-string (car l)))))

;(define (tmhtml-pageref l)
;  (list 'pageref (cork->html (force-string (car l)))))

(define (tmhtml-suffix s)
  ;; Change .tm suffix to .xhtml suffix for local files for correct
  ;; conversion of entire web-sites. We might create an option
  ;; in order to disable this suffix change
  (let* ((sdir (string-rindex s #\/))
	 (sep (string-rindex s #\#)))
    (cond ((or (string-starts? s "http:")
               (string-starts? s "https:")
               (string-starts? s "ftp:")) s)
          ((and sep (or (not sdir) (< sdir sep)))
	   (string-append (tmhtml-suffix (substring s 0 sep))
			  (string-drop s sep)))
	  ((string-ends? s ".tm")
	   (string-append (string-drop-right s 3)
			  (if tmhtml-mathml? ".xhtml" ".html")))
	  ((string-ends? s ".texmacs")
	   (string-append (string-drop-right s 8) ".tm"))
	  (else s))))

(define (tmhtml-hyperlink l)
  ;; WARNING: bad conversion if URI is not a string.
  ;; TODO: change label at start of content into ID attribute, move other
  ;; labels out (A elements cannot be nested!).
  (let* ((body (tmhtml (first l)))
	 (to (cork->html (force-string (second l)))))
    (if (string-starts? to "$")
	body ;; temporary fix for URLs like $TEXMACS_PATH/...
	`((h:a (@ (href ,(tmhtml-suffix to))) ,@body)))))

(define (tmhtml-specific l)
  (cond ((== (car l) "html") (list (tmstring->string (force-string (cadr l)))))
	((== (car l) "html*") (tmhtml (cadr l)))
	((== (car l) "image") (tmhtml-png (cadr l)))
	(else '())))

(define (tmhtml-action l)
  (if tmhtml-css?
      (tmhtml (car l))
      `((h:u ,@(tmhtml (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map* fun l)
  (list-filter (map fun l) identity))

(define (html-css-attrs l)
  ;; l is a list of either key-value lists (XML) or strings (CSS)
  ;; we return a list with the corresponding @-style attribute
  (if (null? l) '()
      (receive (css html) (list-partition l string?)
	(if (nnull? css)
	    (with style (apply string-append (list-intersperse css "; "))
	      (set! html (cons `(style ,style) html))))
	`((@ ,@html)))))

(define (html-css-pattern x)
  (let* ((u (tmhtml-force-string (cadr x)))
         (w (or (tmlength->htmllength (caddr x) #t) "100%"))
         (h (or (tmlength->htmllength (cadddr x) #t) "100%"))
         (im (string-append "background: url('" u "')"))
         (sz (string-append "background-size: " w " " h)))
    (string-append im "; " sz)))

(define (length-attr what x . opt)
  (with len (tmlength->htmllength x #t)
    (and len (apply string-append (cons* what ": " len opt)))))

(define (border-attr what x)
  (length-attr what x " solid"))

(define (tmhtml-make-cell-attr x all)
  (cond ((== (car x) "cell-width") (length-attr "width" (cadr x)))
	((== (car x) "cell-height") (length-attr "height" (cadr x)))
	((== x '("cell-halign" "l")) "text-align: left")
	((== x '("cell-halign" "c")) "text-align: center")
	((== x '("cell-halign" "r")) "text-align: right")
	((== x '("cell-valign" "t")) "vertical-align: top")
	((== x '("cell-valign" "c")) "vertical-align: middle")
	((== x '("cell-valign" "b")) "vertical-align: bottom")
	((== x '("cell-valign" "B")) "vertical-align: baseline")
	((== (car x) "cell-background")
         (cond ((not tmhtml-css?)
                `(bgcolor ,(tmcolor->htmlcolor (cadr x))))
               ((tm-atomic? (cadr x))
                (string-append "background-color: "
                               (tmbgcolor->htmlbgcolor (cadr x))))
               ((tm-func? (cadr x) 'pattern 3)
                (html-css-pattern (cadr x)))
               (else #f)))
	((== (car x) "cell-lborder") (border-attr "border-left" (cadr x)))
	((== (car x) "cell-rborder") (border-attr "border-right" (cadr x)))
	((== (car x) "cell-tborder") (border-attr "border-top" (cadr x)))
	((== (car x) "cell-bborder") (border-attr "border-bottom" (cadr x)))
	((== (car x) "cell-lsep") (length-attr "padding-left" (cadr x)))
	((== (car x) "cell-rsep") (length-attr "padding-right" (cadr x)))
	((== (car x) "cell-tsep") (length-attr "padding-top" (cadr x)))
	((== (car x) "cell-bsep") (length-attr "padding-bottom" (cadr x)))
	((== x '("cell-block" "no")) "white-space: nowrap")
	((== x '("cell-block" "yes")) #f)
	((== x '("cell-block" "auto"))
         (if (or (in? '("cell-hyphen" "t") all)
                 (in? '("cell-hyphen" "c") all)
                 (in? '("cell-hyphen" "b") all))
             #f
             "white-space: nowrap"))
	(else #f)))

(define (tmhtml-make-cell c cellf)
  (if (not (tm-func? c 'cell 1)) (set! c `(cell ,c)))
  (ahash-with tmhtml-env :left-margin 0
    (with make (lambda (attr) (tmhtml-make-cell-attr attr cellf))
      `(h:td ,@(html-css-attrs (map* make cellf))
             ,@(tmhtml (cadr c))))))

(define (tmhtml-make-cells-bis l cellf)
  (if (null? l) l
      (cons (tmhtml-make-cell (car l) (car cellf))
	    (tmhtml-make-cells-bis (cdr l) (cdr cellf)))))

(define (tmhtml-width-part attrl)
  (cond ((null? attrl) 0)
	((== (caar attrl) "cell-hpart") (string->number (cadar attrl)))
	(else (tmhtml-width-part (cdr attrl)))))

(define (tmhtml-width-replace attrl sum)
  (with part (tmhtml-width-part (reverse attrl))
    (if (== part 0) attrl
	(with l (list-filter attrl (lambda (x) (!= (car x) "cell-width")))
	  (with w (number->htmlstring (/ part sum))
	    (cons (list "cell-width" (string-append w "par")) l))))))

(define (tmhtml-make-cells l cellf)
  (let* ((partl (map tmhtml-width-part (map reverse cellf)))
	 (sum (apply + partl)))
    (if (!= sum 0) (set! cellf (map (cut tmhtml-width-replace <> sum) cellf)))
    (tmhtml-make-cells-bis l cellf)))

(define (tmhtml-make-row-attr x all)
  (tmhtml-make-cell-attr x all))

(define (tmhtml-make-row r rowf cellf)
  `(h:tr ,@(html-css-attrs (map* (cut tmhtml-make-row-attr <> rowf) rowf))
	 ,@(tmhtml-make-cells (cdr r) cellf)))

(define (tmhtml-make-rows l rowf cellf)
  (if (null? l) l
      (cons (tmhtml-make-row  (car l) (car rowf) (car cellf))
	    (tmhtml-make-rows (cdr l) (cdr rowf) (cdr cellf)))))

(define (tmhtml-make-column-attr x all)
  (tmhtml-make-cell-attr x all))

(define (tmhtml-make-col colf)
  `(h:col ,@(html-css-attrs (map* (cut tmhtml-make-column-attr <> colf) colf))))

(define (tmhtml-make-column-group colf)
  (if (list-every null? colf) '()
      `((h:colgroup ,@(map tmhtml-make-col colf)))))

(define (tmhtml-make-table-attr x)
  (cond ((== (car x) "table-width") (length-attr "width" (cadr x)))
	((== (car x) "table-height") (length-attr "height" (cadr x)))
	((== (car x) "table-lborder") (border-attr "border-left" (cadr x)))
	((== (car x) "table-rborder") (border-attr "border-right" (cadr x)))
	((== (car x) "table-tborder") (border-attr "border-top" (cadr x)))
	((== (car x) "table-bborder") (border-attr "border-bottom" (cadr x)))
	((== (car x) "table-lsep") (length-attr "padding-left" (cadr x)))
	((== (car x) "table-rsep") (length-attr "padding-right" (cadr x)))
	((== (car x) "table-tsep") (length-attr "padding-top" (cadr x)))
	((== (car x) "table-bsep") (length-attr "padding-bottom" (cadr x)))
	(else #f)))

(define (tmhtml-make-table t tablef colf rowf cellf)
  (let* ((attrs (map* tmhtml-make-table-attr tablef))
         (va "vertical-align: middle"))
    (if (not (list-find attrs (cut == <> "width: 100%")))
	(set! attrs (cons* "display: inline-table" va attrs)))
    `(h:table ,@(html-css-attrs attrs)
	      ,@(tmhtml-make-column-group colf)
	      (h:tbody ,@(tmhtml-make-rows (cdr t) rowf cellf)))))

(define (tmhtml-table l)
  (list (tmhtml-make-table (cons 'table l) '() '() '() '())))

(define (tmhtml-tformat l)
  (with t (tmtable-normalize (cons 'tformat l))
    (receive (tablef colf rowf cellf) (tmtable-properties* t)
      (list (tmhtml-make-table (cAr t) tablef colf rowf cellf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pictures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-collect-labels x)
  (cond ((nlist? x) '())
	((and (func? x 'label 1) (string? (cadr x))) `((id ,(cadr x))))
	(else (append-map tmhtml-collect-labels (cdr x)))))

(define (tmhtml-image-names ext)
  (set! tmhtml-image-serial (+ tmhtml-image-serial 1))
  (let* ((postfix (string-append
		   "-" (number->string tmhtml-image-serial) "." ext))
	 (name-url (url-glue tmhtml-image-root-url postfix))
	 (name-string (string-append tmhtml-image-root-string postfix)))
    (values name-url name-string)))

(define (magic-png-number)
  (let* ((css (get-preference "texmacs->html:css-stylesheet"))
         (s (url->string (url-tail css))))
    (if (string-starts? s "web-") 19000.0 20625.0)))

(define (tmhtml-png y)
  ;; converts generic markup that can be displayed on screen to png format
  (let* ((mag (ahash-ref tmhtml-env :mag))
	 (x (if (or tmhtml-css? (nstring? mag) (== mag "1")) y
                (with nmag `(times (value "magnification") ,mag)
                  `(with "magnification" ,nmag ,y))))
	 (l1 (tmhtml-collect-labels y))
	 (l2 (if (null? l1) l1 (list (car l1)))))
    (with cached (ahash-ref tmhtml-image-cache x)
      (if (not cached)
	  (receive (name-url name-string) (tmhtml-image-names "png")
	    ;;(display* x " -> " name-url ", " name-string "\n")
	    (let* ((extents (print-snippet name-url x #t))
                   (m (magic-png-number))
                   (unit (/ 6000.0 (* (ninth extents) (tenth extents) m)))
		   (x3 (* (first extents) unit))
		   (y3 (* (second extents) unit))
		   (x4 (* (third extents) unit))
		   (y4 (* (fourth extents) unit))
		   (x1 (* (fifth extents) unit))
		   (y1 (* (sixth extents) unit))
		   (x2 (* (seventh extents) unit))
		   (y2 (* (eighth extents) unit))
                   (lmar (number->htmlstring (- x3 x1)))
                   (bmar (number->htmlstring (- y3 y1)))
                   (rmar (number->htmlstring (- x2 x4)))
                   (tmar (number->htmlstring (- y2 y4)))
		   (valign (number->htmlstring y1))
		   (height (number->htmlstring (- y4 y3)))
		   (style (string-append "margin-left: " lmar "em; "
                                         "margin-bottom: " bmar "em; "
                                         "margin-right: " rmar "em; "
                                         "margin-top: " tmar "em; "
                                         "vertical-align: " valign "em; "
                                         "height: " height "em"))
                   (attrs (if tmhtml-css?
                              `((src ,name-string) (style ,style) ,@l2)
                              `((src ,name-string) ,@l2)))
                   (img (if (url-exists? name-url)
                            `((h:img (@ ,@attrs)))
                            `())))
	      ;;(display* x " -> " extents "\n")
	      (set! cached img)
	      (ahash-set! tmhtml-image-cache x cached)))
	  cached))))

(define (tmhtml-graphics l)
  (tmhtml-png (cons 'graphics l)))

(define (tmhtml-draw-over l)
  (tmhtml-png (cons 'draw-over l)))

(define (tmhtml-draw-under l)
  (tmhtml-png (cons 'draw-under l)))

(define (tmhtml-image-suffix name)
  (cond ((string? name) (url-suffix name))
        ((and (func? name 'tuple 2) (string? (caddr name)))
         (if (== (url-suffix (caddr name)) "") name (url-suffix (caddr name))))
        (else "")))

(define (tmhtml-web-image-name name)
  ;; make sure image is accessible from html
  ;; FIXME: we should replace ~, environment variables, etc.
  (let* ((un  (unix->url name))
         (u (url-relative current-save-target un))
         (suf (url-suffix un)))
    (if (url-exists? u)
        name
      (with u1 (url-relative (current-buffer-url) un)
        (if (url-exists? u1)
          (receive (name-url name-string) (tmhtml-image-names suf)
            (system-copy u1 name-url)
            name-string)
           name))))) ;; image does not exist, can't do much

(define (tmhtml-image l)
  ;; FIXME: Should also test that width and height are not magnifications.
  ;; Currently, magnifications make tmlength->htmllength return #f.
  (cond ((nin? (tmhtml-image-suffix (car l)) tmhtml-web-formats)
         ;; linked or embedded non-web image: convert & save to png  
         (tmhtml-png (cons 'image l)))
        ((and (func? (car l) 'tuple 2)
              (func? (cadar l) 'raw-data 1)
              (string? (cadr (cadar l)))
              (string? (caddar l)))
         ;; embedded web image, extract it    
         (receive (name-url name-string)
                  (tmhtml-image-names (url-suffix (cork->utf8 (caddar l))))
           (string-save (cadr (cadar l)) name-url)
           (tmhtml-image (cons name-string (cdr l)))))
        ((nstring? (first l))
         ;; treat complex images as generic markup
         (tmhtml-png (cons 'image l)))
        (else
          ;; web images
          (let* ((s (tmhtml-web-image-name (first l)))
                 (w (tmlength->htmllength (second l) #f))
                 (h (tmlength->htmllength (third l) #f)))
            `((h:img (@ (class "image")
                        (src ,s)
                        ,@(if w `((width ,w)) '())
                        ,@(if h `((height ,h)) '()))))))))

(define (tmhtml-ornament-get-env-style)
  (let* ((l0 (hash-map->list list tmhtml-env))
         (l1 (filter (lambda (x)
                       (and (list>0? (car x))
                            (cadr x)
                            (string-prefix? "#:ornament-"
                                            (object->string (caar x))))) l0))
         (l2   (map car l1))
         (args (map cadr l1))
         (funs (map cAr l2))
         (stys (map (lambda (x) (cdr (cDr x))) l2)))
    (apply
      string-append
      (list-intersperse
        (map (lambda (f arg sty)
               (with args (string-tokenize-by-char arg #\;)
                 (apply
                   string-append
                   (list-intersperse
                     (cond ((== (length args) (length sty))
                            (map (lambda (x y)
                                   (string-append x ":" (f y))) sty args))
                           ((>= 1 (length sty))
                            (map (lambda (y)
                                   (string-append (car sty) ":" (f y))) args))
                           (else '()))
                     ";"))))
             funs args stys) ";"))))

(define (contains-surround? l)
  (cond ((nlist? l) #f)
        ((func? l 'surround 3) #t)
        (else (with r #f
                (for-each (lambda (x)
                            (set! r (or r (contains-surround? x)))) l)
                r))))

(define (tmhtml-ornament l)
  (let* ((body (tmhtml (car l)))
         (styl (tmhtml-ornament-get-env-style))
         (styl (if (contains-surround? l)
                 (string-append styl ";display:block;") styl))
         (args (if (== styl "") '() `((style ,styl))))
         (tag  (if (stm-block-structure? (car l)) 'h:div 'h:span)))
    `((,tag (@ (class "ornament") ,@args) ,@body))))

(define (tmhtml-balloon l)
  (let* ((anch (tmhtml (car  l)))
         (body (tmhtml (cadr l)))
         (tag1 (if (stm-block-structure? (car  l)) 'h:div 'h:span))
         (tag2 (if (stm-block-structure? (cadr l)) 'h:div 'h:span)))
    `((,tag1 (@ (class "balloon-anchor")) ,@anch
             (,tag2 (@ (class "balloon-body") (hidden "hidden")) ,@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Standard markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transform-item-post l)
  (if (not (tm-is? (car l) '!item))
      `(document ,@l)
      `(!item ,(cadar l) (document ,(caddar l) ,@(cdr l)))))

(define (item? x) (tm-in? x '(item item*)))

(define (transform-items x)
  (cond ((and (tm-is? x 'concat) (nnull? (cdr x))
              (item? (cadr x)))
         `(!item ,(cadr x) (concat ,@(cddr x))))
        ((and (tm-is? x 'concat) (nnull? (cdr x)) (nnull? (cddr x))
              (tm-is? (cadr x) 'assign) (item? (caddr x)))
         `(!item ,(caddr x) (concat ,@(cdddr x))))
        ((item? x)
         `(!item ,x (hspace "1pt")))
        ((tm-is? x 'with)
         `(,@(cDr x) ,(transform-items (cAr x))))
        ((tm-is? x 'document)
         (let* ((r  (map transform-items (cdr x)))
                (p? (lambda (i) (tm-is? i '!item)))
                (sr (list-scatter r p? #t))
                (fr (list-filter sr nnull?)))
           `(document ,@(map transform-item-post fr))))
        (else x)))

;; TODO: when the first data of the list is a label, it must be used to set the
;; ID attribute of the resulting xhtml element. When that is done, remove the
;; warning comment from htmltm-handler.

(define (tmhtml-post-item args)
  (let* ((i (car args))
         (r (tmhtml (cadr args))))
    (if (or (tm-is? i 'item) (null? (cdr i)))
        `((h:li ,@r))
        `((h:dt ,@(tmhtml (cadr i)))
          (h:dd ,@r)))))

(define (tmhtml-itemize args)
  `((h:ul ,@(tmhtml (transform-items (car args))))))

(define (tmhtml-enumerate args)
  `((h:ol ,@(tmhtml (transform-items (car args))))))

(define (tmhtml-description args)
  `((h:dl ,@(tmhtml (transform-items (car args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-verbatim args)
  ;; Block-level verbatim environments should only contain inline elements.
  ;;
  ;; @args should be a single element list, we will call this element @body.
  ;;
  ;; If @body is a block structure, it will be either:
  ;; -- a simple DOCUMENT (normal case), and @(tmhtml body) will produce a list
  ;;    of h:p elements;
  ;; -- a block structure producing a single element (degenerate case).
  ;;
  ;; Verbatim structures which do not contain a DOCUMENT but are direct
  ;; children of a DOCUMENT (i.e. they occupy a whole paragraph) are degenerate
  ;; cases of block-level verbatim and must be exported as PRE.
  ;;
  ;; Inline verbatim has little special significance for display in TeXmacs. In
  ;; LaTeX it is used to escape special characters (and protect multiple inline
  ;; spaces, yuck!), but in TeXmacs there is no such problem.
  (with body (first args)
    (with pre? #f
      (when (func? body 'with 1)
      (set! body (cAr body)))
      (when (and (func? body 'with 3) (== (cadr body) "locus-color"))
        (set! body (cAr body)))
      (when (func? body 'action)
        (set! body (cadr body))
        (set! pre? (string? body)))
      (if (or (stm-block-structure? body) pre?)
          (verbatim-pre
           (ahash-with tmhtml-env :preformatted #t
             (tmhtml body)))
          (verbatim-tt (tmhtml body))))))

(define (verbatim-tt content)
  `((h:tt (@ (class "verbatim")) ,@content)))

(define (verbatim-pre content)
  `((h:pre (@ (class "verbatim") (xml:space "preserve")) ,@content)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-doc-title-block l)
  `((h:table (@ (class "title-block"))
	     (h:tr (h:td ,@(tmhtml (car l)))))))

(define (tmhtml-equation* l)
  (with first (simplify-document (car l))
    (with x `(with "mode" "math" (with "math-display" "true" ,first))
      (ahash-with tmhtml-env :math-display #t
        `((h:center ,@(tmhtml x)))))))

(define (tmhtml-equation-lab l)
  (with first (simplify-document (car l))
    (with x `(with "mode" "math" (with "math-display" "true" ,first))
      (ahash-with tmhtml-env :math-display #t
        `((h:table (@ (width "100%"))
                   (h:tr (h:td (@ (align "center") (width "100%"))
                               ,@(tmhtml x))
                         (h:td (@ (align "right"))
                               "(" ,@(tmhtml (cadr l)) ")"))))))))

(define (tmhtml-mathjax-formula* x)
  ;;(display* "x= " x "\n")
  (let* ((opts (list (cons "texmacs->latex:mathjax" "on")))
         (s (serialize-latex (texmacs->latex x opts)))
         (display? (ahash-ref tmhtml-env :math-display))
         (style (if display? "\\displaystyle " ""))
         (mj (string-append "\\(" style s "\\)")))
    (when (and (string-starts? mj "\\(\\displaystyle \\begin{array}{rcl}")
               (string-ends?   mj "\\end{array}\\)"))
      (set! mj (string-append "\\begin{eqnarray*}"
                              (substring mj 34 (- (string-length mj) 13))
                              "\\end{eqnarray*}")))
    (list mj)))

(define (tmhtml-mathjax-formula x)
  (cond ((tm-func? x 'with 1)
         (tmhtml-mathjax-formula (cadr x)))
        ((and (tm-func? x 'with 3)
              (== (cadr x) "math-display")
              (== (caddr x) "true"))
         (ahash-with tmhtml-env :math-display #t
           (tmhtml-mathjax-formula (cadddr x))))
        (else (tmhtml-mathjax-formula* x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equation arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-concat-children t)
  (if (tm-func? t 'concat)
      (append-map tm-concat-children (tm-children t))
      (list t)))

(define (split-htab r)
  (let* ((a (tm-concat-children r))
         (f (list-find-index a (lambda (x) (tm-func? x 'htab))))
         (al (if f (sublist a 0 f) a))
         (ar (if f (sublist a (+ f 1) (length a)) (list)))
         (rl (apply tmconcat al))
         (rr (apply tmconcat ar)))
    (list rl rr)))

(define (rewrite-eqnarray* t)
  (cond ((tm-atomic? t) t)
        ((tm-func? t 'row 3)
         (let* ((l (tm-ref t 0 0))
                (c (tm-ref t 1 0))
                (r (split-htab (tm-ref t 2 0))))
           `(row (cell (big-math ,l))
                 (cell (big-math ,c))
                 (cell (big-math ,(car r)))
                 (cell ,(cadr r)))))
        (else (cons (tm-label t) (map rewrite-eqnarray* (tm-children t))))))

(tm-define (ext-tmhtml-eqnarray* body)
  (:secure #t)
  (if (tm-func? body 'document 1)
      `(document ,(ext-tmhtml-eqnarray* (tm-ref body 0)))
      (cond ((null? (tm-search body (lambda (x) (tm-func? x 'htab))))
             `(equation* (rcl-table ,body)))
            ((and (tm-func? body 'tformat)
                  (tm-func? (tm-ref body :last) 'table 1)
                  (tm-func? (tm-ref body :last 0) 'row 3))
             (let* ((row (tm-ref body :last 0))
                    (l (tm-ref row 0 0))
                    (c (tm-ref row 1 0))
                    (r (split-htab (tm-ref row 2 0)))
                    (row1 `(row (cell ,l) (cell ,c) (cell ,(car r))))
                    (rcl `(rcl-table (tformat (table ,row1))))
                    (row2 `(row (cell (big-math ,rcl)) (cell ,(cadr r))))
                    (res `(cx-table (tformat (table ,row2)))))
               res))
            (else
              `(rclx-table ,(rewrite-eqnarray* body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags for customized html generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-append-attribute-sub l var val)
  (cond ((null? l) (list (list var val)))
	((and (list-2? (car l)) (== (caar l) var) (string? (cadar l)))
	 (cons (list var (string-append (cadar l) " " val)) (cdr l)))
	(else (cons (car l) (tmhtml-append-attribute-sub (cdr l) var val)))))

(define (tmhtml-append-attribute t var val)
  (cond ((and (func? t 'h:img) (== var 'style))
         `(class (@ (,var ,val)) ,t))
        ((and (func? t 'h:p 1) (func? (cadr t) 'h:p))
         (tmhtml-append-attribute (cadr t) var val))
        ((and (pair? t) (pair? (cdr t)) (list? t)
              (pair? (cadr t)) (== (caadr t) '@) (list? (cadr t)))
         (with l (tmhtml-append-attribute-sub (cdadr t) var val)
           `(,(car t) (@ ,@l) ,@(cddr t))))
        ((and (pair? t) (list? t))
         `(,(car t) (@ (,var ,val)) ,@(cdr t)))
        ((== var 'class) `(font (@ (,var ,val)) ,t))
        (else `(class (@ (,var ,val)) ,t))))

(define (tmhtml-html-tag l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (list `(,(string->symbol s) ,@r))))

(define (tmhtml-html-attr l)
  (let* ((a (string->symbol (tmhtml-force-string (car l))))
         (v (tmhtml-force-string (cadr l)))
         (r (tmhtml (caddr l))))
    (map (cut tmhtml-append-attribute <> a v) r)))

(define (tmhtml-html-style l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (map (cut tmhtml-append-attribute <> 'style s) r)))

(define (tmhtml-html-class l)
  (let* ((s (tmhtml-force-string (car l)))
         (r (tmhtml (cadr l))))
    (map (cut tmhtml-append-attribute <> 'class s) r)))

(define (tmhtml-html-div-style l)
  (list `(h:div (@ (style ,(tmhtml-force-string (car l))))
		,@(tmhtml (cadr l)))))

(define (tmhtml-html-div-class l)
  (list `(h:div (@ (class ,(tmhtml-force-string (car l))))
		,@(tmhtml (cadr l)))))

(define (tmhtml-html-javascript l)
  (list `(h:script (@ (language "javascript"))
		   ,(tmhtml-force-string (car l)))))

(define (tmhtml-html-javascript-src l)
  (list `(h:script (@ (language "javascript")
		      (src ,(tmhtml-force-string (car l)))))))

(define (tmhtml-html-video l)
  (let* ((dest (cork->html (force-string (car l))))
         (mp4 (string-append dest ".mp4"))
         (ogg (string-append dest ".ogg"))
         (webm (string-append dest ".webm"))
         (swf (string-append dest ".swf"))
         (width (force-string (cadr l)))
         (height (force-string (caddr l))))
    (list `(h:video (@ (width ,width) (height ,height) (controls "controls"))
             (h:source (@ (src ,mp4) (type "video/mp4")))
             (h:source (@ (src ,ogg) (type "video/ogg")))
             (h:source (@ (src ,webm) (type "video/webm")))
             (h:object (@ (data ,mp4) (width ,width) (height ,height))
               (h:embed (@ (src ,swf) (width ,width) (height ,height))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tmdoc tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-make-block content)
  (let* ((l '(h:td
	      (@ (align "left"))
	      (h:img (@ (src "https://www.texmacs.org/Images/tm_gnu1b.png")))))
	 (c `(h:td
	      (@ (align "center") (width "100%"))
	      ,@(tmhtml content)))
	 (r '(h:td
	      (@ (align "right"))
	      (h:img (@ (src "https://www.texmacs.org/Images/tm_gnu2b.png")))))
	 (row `(h:tr ,l ,c ,r)))
    `(h:table (@ (width "100%") (cellspacing "0") (cellpadding "3")) ,row)))

(define (tmhtml-tmdoc-title l)
  (list `(h:div (@ (class "tmdoc-title-1"))
		,(tmhtml-make-block (car l)))))

(define (tmhtml-tmdoc-title* l)
  (list `(h:div (@ (class "tmdoc-title-2"))
		,(tmhtml-make-block (car l)))
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
      `("&copy;" " " ,@(tmhtml (car l))
	" " ,@(tmhtml (cadr l))
	,@(tmhtml-tmdoc-copyright* (cddr l)))
    (list `(h:div (@ (class "tmdoc-copyright")) ,@content))))

(define (tmhtml-tmdoc-license l)
  (list `(h:div (@ (class "tmdoc-license")) ,@(tmhtml (car l)))))

(define (tmhtml-key l)
  ;; `((h:u (h:tt ,@(tmhtml (tm->stree (tmdoc-key (car l))))))))
  `((h:u (h:tt ,@(tmhtml (car l))))))

(define (tmhtml-tmdoc-bar? y)
  (or (func? y 'h:h1)
      (func? y 'h:h2)
      (and (func? y 'h:div)
	   (nnull? (cdr y))
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
;; Css finalization (breaking around images)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-breaks-post l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (func? (car l) 'h:img)
              (string? (cadr l))
              (not (string-starts? (cadr l) " ")))
         (let* ((s (cadr l))
                (i (string-index s #\space))
                (s1 (if i (substring s 0 i) s))
                (s2 (and i (substring s i (string-length s))))
                (nb `(h:span (@ (class "no-breaks")) ,(car l) ,s1))
                (t (if s2 (cons s2 (cddr l)) (cddr l))))
           (cons nb (tmhtml-breaks-post t))))
        (else (cons (car l) (tmhtml-breaks-post (cdr l))))))

(define (tmhtml-breaks-post* l)
  (let* ((n (length l))
         (h (quotient n 2)))
    (if (<= n 10000) (tmhtml-breaks-post l)
        (append (tmhtml-breaks-post* (sublist l 0 h))
                (tmhtml-breaks-post* (sublist l h n))))))

(define (tmhtml-css-post body)
  (if (pair? body)
      `(,(car body) ,@(tmhtml-breaks-post* (map tmhtml-css-post (cdr body))))
      body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmhtml-list l)
  (append-map tmhtml l))

(define (tmhtml-dispatch htable l)
  (let ((x (logic-ref ,htable (car l))))
    (cond ((not x) #f)
	  ((procedure? x) (x (cdr l)))
	  (else (tmhtml-post-simplify-element
		 (append x (tmhtml-list (cdr l))))))))

(define (tmhtml-implicit-compound l)
  (or (tmhtml-dispatch 'tmhtml-stdmarkup% l)
      (and (!= tmhtml-site-version "2")
           (tmhtml-dispatch 'tmhtml-tmdocmarkup% l))
      '()))

(tm-define (tmhtml-root x)
  (ahash-with tmhtml-env :mag "1"
    (ahash-with tmhtml-env :math #f
      (ahash-with tmhtml-env :math-display #f
        (ahash-with tmhtml-env :preformatted #f
          (ahash-with tmhtml-env :left-margin 0
            (ahash-with tmhtml-env :right-margin 0
              (tmhtml x))))))))

(define (tmhtml x)
  ;; Main conversion function.
  ;; Takes a TeXmacs tree in Scheme notation and produce a SXML node-set.
  ;; All handler functions have a similar prototype.
  (cond ((and tmhtml-mathjax? (ahash-ref tmhtml-env :math))
         (tmhtml-mathjax-formula x))
        ((and tmhtml-mathml? (ahash-ref tmhtml-env :math))
	 `((m:math (@ (xmlns "http://www.w3.org/1998/Math/MathML"))
		   ,(texmacs->mathml x tmhtml-env))))
	((and tmhtml-images? (ahash-ref tmhtml-env :math)
              (!= tmhtml-image-root-string "image"))
         (tmhtml-png `(with "mode" "math" ,x)))
	((string? x)
	 (if (string-null? x) '() (tmhtml-text x))) ; non-verbatim string nodes
	(else (or (tmhtml-dispatch 'tmhtml-primitives% x)
		  (tmhtml-implicit-compound x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(logic-dispatcher tmhtml-primitives%
  (document tmhtml-document)
  (para tmhtml-paragraph)
  (surround tmhtml-surround)
  (concat tmhtml-concat)
  (rigid tmhtml-id)
  (hgroup tmhtml-id)
  (hidden tmhtml-hidden)
  (freeze tmhtml-id)
  (unfreeze tmhtml-id)
  (hspace tmhtml-hspace)
  (vspace* tmhtml-vspace)
  (vspace tmhtml-vspace)
  (space tmhtml-hspace)
  (htab tmhtml-hspace)
  (split tmhtml-noop)
  (move tmhtml-move)
  (shift tmhtml-move)
  (resize tmhtml-resize)
  (clipped tmhtml-resize)
  (repeat tmhtml-repeat)
  (repeat* tmhtml-repeat)
  (float tmhtml-float)
  (underline tmhtml-underline)
  (overline tmhtml-overline)
  (strike-through tmhtml-strike-through)
  (deleted tmhtml-deleted)
  (marked tmhtml-marked)
  (fill-out tmhtml-fill-out)
  (datoms tmhtml-datoms)
  (dlines tmhtml-datoms)
  (dpages tmhtml-datoms)
  (dbox tmhtml-datoms)
  
  (with-limits tmhtml-noop)
  (line-break tmhtml-noop)
  (new-line tmhtml-new-line)
  (line-sep tmhtml-noop)
  (next-line tmhtml-next-line)
  (no_break tmhtml-noop)
  (no-indent tmhtml-noop)
  (yes-indent tmhtml-noop)
  (no-indent* tmhtml-noop)
  (yes-indent* tmhtml-noop)
  (page-break* tmhtml-noop)
  (page-break tmhtml-noop)
  (no-page-break* tmhtml-noop)
  (no-page-break tmhtml-noop)
  (no-break-here* tmhtml-noop)
  (no-break-here tmhtml-noop)
  (no-break-start tmhtml-noop)
  (no-break-end tmhtml-noop)
  (new-page* tmhtml-noop)
  (new-page tmhtml-noop)
  (new-dpage* tmhtml-noop)
  (new-dpage tmhtml-noop)

  ((:or around around* big-around) tmhtml-concat)
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
  (rsubsup tmhtml-subsup)
  (frac tmhtml-frac)
  (sqrt tmhtml-sqrt)
  (wide tmhtml-wide)
  (neg tmhtml-neg)
  (tree tmhtml-tree)
  (syntax tmhtml-syntax)
  ((:or old-matrix old-table old-mosaic old-mosaic-item)
   tmhtml-noop)
  (table tmhtml-table)
  (tformat tmhtml-tformat)
  ((:or twith cwith tmarker row cell sub-table) tmhtml-noop)

  (assign tmhtml-noop)
  (with tmhtml-with)
  (provides tmhtml-noop)
  ((:or value quote-value) tmhtml-compound)
  ((:or macro drd-props arg quote-arg) tmhtml-noop)
  (compound tmhtml-compound)
  ((:or xmacro get-label get-arity map-args eval-args) tmhtml-noop)
  (mark tmhtml-mark)
  (eval tmhtml-noop)
  ((:or if if* case while for-each extern) tmhtml-noop)
  (include tmhtml-include)
  (use-package tmhtml-noop)

  ((:or or xor and not plus minus times over div mod merge length range
	number date translate is-tuple look-up equal unequal less lesseq
	greater greatereq if case while extern authorize)
   tmhtml-noop)

  ((:or style-with style-with* style-only style-only*
	active active* inactive inactive* rewrite-inactive) tmhtml-noop)
  (inline-tag tmhtml-inline-tag)
  (open-tag tmhtml-open-tag)
  (middle-tag tmhtml-middle-tag)
  (close-tag tmhtml-close-tag)
  (symbol tmhtml-noop)
  (latex tmhtml-noop)
  (hybrid tmhtml-noop)

  (locus tmhtml-datoms)
  (label tmhtml-label)
  (reference tmhtml-noop)
  (pageref tmhtml-noop)
  (hlink tmhtml-hyperlink)
  (action tmhtml-action)
  (write tmhtml-noop)
  
  ((:or tuple collection associate) tmhtml-noop)
  (specific tmhtml-specific)

  (graphics tmhtml-graphics)
  ((:or point line arc bezier) tmhtml-noop)
  (image tmhtml-image)

  (ornament tmhtml-ornament)

  (format tmhtml-noop)
  ((:or tag meaning) tmhtml-noop)

  (vgroup tmhtml-id)
  ((:or switch fold exclusive progressive superposed) tmhtml-noop)
  ((:or mouse-over-balloon mouse-over-balloon*
        hover-balloon hover-balloon*
        popup-balloon popup-balloon*) tmhtml-balloon)
  
  (!file tmhtml-file))

(logic-table tmhtml-stdmarkup%
  ;; special auxiliary tags
  (!left ,tmhtml-align-left)
  (!middle ,tmhtml-align-middle)
  (!right ,tmhtml-align-right)
  ;; Sectioning
  (chapter-title (h:h1))
  (section-title (h:h2))
  (subsection-title (h:h3))
  (subsubsection-title (h:h4))
  (paragraph-title (h:h5))
  (subparagraph-title (h:h6))
  ;; Lists
  ((:or itemize itemize-minus itemize-dot itemize-arrow)
   ,tmhtml-itemize)
  ((:or enumerate enumerate-numeric enumerate-roman enumerate-Roman
	enumerate-alpha enumerate-Alpha)
   ,tmhtml-enumerate)
  ((:or description description-compact description-dash
	description-aligned description-long description-paragraphs)
   ,tmhtml-description)
  (item* (h:dt))
  (!item ,tmhtml-post-item)
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
  (nbsp ,(lambda x '("&nbsp;")))
  ;; Presentation
  (tt (h:tt))
  (hrule (h:hr))
  ;; Names
  (TeXmacs ,(lambda x '("TeXmacs")))
  (TeX ,(lambda x '("TeX")))
  (LaTeX ,(lambda x '("LaTeX")))
  ;; additional tags
  (shown ,tmhtml-shown)
  (hidden-title ,tmhtml-noop)
  (doc-title-block ,tmhtml-doc-title-block)
  (equation* ,tmhtml-equation*)
  (equation-lab ,tmhtml-equation-lab)
  (equations-base ,tmhtml-equation*)
  (wide-float ,tmhtml-float)
  (draw-over ,tmhtml-draw-over)
  (draw-under ,tmhtml-draw-under)
  ;; tags for customized html generation
  (html-tag ,tmhtml-html-tag)
  (html-attr ,tmhtml-html-attr)
  (html-div-style ,tmhtml-html-div-style)
  (html-div-class ,tmhtml-html-div-class)
  (html-style ,tmhtml-html-style)
  (html-class ,tmhtml-html-class)
  (html-javascript ,tmhtml-html-javascript)
  (html-javascript-src ,tmhtml-html-javascript-src)
  (html-video ,tmhtml-html-video)
  ;; tmdoc tags
  (web-title ,tmhtml-noop)
  (hyper-link ,tmhtml-hyperlink))

(logic-table tmhtml-tmdocmarkup%
  ;; deprecated direct exports of tmdoc tags
  (tmdoc-title ,tmhtml-tmdoc-title)
  (tmdoc-title* ,tmhtml-tmdoc-title*)
  (tmdoc-title** ,tmhtml-tmdoc-title**)
  (tmdoc-flag ,tmhtml-tmdoc-flag)
  (tmdoc-copyright ,tmhtml-tmdoc-copyright)
  (tmdoc-license ,tmhtml-tmdoc-license))

;; (name (h:name)) ; not in HTML4
;; (person (h:person)))) ; not in HTML4

(logic-table tmhtml-with-cmd%
  ("mode" ,tmhtml-with-mode)
  ("math-display" ,tmhtml-with-math-display)
  ("color" ,tmhtml-with-color)
  ("font-size" ,tmhtml-with-font-size)
  ("par-left" ,tmhtml-with-par-left)
  ("par-right" ,tmhtml-with-par-right)
  ("par-first" ,tmhtml-with-par-first)
  ("par-par-sep" ,tmhtml-with-par-par-sep)
  (("ornament-hpadding")      (:ornament-hpadding
                                "padding-left" "padding-right"
                                ,tmlength->htmllength))
  (("ornament-vpadding")      (:ornament-vpadding
                                "padding-top"  "padding-bottom"
                                ,tmlength->htmllength))
  (("ornament-border")        (:ornament-border
                                "border-width"     ,tmlength->htmllength))
  (("ornament-shape")         (:ornament-shape
                                "border-radius"    ,tmshape->htmllength))
  (("ornament-color")         (:ornament-color
                                "background-color" ,tmcolor->htmlcolor))
  (("ornament-shadow-color")  (:ornament-shadow-color
                                "border-bottom-color" "border-right-color"
                                ,tmcolor->htmlcolor))
  (("ornament-sunny-color")   (:ornament-sunny-color
                                "border-left-color" "border-top-color"
                                ,tmcolor->htmlcolor))
  ;;(("ornament-extra-color")   :ornament-extra-color ""  ,tmcolor->htmlcolor))
  ;;(("ornament-swell")        :ornament-swell "" ,identity))
  ;;(("ornament-title-style")   :ornament-title-style "" ,identity))
  (("font-family" "tt") (h:tt))
  (("font-family" "ss") (h:class (@ (style "font-family: sans-serif"))))
  (("font-series" "bold") (h:b))
  (("font-shape" "italic") (h:i))
  (("font" "roman") (h:class (@ (style "font-family: Times New Roman"))))
  (("font" "times") (h:class (@ (style "font-family: Times New Roman"))))
  (("font" "helvetica") (h:class (@ (style "font-family: Helvetica"))))
  (("font" "courier") (h:class (@ (style "font-family: Coutier"))))
  (("math-font" "cal") (h:class (@ (style "font-family: Flemish Script"))))
  (("math-font" "frak") (h:class (@ (style "font-family: Bernhard Modern"))))
  (("font-series" "medium") (h:class (@ (style "font-weight: normal"))))
  (("font-shape" "right") (h:class (@ (style "font-style: normal"))))
  (("font-shape" "small-caps")
   (h:class (@ (style "font-variant: small-caps")))))

(logic-table tmhtml-with-cmd% ; deprecated
  (("par-mode" "left") (h:div (@ (align "left"))))
  (("par-mode" "justify") (h:div (@ (align "justify"))))
  (("par-mode" "center") (h:center)))

(logic-table tmhtml-with-cmd% ; netscape4
  (("par-columns" "1") (h:multicol (@ (cols "1"))))
  (("par-columns" "2") (h:multicol (@ (cols "2"))))
  (("par-columns" "3") (h:multicol (@ (cols "3"))))
  (("par-columns" "4") (h:multicol (@ (cols "4"))))
  (("par-columns" "5") (h:multicol (@ (cols "5")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs->html x opts)
  (if (tmfile? x)
      (let* ((body (tmfile-extract x 'body))
	     (style* (tmfile-extract x 'style))
	     (style (if (list? style*) style* (list style*)))
	     (lan (tmfile-language x))
	     (doc (list '!file body style lan
                        (url->string (get-texmacs-path)))))
	(texmacs->html doc opts))
      (begin
	(tmhtml-initialize opts)
	((if (func? x '!file)
	     tmhtml-finalize-document
	     tmhtml-finalize-selection)
	 (tmhtml-root x)))))
