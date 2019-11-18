
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : xmltm.scm
;; DESCRIPTION : Common tools to import XML data.
;; COPYRIGHT   : (C) 2002-2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools xmltm)
  (:use (convert tools stm) (convert tools sxml)
	(convert tools environment) (convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XML namespace normalization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The built-in parser xml/html parser is not namespace aware. So we need to
;; perform namespace-prefix normalization ourselves after the parser. Only
;; namespace-normalized xml trees can be reliably queried.
;;
;; Namespace-normalization deletes all "xmlns" and "xmlns:*" attributes and
;; change the name of all elements and attributes to a fixed set of normalized
;; namespace prefixes.
;;
;; Normalized namespace prefixes are:
;;   x  --  XML - http://www.w3.org/XML/1998/namespace
;;          Reserved prefix (e.g. xml:space).
;;   h  --  XHTML - http://www.w3.org/1999/xhtml
;;          Any HTML or HTML-like data.
;;   m  --  MathML - http://www.w3.org/1998/Math/MathML
;;
;;Non-Normalized namespace prefixes are:
;;   g  --  Gallina language.
;;   c  --  Coq XML format (we named it CoqML).
;;
;; Since the parser is designed to be used for conversion to STM data format,
;; no provisions are made to preserve the namespace prefixes used in the
;; orginial sxml tree. Namespace normalization is not reversible.

(define xmlns-uri-xml "http://www.w3.org/XML/1998/namespace")
(define xmlns-uri-xhtml "http://www.w3.org/1999/xhtml")
(define xmlns-uri-mathml "http://www.w3.org/1998/Math/MathML")
(define xmlns-uri-gallina "Gallina")
(define xmlns-uri-coqml "CoqML")

;;; Building the namespace bindings environment

(define-macro (with-xmltm-environment env ns . body)
  `(let ((,env (environment)))
     (with-environment ,env ((*default* ,ns)
			     (xml ,xmlns-uri-xml))
       ,@body)))

(define (consume-xmlns env attrs proc)
  (receive (attrs f+bindings )
      ((cut list-partition <> first)
       ((cut map <> attrs)
	(lambda (attr)
	  (if (eq? 'xmlns (first attr))
	      `(#f *default* ,(second attr))
	      (receive (ns-id ncname) (sxml-split-name (first attr))
		(if (== "xmlns" ns-id)
		    (list #f (string->symbol ncname) (second attr))
		    attr))))))
    ;; Either bindings or attributes are prefixed by #f to allow easy
    ;; partition. Regular attributes are (likely) more numerous than xmlns
    ;; attributes, so bindings are prefixed to reduce prefix-stripping cost.
    (if (null? f+bindings) (proc env attrs)
	(with-environment* env (map cdr f+bindings)
	  (cute proc <> attrs)))))

;;; Converting nodes

(tm-define (coqml-parse s)
  (xmltm-parse xmlns-uri-coqml parse-xml s))

(tm-define (gallinatm-parse s)
  (xmltm-parse xmlns-uri-gallina parse-xml s))

(tm-define (htmltm-parse s)
  (xmltm-parse xmlns-uri-xhtml parse-html s))

(tm-define (xmltm-parse default-ns parser s)
  (with-xmltm-environment
   env default-ns
   (let sub ((env env)
	     (t (parser s)))
     (cond ((string? t) t)
	   ((sxml-top-node? t) `(*TOP* ,@(map (cut sub env <>) (cdr t))))
	   ((sxml-control-node? t) t)
	   (else (ns-import-element sub env t))))))

(define (null->false x) (if (null? x) #f x))

(define (ns-import-element sub env t)
  (consume-xmlns
   env (sxml-attr-list t)
   (lambda (env attrs)
     ((cut sxml-set-name <> (ns-import-name env #t (sxml-name t)))
      ((cut sxml-set-attrlist <> (null->false (ns-import-attrs env attrs)))
       (sxml-set-content t (map (cut sub env <>)
				(sxml-content t))))))))

(define (ns-import-attrs env attrs)
  ((cut map <> attrs)
   (lambda (attr)
     ;; handles correctly the pseudo-sxml produced by enumerated html
     ;; attributes without left hand side. (e.g. <frame noresize>)
     (cons (string->symbol (ns-import-name env #f (first attr)))
	   (cdr attr)))))

(define (ns-import-name env use-default? name)
  (receive (ns-id ncname) (sxml-split-name name)
    (let ((ns-uri (cond (ns-id (environment-ref* env (string->symbol ns-id)))
			(use-default? (environment-ref env *default*))
			(else ""))))
      (string-append
       ;; FIXME: user namespace prefix list should be extensible
       (cond ((== ns-uri xmlns-uri-xhtml) "h:")
	     ((== ns-uri xmlns-uri-mathml) "m:")
	     ((== ns-uri xmlns-uri-gallina) "g:")
	     ((== ns-uri xmlns-uri-coqml) "c:")
	     ((== ns-uri xmlns-uri-xml) "x:")
	     ((string-null? ns-uri) "")
	     (else (string-append ns-uri ":")))
       ncname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmltm environment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (initialize-htmltm env proc)
  (with-environment* env '((preserve-space? #f)) proc))

(tm-define (htmltm-preserve-space? env)
  (environment-ref env preserve-space?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (xmltm-text s)
  (cork-grave->backquote (utf8->cork s)))

(tm-define (xmltm-url-text s)
  ;; (cork-grave->backquote (utf8->cork (url-decode s)))
  ;; NOTE: don't decode URL names, or also implement a corresponding
  ;; routine for the encoding, when we click on a hyperlink
  (string->tmstring s))

;; Conversion of Cork GRAVE ACCENT to LEFT SINGLE QUOTATION MARK

(define cork-grave-char #\nul)
(define cork-grave (list->string '(#\nul)))
(define cork-backquote (list->string '(#\`))) ;; that is GRAVE ACCENT in ASCII

(define (cork-grave->backquote s)
  (if (string-index s cork-grave-char)
      (string-replace s cork-grave cork-backquote)
      s))

;; Decoding URL strings
;;
;; url-decode uses a state-machine.
;; State is represented as a procedure of type P = ((:or char #f) -> P).
;; The next state is produced by applying the state to the next char.
;; Applying the state to #f yields the inverted list of chars of the decoded
;; string.

(define (url-decode s)
  ;; Decode a URL-encoded (rfc-2141) UTF-8 string.
  (url-decode/finish (string-fold (lambda (kar kdr) (kdr kar))
				  (cut url-decode/trans <> '())
				  s)))

(define (url-decode/finish proc)
  (reverse-list->string (proc #f)))

(define (url-decode/trans kar cs)
  ;; State when the next character is not escaped.
  (if (not kar) cs
      (if (char=? #\% kar)
	  (cut url-decode/hex-1 <> cs)
	  (cut url-decode/trans <> (cons kar cs)))))
  
(define (url-decode/hex-1 kar cs)
  ;; State when the previous character was a % (escape character).
  ;; When a % is not followed by a pair of hexadecimal digits (that is an
  ;; error), then % and its following chars are preserved.
  (if (not kar) (cons #\% cs)
      (if (char-hexadecimal? kar)
	  (cut url-decode/hex-2 <> cs kar)
	  (cut url-decode/trans <> (cons* kar #\% cs)))))

(define (url-decode/hex-2 kar cs hex1)
  ;; State when the last two characters were a % and an hexadecimal digit
  (if (not kar) (cons* hex1 #\% cs)
      (if (char-hexadecimal? kar)
	  (cut url-decode/trans <> (cons (hexadecimal->char hex1 kar) cs))
	  (cut url-decode/trans <> (cons* kar hex1 #\% cs)))))

(tm-define (char-hexadecimal? c)
  ;; Is @c an hexadecimal digit.
  (char-in-string? c "01234567890ABCDEFabcdef"))

(define (hexadecimal->char hex1 hex2)
  ;; Convert two hexadecimal digits @hex1 and @hex2 to a single character.
  (integer->char (+ (* 16 (hexadecimal-digit->integer hex1))
		    (hexadecimal-digit->integer hex2))))

(tm-define (hexadecimal-digit->integer c)
  (cond ((char-numeric? c) (- (char->integer c) 48))
	((char-in-string? c "ABCDEF") (- (char->integer c) 55))
	((char-in-string? c "abcdef") (- (char->integer c) 87))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (xmltm-attr->label a name)
  (let ((val (shtml-attr-non-null a name)))
    (and val `(label ,(xmltm-url-text val)))))

(tm-define (xmltm-label-decorate a name t)
  (let ((label (xmltm-attr->label a name)))
    (if label
	(stm-insert-first-data t label)
	t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (htmltm-space-element env l)
  ;; remove string nodes in the sxml node-list @l.
  (sxml-filter-element-content l))

(define (htmltm-collapse-spaces s)
  ;; normalize (convert to #\space) and collapse whitespace in s.
  (list->string (string-fold-right htmltm-collapse-spaces/kons '() s)))

(define (htmltm-collapse-spaces/kons kar kdr)
  (cond ((not (tm-char-whitespace? kar)) (cons kar kdr))
	((null? kdr) (list #\space))
	((tm-char-whitespace? (first kdr)) kdr)
	(else (cons #\space kdr))))

(tm-define (htmltm-space-collapse env l)
  ;; Collapses whitespaces in sxml node list @l. Correctly merges consecutive
  ;; string nodes in @l.
  (cond ((null? l) '())
	((htmltm-preserve-space? env) (htmltm-space-preserve l))
	(else (let ((l2 (list-fold-right htmltm-space-collapse/kons #f l)))
		(if (string? (first l2))
                    (cons (htmltm-collapse-spaces (car l2)) (cdr l2))
		    l2)))))

(tm-define (htmltm-space-mixed env l)
  ;; remove heading and trailing spaces, and collapses whitespaces in sxml node
  ;; list @l. Correctly merges consecutive string nodes in @l.
  (cond ((null? l) '())
	((htmltm-preserve-space? env) (htmltm-space-preserve l))
	(else (let ((l2 (list-fold-right htmltm-space-mixed/kons #f l)))
		(if (string? (first l2))
		    (cons (htmltm-collapse-spaces (tm-string-trim (car l2)))
			  (cdr l2))
		    l2)))))

(define (htmltm-space-collapse/kons kar kdr)
  (cond ((not kdr)			; kar is last node
	 (list kar))
	((string? kar)
	 (if (string? (first kdr))
	     (cons (string-append kar (car kdr)) (cdr kdr))
	     (cons kar kdr)))
	((string? (first kdr))
	 (cons kar (cons (htmltm-collapse-spaces (car kdr)) (cdr kdr))))
	(else
	 (cons kar kdr))))

(define (htmltm-space-mixed/kons kar kdr)
  (cond ((not kdr)			; kar is last node
	 (if (string? kar)
	     (list (tm-string-trim-right kar))
	     (list kar)))
	((string? kar)
	 (if (string? (first kdr))
	     (cons (string-append kar (car kdr)) (cdr kdr))
	     (cons kar kdr)))
	((string? (first kdr))
	 (cons kar (cons (htmltm-collapse-spaces (car kdr)) (cdr kdr))))
	(else
	 (cons kar kdr))))

(define (htmltm-space-preformatted env l)
  ;; Drop newline at start and end of @l if present.
  ;; Assumes the parser did newline normalization.
  ;; Convert other newlines to <p> elements.
  (cond ((null? l) '())
	((htmltm-preserve-space? env) (htmltm-space-preserve l))
	(else (htmltm-space-preserve (htmltm-space-pre-first
				      (htmltm-space-pre-last l))))))

(define (htmltm-space-pre-first l)
  (let ((x (first l)))
    (if (and (string? x) (string-starts? x "\n")) ; WARNING: \n is not R5RS
	(cons (string-drop x 1) (cdr l))
	l)))

(define (htmltm-space-pre-last l)
  ;; If @l ends with newline, drop newline.
  ;;
  ;; WARNING: non-standard (Joris wants to guess the editor's intent).
  ;; If @l ends with a line containing only spaces, drop the line.
  (let ((x (last l)))
    (cond ((nstring? x) l)
	  ;; Standard compliant case. Supersed by non-standard case.
	  ;; ((string-ends? x "\n")	; WARNING: \n is not R5RS
	  ;;  (rcons (but-last l) (string-drop-right x 1)))
	  ((do ((i (1- (string-length x)) (1- i)))
	       ((!= #\space (string-ref x i))
		(and (== #\newline (string-ref x i)) i)))
	   => (lambda (n) (rcons (but-last l) (string-take x n))))
	  (else l))))

(define (htmltm-space-preserve l)
  ;; Convert newlines to <p> elements.
  (append-map htmltm-space-preserve/sub l))

(define (htmltm-space-preserve/sub x)
  (if (string? x)
      (let ((l (string-split-lines x)))
	(if (null? (cdr l)) l (map (lambda (x) `(h:p ,x)) l)))
      (list x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing handlers for dispatch table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (htmltm-handler model kind method args->serial)
  ;; Produce a handler for the htmltm-methods% table
  ;;  model: content model category, for whitespace handling
  ;;         :empty -- element is defined to be empty, do not change contents.
  ;;         :element -- text nodes are ignored
  ;;           TODO: might fallback to 'mixed' if some text is present
  ;;         :mixed -- drop heading and trailing whitespaces, normalize and
  ;;           collapse internal whitespaces.
  ;;         :collapse -- normalize and collapse whitespaces. Preserve heading
  ;;           and trailing whitespaces.
  ;;         :pre -- drop newlines at ends and switch to preserved spaces
  ;;           mode.
  ;;   kind: either :block or :inline, how is the element rendered
  ;; method: <procedure> to convert the element content to a node-list.
  ;;             @method will be passed the contents of html element.
  ;;         <string> name of a unary macro to contain the converted contents
  ;;         (<symbol> ...) use contents converted to a serial as the last
  ;;             element of this structure.
  ;;          <node list> convert to this literal, ignoring element contents.
  ;; args->serial: used iff @method matches <string> or (<symbol> ...).
  ;;             Function to convert a list of sxml nodes to a stm serial.
  ;;
  ;; The handler takes care of whitespace cleaning and (except when an
  ;; <procedure> method is provided) insert label for id attributes.
  ;;
  ;; WARNING: If a list element has an id attribute, the converted result will
  ;; not be correctly converted back by tmhtml because the first item marker
  ;; will be preceded by a label.
  ;;
  ;; *Preconditions*
  ;; List environments must have :block kind.
  ;; *Postconditions*
  ;; The handler must return a texmacs node list given a sxhtml node list.
  ;; Its result may be the empty list, a list of line-structures, or a list
  ;; containing a single document node.
  ;; *Invariants*
  ;; If @kind is :block, the handler will always return a single 'document'.
  ;; If @kind is :inline and @method is <string> or (with ...), the handler
  ;; will return a 'document' iff the serial built from the converted contents
  ;; of the html element is a 'document'.
  ;;
  ;; WARNING: 2003-08-22. The htmltm-handler mechanism makes some on-the-fly
  ;; transformation on the tree while traversing. The subtrees stored as
  ;; children of xpath-parent and xpath-root are not processed. So the
  ;; descendence of various ancestors might not be consistent.
  (let ((clean (cond ((eq? model :empty) (lambda (env c) c))
		     ((eq? model :element) htmltm-space-element)
		     ((eq? model :collapse) htmltm-space-collapse)
		     ((eq? model :mixed) htmltm-space-mixed)
		     ((eq? model :pre) htmltm-space-preformatted)
		     (else (error "Bad model: " model))))
	(proc-alist
	 (cond ((eq? kind :inline)
		`((:procedure . ,htmltm-handler/procedure/inline)
		  (:environment . ,htmltm-handler/environment/inline)
		  (:literal . ,htmltm-handler/literal/inline)))
	       ((eq? kind :block)
		`((:procedure . ,htmltm-handler/procedure/block)
		  (:environment . ,htmltm-handler/environment/block)
		  (:literal . ,htmltm-handler/literal/block)))
	       (error "Bad kind: " kind))))

    (define (proc key) (cdr (assq key proc-alist)))
    (define (make-handler proc . extra)
      (lambda (env a c)
	(let ((cc (clean env c)))
	  (with-environment env
	      ((preserve-space? (or (eq? model :pre)
				    (htmltm-preserve-space? env))))
	    (apply proc env a cc extra)))))

    (cond ((procedure? method)
	   (make-handler (proc :procedure) method))
	  ((or (string? method) (symbol? (first method)))
	   (make-handler (proc :environment)
			 (if (string? method) ; this is ugly, should be removed
			     (if (stm-primitive? (string->symbol method))
				 `(expand ,method)
				 `(,(string->symbol method)))
			     method)
			 args->serial))
	   (else (cut (proc :literal) <> <> <> (stm-serial method))))))

(define (htmltm-handler/procedure/inline env a c proc)
  (proc env a c))

(define (htmltm-handler/procedure/block env a c proc)
  (list (stm-unary-document (htmltm-serial
			     (htmltm-preserve-space? env)
			     (proc env a c)))))

(define (htmltm-handler/literal/inline env a c literal)
  (list (xmltm-label-decorate a 'id literal)))

(define (htmltm-handler/literal/block env a c literal)
  (list (xmltm-label-decorate a 'id (stm-unary-document literal))))

(define (htmltm-handler/environment/inline env a c head args->serial)
  (list (let ((t (xmltm-label-decorate a 'id (args->serial env c))))
	  (if (stm-document? t)
	      `(document (,@head ,(stm-remove-unary-document t)))
	      `(,@head ,t)))))

(define (htmltm-handler/environment/block env a c head args->serial)
  (let ((make-block (if (stm-block-environment? head)
			stm-unary-document noop))
	(make-list (if (stm-list-environment? head)
		       htmltm-list-glue noop))
	(make-label (if (stm-section-environment? head)
			(lambda (x)
			  (xmltm-label-decorate a 'id (rcons head x)))
			(lambda (x)
			  (rcons head (xmltm-label-decorate a 'id x))))))
    (list `(document ,(make-label (make-list (make-block
					      (args->serial env c))))))))

(define (htmltm-list-glue x)
  ;; assert (stm-document? x)
  (cons 'document
	(receive (first kdr)
          (car+cdr (list-fold-right htmltm-list-glue/kons '(#f . ()) (cdr x)))
	  (htmltm-list-glue/flush first kdr))))
    
(define (htmltm-list-glue/kons kar next+kdr)
  (cons kar (receive (next kdr) (car+cdr next+kdr)
	      (if (stm-list-marker? (stm-first-data kar))
		  (cond ((not next) (list kar))
			((stm-list-marker? (stm-first-data next))
			 (cons kar kdr))
			(else
			 (cons
			  ; WARNING: no extra whitespace cleanup here
			  (htmltm-serial #t (list kar next)) kdr)))
		  ;; previous item may be a marker, do not stack kar
		  ;; but if next is not a marker, stack it now
		  (htmltm-list-glue/flush next kdr)))))

(define (htmltm-list-glue/flush line stack)
  (cond ((not line) stack)
        ((stm-list-marker? (stm-first-data line)) stack)
	(else (cons line stack))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing mathml handlers for dispatch table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (mathtm-handler model method)
  ;;  model: content model category
  ;;         :empty -- element defined to be empty
  ;;         :element -- text node are ignored
  ;;  TODO: MathML error if actual content do not match model.
  ;;         :mixed -- drop heading and trailing whitespaces, normalize and
  ;;           collapse internal whitespaces.
  ;;  method: <procedure> to convert the element content to a node-list.
  (if (not (in? model '(:empty :element :mixed)))
      (error "Bad model: " model))
  (if (not (procedure? method))
      (error "Bad method: " method))
  (let ((clean (cond ((eq? model :empty) (lambda (env c) c))
		     ((eq? model :element) htmltm-space-element)
		     ((eq? model :mixed) htmltm-space-mixed))))
    (let ((proc method))
      (lambda (env a c)
	(mathtm-handler/procedure
	 env a (clean env c) proc)))))

(define (mathtm-handler/procedure env a c proc)
  (proc env a c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing gallina handlers for dispatch table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gallinatm-raw    htmltm-space-preformatted)
(define gallinatm-terms  htmltm-space-element)
(define gallinatm-vernac htmltm-space-element)
(define gallinatm-toplvl htmltm-space-element)
(define gallinatm-ltac   htmltm-space-element)

(define (gallinatm-handler/inline env a c proc)
  (proc env a c))

(define (gallinatm-handler/bloc env a c proc)
  `((document ,@(proc env a c))))

(tm-define (gallinatm-handler model method)
  ;;  model:  content model category
  ;;          :toplvl -- text node are ignored
  ;;          :terms  -- text node are ignored
  ;;          :vernac -- text node are ignored
  ;;          :ltac   -- text node are ignored
  ;;          :raw -- drop heading and trailing whitespaces, normalize and
  ;;            collapse internal whitespaces.
  ;;  method: <procedure> to convert the element content to a node-list.
  (if (not (in? model '(:raw :terms :vernac :toplvl :ltac)))
      (error "Bad model: " model))
  (if (not (procedure? method))
      (error "Bad method: " method))
  (let ((clean (cond ((eq? model :raw)    gallinatm-raw)
                     ((eq? model :terms)  gallinatm-terms)
                     ((eq? model :toplvl) gallinatm-toplvl)
                     ((eq? model :vernac) gallinatm-vernac)
                     ((eq? model :ltac)   gallinatm-ltac)))
        (para  (cond ((eq? model :raw)    gallinatm-handler/inline)
                     ((eq? model :terms)  gallinatm-handler/inline)
                     ((eq? model :ltac)   gallinatm-handler/inline)
                     ((eq? model :toplvl) gallinatm-handler/bloc)
                     ((eq? model :vernac) gallinatm-handler/bloc))))
    (let ((proc method))
      (lambda (env a c)
        (para env a (clean env c) proc)))))

(tm-define (gallinatm-serial p? l)
  (if p? (stm-serial l stm-document?)
      (stm-serial l stm-document? htmltm-make-line htmltm-make-concat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Producing coqml handlers for dispatch table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (blank? s)
  (:synopsis "does @s contain only whitespace?")
  (list-and (map tm-char-whitespace? (string->list s))))

(define (trim-newlines s)
  (letrec ((nl? (lambda (c) (== c #\newline)))
           (trim-right (lambda (l)
                         (if (and (list>0? l) (nl? (car l)))
                           (trim-right (cdr l)) l)))
           (trim-left  (lambda (l)
                         (if (and (list>0? l) (nl? (cAr l)))
                           (trim-left  (cDr l)) l))))
  (list->string (trim-right (trim-left (string->list s))))))

(define (coqml-space-cleaning env l)
  ;; Drop blank lines. Trim newlines at begin and end of strings.
  ;; Conserve spaces. Put text in string tags.
  (set! l (filter (lambda (x) (or (nstring? x)
                                  (not (blank? x)))) l))
  (if (and (nnull? l) (null? (filter nstring? l)))
    (list (trim-newlines (apply string-append l)))
    (map (lambda (x) (if (string? x) `(c:string ,(trim-newlines x)) x)) l)))

(define coqml-pre    coqml-space-cleaning)
(define coqml-elem   htmltm-space-element)

(tm-define (coqml-handler model method)
  ;;  model:  content model category
  ;;          :element -- text nodes are ignored
  ;;          :pre -- Drop blank lines. Trim newlines at beginning and ending
  ;;            of strings.  Conserve spaces. Put text in string tags.
  ;;  method: <procedure> to convert the element content to a node-list.
  (if (not (in? model '(:pre :elem)))
      (error "Bad model: " model))
  (if (not (procedure? method))
      (error "Bad method: " method))
  (let ((clean (cond ((eq? model :pre)  coqml-pre)
                     ((eq? model :elem) coqml-elem))))
    (let ((proc method))
      (lambda (env a c)
        (proc env a (clean env c))))))

(tm-define (coqml-serial p? l)
  (if p? (stm-serial l stm-document?)
      (stm-serial l stm-document? htmltm-make-line htmltm-make-concat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generic XML dispatcher
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sxml-meta-logic-ref ns-id ncname)
  (cond ((== ns-id "h") (logic-ref htmltm-methods% ncname))
	((== ns-id "m") (logic-ref mathtm-methods% ncname))
	((== ns-id "g") (logic-ref gallinatm-methods% ncname))
	((== ns-id "c") (logic-ref coqml-methods% ncname))
	(else #f)))

(tm-define (sxml-dispatch x-string x-pass env t)
  ;; Generic xml dispatcher
  ;; @x-string (string env -> node-list) used to convert strings
  ;; @x-pass (method) pass method
  ;; NOTE: method == (env attrlist content -> node-list)
  (cond ((string? t) (x-string env t))
	((sxml-top-node? t) (x-pass env '() (sxml-content t)))
	((sxml-control-node? t) '())
	(else
	 (receive (ns-id ncname) (sxml-split-name (sxml-name t))
	   (cond ((sxml-meta-logic-ref ns-id (string->symbol ncname))
		  => (cut <> env (sxml-attr-list t) (sxml-content t)))
		 (else (x-pass env (sxml-attr-list t) (sxml-content t))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special serial constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For performance, we maintain the invariant that block structures are always
;; wrapped in an unary 'document'.
;;
;; To maintain this invariant, it is required that htmltm-methods which
;; introduce block structures be declared of :block type.
;;
;; htmltm-handler must also take care that 'expand' and 'with' methods which
;; take a block-structure as input have their result wrapped in a 'document'.

(tm-define (htmltm-serial p? l)
  (if p? (stm-serial l stm-document?)
      (stm-serial l stm-document? htmltm-make-line htmltm-make-concat)))

(define (htmltm-make-line l)
  ;; Trim whitespaces at ends of lines.
  ;; Drop lines which are empty after simplification.
  (stm-concat l htmltm-make-line/concat))

(define (htmltm-make-line/concat l)
  ;; TODO: preserve empty lines if creator is TeXmacs.
  (let ((x (stm-list->concat (stm-line-trim-both l))))
    (if (== "" x) #f x)))

(define (htmltm-make-concat l)
  ;; Trim whitespace around line-breaks.
  ;; Invisible nodes between line-breaks and whitespaces are moved.
  ;;
  ;; TODO: remove whitespaces surrounding invisibles in the middle of concat
  (stm-list->concat
   (if (null? l) '()
       (let ((line-lists (stm-parse-lines l)))
	 (if (null? (cdr line-lists)) l
	     (stm-unparse-lines
	      `(,(stm-line-trim-right (first line-lists))
		,@(map stm-line-trim-both (cDdr line-lists))
		,(stm-line-trim (last line-lists)))))))))

(tm-define (mathtm-serial env l)
  ;; Except for the top-level math element, MathML produce only inlines.
  ;; Collapse whitespaces.
  ;; TODO: consolidate with htmltm-serial
  (with c (apply tmconcat l)
    (if (not (func? c 'concat)) c
	(stm-concat (cdr c) htmltm-make-concat))))
