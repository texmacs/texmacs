
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : sxml.scm
;; DESCRIPTION : XML data as S-expressions
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools sxml))

;; Fundamental acessors
(tm-define sxml-name car)
(define sxml-attr-list! cdadr)

(define (sxml-has-attr-list? e)
  ;; Has the element e an attribute node?
  (and (pair? (cdr e))
       (pair? (cadr e))
       (eq? '@ (caadr e))))

(tm-define (sxml-element-head e)
  ;; Element name and attributes (if present).
  (if (sxml-has-attr-list? e)
      (list (car e) (cadr e))
      (list (car e))))

(tm-define (sxml-content e)
  ;; Complement function of sxml-element-head.
  (if (sxml-has-attr-list? e) (cddr e) (cdr e)))

(tm-define (sxml-set-name e name)
  ;; Set the name of the sxml element e.
  ;; Name is the new element name as a string.
  (cons (string->symbol name) (cdr e)))

(tm-define (sxml-set-content e content)
  ;; Replace the subnodes of e with the node-set content.
  (append (sxml-element-head e) content))

(tm-define (sxml-prepend e l)
  ;; Prepend a node set l to the content of an element e.
  (append (sxml-element-head e) l
	  (sxml-content e)))

(define (sxml-attr-list? e)
  ;; Return the attribute list of element e or #f.
  (and (sxml-has-attr-list? e)
       (sxml-attr-list! e)))

(tm-define (sxml-attr-list e)
  ;; Return the attribute list of element e or empty list.
  (or (sxml-attr-list? e) '()))

(tm-define (shtml-attr-non-null as att)
  ;; Get an HTML attribute or false if the attribute is absent, is not set, or
  ;; is set to the empty string.
  ;; FIXME: this is ugly
  (and-let* ((l (assoc att as))
	     ((list-length=2? l))
	     ((not (string-null? (second l)))))
    (second l)))

(define (sxml-named-attr obj attr-name)
  ;; Named attribute of element e or #f.
  (and-let* ((l (sxml-attr-list? obj)))
    (assq attr-name l)))

(tm-define (sxml-attr obj attr-name)
  ;; Value of a named attribute of element e or #f.
  (and-let* ((x (sxml-named-attr obj attr-name)))
    (cadr x)))

(tm-define (sxml-set-attr e attr)
  ;; Set an attribute of an element e. Attr is a list (symbol? string?).
  ;; Create the attribute list or the attribute if necessary.
  (let ((attr-name (car attr)))
    `(,(sxml-name e)
      (@ ,attr ,@(list-filter (sxml-attr-list e)
			      (lambda (x) (not (eq? x attr-name)))))
      ,@(sxml-content e))))

(tm-define (sxml-set-attrs e attrs)
  ;; Set several attributes of an element e. Attrs is a list of attributes.
  ;; Create the attribute list or attributes if necessary.
  (let rec ((e e) (attrs attrs))
    (if (null? attrs) e
	(rec (sxml-set-attr e (car attrs)) (cdr attrs)))))

(tm-define (sxml-set-attrlist e attrs)
  ;; Replace the attribute list of @obj by @attrs.
  ;; If @attrs is #f, remove the attribute node.
  `(,(sxml-name e)
     ,@(if attrs `((@ ,@attrs)) '())
     ,@(sxml-content e)))

(define-macro (sxml-find-name-separator len)
  ;; optimized (string-rindex name #\:)
  ;; returns position of a separator between namespace-id and LocalName
  ;; (copied from sxml-tools)
  `(let rpt ((pos (1- ,len)))
     (cond
       ((negative? pos) #f)
       ((char=? #\: (string-ref name pos)) pos)
       (else (rpt (1- pos))))))

(tm-define (sxml-ncname obj)
  ;; Returns Local Part of Qualified Name (Namespaces in XML production [6])
  ;; for given obj, which is ":"-separated suffix of its Qualified Name
  ;; If a name of a node given is NCName (Namespaces in XML production [4]),
  ;; then it is returned as is.
  ;; Please note that while SXML name is a symbol this function returns a
  ;; string.
  ;; (copied from sxml-tools)
  (sxml-name->ncname (sxml-name obj)))

(tm-define (sxml-name->ncname sxml-name)
  (let* ((name (symbol->string sxml-name))
	 (len (string-length name)))
    (cond
      ((sxml-find-name-separator len)
       => (lambda (pos)
	    (substring name (+ pos 1) len)))
      (else name))))

(tm-define (sxml-name->ns-id sxml-name)
  ;; Returns namespace-id part of given name, or #f if it's LocalName
  ;; (copied from sxml-tools)
  (let* ((name (symbol->string sxml-name)))
    (cond
      ((sxml-find-name-separator (string-length name))
       => (lambda (pos)
	    (substring name  0 pos)))
      (else #f))))

(tm-define (sxml-split-name sxml-name)
  (let* ((name (symbol->string sxml-name))
	 (len (string-length name)))
    (cond
      ((sxml-find-name-separator len)
       => (lambda (pos)
	    (values (substring name  0 pos)
		    (substring name (+ pos 1) len))))
      (else (values #f name)))))

(tm-define (sxml-strip-ns-prefix prefix x)
  ;; Remove a given namespace prefix wherever it appears in element names.
  ;; Prefix must be the ns-prefix as a string, and x a document fragment.
  (let rec ((x x))
    (if (string? x) x
	(sxml-set-content
	 (if (== prefix (sxml-name->ns-id (sxml-name x)))
	     (sxml-set-name x (sxml-ncname x))
	     x)
	 (map rec (sxml-content x))))))

(tm-define (sxml-set-ns-prefix p x)
  (sxml-set-name x (string-append p ":" (sxml-ncname x))))

; Predicate which returns #t if <obj> is SXML element, otherwise returns #f.
; NOTE: *TOP* is a special element. All element operations are applicable.
(tm-define (sxml-element? obj)
   (and (pair? obj)
	(symbol? (car obj))
	(not (memq (car obj)
	'(@ @@ *PI* *COMMENT* *ENTITY*)))))

; The function ntype-names?? takes a list of acceptable node names as a
; criterion and returns a function, which, when applied to a node,
; will return #t if the node name is present in criterion list and #f
; othervise.
;	ntype-names?? :: ListOfNames -> Node -> Boolean
(tm-define (ntype-names?? crit)
  (lambda(node)
    (and (pair? node)
	 (memq (car node) crit))))

(tm-define (sxml-control-node? x)
  (and (nstring? x)
       (let ((name (symbol->string (sxml-name x))))
	 (and (string-starts? name "*")
	      (string-ends? name "*")))))

(tm-define (sxml-top-node? x)
  (and (nstring? x)
       (== '*TOP* (car x))))

(tm-define (sxml-filter-element-content l)
  (list-filter l (negate string?)))
