
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : stm.scm
;; DESCRIPTION : TeXmacs data as S-expressions
;; COPYRIGHT   : (C) 2002  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools stm))
;; TODO: clean public interface

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of serial nodes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define stm-serial
  ;; Convert a texmacs node list @l to a texmacs serial node.
  ;;
  ;; A serial node is either a 'concat' or a 'document'.
  ;; If no node in @l is a block-structure, build a 'concat'.
  ;; Otherwise, build a 'document' such as every block-structure is a subnode,
  ;; and consecutive non-block-structures are gathered in concat subnodes.
  ;; Zero arity 'concat' are expanded to null-string.
  ;; Zero arity 'document' are interpreted as block separators.
  ;;
  ;; The optional @block? parameter tells wether a given node is a block
  ;; structure, the default predicate needs to traverse the whole tree to
  ;; identify a line-structure.
  ;;
  ;; The optional @make-concat parameter builds a generalized concat from a
  ;; list of simplified line items. It may be specified to collapse consecutive
  ;; spaces or to trim spaces around line breaks.
  ;;
  ;; The optional @make-line parameter build a generalized concat from a list
  ;; of line items. This list is a deconstructed concat produced by
  ;; @make-concat. It is used when building a line from a series of adjacent
  ;; line structures between block structures. If it returns #f, no line is
  ;; produced. It may be specified to trim spaces at ends of lines or drop
  ;; empty lines produced by inline nodes.
  ;;
  ;; If you want to prevent some line items from concat simplification and
  ;; postprocessing during line building, you have to wrap them in a structure.
  ;; Which is the correct thing to do anyway because such line items would be
  ;; logically different from the surrounding text.
  (case-lambda
   ((l) (stm-serial l stm-block-structure?))
   ((l b?) (stm-serial l b? stm-concat stm-list->concat))
   ((l block? make-line make-concat)
    (define (cons-line line blocks)
      (let ((new-block (make-line (stm-concat->list (make-concat line)))))
	(if new-block (cons new-block blocks) blocks)))
    (let ((b?s (map block? l)))
      (if (list-any noop b?s)
	  (stm-list->document (serial/blocks l b?s cons-line))
	  (stm-concat l make-concat))))))

(define (serial/blocks l b?s cons-line)
  ;; Collect all line-structure in concat nodes between block-structures in @l.
  ;; @b?s tells which items of @l are block-structures.
  ;; Performs one-level 'document' simplification.
  (define (kons x b? l+b) (serial/blocks/kons x b? l+b cons-line))
  (receive (line blocks)
    (car+cdr (list-fold-right kons '(().()) l b?s))
    (if (null? line) blocks (cons-line line blocks))))

(define (serial/blocks/kons x b? line+blocks cons-line)
  (define (sub line blocks)
    (cond ((not b?) (cons (cons x line) blocks))
	  ((pair? line)
	   (sub '() (cons-line line blocks)))
	  ((stm-document? x)
	   (cons '() (append (stm-document->list x) blocks)))
	  (else (cons '() (cons x blocks)))))
  (call-with-values (lambda () (car+cdr line+blocks)) sub))

(tm-define stm-concat
  ;; Build a concat node from an list of inline nodes.
  ;; Performs concat simplification and catenate adjacent string nodes.
  ;; Drop root concat if it contains only one item.
  ;; Return null-string instead of an empty concat.
  ;; The optional @make-concat parameter is used to build the concat node from
  ;; the simplified item list. It may be specified to hook postprocessing in.
  (case-lambda
   ((l) (stm-concat l stm-list->concat))
   ((l make-concat)
    (make-concat
     (receive (strs line)
       (car+cdr (list-fold-right concat/kons
				 '(().()) (append-map stm-concat->list l)))
       (concat/flush strs line))))))

(define (concat/kons x strs+line)
  (receive (strs line) (car+cdr strs+line)
    (if (string? x)
	(cons (cons x strs) line)
	;; concats have been simplified before
	(cons '() (cons x (concat/flush strs line))))))

(define (concat/flush strs line)
  (let ((str (string-concatenate strs)))
    (if (string-null? str) line (cons str line))))

;; Document and concat constructors and deconstructors

(tm-define (stm-list->concat l)
  ;; This is the reverse of concat->list.
  ;; Both functions assume that (concat) is equivalent to null-string.
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-define (stm-concat->list x)
  (if (stm-concat? x)
      (if (null? (cdr x)) '("") (cdr x))
      (list x)))

(tm-define (stm-list->document l)
  (cons 'document l))

(tm-define (stm-document->list x)
  (cdr x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Texmacs objects predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stm-primitive? label)
  (:synopsis "Is it the label of a primitive texmacs construct?")
  (:type (-> symbol bool))
  (not (tree-label-extension? label)))

(tm-define (stm-block-structure? x)
  (:synopsis "Is the texmacs document fragment @x a block-level structure?")
  (:type (-> stree bool))
  (tree-multi-paragraph? (stree->tree x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Physical Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Basic predicates

(tm-define (stm-document? x) (func? x 'document))
(tm-define (stm-concat? x) (func? x 'concat))
(tm-define (stm-paragraph? x) (func? x 'para))
(tm-define (stm-with? x) (func? x 'with))
(tm-define (stm-with-document? x)
  (and (stm-with? x) (stm-document? (last x))))
(tm-define (stm-label? x) (func? x 'label))
(tm-define (stm-line-break? x)
  (in? x '((new-line) (next-line))))

;; Expansion predicates

(define (stm-compound-unary? x)
  (and (pair? x) (== (first x) 'compound) (= 3 (length x))))

(define (stm-compound-document? x)
  (and (stm-compound-unary? x) (stm-document? (third x))))

(define (stm-implicit-compound? x)
  (and (pair? x) (not (stm-primitive? (first x)))))

(define (stm-implicit-compound-unary? x)
  (and (stm-implicit-compound? x) (= 2 (length x))))

(define (stm-implicit-compound-document? x)
  (and (stm-implicit-compound-unary? x) (stm-document? (second x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logical Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; WARNING: all these functions should use DRD

(tm-define (stm-list-environment? head)
  (in? (first head) '(itemize enumerate description)))

(tm-define (stm-block-environment? head)
  ;; A block environment must always contain a document node (not a concat).
  (or (and (== (first head) 'with) (in? (second head) '("par-mode")))
      (in? (first head) '(quotation code))
      (stm-list-environment? head)))

(tm-define (stm-list-marker? x)
  (or (== x '(item))
      (func? x 'item*)))

(tm-define (stm-section-environment? head)
  (in? (first head)
       '(part chapter section subsection subsubsection
	 paragraph subparagraph)))

(define (stm-section-accepts? x)
  ;; Can a Valid document contain @x inside a section-environment?
  (not (stm-label? x)))

(tm-define (stm-invisible? x)
  (stm-label? x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stm-unary-document t)
  (if (stm-document? t) t `(document ,t)))

(tm-define (stm-remove-unary-document t)
  (if (func? t 'document 1) (second t) t))

(tm-define (stm-insert-first-data serial x)
  ;; Given a @serial and a line node @x (must not be a block-structure), return
  ;; @serial with @x inserted at the first possible position, doing its best
  ;; not to create any new paragraph and not to change a Valid @serial into an
  ;; Invalid one.
  ;;
  ;; Assumes that x is limited to the Structural subset (read, x contains
  ;; nothing fancy) and is simplified (it contains no empty serials).
  ;;
  ;; To preserve Validity, insertion will be done after (item) and inside
  ;; (item*) and section environments. If @x is not allowed inside section
  ;; environment it will be concat'ed after the section.
  ;;
  ;; TODO: merge this with the DRD logic of stm-block-structure?
  (let rec ((ser serial))
    (cond ((string? ser)
	   (stm-concat (list x ser)))
	  ((stm-concat? ser)
	   (stm-concat (cons (rec (second ser)) (cddr ser))))
	  ((stm-paragraph? ser)
	   `(para ,(rec (second ser)) ,@(cddr ser)))
	  ((stm-document? ser)
	   `(document ,(rec (second ser)) ,@(cddr ser)))
	  ((stm-compound-document? ser)
	   `(,(first ser) ,(second ser) ,(rec (third ser))))
	  ((stm-with-document? ser)
	   (rcons (but-last ser) (rec (last ser))))
	  ;; WARNING: should be DRD aware
	  ((== ser '(item)) (stm-concat (list ser x)))
	  ((func? ser 'item* 1) `(item* ,(rec (second ser))))
	  ((stm-section-environment? ser)
	   (if (stm-section-accepts? x)
	       `(,(first ser) ,(rec (second ser)))
	       ;; if section contains a document, produce invalid structure
	       (stm-concat (list ser x))))
	  ((stm-implicit-compound-document? ser)
	   `(,(first ser) ,(rec (second ser))))
	  (else (stm-serial (list x ser))))))

(tm-define (stm-first-data doc-item)
  ;; Given a doc-item, return its first data node. That is the first node found
  ;; by preorder traversal which is neither a 'concat' nor a 'paragraph'.
  ;;
  ;; If traversal finds an empty paragraph-item, return "".
  ;;
  ;; Assume the doc-item is partially concat and paragraph simplified.
  (let rec ((x doc-item))
    (if (string? x) x
	(if (or (eq? 'concat (car x))
		(eq? 'para (car x)))
	    (rec (cadr x))	      ; assume at least one para/concat-item
	    x))))

(tm-define (stm-remove-first-data doc-item)
  ;; Given a doc-item, remove its first data node. Simplify concats and
  ;; preserve paragraph structure. If the first data node is an empty
  ;; paragraph-item, do nothing.
  ;;
  ;; Assume the doc-item is partially concat and paragraph simplified.
  (let rec ((x doc-item))
    (cond ((string? x) "")
	  ((eq? 'para (car x))		; assume at least one para-item
	   `(para ,(rec (cadr x)) ,@(cddr x)))
	  ((eq? 'concat (car x))	; assume at least one concat-item
	   (if (null? (cddr x)) ""	; simplify null concat
	       `(concat ,@(cddr x))))
	  (else ""))))

(tm-define (stm-list-map proc pred doc-body)
  ;; General mapping over LaTeX-style lists
  ;; proc: mark item -> x. Mapping function
  ;; pred: doc-item -> ?. Is the doc-item an list item start?
  ;; doc-body: List body.
  ;;
  ;; Each item is the list of doc-items between the start of the doc-body,
  ;; doc-items whose first data is a marker and the end of the doc-body. The
  ;; proc is applied with the marker of the current item as first parameter,
  ;; and the item data as second parameter. If the first doc-item does not
  ;; start by a mark, the first application is done with mark=#f.
  (define (marked? x) (pred (stm-first-data x)))
  (if (null? doc-body) '()
      (let rec ((doc-body (cdr doc-body))
		(mark (if (marked? (car doc-body))
			  (stm-first-data (car doc-body))
			  #f))
		(accum (list (if (marked? (car doc-body))
				 (stm-remove-first-data (car doc-body))
				 (car doc-body)))))
	(cond ((null? doc-body)
	       (list (proc mark (reverse accum))))
	      ((marked? (car doc-body))
	       (cons (proc mark (reverse accum))
		     (rec (cdr doc-body)
			  (stm-first-data (car doc-body))
			  (list (stm-remove-first-data (car doc-body))))))
	      (else (rec (cdr doc-body) mark
			 (cons (car doc-body) accum)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: extend to allow preservation of the line break nodes through a
;;   parsing-unparsing cycle.

(tm-define (stm-parse-lines l)
  (list-fold-right parse-lines/kons '(().()) l))

(define (parse-lines/kons kar kdr)
  (receive (line tail) (car+cdr kdr)
    (if (stm-line-break? kar)
	(cons '() (cons line tail))
	(cons (cons kar line) tail))))

(tm-define (stm-unparse-lines l)
  (list-concatenate (list-intersperse l '((next-line)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line trimming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stm-line-trim-both l)
  (cond ((null? l) '())
	((pair? (cdr l)) (stm-line-trim-right (stm-line-trim l)))
	((string? (first l)) (list (tm-string-trim-both (first l))))
	(else l)))

(tm-define (stm-line-trim l)
  (line-trim/sub l tm-string-trim))

(tm-define (stm-line-trim-right l)
  (reverse! (line-trim/sub (reverse l) tm-string-trim-right)))

(define (line-trim/sub l trim)
  (let rec ((l l))
    (receive (invisibles visibles) (list-span l stm-invisible?)
      (if (null? visibles) l
	  (receive (kar kdr) (car+cdr visibles)
	    (append invisibles
		    (if (string? kar)
			(let ((trimmed-kar (trim kar)))
			  (if (string-null? trimmed-kar)
			      (rec kdr)
			      (cons trimmed-kar kdr)))
			(cons kar kdr))))))))
