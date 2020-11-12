
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-edit.scm
;; DESCRIPTION : editing macros
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-edit)
  (:use (utils library cursor)
        (generic document-part)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the definition of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (first-match l pred?)
  (cond ((null? l) #f)
        ((pred? (car l)) (pred? (car l)))
        (else (first-match (cdr l) pred?))))

(define (get-definition** l t)
  (cond ((and (tree-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) l)) t)
        ((tree-atomic? t) #f)
        ((tree-in? t '(document concat surround with))
         (first-match (reverse (tree-children t))
                      (cut get-definition** l <>)))
        (else #f)))

(tm-define (get-definition* l t)
  (cond ((tm-func? t 'hide-preamble 1) (get-definition** l (tree-ref t 0)))
        ((tm-func? t 'show-preamble 1) (get-definition** l (tree-ref t 0)))
        ((tree-atomic? t) #f)
        ((tree-in? t '(document concat surround with))
         (first-match (tree-children t) (cut get-definition* l <>)))
        (else #f)))

(tm-define (get-definition l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (get-definition* l (buffer-tree))
      (tree 'assign l (get-init-tree l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rendering of edit-macro tag
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-edit-macro a)
  (:secure #t)
  (let* ((c (tree-children a))
         (name (car c))
	 (name* (if (not (tm-equal? name "")) name
		    `(concat (with "color" "red" "font-shape" "italic"
				   "enter-name") ,name)))
         (args (cDr (cdr c)))
         (args* (map (lambda (x) `(src-arg ,x)) args))
         (body (cAr c))
	 (body* (if (not (and (tm-equal? name "") (tm-equal? body ""))) body
		    `(concat (with "color" "red" "font-shape" "italic"
				   "enter-body") ,body))))		    
    `(with "par-first" "0em" "par-par-sep" "0.5em"
       (document
         (concat
           (inline-tag ,name* ,@args*)
           " "
           (math "<assign>"))
         ,body*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listing the available macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-load-style* s)
  (with f (if (string-ends? s ".ts") s (string-append s ".ts"))
    (if (url-exists? (url-relative (current-buffer) f))
        (tree-load-style (url->string (url-relative (current-buffer) f)))
        (tree-load-style s))))

(define (list-macros-in t)
  (cond ((not (tree? t)) (list))
        ((and (tree-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (tm-in? (tree-ref t 1) '(macro xmacro)))
         (list (tree->string (tree-ref t 0))))
        ((tree-in? t '(document concat surround with))
         (append-map list-macros-in (tree-children t)))
        (else (list))))

(tm-define (built-in-style? s)
  (with d (url-complete (url-append "$TEXMACS_PATH/styles" (url-any)) "dr")
    (with name (string-append s ".ts")
      (with f (url-complete (url-append (url-expand d) name) "fr")
        (nnull? (url->list (url-expand f)))))))

(tm-define (get-public-style-list)
  (with st (get-style-list)
    (if (null? st) st
        (with l (list-filter (cdr st) (negate hidden-package?))
          (if (built-in-style? (car st)) l
              (cons (car st) l))))))

(tm-define (get-macro-list type . opts)
  (cond ((and (nnull? opts) (== (car opts) :sort))
         (sort (apply get-macro-list (cons type (cdr opts))) string<=?))
        ((and (nnull? opts) (integer? (car opts)))
         (with l (apply get-macro-list (cons type (cdr opts)))
           (sublist l 0 (min (length l) (car opts)))))
        ((== type :preamble)
         (list-macros-in (document-get-preamble (buffer-tree))))
        ((== type :packages)
         (append-map get-macro-list (get-public-style-list)))
        ((== type :all)
         (append (get-macro-list :preamble) (get-macro-list :packages)))
        ((string? type)
         (list-macros-in (tree-load-style* type)))
        (else (list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching a definition in style files and packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-style-definition-in done l name)
  (when (and (tree? name) (tree-atomic? name))
    (set! name (tree->string name)))
  (and (string? name)
       (not (in? name done))
       (with t (tree-load-style* name)
         (search-style-definition (cons name done) l t))))

(define (search-style-definition-in-list done l packs)
  (first-match (reverse packs)
               (cut search-style-definition-in '() l <>)))

(define (search-style-definition done l t)
  (cond ((and (tree-func? t 'assign 2)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) l))
         (car done))
        ((tree-atomic? t) #f)
        ((tree-is? t 'use-package)
         (search-style-definition-in-list done l (tree-children t)))
        ((tree-in? t '(document concat surround with))
         (first-match (reverse (tree-children t))
                      (cut search-style-definition done l <>)))
        (else #f)))

(define (search-style-package l)
  (with packs (get-style-list)
    (search-style-definition-in-list '() l packs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct editing of the source of a macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (edit-assign-macro def)
  (and-with m (tree-ref def 1)
    (if (tree-is? m 'macro)
        (tree-go-to m :last :start)
        (tree-go-to m :start))
    #t))

(define (edit-macro-in-preamble l)
  (with b (buffer-tree)
    (and-with def (get-definition* l b)
      (when (tree-is? (tree-ref b 0) 'hide-preamble)
        (tree-assign-node (tree-ref (buffer-tree) 0) 'show-preamble)
        (with other `(ignore (document ,@(cdr (tree-children b))))
          (tree-remove! b 1 (- (tree-arity b) 1))
          (tree-insert! b 1 (list other))))
      (edit-assign-macro def))))

(define (edit-macro-in-style-file l)
  (and-with name (search-style-package l)
    (let* ((style-name (string-append name ".ts"))
           (style-url (url-append "$TEXMACS_STYLE_PATH" style-name))
           (file-name (url-resolve style-url "r")))
      (cursor-history-add (cursor-path))
      (load-document file-name)
      (delayed
        (:idle 1)
        (and-with def (get-definition** l (buffer-tree))
          (edit-assign-macro def))))))

(tm-define (has-macro-source? l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (get-definition* l (buffer-tree))
      (search-style-package l)))

(tm-define (edit-macro-source l)
  (if (symbol? l) (set! l (symbol->string l)))
  (or (edit-macro-in-preamble l)
      (edit-macro-in-style-file l)))

(tm-define (macro-label t)
  (cond ((tm-atomic? t) 'string)
        ((and (tm-is? t 'compound) (tm-atomic? (tm-ref t 0)))
         (string->symbol (tm->string (tm-ref t 0))))
        ((tm-compound? t) (tm-label t))
        (else #f)))

(tm-define (edit-focus-macro-source)
  (edit-macro-source (macro-label (focus-tree))))
