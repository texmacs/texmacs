
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmdoc-search.scm
;; DESCRIPTION : search documentation for specific entity (tag, package, etc.)
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc tmdoc-search))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract master routine for searching in documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-search-local-one grep-string searcher lan)
  (let* ((docpath (string->url "$TEXMACS_DOC_PATH"))
         (suffix (url-wildcard (string-append "*." lan ".tm")))
         (docfiles (url-append docpath (url-append (url-any) suffix)))
         (candidates (url->list (url-grep grep-string docfiles))))
    (append-map searcher candidates)))

(tm-define (tmdoc-search-local-several queries lan)
  (if (and (nnull? queries) (nnull? (cdr queries)))
      (append (tmdoc-search-local-one (car queries) (cadr queries) lan)
	      (tmdoc-search-local-several (cddr queries) lan))
      (list)))

(tm-define (tmdoc-search-local queries lan)
  (with l (tmdoc-search-local-several queries lan)
    (and (nnull? l) (tm->tree `(document ,@l)))))

(tm-define (tmdoc-search . queries)
  ;; queries is a list which alternates grep-strings and search routines
  (with lan (string-take (language-to-locale (get-output-language)) 2)
    (or (tmdoc-search-local queries lan)
        (and (!= lan "en")
             (tmdoc-search-local queries "en")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((wrap-explain pred?) t)
  (and (tm-func? t 'explain 2)
       (tm-find (tree-ref t 0) pred?)))

(define (url-search-exact u what)
  (with pred? (wrap-explain (cut tm-equal? <> what))
    (tm-search (tree-load-inclusion u) pred?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching TeXmacs styles or packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdoc-search-style style)
  (tmdoc-search (string-append "<tmstyle|" style ">")
                (cut url-search-exact <> `(tmstyle ,style))
		(string-append "<tmpackage|" style ">")
                (cut url-search-exact <> `(tmpackage ,style))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching TeXmacs tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (explain-macro? t tag)
  (and (tm-is? t 'explain-macro)
       (tm-atomic? (tm-ref t 0))
       (== (tm->string (tm-ref t 0)) tag)))

(define (url-search-explain-macro u tag)
  (with pred? (wrap-explain (cut explain-macro? <> tag))
    (tm-search (tree-load-inclusion u) pred?)))

(tm-define (tmdoc-search-tag tag)
  (if (symbol? tag) (set! tag (symbol->string tag)))
  (tmdoc-search (string-append "<explain-macro|" tag "|")
                (cut url-search-explain-macro <> tag)
		(string-append "<markup|" tag ">")
                (cut url-search-exact <> `(markup ,tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (var-val? t var)
  (and (tm-is? t 'var-val)
       (tm-atomic? (tm-ref t 0))
       (== (tm->string (tm-ref t 0)) var)))

(define (url-search-parameter u var)
  (with pred? (wrap-explain (cut var-val? <> var))
    (tm-search (tree-load-inclusion u) pred?)))

(tm-define (tmdoc-search-parameter var)
  (if (symbol? var) (set! var (symbol->string var)))
  (tmdoc-search (string-append "<var-val|" var "|")
                (cut url-search-parameter <> var)
		(string-append "<src-var|" var ">")
                (cut url-search-exact <> `(src-var ,var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching scheme functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (name-starts? t prefix)
  (cond ((tm-atomic? t) (string-starts? (tm->string t) prefix))
        ((tm-in? t '(concat document))
         (with l (tm-children t)
           (and (nnull? l) (name-starts? (car l) prefix))))
        (else #f)))

(define (scheme-function? t fun)
  (and (tm-func? t 'scm 1)
       (or (name-starts? (tm-ref t 0) (string-append "(" fun " "))
           (name-starts? (tm-ref t 0) (string-append "(" fun ")")))))

(define (url-search-scheme-function u fun)
  (with pred? (wrap-explain (cut scheme-function? <> fun))
    (tm-search (tree-load-inclusion u) pred?)))

(tm-define (tmdoc-search-scheme f)
  (let* ((fun (string->tmstring f))
         (fun* (string-replace (string-replace fun "<" "\\<") ">" "\\>")))
    (tmdoc-search (string-append "<scm|(" fun*)
                  (cut url-search-scheme-function <> fun)
		  (string-append "<scm|" fun* ">")
                  (cut url-search-exact <> `(scm ,fun)))))
