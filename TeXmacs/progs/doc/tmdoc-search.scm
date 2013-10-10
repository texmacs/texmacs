
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

(tm-define (tmdoc-search-local grep-string searcher lan)
  (let* ((docpath (string->url "$TEXMACS_DOC_PATH"))
         (suffix (url-wildcard (string-append "*." lan ".tm")))
         (docfiles (url-append docpath (url-append (url-any) suffix)))
         (candidates (url->list (url-grep grep-string docfiles)))
         (fragments (append-map searcher candidates)))
    (and (nnull? fragments)
         (tm->tree `(document ,@fragments)))))

(tm-define (tmdoc-search grep-string searcher)
  (with lan (string-take (language-to-locale (get-output-language)) 2)
    (or (tmdoc-search-local grep-string searcher lan)
        (and (!= lan "en")
             (tmdoc-search-local grep-string searcher "en")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((wrap-explain pred?) t)
  (and (tm-func? t 'explain 2)
       (tm-find (tree-ref t 0) pred?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching TeXmacs tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (explain-macro? t tag)
  (and (tm-is? t 'explain-macro)
       (tm-atomic? (tm-ref t 0))
       (== (tm->string (tm-ref t 0)) tag)))

(define (url-search-tag u tag)
  (with pred? (wrap-explain (cut explain-macro? <> tag))
    (tm-search (tree-load-inclusion u) pred?)))

(tm-define (tmdoc-search-tag tag)
  (if (symbol? tag) (set! tag (symbol->string tag)))
  (tmdoc-search (string-append "<explain-macro|" tag "|")
                (cut url-search-tag <> tag)))

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
                (cut url-search-parameter <> var)))

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

(tm-define (tmdoc-search-scheme-function f)
  (let* ((fun (string->tmstring f))
         (fun* (string-replace (string-replace fun "<" "\\<") ">" "\\>")))
    (tmdoc-search (string-append "<scm|(" fun*)
                  (cut url-search-scheme-function <> fun))))
