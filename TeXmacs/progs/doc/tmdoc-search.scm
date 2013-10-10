
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

(define (explain-macro? t tag)
  (and (tm-is? t 'explain-macro)
       (tm-atomic? (tm-ref t 0))
       (== (tm->string (tm-ref t 0)) tag)))

(define ((wrap-explain pred?) t)
  (and (tm-func? t 'explain 2)
       (tm-find (tree-ref t 0) pred?)))

(define (url-search-tag u tag)
  (with pred? (wrap-explain (cut explain-macro? <> tag))
    (tm-search (tree-load-inclusion u) pred?)))

(tm-define (tmdoc-search grep-string searcher)
  (let* ((docpath (string->url "$TEXMACS_DOC_PATH"))
         (docfiles (url-append docpath (url-any)))
         (candidates (url->list (url-grep grep-string docfiles)))
         (fragments (append-map searcher candidates)))
    (and (nnull? fragments)
         (tm->tree `(document ,@fragments)))))

(tm-define (tmdoc-search-tag tag)
  (if (symbol? tag) (set! tag (symbol->string tag)))
  (tmdoc-search (string-append "<explain-macro|" tag "|")
                (cut url-search-tag <> tag)))
