
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

(define (tree-search-tag-sub t tag)
  (cond ((tree-atomic? t) #f)
        ((and (tree-is? t 'explain-macro)
              (tree-atomic? (tree-ref t 0))
              (== (tree->string (tree-ref t 0)) tag)) #t)
        (else (list-find (tree-children t) (cut tree-search-tag-sub <> tag)))))

(define (tree-search-tag t tag)
  (cond ((tree-atomic? t) (list))
        ((and (tree-func? t 'explain 2)
              (tree-search-tag-sub (tree-ref t 0) tag))
         (list t))
        (else (append-map (cut tree-search-tag <> tag) (tree-children t)))))

(define (url-search-tag u tag)
  (tree-search-tag (tree-load-inclusion u) tag))

(tm-define (tmdoc-search-tag tag)
  (if (symbol? tag) (set! tag (symbol->string tag)))
  (let* ((docpath (string->url "$TEXMACS_DOC_PATH"))
         (docfiles (url-append docpath (url-any)))
         (what (string-append "<explain-macro|" tag "|"))
         (candidates (url->list (url-grep what docfiles)))
         (fragments (append-map (cut url-search-tag <> tag) candidates)))
    (and (nnull? fragments)
         (apply tree (cons 'document fragments)))))
