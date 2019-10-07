
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : lp-edit.scm
;; DESCRIPTION : editing literate programs
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils literate lp-edit)
  (:use (utils library cursor)
        (generic document-edit)
        (dynamic dynamic-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DRD properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group chunk-tag
  generic-chunk verbatim-chunk scm-chunk cpp-chunk mmx-chunk
  python-chunk scilab-chunk shell-chunk scala-chunk java-chunk)

(define-group variant-tag
  (chunk-tag))

(define-group similar-tag
  (chunk-tag))

(define-group appended-tag
  folded-newline-before unfolded-newline-before
  folded-opening unfolded-opening
  folded-ending unfolded-ending)

(define-fold folded-newline-before unfolded-newline-before)
(define-fold folded-opening unfolded-opening)
(define-fold folded-ending unfolded-ending)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching chunks in document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm-chunk? t)
  (and (tm-in? t (chunk-tag-list)) (== (tm-arity t) 4)))

(tm-define (search-chunks t)
  (cond ((tm-atomic? t) (list))
        ((tm-func? t 'document)
         (append-map search-chunks (tm-children t)))
        ((tm-chunk? t)
         (if (and (tm-atomic? (tm-ref t 0))) (list t) (list)))
        (else
          (with l (list-filter (tm-children t) (cut tm-func? <> 'document))
            (append-map search-chunks l)))))

(tm-define (search-named-chunks t name)
  (with l (search-chunks t)
    (list-filter l (lambda (c) (== (tm->string (tm-ref c 0)) name)))))

(tm-define (search-chunk-types t)
  (let* ((l (search-chunks t))
         (r (map (lambda (c) (tm->string (tm-ref c 0))) l)))
    (list-remove-duplicates r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maintaining states (links to previous and next chunks)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (update-chunk-states name)
  (with l (search-named-chunks (buffer-tree) name)
    (when (nnull? l)
      (tree-set (tm-ref (car l) 1) "false")
      (for (x (cdr l))
        (tree-set (tm-ref x 1) "true"))
      (tree-set (tm-ref (cAr l) 2) "false")
      (for (x (cDr l))
        (tree-set (tm-ref x 2) "true")))))

(tm-define (update-all-chunk-states)
  (for (name (search-chunk-types (buffer-tree)))
    (update-chunk-states name)))

(tm-define (update-document what)
  (:require (style-has? "literate-dtd"))
  (former what)
  (when (or (== what "all") (== what "chunks"))
    (update-all-chunk-states)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new chunks; semi-automatic determination of appropriate language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (chunk-format name)
  (with suffix (locase-all (url-suffix name))
    (cond ((== suffix "") "generic")
          ((in? suffix '("txt")) "verbatim")
          ((in? suffix '("scm")) "scm")
          ((in? suffix '("scala")) "scala")
          ((in? suffix '("java")) "java")
          ((in? suffix '("c" "cc" "cpp" "h" "hh" "hpp")) "cpp")
          ((in? suffix '("mmx" "mmh")) "mmx")
          ((in? suffix '("py")) "python")
          ((in? suffix '("sce" "sci")) "scilab")
          ((in? suffix '("bat" "sh")) "shell")
          (else "verbatim"))))

(define (chunk-tag name)
  (string->symbol (string-append (chunk-format name) "-chunk")))

(define (similar-chunk-tag name)
  (with l (search-named-chunks (buffer-tree) name)
    (if (null? l) (chunk-tag name) (tree-label (car l)))))

(tm-define (insert-new-chunk tag)
  (insert-go-to `(,tag "" "false" "false" (document "")) '(0 0)))

(tm-define (insert-next-chunk name)
  (:argument name "Chunk name")
  (with tag (similar-chunk-tag name)
    (insert-go-to `(,tag ,name "true" "false" (document "")) '(3 0))
    (update-chunk-states name)))

(tm-define (kbd-enter t shift?)
  (:require (and (tm-chunk? t) (cursor-inside? (tm-ref t 0))))
  (let* ((name (if (tm-atomic? (tm-ref t 0)) (tm->string (tm-ref t 0)) ""))
         (tag (chunk-tag name)))
    (when (!= tag 'generic-chunk)
      (tree-assign-node! t tag))
    (tree-go-to t 3 :start)
    (update-all-chunk-states)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing chunks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-remove t forwards?)
  (:require (tm-chunk? t))
  (cond ((selection-active-any?)
         (former t forwards?)
         (update-all-chunk-states))
        ((and (tree-empty? (tm-ref t 0)) (tree-empty? (tm-ref t 3)))
         (tree-select t)
         (clipboard-cut "nowhere")
         (update-all-chunk-states))
        ((and (tree-cursor-at? t 0 :start) (not forwards?))
         (tree-go-to t :start))
        ((and (tree-cursor-at? t 0 :end) forwards?)
         (tree-go-to t 3 :start))
        ((and (tree-cursor-at? t 3 :start) (not forwards?))
         (tree-go-to t 0 :end))
        ((and (tree-cursor-at? t 3 :end) forwards?)
         (tree-go-to t :end))
        ((cursor-inside? (tree-ref t 0))
         (former t forwards?)
         (update-all-chunk-states))
        (else (former t forwards?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ((inside-named-chunk? name nr same-type? other?) t)
  (and-with c (tree-search-upwards t tm-chunk?)
    (and (or (not same-type?)
             (tm-equal? (tm-ref c 0) name))
         (or (not other?)
             (!= (tree->path (tree-ref c 0))
                 (tree->path name)))
         (cursor-inside? (tm-ref c nr)))))

(define (go-to-next-in-chunk fun same-type? other?)
  (with-innermost t tm-chunk?
    (if (not t) (fun)
        (let* ((name (tm-ref t 0))
               (nr (tree-index (tree-down t)))
               (inside? (inside-named-chunk? name nr same-type? other?)))
          ;;(go-to-next-inside fun inside?)))))
          (go-to-next-such-that fun inside?)))))

(define (go-to-start-chunk)
  (with-innermost t tm-chunk?
    (when (and t (tree-down t))
      (with nr (tree-index (tree-down t))
        (tree-go-to t nr :start)))))

;;(tm-define (kbd-horizontal t forwards?)
;;  (:require (tm-chunk? t))
;;  (go-to-next-in-chunk (if forwards? go-right go-left) #f #f))

(tm-define (kbd-vertical t downwards?)
  (:require (tm-chunk? t))
  (go-to-next-in-chunk (if downwards? go-down go-up) #f #f))

(tm-define (kbd-incremental t downwards?)
  (:require (tm-chunk? t))
  (go-to-next-in-chunk (if downwards? go-down go-up) #f #t)
  (go-to-start-chunk))

(tm-define (traverse-extremal t forwards?)
  (:require (tm-chunk? t))
  (with move (if forwards? go-down go-up)
    (with chunk-move (lambda ()
                       (go-to-next-in-chunk move #t #t)
                       (go-to-start-chunk))
      (go-to-repeat chunk-move))))

(tm-define (traverse-incremental t downwards?)
  (:require (tm-chunk? t))
  (go-to-next-in-chunk (if downwards? go-down go-up) #t #t)
  (go-to-start-chunk))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tm-appended? t)
  (tm-in? t (appended-tag-list)))

(tm-define (search-appended t)
  (cond ((tm-atomic? t) (list))
        ((tm-func? t 'document) (append-map search-appended (tm-children t)))
        ((tm-appended? t) (list t))
        (else
          (with l (list-filter (tm-children t) (cut tm-func? <> 'document))
            (append-map search-appended l)))))

(tm-define (search-appended-folded t)
  (list-filter (search-appended t) alternate-standard-first?))

(tm-define (search-appended-unfolded t)
  (list-filter (search-appended t) alternate-standard-second?))

(tm-define (fold-appended)
  (with l (search-appended (buffer-tree))
    (for-each alternate-fold l)))

(tm-define (unfold-appended)
  (with l (search-appended (buffer-tree))
    (for-each alternate-unfold l)))
