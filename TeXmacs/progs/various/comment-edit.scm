
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-edit.scm
;; DESCRIPTION : editing various types of comments
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-edit)
  (:use (generic document-edit)
        (link ref-edit)
        (various comment-drd)
        (part part-shared)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caching highly volatile computations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define volatile-cache-stamp #f)
(define volatile-cache (make-ahash-table))

(tm-define-macro (with-cache time-stamp feature . body)
  `(let ((stamp ,time-stamp)
         (key ,feature))
     (when (!= volatile-cache-stamp stamp)
       (set! volatile-cache-stamp stamp)
       (set! volatile-cache (make-ahash-table)))
     (when (not (ahash-ref volatile-cache key))
       (ahash-set! volatile-cache key (begin ,@body)))
     (ahash-ref volatile-cache key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; External macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-abbreviate-name t)
  (:secure #t)
  (if (not (and (tree? t) (tree-atomic? t))) t
      (let* ((s (tree->string t))
             (i (string-search-forwards " " 0 s)))
        (if (>= i 0) (substring s 0 i) s))))

(tm-define (ext-contains-visible-comments? t)
  (:secure #t)
  (if (nnull? (tree-search t visible-comment-context?)) "true" "false"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define comment-mode :visible)

(tm-define (comment-context? t)
  (and (tm-in? t (cond ((== comment-mode :visible)
                        (comment-tag-list))
                       ((== comment-mode :invisible)
                        (invisible-comment-tag-list))
                       (else
                        (any-comment-tag-list))))
       (== (tm-arity t) 6)))

(tm-define (hidden-comment-context? t)
  (and (tree-in? t (cond ((== comment-mode :visible)
                          (hidden-comment-tag-list))
                         ((== comment-mode :invisible)
                          (invisible-hidden-comment-tag-list))
                         (else
                          (any-hidden-comment-tag-list))))
       (== (tree-arity t) 6)))

(define (visible-comment-context? t)
  (and (tm-in? t (visible-comment-tag-list))
       (== (tm-arity t) 6)))

(define (any-comment-context? t)
  (and (tm-in? t (any-comment-tag-list))
       (== (tm-arity t) 6)))

(define (comment-id t)
  (and (any-comment-context? t)
       (tm->string (tree-ref t 1))))

(define (comment-type t)
  (or (and (any-comment-context? t)
           (tm->string (tree-ref t 2)))
      "?"))

(define (comment-by t)
  (or (and (any-comment-context? t)
           (tm->string (tree-ref t 3)))
      "?"))

(define (comment-preview t)
  (and (hidden-comment-context? t)
       `(preview-comment ,@(tm-children t))))

(define (behind-hidden-comment?)
  (and (== (cAr (cursor-path)) 1)
       (== (cDr (cursor-path)) (tree->path (cursor-tree)))
       (hidden-comment-context? (path->tree (cDr (cursor-path))))
       (list (tree-label (cursor-tree))
             (tree->path (cursor-tree)))))

(tm-define (hidden-child? t i)
  (:require (any-comment-context? t))
  (in? i (list 2 3)))

(tm-define (hidden-child? t i)
  (:require (tree-is? t 'mirror-comment))
  #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Searching comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (search-comments t)
  (tree-search t comment-context?))

(tm-define (comments-in-buffer)
  (with-cache (change-time) (list :comments-in-buffer comment-mode)
    (and-nnull? (search-comments (buffer-tree)))))

(define (comment-list)
  (with-cache (change-time) (list :comment-list comment-mode)
    (if (selection-active-any?)
        (append-map search-comments (selection-trees))
        (search-comments (buffer-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a new comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nest? t)
  (or (any-comment-context? t)
      (tree-is? t 'mirror-comment)))

(tm-define (make-comment type)
  (let* ((lab (if (tree-innermost nest?) 'nested-comment 'show-comment))
         (id (create-unique-id))
         (mirror-id (create-unique-id))
         (by (buffer-get-metadata (current-buffer) "author"))
         (date (number->string (current-time))))
    (insert-go-to `(,lab ,id ,mirror-id ,type ,by ,date "")
                  (list 5 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-comment dir)
  (:applicable (comment-list))
  (list-go-to (comment-list) dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operate on comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (operate-on-comments-in op l)
  (for (c (reverse l))
    (with lab (tree-label c)
      (cond ((== op :show) (tree-assign-node c 'show-comment))
            ((== op :hide) (tree-assign-node c 'hide-comment))
            ((== op :cut) (tree-cut c))
            ((and (== op :invisible) (in? lab (visible-comment-tag-list)))
             (with lab* (symbol-append 'invisible- lab)
               (tree-assign-node c lab*)))
            ((and (== op :visible) (in? lab (invisible-comment-tag-list)))
             (with lab* (symbol-drop lab 10)
               (tree-assign-node c lab*)))))))

(tm-define (operate-on-comments op)
  (:applicable (nnull? (comment-list)))
  (operate-on-comments-in op (comment-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (comment-type-list mode)
  (with-cache (change-time) (list :comment-type-list mode)
    (with-global comment-mode mode
      (with l (map comment-type (comment-list))
        (sort (list-remove-duplicates l) string<=?)))))

(tm-define (comment-test-type? tp)
  (nin? tp (comment-type-list :invisible)))

(tm-define (comment-toggle-type tp)
  (let* ((new-mode (if (comment-test-type? tp) :invisible :visible))
         (l (with-global comment-mode :all (comment-list)))
         (f (list-filter l (lambda (c) (== (comment-type c) tp)))))
    (operate-on-comments-in new-mode f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (comment-by-list mode)
  (with-cache (change-time) (list :comment-by-list mode)
    (with-global comment-mode mode
      (with l (map comment-by (comment-list))
        (sort (list-remove-duplicates l) string<=?)))))

(tm-define (comment-test-by? by)
  (nin? by (comment-by-list :invisible)))

(tm-define (comment-toggle-by by)
  (let* ((new-mode (if (comment-test-by? by) :invisible :visible))
         (l (with-global comment-mode :all (comment-list)))
         (f (list-filter l (lambda (c) (== (comment-by c) by)))))
    (operate-on-comments-in new-mode f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open comments editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (comments name type)
  (in? type (list "read")))

(tmfs-title-handler (comments name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - Comments")))

(define (mirror-comment t)
  (let* ((id (if (tm-atomic? (tm-ref t 0))
                 (string-append (tm-ref t 0) "-edit")
                 (create-unique-id))))
    `(mirror-comment ,id ,@(cdr (tm-children t)))))

(tmfs-load-handler (comments name)
  (let* ((u (tmfs-string->url name))
         (doc (tree->stree (buffer-get u))))
    (tm-replace doc (cut tm-func? <> 'body 1)
                (lambda (t)
                  (let* ((l (tm-search t comment-context?))
                         (r (map mirror-comment l)))
                    `(body (document ,@r)))))))

(tm-define (open-comments-editor)
  (:applicable (comments-in-buffer))
  (let* ((u (current-buffer))
         (cu (string-append "tmfs://comments/" (url->tmfs-string u))))
    (load-buffer-in-new-window cu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Previewing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (update-comment-tooltip)
  (delayed
    (:idle 100)
    (let* ((id (comment-id (cursor-tree)))
           (tip (comment-preview (cursor-tree))))
      (if tip
          (show-tooltip id (cursor-tree) tip
                        "auto" "auto" "keyboard" 0.7)
          (close-tooltip)))))

(tm-define (mouse-event key x y mods time)
  (with before? (behind-hidden-comment?)
    (former key x y mods time)
    (with after? (behind-hidden-comment?)
      (when (!= before? after?)
        (update-comment-tooltip)))))

(tm-define (keyboard-press key time)
  (with before? (behind-hidden-comment?)
    (former key time)
    (with after? (behind-hidden-comment?)
      (when (!= before? after?)
        (update-comment-tooltip)))))
