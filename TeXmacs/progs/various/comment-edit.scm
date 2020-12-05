
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
;; Basic subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (comment-context? t)
  (and (tm-in? t (comment-tag-list))
       (== (tm-arity t) 6)))

(tm-define (hidden-comment-context? t)
  (and (tree-in? t (hidden-comment-tag-list))
       (== (tree-arity t) 6)))

(define (behind-hidden-comment?)
  (and (== (cAr (cursor-path)) 1)
       (== (cDr (cursor-path)) (tree->path (cursor-tree)))
       (hidden-comment-context? (path->tree (cDr (cursor-path))))
       (list (tree-label (cursor-tree))
             (tree->path (cursor-tree)))))

(define (comment-id t)
  (and (comment-context? t)
       (tm->string (tree-ref t 1))))

(define (comment-preview t)
  (and (hidden-comment-context? t)
       `(preview-comment ,@(tm-children t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbreviations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-abbreviate-name x)
  (:secure #t)
  (if (not (and (tree? x) (tree-atomic? x))) x
      (let* ((s (tree->string x))
             (i (string-search-forwards " " 0 s)))
        (if (>= i 0) (substring s 0 i) s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a new comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-comment)
  (let* ((id (create-unique-id))
         (mirror-id (create-unique-id))
         (by (buffer-get-metadata (current-buffer) "author"))
         (date (number->string (current-time))))
    (insert-go-to `(show-comment ,id ,mirror-id "comment" ,by ,date "")
                  (list 5 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comment navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (search-comments t)
  (tree-search t comment-context?))

(tm-define (comments-in-buffer)
  (with-cache (change-time) :comments-in-buffer
    (and-nnull? (search-comments (buffer-tree)))))

(tm-define (go-to-comment dir)
  (:applicable (comments-in-buffer))
  (list-go-to (comments-in-buffer) dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types and authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (hidden-child? t i)
  (:require (comment-context? t))
  (in? i (list 2 3)))

(define (search-comment-types t)
  (with l (if (comment-context? t) (list (tm->string (tm-ref t 2))) (list))
    (list-remove-duplicates l)))

(define (search-comments-types l)
  (sort (list-remove-duplicates (append-map search-comment-types l))
        string<=?))

(tm-define (comment-type-list)
  (with-cache (change-time) :comment-type-list
    (search-comments-types (comment-list))))

(tm-define (comment-test-type? tp)
  (in? tp (comment-type-list)))

(tm-define (comment-toggle-type tp)
  (noop))

(define (search-comment-by t)
  (with l (if (comment-context? t) (list (tm->string (tm-ref t 3))) (list))
    (list-remove-duplicates l)))

(define (search-comments-by l)
  (sort (list-remove-duplicates (append-map search-comment-by l))
        string<=?))

(tm-define (comment-by-list)
  (with-cache (change-time) :comment-by-list
    (search-comments-by (comment-list))))

(tm-define (comment-test-by? by)
  (in? by (comment-by-list)))

(tm-define (comment-toggle-by by)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operate on comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (comment-list)
  (with-cache (change-time) :comment-list
    (if (selection-active-any?)
        (append-map search-comments (selection-trees))
        (search-comments (buffer-tree)))))

(tm-define (operate-on-comments op)
  (:applicable (nnull? (comment-list)))
  (for (c (reverse (comment-list)))
    (cond ((== op :show) (tree-assign-node c 'show-comment))
          ((== op :hide) (tree-assign-node c 'hide-comment))
          ((== op :cut) (tree-cut c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open comment editor
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
