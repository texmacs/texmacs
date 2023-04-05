
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
  (:use (utils library tree)
        (utils library cursor)
        (generic document-edit)
        (link ref-edit)
        (various comment-drd)
        (part part-shared)
        (database db-users)))

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

(tm-define (ext-contains-shown-comments? t)
  (:secure #t)
  (if (nnull? (tree-search t shown-comment-context?)) "true" "false"))

(tm-define (ext-comment-color type by)
  (:secure #t)
  (get-comment-color (or (tm->string type) "?")
                     (or (tm->string by) "?")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Colors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (default-comment-color type by)
  (cond ((== type "reminder") "#844")
        ((== by (utf8->cork (get-user-info "name"))) "#277")
        (else "#727")))

(tm-define (get-comment-color type by)
  (let* ((key (string-append by " " type " color"))
         (val (default-comment-color type by)))
    (cpp-get-preference key val)))

(tm-define (default-comment-color? type by)
  (with key (string-append by " " type " color")
    (not (cpp-has-preference? key))))

(tm-define (reset-comment-color type by)
  (with key (string-append by " " type " color")
    (cpp-reset-preference key)
    (for (t (tree-search (buffer-tree) any-comment-context?))
      (update-tree t))))
  
(tm-define (set-comment-color type by val)
  (with key (string-append by " " type " color")
    (cpp-set-preference key val)
    (for (t (tree-search (buffer-tree) any-comment-context?))
      (update-tree t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define comment-mode :show)

(tm-define (comment-context? t)
  (and (tm-in? t (cond ((== comment-mode :show)
                        (comment-tag-list))
                       ((== comment-mode :hide)
                        (hidden-comment-tag-list))
                       (else
                        (any-comment-tag-list))))
       (== (tm-arity t) 7)))

(tm-define (folded-comment-context? t)
  (and (tree-in? t (cond ((== comment-mode :show)
                          (folded-comment-tag-list))
                         ((== comment-mode :hide)
                          (hidden-folded-comment-tag-list))
                         (else
                          (any-folded-comment-tag-list))))
       (== (tree-arity t) 7)))

(define (shown-comment-context? t)
  (and (tm-in? t (shown-comment-tag-list))
       (== (tm-arity t) 7)))

(tm-define (any-comment-context? t)
  (and (tm-in? t (any-comment-tag-list))
       (== (tm-arity t) 7)))

(tm-define (comment-id t)
  (and (any-comment-context? t)
       (tm->string (tree-ref t 1))))

(tm-define (comment-type t)
  (or (and (any-comment-context? t)
           (tm->string (tree-ref t 2)))
      "?"))

(tm-define (comment-by t)
  (or (and (any-comment-context? t)
           (tm->string (tree-ref t 3)))
      "?"))

(define (comment-preview t)
  (and (folded-comment-context? t)
       `(preview-comment ,@(tm-children t))))

(tm-define (behind-folded-comment?)
  (and (== (cAr (cursor-path)) 1)
       (== (cDr (cursor-path)) (tree->path (cursor-tree)))
       (folded-comment-context? (path->tree (cDr (cursor-path))))
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
  (with-cache (change-time) :comments-in-buffer
    (with-global comment-mode :all
      (and-nnull? (search-comments (buffer-tree))))))

(define (comment-list)
  (with-cache (change-time) (list :comment-list comment-mode)
    (if (selection-active-any?)
        (append-map search-comments (selection-trees))
        (search-comments (buffer-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notifying comments editor in case of added of removed comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-comments-editor)
  (let* ((u (current-buffer))
         (cu (string-append "tmfs://comments/" (url->tmfs-string u))))
    (when (buffer-exists? cu)
      (with-buffer cu
        (revert-buffer-revert)))))

(tm-define (clipboard-cut which)
  (with l (tree-search (selection-tree) any-comment-context?)
    (former which)
    (when (nnull? l) (notify-comments-editor))))

(tm-define (clipboard-paste which)
  (with l (tree-search (clipboard-get which) any-comment-context?)
    (former which)
    (when (nnull? l) (notify-comments-editor))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting a new comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nest? t)
  (or (any-comment-context? t)
      (tree-is? t 'mirror-comment)))

(tm-define (inside-comment?)
  (tree-innermost nest?))
  
(tm-define (make-comment lab type pos)
  (let* ((id (create-unique-id))
         (mirror-id (create-unique-id))
         (by (utf8->cork (get-user-info "name")))
         (date (number->string (current-time))))
    (insert-go-to `(,lab ,id ,mirror-id ,type ,by ,date "" "") pos)
    (notify-comments-editor)))

(tm-define (make-unfolded-comment type)
  (with lab (if (inside-comment?) 'nested-comment 'unfolded-comment)
    (make-comment lab type (list 6 0))))

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
      (cond ((== op :cut) (tree-cut c))
            ((and (== op :fold) (== lab 'unfolded-comment))
             (tree-assign-node c 'folded-comment))
            ((and (== op :fold) (== lab 'hidden-unfolded-comment))
             (tree-assign-node c 'hidden-folded-comment))
            ((and (== op :unfold) (== lab 'folded-comment))
             (tree-assign-node c 'unfolded-comment))
            ((and (== op :unfold) (== lab 'hidden-folded-comment))
             (tree-assign-node c 'hidden-unfolded-comment))
            ((and (== op :hide) (in? lab (shown-comment-tag-list)))
             (with lab* (symbol-append 'hidden- lab)
               (tree-assign-node c lab*)))
            ((and (== op :show) (in? lab (hidden-comment-tag-list)))
             (with lab* (symbol-drop lab 7)
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
  (nin? tp (comment-type-list :hide)))

(tm-define (comment-toggle-type tp)
  (let* ((new-mode (if (comment-test-type? tp) :hide :show))
         (l (with-global comment-mode :all (comment-list)))
         (f (list-filter l (lambda (c) (== (comment-type c) tp)))))
    (operate-on-comments-in new-mode f)))

(tm-define (child-proposals t i)
  (:require (and (any-comment-context? t) (== i 2)))
  (rcons (comment-type-list :all) :other))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (comment-by-list mode)
  (with-cache (change-time) (list :comment-by-list mode)
    (with-global comment-mode mode
      (with l (map comment-by (comment-list))
        (sort (list-remove-duplicates l) string<=?)))))

(tm-define (comment-test-by? by)
  (nin? by (comment-by-list :hide)))

(tm-define (comment-toggle-by by)
  (let* ((new-mode (if (comment-test-by? by) :hide :show))
         (l (with-global comment-mode :all (comment-list)))
         (f (list-filter l (lambda (c) (== (comment-by c) by)))))
    (operate-on-comments-in new-mode f)))

(tm-define (child-proposals t i)
  (:require (and (any-comment-context? t) (== i 3)))
  (rcons (comment-by-list :all) :other))

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

(tm-define (mouse-event key x y mods time data)
  (with before? (behind-folded-comment?)
    (former key x y mods time data)
    (with after? (behind-folded-comment?)
      (when (and (or (!= before? after?) after?) (== key "release-left"))
        (update-comment-tooltip)))))

(tm-define (keyboard-press key time)
  (with before? (behind-folded-comment?)
    (former key time)
    (with after? (behind-folded-comment?)
      (when (or (!= before? after?) after?)
        (update-comment-tooltip)))))
