
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-widgets.scm
;; DESCRIPTION : special widgets for editing comments
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-widgets)
  (:use (various comment-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing a simple comment in a separate widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define comment-quit-command ignore)

(tm-widget ((comment-editor u packs doc) quit)
  (with dummy (set! comment-quit-command quit)
    (padded
      (resize "600px" "300px"
        (texmacs-input doc `(style (tuple ,@packs)) u))
      ===
      (hlist
        >>
        (explicit-buttons
          ("Done" (quit)))))))

(tm-define (open-comment-editor)
  (:applicable (behind-folded-comment?))
  (:interactive #t)
  (and-let* ((c (tm->stree (tree-innermost any-comment-context? #t)))
             (b (current-buffer-url))
             (u (string->url "tmfs://aux/edit-comment"))
             (packs (get-style-list)))
    (and-with doc `(document ,(mirror-comment c 'carbon-comment))
      (dialogue-window (comment-editor u packs doc)
                       (lambda x (set! comment-quit-command ignore))
                       "Comment editor" u)
      (buffer-set-master u b))))

(tm-define (kbd-control-return)
  (:require (behind-folded-comment?))
  (open-comment-editor))

(tm-define (kbd-control-return)
  (:require (inside? 'carbon-comment))
  (comment-quit-command))

(tm-define (make-folded-comment type)
  (:applicable (not (inside-comment?)))
  (make-comment 'folded-comment type)
  (open-comment-editor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Open comments editor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (comments name type)
  (in? type (list "read")))

(tmfs-title-handler (comments name doc)
  (with u (tmfs-string->url name)
    (string-append (url->system (url-tail u)) " - Comments")))

(define (mirror-comment t . opt-lab)
  (let* ((id (if (tm-atomic? (tm-ref t 0))
                 (string-append (tm-ref t 0) "-edit")
                 (create-unique-id)))
         (l (tm-children t))
         (lab (if (null? opt-lab) 'mirror-comment (car opt-lab))))
    `(,lab ,id ,@(cDr (cDr (cdr l))) "" ,(cAr l))))

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
  (let* ((l (comments-in-buffer))
         (u (current-buffer))
         (cu (string-append "tmfs://comments/" (url->tmfs-string u))))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :next))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :previous))
    (when (not (tree-innermost any-comment-context? #t))
      (list-go-to l :first))
    (let* ((t (tree-innermost any-comment-context? #t))
           (id (comment-id t)))
      (when t (tree-select t))
      (load-buffer-in-new-window cu)
      (buffer-set-master cu u)
      (delayed
        (:pause 50)
        (and-let* ((b (buffer-get-body cu))
                   (c (search-comment b id)))
          (with-buffer cu
            (tree-go-to c :start)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pairing cursor positions: comments -> commented
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-comments-editor?)
  (string-starts? (url->string (current-buffer)) "tmfs://comments/"))

(define ((comment-by-id? id) t)
  (and (any-comment-context? t)
       (== (comment-id t) id)))

(define (search-comment t id)
  (with l (tree-search t (comment-by-id? id))
    (and (nnull? l) (car l))))

(define (sync-master-cursor)
  (and-let* ((c (tree-innermost 'mirror-comment))
             (m (buffer-get-master (current-buffer)))
             (b (buffer-get-body m))
             (i (comment-id c))
             (t (search-comment b i)))
    (with-buffer m
      (tree-select t)
      (tree-go-to t :end)
      (when (and (not (cursor-accessible?)) (not (in-source?)))
        (cursor-show-hidden)))))

(tm-define (mouse-event key x y mods time)
  (former key x y mods time)
  (when (and (in-comments-editor?) (!= key "move"))
    (sync-master-cursor)))

(tm-define (keyboard-press key time)
  (former key time)
  (when (in-comments-editor?)
    (sync-master-cursor)))
