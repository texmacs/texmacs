
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 300a1366-dabc-42fe-97ee-48543e6a789a
;;
;; MODULE      : proclus-target.scm
;; DESCRIPTION : Fundamental operations on Proclus targets
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (proclus-target)
  (:use (kernel tools tm-misc) (search-in-tree)
        (proclus-list) ;; quadripartite
        (proclus-absname)
        (proclus-lib)
        (ice-9 common-list)) ;; uniq
  (:export
    target-path? not-target-path? target-or-not-target-path?
    get-target-path get-not-target-path get-target-or-not-target-path

    target? not-target? target-or-not-target?
    get-target get-not-target get-target-or-not-target

    target-text target-absname target-id
    target-self-link target-links target-drop-links

    make-target target-set-text target-set-text-go-to

    link-absname link-id link-types link-comment
    add-link-end

    target-path go-to-target))

;; Structure of a TARGET tag:
;; (target <body>
;;   (tuple
;;      "absname of current document" (not sure!)
;;      "TARGET id, for use as a destination"
;;
;;      "absname of destination document"
;;      "id of destination"
;;      "comment"
;;      Those last three items can be repeated multiple times.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access a target in the buffer given its path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (target-path? p)
  (eq? 'target (tree-get-label (tm-subtree p))))

(define (not-target-path? p)
  (eq? 'not-target (tree-get-label (tm-subtree p))))

(define (target-or-not-target-path? p)
  (memq? (tree-get-label (tm-subtree p)) '(target not-target)))

(define (memq? x l)
  ;; (not (not x)) converts x to a boolean object
  (not (not (memq x l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target accessors, expect a target in Scheme form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (target? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'target)))

(define (not-target? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'not-target)))

(define (target-or-not-target? x)
  (and (pair? x)
       (>= (length x) 3)
       (memq? (car x) '(target not-target))))

(define (target-text t) (second t))

(define (target-tuple t)
  ;; A target tag has the structure (target body extra), where body is the
  ;; visible content and extra is the hidden metadata.
  (tuple->list (third t)))

(define (tuple->list x)
  ;; Strip tree labels recursively from a texmacs snippet in scheme format.
  (let sub ((x x))
    (if (not (pair? x)) x (map sub (cdr x)))))

(define (target-absname t)
  (first (target-tuple t)))

(define (target-id t)
  (second (target-tuple t)))

(define (target-links t)
  (quadripartite (target-links-flat t)))

(define (target-links-flat t)
  (cddr (target-tuple t)))

(define (target-self-link t)
  (let ((tt (target-tuple t)))
    (list (first tt) (second tt) "")))

(define (target-drop-links t)
  `(target ,(target-text t) (tuple ,(target-absname t) ,(target-id t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify a target in the buffer given its path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (target-set-text p x)
  (tm-assign (rcons p 0)
             (object->tree x)))

(define (target-set-text-go-to p x ppos)
  (target-set-text p x)
  (tm-go-to (append p '(0) ppos)))

(define (target-set-tuple p l)
  ;; Set the metadata of the target at @p to the list @l converted to a tuple.
  (tm-assign (rcons p 1)
             (object->tree (list->tuple l))))

(define (list->tuple l)
  (let sub ((x l))
    (if (not (pair? x)) x (cons 'tuple (map sub x)))))

(define (target-set-links p links)
  (target-set-links-flat p (list-concatenate links)))

(define (target-set-links-flat p links)
  (let ((t (target-tuple (tm-subobject p))))
    (target-set-tuple p (cons* (first t) (second t) links))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access the deepest target in the buffer enclosing the caret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-target-or-not-target-path)
  (and-let* ((p (search-upwards-in-set '(target not-target)))
             ((pair? p)))
    p))

(define (get-target-path)
  (and-let* ((p (search-upwards "target"))
             (pair? p))
    p))

(define (get-not-target-path)
  (and-let* ((p (search-upwards "not-target"))
             (pair? p))
    p))

(define (get-target-or-not-target)
  (and-let* ((p (get-target-or-not-target-path)))
    (tm-subobject p)))

(define (get-target)
  (and-let* ((p (get-target-path)))
    (tm-subobject p)))

(define (get-not-target)
  (and-let* ((p (get-not-target-path)))
    (tm-subobject p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-target absname n)
  ;; FIXME: do not use clipboard, instead work with primitive buffer ops
  (let ((sel? (selection-active-any?)))
    (if sel? (clipboard-cut "ah"))
    (insert-object-go-to `(target "" (tuple ,absname ,n))
                         '(0 0))
    (if sel? (clipboard-paste "ah"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structure of a link: (absolute-name target-id (type ...) comment-string)
(define (link-absname link) (first link))
(define (link-id link) (second link))
(define (link-types link) (third link))
(define (link-comment link) (fourth link))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create or modify a link end, in an existing TARGET node
(define (add-link-end source but types)
  (let ((source-absname (link-absname source))
        (source-id (link-id source))
        (but-absname (link-absname but))
        (but-id (link-id but)))
    (switch-to-active-buffer (absolute-name->url source-absname))
    (target-add-link (target-path source-id) but-absname but-id types)))

(define (target-add-link path absname id types)
  (let ((links (target-links (tm-subobject path))))
    (target-set-links
     path (if (link-in? absname id links)
              (add-types absname id types links)
              (rcons links (list absname id types ""))))))

(define (link-in? absname id links)
  (let next ((links links))
    (and (pair? links)
         (or (and (== (link-absname (car links)) absname)
                  (== (link-id (car links)) id))
             (next (cdr links))))))

(define (add-types absname id types links)
  (map
   (lambda (lnk)
     (if (not (and (== (link-absname lnk) absname)
                   (== (link-id lnk) id)))
         lnk
         (list (link-absname lnk)
               (link-id lnk)
               (uniq (append (link-types lnk) types))
               (link-comment lnk))))
     links))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Browsing targets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (target-path id)
  ;; path of the first target in the buffer whose id is @id
  (let sub ((p '()))
    (search-in-tree-from (the-buffer) p 'target
                         (lambda (p t)
                           (if (== id (target-id (tm-subobject p)))
                               p (sub (rcons p 0)))))))

(define (go-to-target lk)
  (switch-to-active-buffer (absolute-name->url (link-absname lk)))
  (tm-go-to (append (target-path (link-id lk)) '(0 0))))
