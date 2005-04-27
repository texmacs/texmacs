
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 300a1366-dabc-42fe-97ee-48543e6a789a
;;
;; MODULE      : proclus-locus.scm
;; DESCRIPTION : Fundamental operations on Proclus loci
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

(texmacs-module (proclus-locus)
  (:use (kernel tools tm-misc) (search-in-tree)
        (proclus-list) ;; quadripartite
        (proclus-absname)
        (proclus-lib)
        (ice-9 common-list)) ;; uniq
  (:export
    locus-path? not-locus-path? locus-or-not-locus-path?
    get-locus-path get-not-locus-path get-locus-or-not-locus-path

    locus? not-locus? locus-or-not-locus?
    get-locus get-not-locus get-locus-or-not-locus
    locus-with-link?

    locus-text locus-absname locus-id
    locus-types
    locus-self-link locus-links locus-drop-links

    make-locus locus-set-text locus-set-text-go-to

    make-link make-root-link
    link-absname link-id link-root? link-types link-comment
    add-link-end remove-link-end
    locus-link-types locus-set-link-types

    locus-path go-to-locus go-to-locus-buffer))

;; Structure of a LOCUS tag:
;; (locus <body>
;;   (tuple
;;      "absname of current document" (not sure!)
;;      "LOCUS id, for use as a destination"
;;
;;      "absname of destination document"
;;      "id of destination"
;;      "comment"
;;      Those last three items can be repeated multiple times.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access a locus in the buffer given its path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus-path? p)
  (eq? 'locus (tree-get-label (tm-subtree p))))

(define (not-locus-path? p)
  (eq? 'not-locus (tree-get-label (tm-subtree p))))

(define (locus-or-not-locus-path? p)
  (memq? (tree-get-label (tm-subtree p)) '(locus not-locus)))

(define (memq? x l)
  ;; (not (not x)) converts x to a boolean object
  (not (not (memq x l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locus accessors, expect a locus in Scheme form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'locus)))

(define (locus-with-link? x)
  (and (locus? x)
       (> (length (last x)) 3)))

(define (not-locus? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'not-locus)))

(define (locus-or-not-locus? x)
  (and (pair? x)
       (>= (length x) 3)
       (memq? (car x) '(locus not-locus))))

(define (locus-text t) (second t))

(define (locus-tuple t)
  ;; A locus tag has the structure (locus body extra), where body is the
  ;; visible content and extra is the hidden metadata.
  (tuple->list (third t)))

(define (locus-absname t)
  (first (locus-tuple t)))

(define (locus-id t)
  (second (locus-tuple t)))

(define (locus-types t)
  (cdr (cADr (third t))))

(define (locus-links t)
  (quadripartite (locus-links-flat t)))

(define (locus-links-flat t)
  (cddr (locus-tuple t)))

(define (locus-self-link t)
  (let ((tt (locus-tuple t)))
    (list (first tt) (second tt) "")))

(define (locus-drop-links t)
  `(locus ,(locus-text t) (tuple ,(locus-absname t) ,(locus-id t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modify a locus in the buffer given its path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus-set-text p x)
  (tm-assign (rcons p 0) x))

(define (locus-set-text-go-to p x ppos)
  (locus-set-text p x)
  (tm-go-to (append p '(0) ppos)))

(define (locus-set-tuple p l)
  ;; Set the metadata of the locus at @p to the list @l converted to a tuple.
  (tm-assign (rcons p 1) (list->tuple l)))

(define (locus-set-links p links)
  (locus-set-links-flat p (list-concatenate links)))

(define (locus-set-links-flat p links)
  (let ((t (locus-tuple (tm-substree p))))
    (locus-set-tuple p (cons* (first t) (second t) links))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access the deepest locus in the buffer enclosing the caret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-locus-or-not-locus-path)
  (and-let* ((p (search-upwards-in-set '(locus not-locus)))
             ((pair? p)))
    p))

(define (get-locus-path)
  (and-let* ((p (search-upwards "locus"))
             (pair? p))
    p))

(define (get-not-locus-path)
  (and-let* ((p (search-upwards "not-locus"))
             (pair? p))
    p))

(define (get-locus-or-not-locus)
  (and-let* ((p (get-locus-or-not-locus-path)))
    (tm-substree p)))

(define (get-locus)
  (and-let* ((p (get-locus-path)))
    (tm-substree p)))

(define (get-not-locus)
  (and-let* ((p (get-not-locus-path)))
    (tm-substree p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locus creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-locus absname n)
  ;; FIXME: do not use clipboard, instead work with primitive buffer ops
  (let ((sel? (selection-active-any?)))
    (if sel? (clipboard-cut "ah"))
    (insert-go-to `(locus "" (tuple ,absname ,n)) '(0 0))
    (if sel? (clipboard-paste "ah"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Structure of a link: (absolute-name locus-id (type ...) comment-string)
;; A locus-id of "0" means "the whole document".
(define (make-link absname id) (list absname id ""))
(define (make-root-link absname) (make-link absname "0"))
(define (link-absname link) (first link))
(define (link-id link) (second link))
(define (link-root? link) (== "0" (link-id link)))
(define (link-types link) (third link))
(define (link-comment link) (fourth link))

(define (link-dest link) (list (first link) (second link)))
(define (link-dest-equal? lnk1 lnk2)
  (and (== (first lnk1) (first lnk2)) (== (second lnk1) (second lnk2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create or modify a link end, in an existing LOCUS node
(define (add-link-end source but types)
  (let ((source-absname (link-absname source))
        (source-id (link-id source))
        (but-absname (link-absname but))
        (but-id (link-id but)))
    (switch-to-active-buffer (absolute-name->url source-absname))
    (locus-add-link (locus-path source-id) but-absname but-id types)))

(define (locus-add-link path absname id types)
  (let ((links (locus-links (tm-substree path))))
    (locus-set-links
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
;; Link suppression
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove a link end, in an existing source LOCUS
(define (remove-link-end source but)
  (let ((source-absname (link-absname source))
	(source-id (link-id source)))
    (switch-to-active-buffer (absolute-name->url source-absname))
    (locus-remove-link (locus-path source-id) but)))

(define (locus-remove-link path link-to-rm)
  (let ((absname (link-absname link-to-rm))
	(id (link-id link-to-rm)))
    (locus-set-links
     path (list-filter (locus-links (tm-substree path))
		       (lambda (lnk)
			 (not (and (== (link-absname lnk) absname)
				   (== (link-id lnk) id))))))))

(define (locus-link-types source but)
  (let ((source-absname (link-absname source))
        (source-id (link-id source))
        (but-absname (link-absname but))
        (but-id (link-id but)))
    (switch-to-active-buffer (absolute-name->url source-absname))
    (list-concatenate
     (map link-types
          (list-filter (locus-links (tm-substree (locus-path source-id)))
                       (cut link-dest-equal? but <>))))))

(define (locus-set-link-types source but types)
  (let ((source-absname (link-absname source))
        (source-id (link-id source))
        (but-absname (link-absname but))
        (but-id (link-id but)))
    (switch-to-active-buffer (absolute-name->url source-absname))
    (let ((path (locus-path source-id)))
      (if (null? types)
          (locus-remove-link path but)
          (locus-set-links
           path (map (lambda (lnk)
                       (if (link-dest-equal? but lnk)
                           (list but-absname
                                 but-id
                                 types
                                 (link-comment lnk))
                           lnk))
                     (locus-links (tm-substree path))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Browsing loci
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus-path id)
  ;; path of the first locus in the buffer whose id is @id
  ;; #f if no locus with the given id is found in the buffer
  (let sub ((p '()))
    (search-in-tree-from (the-buffer) p 'locus
                         (lambda (p t)
                           (if (== id (locus-id (tree->stree t)))
                               (reverse (tree-ip t))
                               (sub (rcons p 0)))))))

(define (go-to-locus lk)
  ;; FIXME: raise distinctive exception for root links and id not found
  (go-to-locus-buffer lk)
  (tm-go-to (append (locus-path (link-id lk)) '(0 0))))

(define (go-to-locus-buffer lk)
  (switch-to-active-buffer (absolute-name->url (link-absname lk))))