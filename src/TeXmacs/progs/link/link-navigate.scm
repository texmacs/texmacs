
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-navigate.scm
;; DESCRIPTION : navigation routines for links
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-navigate)
  (:use (link link-edit) (link link-extern)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id->link-list id)
  (let* ((lns (id->links id))
	 (sts (map tree->stree lns)))
    (map (cut cons id <>) (map cdr sts))))

(tm-define (exact-link-list t)
  (let* ((ids (tree->ids t))
	 (lns (map id->link-list ids)))
    (apply append lns)))

(define (upward-link-list-sub t)
  (with l (exact-link-list t)
    (if (== (buffer-path) (tree->path t)) l
	(append l (upward-link-list-sub (tree-up t))))))

(tm-define (upward-link-list)
  (with t (cursor-tree)
    (upward-link-list-sub t)))

(tm-define (complete-link-list t)
  (with l (exact-link-list t)
    (if (tree-atomic? t) l
	(with ls (map complete-link-list (tree-children t))
	  (apply append (cons l ls))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (navigate-list-sub type nr source l)
  (if (null? l) l
      (let* ((head (list type nr source (car l)))
	     (tail (navigate-list-sub type (+ nr 1) source (cdr l))))
	(if (== source (Id->id (car l))) tail
	    (cons head tail)))))

(tm-define (link-list->navigate-list l)
  (if (null? l) l
      (let* ((item (car l))
	     (source (car item))
	     (type (cadr item))
	     (components (cddr item))
	     (h (navigate-list-sub type 0 source components))
	     (r (link-list->navigate-list (cdr l))))
	(list-remove-duplicates (append h r)))))

(tm-define (upward-navigate-list)
  (link-list->navigate-list (upward-link-list)))

(tm-define (navigate-list-types l)
  (list-remove-duplicates (map car l)))

(tm-define (navigate-list-find-type l type)
  (cond ((null? l) #f)
	((== (caar l) type) (cadddr (car l)))
	(else (navigate-list-find-type (cdr l) type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (link-may-follow?)
  (:synopsis "Does the current locus contain an active link?")
  (nnull? (upward-navigate-list)))

(tm-define (go-to-id id)
  (with l (id->trees id)
    (if (nnull? l)
	(tree-go-to (car l) :last :end)
	(and (resolve-id id)
	     (delayed (:idle 100) (go-to-id id))))))

(tm-define (go-to-Id Id)
  (cond ((func? Id 'id 1) (go-to-id (cadr Id)))
	(else (noop))))

(tm-define (link-follow-typed type)
  (:synopsis "Follow the first link with given @type at the current locus.")
  (:argument type "Link type")
  (:proposals type (navigate-list-types (upward-navigate-list)))
  (and-with Id (navigate-list-find-type (upward-navigate-list) type)
    (go-to-Id Id)))

(tm-define (link-follow)
  (:synopsis "Follow one of the links at the current locus.")
  (let* ((l (link-list->navigate-list (upward-link-list)))
	 (types (navigate-list-types l)))
    (cond ((null? types) (noop))
	  ((null? (cdr types)) (go-to-Id (cadddr (car l))))
	  (else (interactive link-follow-typed)))))
