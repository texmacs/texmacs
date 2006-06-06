
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
;; Navigation mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define navigation-bidirectional-links? #t)
(define navigation-external-links? #t)
(define navigation-blocked-types (make-ahash-table))

(define (navigation-bidirectional?) navigation-bidirectional-links?)
(tm-define (navigation-toggle-bidirectional)
  (:synopsis "Toggle whether we may follow links in both directions.")
  (:check-mark "v" navigation-bidirectional?)
  (toggle! navigation-bidirectional-links?))

(define (navigation-external?) navigation-external-links?)
(tm-define (navigation-toggle-external)
  (:synopsis "Toggle whether we may follow links defined in other loci.")
  (:check-mark "v" navigation-external?)
  (toggle! navigation-external-links?))

(define (navigation-allow-type? type)
  (not (ahash-ref navigation-blocked-types type)))
(tm-define (navigation-toggle-type type)
  (:synopsis "Toggle whether we may follow links of a given @type.")
  (:check-mark "v" navigation-allow-type?)
  (ahash-set! navigation-blocked-types type
	      (not (ahash-ref navigation-blocked-types type))))

(define (navigation-allow-no-types?)
  (with l (ahash-table->list navigation-blocked-types)
    (null? (list-difference (current-link-types)
			    (map car (list-filter l cdr))))))
(tm-define (navigation-allow-no-types)
  (:synopsis "Disallow any link types from being followed.")
  (:check-mark "v" navigation-allow-no-types?)
  (for-each (cut ahash-set! navigation-blocked-types <> #t)
	    (current-link-types)))

(define (navigation-allow-all-types?)
  (with l (ahash-table->list navigation-blocked-types)
    (null? (list-filter l cdr))))
(tm-define (navigation-allow-all-types)
  (:synopsis "Allow all link types to be followed.")
  (:check-mark "v" navigation-allow-all-types?)
  (set! navigation-blocked-types (make-ahash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id->link-list id)
  (let* ((lns (id->links id))
	 (sts (map tree->stree lns)))
    (map (cut cons id <>) (map cdr sts))))

(define (exact-link-list-global t)
  (let* ((ids (tree->ids t))
	 (lns (map id->link-list ids)))
    (apply append lns)))

(define (exact-link-list-local t)
  (if (not (tm-func? t 'locus)) '()
      (let* ((id (locus-id t))
	     (ch (cDdr (tree->stree t)))
	     (lns (list-filter ch (cut func? <> 'link))))
	(map (cut cons id <>) (map cdr lns)))))

(define (filter-on-bidirectional x)
  (with (id type first . other) x
    (== id (Id->id first))))

(define (filter-on-type x)
  (with (id type . args) x
    (navigation-allow-type? type)))

(tm-define (exact-link-list t filter?)
  (with l (if (and filter? (not (navigation-external?)))
	      (exact-link-list-local t)
	      (exact-link-list-global t))
    (if (and filter? (not (navigation-bidirectional?)))
	(set! l (list-filter l filter-on-bidirectional)))
    (if filter?
	(set! l (list-filter l filter-on-type)))
    l))

(define (upward-link-list-sub t filter?)
  (with l (exact-link-list t filter?)
    (if (== (buffer-path) (tree->path t)) l
	(append l (upward-link-list-sub (tree-up t) filter?)))))

(tm-define (upward-link-list filter?)
  (with t (cursor-tree)
    (upward-link-list-sub t filter?)))

(tm-define (complete-link-list t filter?)
  (with l (exact-link-list t filter?)
    (if (tree-atomic? t) l
	(with ls (map (cut complete-link-list <> filter?) (tree-children t))
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
  (link-list->navigate-list (upward-link-list #t)))

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
  (let* ((l (link-list->navigate-list (upward-link-list #t)))
	 (types (navigate-list-types l)))
    (cond ((null? types) (noop))
	  ((null? (cdr types)) (go-to-Id (cadddr (car l))))
	  (else (interactive link-follow-typed)))))
