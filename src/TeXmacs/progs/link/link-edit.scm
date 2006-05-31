
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-edit.scm
;; DESCRIPTION : editing routines for links
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-edit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-registry (make-ahash-table))
(define link-participants (make-ahash-table))

(tm-define (link-register ln)
  (ahash-set! link-registry ln #t))

(define (link-remove-participant nr)
  (with p (ahash-ref link-participants nr)
    (when p
      (ahash-remove! link-participants nr)
      (position-delete p))))

(tm-define (link-add-participant nr)
  (when (selection-active-any?)
    (link-remove-participant nr)
    (let* ((p (selection-path))
	   (t (path->tree p))
	   (r (tree-right-index t)))
      (ahash-set! link-participants nr (position-new-path (rcons p r)))
      (selection-cancel))))

(define (link-participating-trees nr)
  (with p (ahash-ref link-participants nr)
    (if (not p) '()
	(with t (path->tree (cDr (position-get p)))
	  (link-remove-participant nr)
	  (cons t (link-participating-trees (+ nr 1)))))))

(tm-define (link-under-construction?)
  (nnot (ahash-ref link-participants 0)))

(tm-define (link-create type)
  (:argument type "Link type")
  (with l (link-participating-trees 0)
    (when (nnull? l)
      (link-register (link-construct (string->symbol type) l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (exact-link-list t)
  (with l (tree->links t)
    (map (lambda (x) (cons t x)) l)))

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
	(if (tree-eq? source (car l)) tail
	    (cons head tail)))))

(tm-define (link-list->navigate-list l)
  (if (null? l) l
      (let* ((source (caar l))
	     (type (link-type (cdar l)))
	     (components (link-components (cdar l)))
	     (h (navigate-list-sub type 0 source components))
	     (r (link-list->navigate-list (cdr l))))
	(append h r))))

(tm-define (upward-navigate-list)
  (link-list->navigate-list (upward-link-list)))

(tm-define (navigate-list-types l)
  (with types (list-remove-duplicates (map car l))
    (map symbol->string types)))

(tm-define (navigate-list-find-type l type)
  (cond ((null? l) #f)
	((== (symbol->string (caar l)) type) (cadddr (car l)))
	(else (navigate-list-find-type (cdr l) type))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actual navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (link-may-follow?)
  (nnull? (upward-navigate-list)))

(tm-define (link-follow-typed type)
  (:argument type "Link type")
  (:proposals type (navigate-list-types (upward-navigate-list)))
  (and-with t (navigate-list-find-type (upward-navigate-list) type)
    (tree-go-to t :end)))

(tm-define (link-follow)
  (let* ((l (link-list->navigate-list (upward-link-list)))
	 (types (navigate-list-types l)))
    (cond ((null? types) (noop))
	  ((null? (cdr types)) (tree-go-to (cadddr (car l)) :end))
	  (else (interactive link-follow-typed)))))
