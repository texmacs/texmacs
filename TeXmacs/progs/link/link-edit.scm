
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
;; Loci with unique identifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define texmacs-session-id (var-eval-system "openssl rand -base64 6"))
(define texmacs-serial-id (* 1000000000 (abs (texmacs-time))))

(define (base64 x)
  (if (== x 0) '()
      (append (base64 (quotient x 64))
	      (list (remainder x 64)))))

(define (aschar x)
  (with c (integer->char (+ x 48))
    (cond ((== c #\<) #\{)
	  ((== c #\>) #\})
	  (else c))))

(define (number->base64 x)
  (list->string (map aschar (base64 x))))

(tm-define (create-unique-id)
  (set! texmacs-serial-id (+ texmacs-serial-id 1))
  (string-append texmacs-session-id (number->base64 texmacs-serial-id)))

(tm-define (make-locus)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(locus (id ,(create-unique-id)) ,t)
		  (cons 1 (path-end t '())))))

(tm-define (locus-id t)
  (and (tm-func? t 'locus)
       (>= (tm-length t) 2)
       (tm-func? (tm-ref t 0) 'id 1)
       (tree-atomic? (tm-ref t 0 0))
       (tree->string (tm-ref t 0 0))))

(define (locus-insert-link t ln)
  (tree-insert t (- (tree-arity t) 1) `(locus ,(tree-copy ln))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-participants (make-ahash-table))

(define (link-remove-participant nr)
  (with p (ahash-ref link-participants nr)
    (when p
      (ahash-remove! link-participants nr)
      (tree-pointer-detach p))))

(tm-define (link-insert-locus nr)
  (with-innermost t 'locus
    (ahash-set! link-participants nr (tree->tree-pointer t))))

(tm-define (link-under-construction?)
  (nnot (ahash-ref link-participants 0)))

(define (link-participating-trees nr)
  (with p (ahash-ref link-participants nr)
    (if (not p) '()
	(with t (tree-pointer->tree p)
	  (link-remove-participant nr)
	  (cons t (link-participating-trees (+ nr 1)))))))

(tm-define (make-link type)
  (:argument type "Link type")
  (let* ((l (link-participating-trees 0))
	 (ids (map locus-id l))
	 (ln (tm->tree `(link ,type ,@ids))))
    (when (and (nnull? l) (list-and ids))
      (for-each (cut locus-insert-link <> ln) l))))

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
	(if (== source (car l)) tail
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
  (nnull? (upward-navigate-list)))

(tm-define (go-to-id id)
  (with l (id->trees id)
    (when (nnull? l)
      (tree-go-to (car l) :last :end))))

(tm-define (link-follow-typed type)
  (:argument type "Link type")
  (:proposals type (navigate-list-types (upward-navigate-list)))
  (and-with id (navigate-list-find-type (upward-navigate-list) type)
    (go-to-id id)))

(tm-define (link-follow)
  (let* ((l (link-list->navigate-list (upward-link-list)))
	 (types (navigate-list-types l)))
    (cond ((null? types) (noop))
	  ((null? (cdr types)) (go-to-id (cadddr (car l))))
	  (else (interactive link-follow-typed)))))
