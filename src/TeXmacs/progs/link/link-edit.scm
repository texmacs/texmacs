
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
  (cond ((< x 10) (integer->char (+ x 48)))
	((< x 36) (integer->char (+ x 54)))
	((< x 62) (integer->char (+ x 60)))
	((== x 62) #\{)
	(else #\})))

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
	 (Ids (map (lambda (x) `(id ,x)) ids))
	 (ln (tm->tree `(link ,type ,@Ids))))
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

(define (Id->id Id)
  (cond ((func? Id 'id 1) (Id->id (cadr Id)))
	(else Id)))

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
  (:argument type "Link type")
  (:proposals type (navigate-list-types (upward-navigate-list)))
  (and-with Id (navigate-list-find-type (upward-navigate-list) type)
    (go-to-Id Id)))

(tm-define (link-follow)
  (let* ((l (link-list->navigate-list (upward-link-list)))
	 (types (navigate-list-types l)))
    (cond ((null? types) (noop))
	  ((null? (cdr types)) (go-to-Id (cadddr (car l))))
	  (else (interactive link-follow-typed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the list of link locators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id-locations id)
  (let* ((get-name (lambda (t) (get-name-buffer-path (tree->path t))))
	 (l1 (id->trees id))
	 (l2 (map get-name l1))
	 (l3 (list-filter l2 (lambda (u) (not (url-none? u))))))
    (map (cut cons id <>) l3)))

(define (Id-locations Id)
  (if (func? Id 'id 1)
      (id-locations (cadr Id))
      '()))

(define (link-item-id-locations ln)
  (with (id type . lns) ln
    (append-map Id-locations lns)))

(define (link-list-id-locations l)
  (append-map link-item-id-locations l))

(define (get-id-locations here t)
  (let* ((l (complete-link-list t))
	 (l1 (link-list-id-locations l))
	 (l2 (list-filter l1 (lambda (x) (!= (cdr x) here))))
	 (l3 (map (lambda (x) (cons (car x) (url-delta here (cdr x)))) l2)))
    (list-remove-duplicates l3)))

(define (encode-link-location x)
  (let* ((id (car x))
	 (u (cdr x))
	 (s (url->string u)))
    `(locator ,id ,s)))

(tm-define (get-link-locations here t)
  (with l (get-id-locations here t)
    (tm->tree `(collection ,@(map encode-link-location l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving the list of link locators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define already-loaded-table (make-ahash-table))
(define link-locator-table (make-ahash-table))

(define (register-link-location here t)
  (with (dummy id loc) t
    (with u (url-relative here loc)
      (ahash-set! link-locator-table id
		  (list-union (list u)
			      (ahash-ref* link-locator-table id '()))))))

(define (resolve-id-sub id l)
  (cond ((null? l) #f)
	((not (url-exists? (car l))) (resolve-id-sub id (cdr l)))
	(else
	 (ahash-set! already-loaded-table (car l) #t)
	 (texmacs-load-buffer (car l) "generic" 0 #f))))

(define (resolve-id id)
  (let* ((not-loaded? (lambda (u) (not (ahash-ref already-loaded-table u))))
	 (l1 (ahash-ref link-locator-table id))
	 (l2 (list-filter l1 not-loaded?)))
    (resolve-id-sub id l2)))

(tm-define (register-link-locations here t)
  (with l (cdr (tree->stree t))
    (for-each (cut register-link-location here <>) l)))
