
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-serialize.scm
;; DESCRIPTION : serialization of trees with links
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-serialize)
  (:use (link link-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unique identifier management
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

(define (tree-create-unique-id t)
  (if (== (tree-get-unique-id t) "")
      (tree-set-unique-id t (create-unique-id))))

(define (link-item-create-unique-ids x)
  (tree-create-unique-id (car x))
  (for-each tree-create-unique-id (link-components (cdr x))))

(define (link-list-create-unique-ids l)
  (for-each link-item-create-unique-ids l))

(tm-define (tree-create-unique-ids t)
  (link-list-create-unique-ids (complete-link-list t)))

(define (link-item-get-unique-ids x)
  (cons (tree-get-unique-id (car x))
	(map tree-get-unique-id (link-components (cdr x)))))

(define (link-list-get-unique-ids l)
  (with ids (append-map link-item-get-unique-ids l)
    (cdr (list-remove-duplicates (cons "" ids)))))

(tm-define (tree-get-unique-ids t)
  (link-list-get-unique-ids (complete-link-list t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make links in tree explicit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unique-ids-build-table h l nr)
  (when (nnull? l)
    (ahash-set! h (car l) (number->string nr))
    (unique-ids-build-table h (cdr l) (+ nr 1))))

(define (tree-encode t h)
  (let* ((enc (cut tree-encode <> h))
	 (id (tree-get-unique-id t))
	 (r (if (tree-atomic? t) t
		(cons (tree-label t) (map enc (tree-children t))))))
      (if (== id "") r (tree '$ (ahash-ref h id) r))))

(define (unique-ids-encode l nr)
  (if (null? l) l
      (cons `(associate ,(number->string nr) ,(car l))
	    (unique-ids-encode (cdr l) (+ nr 1)))))

(define (link-encode ln h)
  (let* ((type (link-type ln))
	 (ts (link-components ln))
	 (ids (map tree-get-unique-id ts))
	 (encl (map (cut ahash-ref h <>) ids)))
    (cons type encl)))

(tm-define (tree-linked? t)
  (nnull? (complete-link-list t)))

(tm-define (tree->linked-tree t)
  (let* ((l (complete-link-list t))
	 (d1 (link-list-create-unique-ids l))
	 (ids (link-list-get-unique-ids l))
	 (h (make-ahash-table))
	 (d2 (unique-ids-build-table h ids 0))
	 (enc1 (tree-encode t h))
	 (enc2 (unique-ids-encode ids 0))
	 (lns (list-remove-duplicates (map cdr l)))
	 (enc3 (map (cut link-encode <> h) lns)))
    (tm->tree `(hyperlinked ,enc1
			    (collection ,@enc2)
			    (collection ,@enc3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construct links from serialized representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unique-ids-decode-table h l)
  (when (nnull? l)
    (with (enc id) (cdar l)
      (ahash-set! h enc id)
      (unique-ids-decode-table h (cdr l)))))

(define (tree-decode t h)
  (cond ((tree-atomic? t) t)
	((and (tm-func? t '$ 2) (tree-atomic? (tree-ref t 0)))
	 (let* ((r (tree-decode (tree-ref t 1) h))
		(enc (tree->string (tree-ref t 0)))
		(dec (ahash-ref h enc)))
	   (if dec (tree-set-unique-id r dec))
	   r))
	(else (let* ((l (tree-label t))
		     (ch (tree-children t)))
		(tm->tree (cons l (map (cut tree-decode <> h) ch)))))))

(define (link-item-decode item h)
  (let* ((type (car item))
	 (ids (map (cut ahash-ref h <>) (cdr item)))
	 (ts (map unique-id->tree ids)))
    (link-register (link-construct type ts))))

(tm-define (linked-tree->tree t)
  (let* ((enc1 (tree-ref t 0))
	 (enc2 (tree->stree (tree-ref t 1)))
	 (enc3 (tree->stree (tree-ref t 2)))
	 (h (make-ahash-table))
	 (d1 (unique-ids-decode-table h (cdr enc2)))
	 (u (tree-decode enc1 h)))
    (for-each (cut link-item-decode <> h) (reverse (cdr enc3)))
    u))
