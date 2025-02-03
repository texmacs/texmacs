
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : link-extern.scm
;; DESCRIPTION : Management of links to extern files
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link link-extern)
  (:use (link link-edit) (link link-navigate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define registry-loaded #f)
(define id-to-name-table (make-ahash-table))
(define name-to-id-table (make-ahash-table))

(define (load-registry-sub x)
  (with (id . l) x
    (for-each (cut ahash-set! name-to-id-table <> id) l)))

(define (load-registry)
  (with name "$TEXMACS_HOME_PATH/system/registry.scm"
    (when (and (not registry-loaded) (url-exists? name))
      (set! registry-loaded #t)
      (let* ((reg (load-object name))
	     (d1 (for-each load-registry-sub reg))
	     (t (list->ahash-table reg)))
	(set! id-to-name-table t)))))

(define (save-registry)
  (with name "$TEXMACS_HOME_PATH/system/registry.scm"
    (save-object name (ahash-table->list id-to-name-table))))

(define (registry-get id)
  (load-registry)
  (ahash-ref* id-to-name-table id '()))

(define (registry-id name)
  (load-registry)
  (when (not (ahash-ref name-to-id-table name))
    (registry-set (create-unique-id) (list name)))
  (ahash-ref name-to-id-table name))

(define (registry-set id names)
  (load-registry)
  (when (!= (ahash-ref id-to-name-table id) names)
    (ahash-set! id-to-name-table id names)
    (for-each (cut ahash-set! name-to-id-table <> id) names)
    (save-registry)))

(define (registry-add id name)
  (load-registry)
  (with l (and (registry-get id) '())
    (registry-set id (list-remove-duplicates (cons name l)))))

(tm-define (get-constellation)
  (load-registry)
  (map car (ahash-table->list name-to-id-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the list of link locators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id-locations id)
  (let* ((l1 (id->trees id))
	 (l2 (map tree->buffer l1))
	 (l2* (id->locations id))
	 (l3 (if (null? l2) l2* l2))
	 (l4 (list-filter l3 nnot)))
    (map (cut cons id <>) l4)))

(define (vertex-locations v)
  (cond ((func? v 'id 1) (id-locations (cadr v)))
	(else '())))

(define (link-item-id-locations item)
  (append-map vertex-locations (link-item-vertices item)))

(define (link-list-id-locations l)
  (append-map link-item-id-locations l))

(define (get-id-locations here t)
  (let* ((l (complete-link-list t #f))
	 (l1 (link-list-id-locations l))
	 (l2 (list-filter l1 (lambda (x) (!= (cdr x) here))))
	 (l3 (map (lambda (x) (cons (car x) (url-delta here (cdr x)))) l2)))
    (list-remove-duplicates l3)))

(define (encode-file-name here x)
  (let* ((id (car x))
	 (u (url-relative here (cdr x)))
	 (s (url->unix u)))
    (list id (list 'id (registry-id s)))))

(define (encode-file-correspondance here id)
  (with u (url-delta here (car (registry-get id)))
    (list id (url->unix u))))

(tm-define (get-link-locations here t)
  (:synopsis "Return locations of extern loci accessible from @t at url @here")
  (with l1 (get-id-locations here t)
    (if (null? l1) (tm->tree '(collection))
	(let* ((l2 (map (cut encode-file-name here <>) l1))
	       (ids (list-remove-duplicates (map cadadr l2)))
	       (l3 (map (cut encode-file-correspondance here <>) ids)))
	  (tm->tree `(collection (id ,(registry-id (url->unix here)))
				 ,@(map (cut cons 'target <>) l3)
				 ,@(map (cut cons 'locator <>) l2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving the list of link locators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define already-loaded-table (make-ahash-table))
(define link-locator-table (make-ahash-table))

(define (id->locations id)
  (if (func? id 'id 1) (set! id (cadr id)))
  (with name (ahash-ref id-to-name-table id)
    (if name (map unix->url name)
	(with l (ahash-ref* link-locator-table id '())
	  (append-map id->locations l)))))

(define (resolve-id-sub id l)
  (cond ((null? l) #f)
	((not (url-exists? (car l))) (resolve-id-sub id (cdr l)))
	(else
          (ahash-set! already-loaded-table (car l) #t)
          (load-buffer (car l)))))

(tm-define (resolve-id id)
  (:synopsis "Load file which contains locus with a given @id")
  (let* ((not-loaded? (lambda (u) (not (ahash-ref already-loaded-table u))))
	 (l1 (id->locations id))
	 (l2 (list-filter l1 not-loaded?)))
    (resolve-id-sub id l2)))

(define (register-unique-id here t)
  (with (dummy id) t
    (registry-add id (url->unix here))))

(define (register-target here t)
  (with (dummy id loc) t
    (with u (url-relative here loc)
      (when (url-exists? u)
	(registry-add id (url->unix u))))))

(define (register-locator here t)
  (with (dummy id id2) t
    (ahash-set! link-locator-table id
		(list-union (list id2)
			    (ahash-ref* link-locator-table id '())))))

(define (register-link-location here t)
  (cond ((func? t 'id 1) (register-unique-id here t))
	((func? t 'target 2) (register-target here t))
	((func? t 'locator 2) (register-locator here t))))

(tm-define (register-link-locations here t)
  (:synopsis "Register extern loci listed in @t at url @here")
  (with l (cdr (tree->stree t))
    (for-each (cut register-link-location here <>) l)))
