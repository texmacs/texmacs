
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

(texmacs-module (link link-edit)
  (:use (link locus-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Current link mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-link-mode "bidirectional")

(define (in-link-mode? mode)
  (== current-link-mode mode))

(tm-define (set-link-mode mode)
  (:synopsis "Set the current linking mode to @mode")
  (:check-mark "v" in-link-mode?)
  (set! current-link-mode mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility routines for links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (link-type ln)
  (if (tree? ln) (set! ln (tree->stree ln)))
  (and (func? ln 'link) (cadr ln)))

(tm-define (link-vertices ln)
  (if (tree? ln) (set! ln (tree->stree ln)))
  (and (func? ln 'link) (cddr ln)))

(tm-define (link-source ln)
  (car (link-vertices ln)))

(tm-define (link-target ln)
  (cadr (link-vertices ln)))

(tm-define (vertex->id r)
  (and (func? r 'id 1) (string? (cadr r)) (cadr r)))

(tm-define (vertex->url r)
  (and (func? r 'url 1) (string? (cadr r)) (cadr r)))

(tm-define (vertex->data r)
  (and (func? r 'link-data 1) (cadr r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering the necessary information for the next link
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-participants (make-ahash-table))

(define (link-on-locus? nr)
  (if (not (inside? 'locus)) ""
      (with-innermost t 'locus
        (with tp (ahash-ref link-participants nr)
          (cond ((not (observer? tp)) "")
                ((== t (tree-pointer->tree tp)) "v")
                (else "o"))))))

(tm-define (link-set-locus nr)
  (:synopsis "Set locus number @nr of a link.")
  (:check-mark "auto" link-on-locus?)
  (with-innermost t 'locus
    (ahash-set! link-participants nr (tree->tree-pointer t))))

(define (link-component-is-url? . args)
  (func? (ahash-ref link-participants nr) 'url))
(tm-define (link-set-url nr name)
  (:synopsis "Set component @nr of a link to an url @name.")
  (:check-mark "o" link-component-is-url?)
  (ahash-set! link-participants nr `(url ,name)))

(define (link-source-is-url? . args)
  (func? (ahash-ref link-participants 0) 'url))
(tm-define (link-set-source-url url)
  (:synopsis "Set source of link to an @url.")
  (:check-mark "o" link-source-is-url?)
  (ahash-set! link-participants 0 `(url ,url)))

(define (link-target-is-url? . args)
  (func? (ahash-ref link-participants 1) 'url))
(tm-define (link-set-target-url url)
  (:synopsis "Set target of link to an @url.")
  (:check-mark "o" link-target-is-url?)
  (ahash-set! link-participants 1 `(url ,url)))

(define (link-component-is-data? nr . args)
  (func? (ahash-ref link-participants nr) 'link-data))
(tm-define (link-set-data nr t)
  (:synopsis "Set component @nr of a link to tree data @t.")
  (:check-mark "o" link-component-is-data?)
  (ahash-set! link-participants nr `(url ,name)))

(tm-define (link-completed?)
  (:synopsis "Did enter all necessary information for constructing a link?")
  (and (ahash-ref link-participants 0)
       (ahash-ref link-participants 1)
       (cond ((== current-link-mode "simple") #t)
             ((== current-link-mode "bidirectional") #t)
             ((== current-link-mode "external") (inside? 'locus))
             (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building the next link
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (link-get-trees nr)
  (with p (ahash-ref link-participants nr)
    (cond ((not p) '())
	  ((observer? p)
	   (with t (tree-pointer->tree p)
	     (cons t (link-get-trees (+ nr 1)))))
	  (else (link-get-trees (+ nr 1))))))

(define (link-vertices nr)
  (with p (ahash-ref link-participants nr)
    (cond ((not p) '())
	  ((observer? p)
	   (with t (tree-pointer->tree p)
	     (cons `(id ,(locus-id t)) (link-vertices (+ nr 1)))))
	  (else (cons p (link-vertices (+ nr 1)))))))

(define (link-remove-participant nr)
  (with p (ahash-ref link-participants nr)
    (ahash-remove! link-participants nr)
    (when (oberver? p)
      (tree-pointer-detach p))))

(define (link-clean nr)
  (and-with p (ahash-ref link-participants nr)
    (if (observer? p) (tree-pointer-detach p))
    (link-clean (+ nr 1))))

(define (link-build type)
  (let* ((ts (link-get-trees 0))
         (vs (link-vertices 0))
         (ln (tm->tree `(link ,type ,@vs))))
    (when (and (nnull? vs) (list-and (map locus-id ts)))
      (cond ((== current-link-mode "simple")
	     (and-let* ((tp (ahash-ref link-participants 0))
			(d (observer? tp))
			(t (tree-pointer->tree tp)))
               (locus-insert-link t ln)))
            ((== current-link-mode "bidirectional")
             (for-each (cut locus-insert-link <> ln) ts))
            ((== current-link-mode "external")
             (with-innermost t 'locus
               (locus-insert-link t ln)))
            (else (set-message "Unsupported link mode" "Make link"))))))

(tm-define (make-link type)
  (:synopsis "Make a link of type @type.")
  (:argument type "Link type")
  (when (link-completed?)
    (for-each link-build (string-tokenize-comma type))
    (link-clean 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destruction of links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus-consider-link? ln check-mode?)
  (and (tm-func? ln 'link)
       (or (not check-mode?)
	   (cond ((== current-link-mode "simple")
		  (and-let* ((id1 (locus-id (tree-up ln)))
			     (id2 (link-source ln)))
		    (== id1 id2)))
		 ((== current-link-mode "bidirectional") #t)
		 ((== current-link-mode "external") #t)
		 (else #f)))))

(tm-define (locus-link-types check-mode?)
  (:synopsis "Get all link types occurring in the current locus.")
  (:argument check-mode? "Filter according to the current link mode?")
  (if (not (inside? 'locus)) '()
      (with-innermost t 'locus
	(let* ((l1 (cDr (tree-children t)))
	       (l2 (list-filter l1 (cut locus-consider-link? <> check-mode?)))
	       (l3 (map cadr (map tree->stree l2))))
	  (list-remove-duplicates l3)))))

(define (remove-link ln type)
  (with st (tree->stree ln)
    (when (and (func? st 'link) (== (cadr st) type))
      (cond ((== current-link-mode "simple")
	     (locus-remove-link ln))
	    ((== current-link-mode "bidirectional")
	     (let* ((ids (filter-map vertex->id (cddr st)))
		    (ts1 (append-map id->loci ids))
		    (fun (lambda (t) (not (tree-eq? t (tree-up ln)))))
		    (ts2 (list-filter ts1 fun)))
	       (for-each (cut locus-remove-all-links <> ln) ts2)
	       (locus-remove-link ln)))
	    ((== current-link-mode "external")
	     (locus-remove-link ln))
	    (else (set-message "Unsupported link mode" "Remove link"))))))

(define (remove-link-of-type type)
  (with-innermost t 'locus
    (for-each (cut remove-link <> type)
	      (reverse (cDr (tree-children t))))))

(tm-define (remove-link-of-types type)
  (:synopsis "Remove all links of type @type.")
  (:argument type "Link type")
  (:proposals type (locus-link-types #t))
  (for-each remove-link-of-type (string-tokenize-comma type)))

(tm-define (remove-all-links)
  (:synopsis "Remove all links from the current locus.")
  (for-each remove-link-of-type (locus-link-types #t)))
