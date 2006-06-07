
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
	((< x 36) (integer->char (+ x 55)))
	((< x 62) (integer->char (+ x 61)))
	((== x 62) #\{)
	(else #\})))

(define (number->base64 x)
  (list->string (map aschar (base64 x))))

(tm-define (create-unique-id)
  (:synopsis "Create a unique file or locus identifier")
  (set! texmacs-serial-id (+ texmacs-serial-id 1))
  (string-append texmacs-session-id (number->base64 texmacs-serial-id)))

(tm-define (make-locus)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(locus (id ,(create-unique-id)) ,t)
		  (cons 1 (path-end t '())))))

(tm-define (locus-id t)
  (:synopsis "Return the unique identifier of the locus @t or #f.")
  (and (tm-func? t 'locus)
       (>= (tm-length t) 2)
       (tm-func? (tm-ref t 0) 'id 1)
       (tree-atomic? (tm-ref t 0 0))
       (tree->string (tm-ref t 0 0))))

(define (locus-insert-link t ln)
  (tree-insert t (- (tree-arity t) 1) `(locus ,(tree-copy ln))))

(define (locus-remove-link ln)
  (tree-remove (tree-up ln) (tree-index ln) 1))

(define (locus-remove-match ln match-with)
  (if (== ln match-with) (locus-remove-link ln)))

(define (locus-remove-all t ln)
  (for-each (cut locus-remove-match <> ln)
	    (reverse (cDr (tree-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-link-mode "bidirectional")

(define (in-link-mode? mode)
  (== current-link-mode mode))

(tm-define (set-link-mode mode)
  (:synopsis "Set the current linking mode to @mode")
  (:check-mark "v" in-link-mode?)
  (set! current-link-mode mode))

(define (decompose-link-type type)
  (if (string-starts? type " ")
      (decompose-link-type (substring type 1 (string-length type)))
      (with pos (string-search-forwards "," 0 type)
	(if (< pos 0) (list type)
	    (cons (substring type 0 pos)
		  (decompose-link-type
		   (substring type (+ pos 1) (string-length type))))))))

(tm-define (Id->id Id)
  (and (func? Id 'id 1) (string? (cadr Id)) (cadr Id)))

(define (tree->locus t)
  (and-with p (tree-up t)
    (and (tm-func? p 'locus) p)))

(tm-define (id->loci id)
  (filter-map tree->locus (id->trees id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-participants (make-ahash-table))

(define (link-remove-participant nr)
  (with p (ahash-ref link-participants nr)
    (when p
      (ahash-remove! link-participants nr)
      (tree-pointer-detach p))))

(define (link-on-locus? nr)
  (if (not (inside? 'locus)) ""
      (with-innermost t 'locus
	(with tp (ahash-ref link-participants nr)
	  (cond ((not tp) "")
		((== t (tree-pointer->tree tp)) "v")
		(else "o"))))))

(tm-define (link-set-locus nr)
  (:synopsis "Set locus number @nr of a link.")
  (:check-mark "auto" link-on-locus?)
  (with-innermost t 'locus
    (ahash-set! link-participants nr (tree->tree-pointer t))))

(tm-define (link-completed-loci?)
  (:synopsis "Did we mark all necessary loci for constructing a link?")
  (and (ahash-ref link-participants 0)
       (ahash-ref link-participants 1)
       (cond ((== current-link-mode "simple") #t)
	     ((== current-link-mode "bidirectional") #t)
	     ((== current-link-mode "external") (inside? 'locus))
	     (else #f))))

(define (link-participating-trees nr)
  (with p (ahash-ref link-participants nr)
    (if (not p) '()
	(with t (tree-pointer->tree p)
	  (cons t (link-participating-trees (+ nr 1)))))))

(define (link-remove-participants nr)
  (and-with p (ahash-ref link-participants nr)
    (with t (tree-pointer->tree p)
      (link-remove-participant nr)
      (link-remove-participants (+ nr 1)))))

(define (make-link-sub type)
  (let* ((l (link-participating-trees 0))
	 (ids (map locus-id l))
	 (Ids (map (lambda (x) `(id ,x)) ids))
	 (ln (tm->tree `(link ,type ,@Ids))))
    (when (and (nnull? l) (list-and ids))
      (cond ((== current-link-mode "simple")
	     (with t (tree-pointer->tree (ahash-ref link-participants 0))
	       (locus-insert-link t ln)))
	    ((== current-link-mode "bidirectional")
	     (for-each (cut locus-insert-link <> ln) l))
	    ((== current-link-mode "external")
	     (with-innermost t 'locus
	       (locus-insert-link t ln)))
	    (else (set-message "Unsupported link mode" "Make link"))))))

(tm-define (make-link type)
  (:synopsis "Make a link of type @type.")
  (:argument type "Link type")
  (when (link-completed-loci?)
    (for-each make-link-sub (decompose-link-type type))
    (link-remove-participants 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Destruction of links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (locus-link-type ln check-mode?)
  (and (tm-func? ln 'link)
       (or (not check-mode?)
	   (cond ((== current-link-mode "simple")
		  (let* ((id (locus-id (tree-up ln)))
			 (id2 (Id->id (caddr (tree->stree ln)))))
		    (== id id2)))
		 ((== current-link-mode "bidirectional") #t)
		 ((== current-link-mode "external") #t)
		 (else #f)))))

(tm-define (locus-link-types check-mode?)
  (:synopsis "Get all link types occurring in the current locus.")
  (:argument check-mode? "Filter according to the current link mode?")
  (if (not (inside? 'locus)) '()
      (with-innermost t 'locus
	(let* ((l1 (cDr (tree-children t)))
	       (l2 (list-filter l1 (cut locus-link-type <> check-mode?)))
	       (l3 (map cadr (map tree->stree l2))))
	  (list-remove-duplicates l3)))))

(define (remove-link-sub-sub ln type)
  (with st (tree->stree ln)
    (when (and (func? st 'link) (== (cadr st) type))
      (cond ((== current-link-mode "simple")
	     (locus-remove-link ln))
	    ((== current-link-mode "bidirectional")
	     (let* ((ids (filter-map Id->id (cddr st)))
		    (ts1 (append-map id->loci ids))
		    (fun (lambda (t) (not (tree-eq? t (tree-up ln)))))
		    (ts2 (list-filter ts1 fun)))
	       (for-each (cut locus-remove-all <> ln) ts2)
	       (locus-remove-link ln)))
	    ((== current-link-mode "external")
	     (locus-remove-link ln))
	    (else (set-message "Unsupported link mode" "Remove link"))))))

(define (remove-link-sub type)
  (with-innermost t 'locus
    (for-each (cut remove-link-sub-sub <> type)
	      (reverse (cDr (tree-children t))))))

(tm-define (remove-link type)
  (:synopsis "Remove all links of type @type.")
  (:argument type "Link type")
  (:proposals type (locus-link-types #t))
  (for-each remove-link-sub (decompose-link-type type)))

(tm-define (remove-all-links)
  (:synopsis "Remove all links from the current locus.")
  (for-each remove-link-sub (locus-link-types #t)))
