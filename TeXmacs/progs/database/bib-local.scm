
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-local.scm
;; DESCRIPTION : editing local modifications to databases in documents
;; COPYRIGHT   : (C) 2017  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-local)
  (:use (database bib-manage)
	(utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Identifying the bibliographies in documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (biblio-context? t)
  (or (tree-func? t 'bibliography 4)
      (tree-func? t 'bibliography* 5)))

(define (biblio-tag)
  (with l (bib-attachments #f)
    (with t (tree-innermost biblio-context?)
      (cond ((and t (tm-atomic? (tm-ref t 0)))
	     (tm->stree (tm-ref t 0)))
	    ((or (null? l) (in? "bib-bibliography" l)) "bib")
	    (else (string-drop-right (car l) 13))))))

(define (linked-biblios-inside t)
  (cond ((tree-atomic? t) (list))
        ((tree-is? t 'document)
         (append-map linked-files-inside (tree-children t)))
        ((tree-in? t '(with with-bib))
         (linked-files-inside (tm-ref t :last)))
        ((or (tree-func? t 'bibliography 4)
	     (tree-func? t 'bibliography* 5))
         (with bib (tm->stree (tm-ref t 0))
	   (if (or (== bib "") (nstring? bib)) (list) (list bib))))
        (else (list))))

(tm-define (biblio-tags)
  (linked-biblios-inside (buffer-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for lists of entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (biblio-as-table l)
  (with t (make-ahash-table)
    (for-each (lambda (e)
		(and-with id (db-entry-ref e "name")
		  (ahash-set! t id e)))
	      l)
    t))

(define (biblio-as-list t)
  (map cdr (ahash-table->list t)))

(define (biblio<=? e1 e2)
  (let* ((id1 (db-entry-ref e1 "name"))
	 (id2 (db-entry-ref e2 "name")))
    (and (string? id1) (string? id2) (string-ci<=? id1 id2))))

(define (biblio-sort l)
  (sort (list-filter l db-entry-any?) biblio<=?))

(define (biblio-append old new)
  (biblio-as-list (biblio-as-table (append old new))))

(define (biblio-diffs old new)
  (let* ((old-t (biblio-as-table old))
	 (new-t (make-ahash-table)))
    (for-each (lambda (new-e)
		(and-with id (db-entry-ref new-e "name")
		  (with old-e (ahash-ref old-t id)
		    (when (and old-e
			       (db-entry-any? old-e)
			       (db-entry-any? new-e)
			       (!= (tm-ref new-e 4) (tm-ref old-e 4)))
		      (ahash-set! new-t id new-e)))))
	      new)
    (biblio-as-list new-t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving lists of entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (biblio-automatic-entries base bib)
  (with-buffer base
    (let* ((key (string-append bib "-bibliography"))
	   (att (tm->stree (get-attachment key)))
	   (l (if (tm-atomic? att) (list) (tm-children att))))
      l)))

(tm-define (biblio-local-entries base bib)
  (with-buffer base
    (let* ((key (string-append bib "-biblio"))
	   (att (tm->stree (get-attachment key)))
	   (l (if (tm-atomic? att) (list) (tm-children att))))
      l)))

(tm-define (biblio-entries base bib)
  (biblio-sort (biblio-append (biblio-automatic-entries base bib)
			      (biblio-local-entries base bib))))

(define (entries-inside t)
  (cond ((tm-atomic? t) (list))
        ((db-entry-any? t) (list t))
	(else (append-map entries-inside (tm-children t)))))

(tm-define (biblio-confirm base bib doc)
  (let* ((old-l (biblio-automatic-entries base bib))
	 (new-l (entries-inside doc))
	 (l (biblio-diffs old-l new-l))
	 (doc `(document ,@l)))
    (with-buffer base
      (set-attachment (string-append bib "-biblio") doc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handlers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-title-handler (biblio name doc)
  (let* ((b (tmfs-car name))
         (f (tmfs-string->url (tmfs-cdr name))))
    (string-append (url->system (url-tail f)) " - " b)))

(tmfs-load-handler (biblio name)
  (let* ((b (tmfs-car name))
         (f (tmfs-string->url (tmfs-cdr name)))
	 (l (biblio-entries f b)))
    (if (null? l) (set! l (list "")))
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "database-bib"))
       (body (document ,@l)))))

(tmfs-permission-handler (biblio name type)
  (in? type (list "read" "write")))

(tmfs-save-handler (biblio name doc)
  (let* ((b (tmfs-car name))
         (f (tmfs-string->url (tmfs-cdr name))))
    (biblio-confirm f b doc)
    (buffer-pretend-modified f)
    #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening the bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (biblio-url base bib)
  (string-append "tmfs://biblio/" bib "/" (url->tmfs-string base)))

(tm-define (open-biblio)
  (:interactive #t)
  (cursor-history-add (cursor-path))
  (revert-buffer (biblio-url (current-buffer) (biblio-tag)))
  (cursor-history-add (cursor-path)))
