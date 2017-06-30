
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
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (linked-biblios-inside t)
  (cond ((tree-atomic? t) (list))
        ((tree-is? t 'document)
         (append-map linked-files-inside (tree-children t)))
        ((tree-is? t 'with)
         (linked-files-inside (tm-ref t :last)))
        ((or (tree-func? t 'bibliography 4)
	     (tree-func? t 'bibliography* 5))
         (with bib (tm->stree (tm-ref t 0))
	   (if (or (== bib "") (nstring? bib)) (list) (list bib))))
        (else (list))))

(tm-define (linked-biblios-list)
  (linked-biblios-inside (buffer-tree)))

(tm-define (biblio-url base bib)
  (string-append "tmfs://biblio/" bib "/" (url->tmfs-string base)))

(tm-define (biblio-entries base bib)
  (with-buffer base
    (let* ((key (string-append bib "-bibliography"))
	   (att (tm->stree (get-attachment key)))
	   (l (if (tm-atomic? att) (list) (tm-children att))))
      l)))

(define (biblio-context? t)
  (or (tree-func? t 'bibliography 4)
      (tree-func? t 'bibliography* 5)))

(define (get-bib-tag)
  (with l (bib-attachments)
    (with t (tree-innermost biblio-context?)
      (cond ((and t (tm-atomic? (tm-ref t 0)))
	     (tm->stree (tm-ref t 0)))
	    ((or (null? l) (in? "bib-bibliography" l)) "bib")
	    (else (string-drop-right (car l) 13))))))

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
    `(document
       (TeXmacs ,(texmacs-version))
       (style (tuple "database-bib"))
       (body (document ,@l)))))

;;(tmfs-permission-handler (biblio name type)
;;  (in? type (list "read" "write")))
;;(tmfs-save-handler (biblio name doc)
;;  (db-confirm-entries-in (tm->tree doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening the bibliography
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-biblio)
  (:interactive #t)
  (cursor-history-add (cursor-path))
  (revert-buffer (biblio-url (current-buffer) (get-bib-tag)))
  (cursor-history-add (cursor-path)))
