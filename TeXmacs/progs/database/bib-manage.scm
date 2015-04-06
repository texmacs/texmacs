
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-manage.scm
;; DESCRIPTION : global bibliography management
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-manage)
  (:use (database bib-db)))

(tm-define (bib-database) (user-database))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caching existing BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define bib-dir "$TEXMACS_HOME_PATH/system/database")
(define bib-cache-dir (string-append bib-dir "/bib"))
(define bib-master (url->url (string-append bib-dir "/bib-master.tmdb")))

(define (bib-cache-id f)
  (with-database bib-master
    (let* ((s (url->system f))
           (l (db-search (list (list "source" s)))))
      (and (== (length l) 1) (car l)))))

(define (bib-cache-stamp f)
  (and-with id (bib-cache-id f)
    (with-database bib-master
      (db-get-field-first id "stamp" #f))))

(define (bib-cache-imported f)
  (and-with id (bib-cache-id f)
    (with-database bib-master
      (system->url (db-get-field-first id "target" #f)))))

(define (bib-cache-up-to-date? f)
  (and-with stamp (bib-cache-stamp f)
    (and (url-exists? f)
         (== (number->string (url-last-modified f)) stamp))))

(define (bib-cache-remove f)
  (and-with id (bib-cache-id f)
    (and-with imported (bib-cache-imported f)
      (system-remove imported)
      (with-database bib-master
        (db-remove-entry id)))))

(define (bib-cache-create f)
  (with-global db-bib-origin (url->string (url-tail f))
    (let* ((bib-doc (string-load f))
           (t (tmbib-document->texmacs* bib-doc))
           (tm-doc (convert t "texmacs-stree" "texmacs-document"))
           (id (with-database bib-master (db-create-id)))
           (imported (url->url (string-append bib-cache-dir "/" id ".tm"))))
      (string-save tm-doc imported)
      (when (url-exists? imported)
        (with-database bib-master
          (with stamp (number->string (url-last-modified f))
            (db-set-field id "source" (list (url->system f)))
            (db-set-field id "target" (list (url->system imported)))
            (db-set-field id "stamp" (list stamp))))))))

(tm-define (bib-cache-bibtex f)
  (when (not (bib-cache-up-to-date? f))
    (bib-cache-remove f))
  (when (not (bib-cache-id f))
    (bib-cache-create f))
  (when (not (bib-cache-id f))
    (texmacs-error "failed to create bibliographic database"
                   "bib-cache-bibtex"))
  (and-with id (bib-cache-id f)
    (url->url (string-append bib-cache-dir "/" id ".tm"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing and exporting BibTeX files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib-import-bibtex f)
  (with imported (bib-cache-bibtex f)
    (when (url-exists? imported)
      (let* ((tm-doc (string-load imported))
             (t (convert tm-doc "texmacs-document" "texmacs-stree"))
             (body (tmfile-extract t 'body)))
        (with-database (bib-database)
          (with-indexing :basic
            (bib-save body)))
        (when (buffer-exists? "tmfs://db/bib/global")
          (revert-buffer "tmfs://db/bib/global"))
        (set-message "Imported bibliographic entries"
                     "import bibliography")))))

(tm-define (bib-export-global f)
  (with-database (bib-database)
    (with all (bib-load)
      (when (and all (tm-func? all 'document))
        (let* ((doc `(document ,@(map db->bib (cdr all))))
               (bibtex-doc (convert doc "texmacs-stree" "bibtex-document")))
          (string-save bibtex-doc f))))))

(define (bib-entry? t)
  (or (tm-func? t 'bib-entry 3)
      (and (db-entry-any? t)
           (tm-atomic? (tm-ref t 1))
           (in? (tm->string (tm-ref t 1)) bib-types-list))))

(tm-define (bib-export-buffer f)
  (when (tm-func? (buffer-tree) 'document)
    (let* ((l1 (list-filter (tm-children (buffer-tree)) bib-entry?))
           (l2 (map tm->stree l1))
           (l3 (map (lambda (x)
                      (if (tm-func? x 'bib-entry 3) x (db->bib x))) l2))
           (doc `(document ,@l3))
           (bibtex-doc (convert doc "texmacs-stree" "bibtex-document")))
      (string-save bibtex-doc f)
      (set-message "Exported bibliographic entries" "export bibliography"))))

(tm-define (bib-exportable?)
  (or (nnull? (bib-attachments))
      (and (tm-func? (buffer-tree) 'document)
           (list-or (map bib-entry? (tm-children (buffer-tree)))))))

(tm-define (bib-export-bibtex f)
  (if (nnull? (bib-attachments))
      (bib-export-attachments f)
      (bib-export-buffer f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieving entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bib-retrieve-one name)
  (and-with l (db-search (list (list "name" name)))
    (when (> (length l) 1)
      (with l* (db-search (list (list "name" name)
                                (list "contributor" (get-default-user))))
        (when (pair? l*) (set! l l*))))
    (and (nnull? l)
         (with e (db-load-entry (car l))
           (cons name e)))))

(define (bib-retrieve-several names)
  (if (null? names) (list)
      (let* ((head (bib-retrieve-one (car names)))
             (tail (bib-retrieve-several (cdr names))))
        (if head (cons head tail) tail))))

(define (bib-retrieve-entries-from-one names db)
  (with-database db
    (bib-retrieve-several names)))

(define (bib-retrieve-entries-from names dbs)
  (if (null? dbs) (list)
      (let* ((r (bib-retrieve-entries-from-one names (car dbs)))
             (done (map car r))
             (remaining (list-difference names done)))
        (append r (bib-retrieve-entries-from remaining (cdr dbs))))))

(define (bib-get-db bib-file)
  (cond ((== bib-file :default) (bib-database))
        ((== (url-suffix bib-file) "tmdb") (url->url bib-file))
        (else (bib-cache-bibtex bib-file))))

(tm-define (bib-retrieve-entries names . bib-files)
  (set! names (list-remove-duplicates names))
  (with l (list-filter (map bib-get-db bib-files) (lambda (ok?) ok?))
    (bib-retrieve-entries-from names l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Running bibtex or its internal replacement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib-generate prefix style doc)
  (with m `(bibtex ,(string->symbol style))
    (module-provide m)
    (bib-process prefix style doc)))

(define (bib-difference l1 l2)
  (with t (list->ahash-set (map car l2))
    (list-filter l1 (lambda (x) (not (ahash-ref t (car x)))))))

(define (bib-compile-sub prefix style names . bib-files)
  (set! names (list-remove-duplicates names))
  (let* ((all-files (rcons bib-files :default))
         (l (apply bib-retrieve-entries (cons names all-files)))
         (bl (map db->bib (map cdr l)))
         (doc `(document ,@bl)))
    (if (in? style (list "tm-abbrv" "tm-acm" "tm-alpha" "tm-elsart-num"
                         "tm-ieeetr" "tm-plain" "tm-siam" "tm-unsrt"))
        (bib-generate prefix (string-drop style 3) doc)
        (let* ((bib-files*
                (list-filter all-files
                             (lambda (f) (and (url? f)
                                              (== (url-suffix f) "bib")))))
               (l* (apply bib-retrieve-entries (cons names bib-files*)))
               (bl* (map db->bib (map cdr (bib-difference l l*))))
               (doc* `(document ,@bl*))
               (bib-docs (map string-load bib-files*))
               (xdoc (convert doc* "texmacs-stree" "bibtex-document"))
               (all-docs (append bib-docs (list "\n") (list xdoc)))
               (full-doc (apply string-append all-docs))
               (auto (url->url "$TEXMACS_HOME_PATH/system/bib/auto.bib")))
          ;;(display* auto "\n-----------------------------\n" full-doc "\n")
          (string-save full-doc auto)
          (bibtex-run prefix style auto names)))))

(tm-define (bib-compile prefix style names . bib-files)
  (when (and (tm? names) (tm-func? names 'document))
    (set! names (tm-children (tm->stree names))))
  ;;(display* "Compile " style ", " names ", " bib-files "\n")
  (cond ((not (supports-sql?)) (tree "Error: SQL not supported"))
        ((not (supports-db?)) (tree "Error: database tool not activated"))
        ((not (and (list? names) (list-and (map string? names))))
         (tree "Error: invalid bibliographic key list"))
        (else
          (with t (apply bib-compile-sub (cons* prefix style names bib-files))
            (if (not (tm? t))
                (tree "Error: failed to produce bibliography")
                (tm->tree t))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pretty printing of bibliography entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (extract-label t)
  (cond ((tm-func? t 'label 1) (tm-ref t 0))
	((pair? t) (or (extract-label (car t)) (extract-label (cdr t))))
	(else #f)))

(define (remove-label t)
  (cond ((tm-func? t 'bibitem*) "")
	((tm-func? t 'label 1) "")
	((tm-func? t 'concat)
	 (apply tmconcat (map remove-label (tm-children t))))
	(else t)))

(define (rewrite-bibitem t)
  (let* ((lab (extract-label t))
	 (t* (remove-label t))
	 (lab* (if (and (string? lab) (string-starts? lab "bib-"))
		   (string-drop lab 4) "?")))
    `(db-result ,lab* ,t*)))

(tm-define (db-pretty l kind fm)
  (:require (and (== kind "bib") (== fm :pretty)))
  (let* ((bib (map db->bib l))
	 (doc `(document ,@bib))
	 (gen (bib-generate "bib" "siam" doc)))
    (when (tm-func? gen 'bib-list)
      (set! gen (tm-ref gen :last)))
    (with r (if (tm-func? gen 'document) (tm-children gen) (list gen))
      (map rewrite-bibitem r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Attaching the bibliography to the current document and automatic importation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (bib-attach prefix names . bib-files)
  (when (supports-db?)
    (when (and (tm? names) (tm-func? names 'document))
      (set! names (tm-children (tm->stree names))))
    (when (and (list? names) (list-and (map string? names)))
      (set! names (list-remove-duplicates names))
      (let* ((all-files (rcons bib-files :default))
             (l (apply bib-retrieve-entries (cons names all-files)))
             (doc `(document ,@(map cdr l))))
        (set-attachment (string-append prefix "-bibliography") doc)))))

(define (bib-attachments)
  (with l (list-attachments)
    (list-filter l (cut string-ends? <> "-bibliography"))))

(tm-define (bib-export-attachments f)
  (let* ((l (bib-attachments))
         (bibs (map tm->stree (map get-attachment l)))
         (b (append-map tm-children bibs))
         (doc `(document ,@(map db->bib b)))
         (bibtex-doc (convert doc "texmacs-stree" "bibtex-document")))
    (string-save bibtex-doc f)
    (set-message "Exported bibliographic references" "export bibliography")))

(tm-define (notify-set-attachment name key val)
  (when (supports-db?)
    (when (string-ends? key "-bibliography")
      (with doc (tm->stree val)
        (with-database (bib-database)
          (with-indexing :basic
            (bib-save doc))))))
  (former name key val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Using the bibliographic database for the GUI
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-importable?)
  (:require (in-bib?))
  (db-url? (current-buffer)))

(tm-define (db-exportable?)
  (:require (bib-exportable?))
  (db-url? (current-buffer)))

(tm-define (db-import-file)
  (:require (in-bib?))
  (choose-file bib-import-bibtex "Import from BibTeX file" "tmbib"))

(tm-define (db-export-file)
  (:interactive #t)
  (choose-file bib-export-bibtex "Export to BibTeX file" "tmbib"))

(tm-define (open-bib-chooser cb)
  (open-db-chooser (bib-database) "bib" "Search bibliographic reference" cb))
