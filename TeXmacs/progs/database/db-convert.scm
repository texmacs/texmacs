
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-convert.scm
;; DESCRIPTION : conversion between databases and TeXmacs documents
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-convert)
  (:use (database db-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for additional conversions for specific formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-load-post t)
  (db-entry-rename t (list (cons "id*" "id")
                           (cons "type*" "type")
                           (cons "name*" "name"))))

(tm-define (db-save-pre t)
  (db-entry-rename t (list (cons "id" "id*")
                           (cons "type" "type*")
                           (cons "name" "name*"))))

(tm-define (db-pretty l kind fm)
  l)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-load-fields l)
  (cond ((null? l) l)
        ((or (nlist? (car l)) (<= (length (car l)) 1))
         (db-load-fields (cdr l)))
        (else (cons `(db-field ,(caar l) ,(cadar l))
                    (db-load-fields (cons (cons (caar l) (cddar l))
                                          (cdr l)))))))

(define (db-meta-field? f)
  (and (func? f 'db-field 2)
       (in? (cadr f) (db-meta-attributes))))

(tm-define (assoc-list->entry id l)
  (let* ((type (assoc-ref l "type"))
         (name (assoc-ref l "name")))
    ;;(display* "Load " id " -> " l "\n")
    (set! type (if (pair? type) (car type) "?"))
    (set! name (if (pair? name) (car name) "?"))
    (set! l (assoc-remove! l "type"))
    (set! l (assoc-remove! l "name"))
    (set! l (db-load-fields l))
    (receive (l1 l2) (list-partition l db-meta-field?)
      (db-load-post `(db-entry ,id ,type ,name
			       (document ,@l1) (document ,@l2))))))

(tm-define (db-load-entry id)
  (with l (db-get-entry id)
    (assoc-list->entry id l)))

(tm-define (db-load-types types)
  (let* ((l (db-search (list (cons "type" types))))
         (i (map db-load-entry l)))
    `(document ,@i)))

(tm-define (db-load)
  (let* ((l (db-search (list)))
         (i (map db-load-entry l)))
    `(document ,@i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-save-field t)
  (if (tm-func? t 'db-field 2)
      (list (tm-children t))
      (list)))

(tm-define db-duplicate-warning? #t)

(tm-define (entry->assoc-list t . opt-skip-pre?)
  (when (null? opt-skip-pre?)
    (set! t (db-save-pre t)))
  (let* ((id (tm-ref t 0))
         (type (tm-ref t 1))
         (name (tm-ref t 2))
         (meta (if (db-entry-any? t) (tm-children (tm-ref t 3)) '()))
         (last (tm-children (tm-ref t :last)))
         (pairs (append-map db-save-field (append meta last))))
    (cons* (list "type" type) (list "name" name) pairs)))

(define db-save-serial 0)

(tm-define (db-save-selected-entry t pred?)
  (set! t (db-save-pre t))
  (when (and (db-entry-any? t) (pred? (tm-ref t 1)))
    (let* ((all (entry->assoc-list t #t))
           (id (tm-ref t 0))
           (type (tm-ref t 1))
           (name (tm-ref t 2)))
      (set! db-save-serial (+ db-save-serial 1))
      (when (== (remainder db-save-serial 10) 0)
        (system-wait (string-append "Processing database entry " name ", ")
                     "please wait"))
      (db-import-entry id all))))

(define (db-duplicate? t)
  (and (db-entry-any? t)
       (with id (tm->string (tm-ref t 0))
         (db-entry-exists? id))))

(tm-define (db-save-selected t pred?)
  (cond ((tm-func? t 'document)
         (for-each (cut db-save-selected <> pred?) (tm-children t)))
        ((or (db-entry-any? t) (tm-func? t 'bib-entry 3))
         (when (not (db-duplicate? t))
           (db-save-selected-entry (tm->stree t) pred?)))
        ((and (tree? t) (tm-compound? t))
         (for-each (cut db-save-selected <> pred?)
                   (tree-accessible-children t)))))

(tm-define (db-save-types t types)
  (db-save-selected t (lambda (x) (in? x types))))

(tm-define (db-save t)
  (db-save-selected t (lambda (x) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing and exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-url? u)
  (with s (url->string u)
    (or (string-starts? s "tmfs://db/")
        (string-ends? s ".tmdb"))))

(tm-define (db-confirm-imported name)
  (with key (string-append "recent-import," (url->system (current-buffer)))
    (learn-interactive key (list (cons "name" (url->system name))))))

(tm-define (db-confirm-exported name)
  (with key (string-append "recent-export," (url->system (current-buffer)))
    (learn-interactive key (list (cons "name" (url->system name))))))

(tm-define (db-recent-imports)
  (let* ((key (string-append "recent-import," (url->system (current-buffer))))
         (learned (learned-interactive key))
         (l (map (cut assoc-ref <> "name") learned))
         (r (list-remove-duplicates (map system->url l))))
    (if (<= (length l) 10) l (sublist l 0 10))))

(tm-define (db-recent-exports)
  (let* ((key (string-append "recent-export," (url->system (current-buffer))))
         (learned (learned-interactive key))
         (l (map (cut assoc-ref <> "name") learned))
         (r (list-remove-duplicates (map system->url l))))
    (if (<= (length l) 10) l (sublist l 0 10))))

(tm-define (db-importable?) #f)
(tm-define (db-exportable?) #f)
(tm-define (db-import-file name) (noop))
(tm-define (db-export-file name) (noop))

(tm-define (db-import-select)
  (:interactive #t)
  (noop))

(tm-define (db-export-select)
  (:interactive #t)
  (noop))

(tm-define (db-import-selection) (noop))
(tm-define (db-import-this-entry) (noop))
(tm-define (db-import-current-buffer) (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for syncing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define db-forced-kinds? #f)

(define (db-force-kinds)
  (when (not db-forced-kinds?)
    (import-from (database bib-manage))
    (set! db-forced-kinds? #t)))

(tm-define (db-change-list uid kind t)
  ;;(display* "Get changes " current-database ", " uid ", " kind ", " t "\n")
  (when (number? t) (set! t (number->string t)))
  (db-force-kinds)
  (let* ((types (or (smart-ref db-kind-table kind) #t))
         (ids '()))
    (with-time :always
      (with-user #t
        (set! ids (db-search `(,@(if (== uid #t) (list)
                                     `(("owner" ,uid)))
                               ,@(if (== types #t) (list)
                                     `(("type" ,@types)))
                               (:modified ,t "10675199165"))))))
    (with-user #t
      (with get (lambda (id)
                  (list id
                        (with-time :always
                          (db-get-field-first id "name" #f))
                        (with-time :now
                          (db-get-entry id))))
        (with r (map get ids)
          ;;(display* "r= " r "\n")
          r)))))
