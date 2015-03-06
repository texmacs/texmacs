
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

(tm-define (db-import-post t)
  (db-entry-rename t (list (cons "mid" "id")
                              (cons "mtype" "type")
                              (cons "mname" "name"))))

(tm-define (db-export-pre t)
  (db-entry-rename t (list (cons "id" "mid")
                              (cons "type" "mtype")
                              (cons "name" "mname"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-import-fields l)
  ;; TODO: take into account string encodings
  (cond ((null? l) l)
        ((or (nlist? (car l)) (<= (length (car l)) 1))
         (db-import-fields (cdr l)))
        (else (cons `(db-field ,(caar l) ,(cadar l))
                    (db-import-fields (cons (cons (caar l) (cddar l))
                                            (cdr l)))))))

(tm-define (db-import-entry id)
  (let* ((l (db-get-all-decoded id))
         (type (assoc-ref l "type"))
         (name (assoc-ref l "name")))
    ;;(display* "Import " id " -> " l "\n")
    (set! type (if (pair? type) (car type) "?"))
    (set! name (if (pair? name) (car name) "?"))
    (set! l (assoc-remove! l "type"))
    (set! l (assoc-remove! l "name"))
    (set! l (db-import-fields l))
    (db-import-post `(db-entry ,id ,type ,name (document ,@l)))))

(tm-define (db-import-type type)
  (let* ((l (db-search (list (list "type" type))))
         (i (map db-import-entry l)))
    `(document ,@i)))

(tm-define (db-import-types types)
  (with l (map cdr (map db-import-type types))
    `(document ,@(apply append l))))

(tm-define (db-import)
  (let* ((l (db-search (list)))
         (i (map db-import-entry l)))
    `(document ,@i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-export-field t)
  (if (tm-func? t 'db-field 2)
      (list (tm-children t))
      (list)))

(tm-define (db-export-selected-entry t pred?)
  (set! t (db-export-pre t))
  (when (and (db-entry? t) (pred? (tm-ref t 1)))
    (let* ((id (tm-ref t 0))
           (type (tm-ref t 1))
           (name (tm-ref t 2))
           (pairs (append-map db-export-field (tm-children (tm-ref t 3))))
           (all (cons* (list "type" type) (list "name" name) pairs)))
      ;;(display* "Export " id " -> " all "\n")
      (db-set id "type" (list type))
      (db-set-all-encoded id all))))

(tm-define (db-export-selected t pred?)
  (cond ((tm-func? t 'document)
         (for-each (cut db-export-selected <> pred?) (tm-children t)))
        ((or (tm-func? t 'db-entry 4) (tm-func? t 'bib-entry 3))
         (db-export-selected-entry (tm->stree t) pred?))
        ((and (tree? t) (tm-compound? t))
         (for-each (cut db-export-selected <> pred?)
                   (tree-accessible-children t)))))

(tm-define (db-export-types t types)
  (db-export-selected t (lambda (x) (in? x types))))

(tm-define (db-export t)
  (db-export-selected t (lambda (x) #t)))
