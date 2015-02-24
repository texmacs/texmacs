
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
  (:use (database db-resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for additional conversions for specific formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-import-post t) t)
(tm-define (db-export-pre t) t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-import-entries l)
  ;; TODO: take into account string encodings
  (cond ((null? l) l)
        ((or (nlist? (car l)) (<= (length (car l)) 1))
         (db-import-entries (cdr l)))
        (else (cons `(db-entry ,(caar l) ,(cadar l))
                    (db-import-entries (cons (cons (caar l) (cddar l))
                                             (cdr l)))))))

(tm-define (db-import-resource rid)
  (let* ((l (db-get-all-decoded rid))
         (type (assoc-ref l "type"))
         (name (assoc-ref l "name")))
    (set! type (if (pair? type) (car type) "?"))
    (set! name (if (pair? name) (car name) "?"))
    (assoc-remove! l "type")
    (assoc-remove! l "name")
    (set! l (db-import-entries l))
    (db-import-post `(db-resource ,rid ,type ,name (document ,@l)))))

(tm-define (db-import-type type)
  (let* ((l (db-search (list (list "type" type))))
         (i (map db-import-resource l)))
    `(document ,@i)))

(tm-define (db-import)
  (let* ((l (db-search (list)))
         (i (map db-import-resource l)))
    `(document ,@i)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exporting databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-export-entry t)
  (if (tm-func? t 'db-entry 2)
      (list (tm-children t))
      (list)))

(tm-define (db-export-resource t)
  (set! t (db-export-pre t))
  (when (and (tm-func? t 'db-resource 4)
             (tm-func? (tm-ref t 3) 'document))
    (let* ((rid (tm-ref t 0))
           (type (tm-ref t 1))
           (name (tm-ref t 2))
           (pairs (append-map db-export-entry (tm-children (tm-ref t 3))))
           (all (cons* (list "type" type) (list "name" name) pairs)))
      (display* rid " -> " all "\n"))))

(tm-define (db-export t)
  (cond ((tm-func? t 'document)
         (for-each db-export (tm-children t)))
        ((or (tm-func? t 'db-resource 4) (tm-func? t 'bib-entry 3))
         (db-export-resource (tm->stree t)))
        ((and (tree? t) (tm-compound? t))
         (for-each db-export (tree-accessible-children t)))))
