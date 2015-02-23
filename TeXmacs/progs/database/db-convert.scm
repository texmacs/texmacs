
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for database markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-entry-find l prop)
  (and (nnull? l)
       (or (and (tm-func? (car l) 'db-entry 2)
                (tm-equal? (tm-ref (car l) 0) prop)
                (tm-ref (car l) 1))
           (db-entry-find (cdr l) prop))))

(tm-define (db-resource-ref t prop)
  (and (tm-func? t 'db-resource 4)
       (tm-func? (tm-ref t :last) 'document)
       (cond ((== prop "rid") (tm-ref t 0))
             ((== prop "type") (tm-ref t 1))
             ((== prop "name") (tm-ref t 2))
             (else (db-entry-find (tm-children (tm-ref t 3)) prop)))))

(tm-define (db-entry-replace l prop val)
  (if (null? l) l
      (cons (if (and (tm-func? (car l) 'db-entry 2)
                     (tm-equal? (tm-ref (car l) 0) prop))
                `(db-entry ,prop ,val)
                (car l))
            (db-entry-replace (cdr l) prop val))))

(tm-define (db-resource-set t prop val)
  (and (tm-func? t 'db-resource 4)
       (tm-func? (tm-ref t :last) 'document)
       (with l (tm-children t)
         (cond ((== prop "rid") `(db-resource ,val ,@(cdr l)))
               ((== prop "type") `(db-resource ,(car l) ,val ,@(cddr l)))
               ((== prop "name")
                `(db-resource ,(car l) ,(cadr l) ,val ,@(cdddr l)))
               (else
                 (with r (tm-entry-replace (tm-children (cadddr l))
                                           prop val)
                   `(db-resource ,(car l) ,(cadr l) ,(caddr l)
                                 (document ,@r))))))))

(tm-define (db-entry-remove l prop)
  (if (null? l) l
      (with r (db-entry-remove (cdr l) prop)
        (if (and (tm-func? (car l) 'db-entry 2)
                 (tm-equal? (tm-ref (car l) 0) prop))
            r
            (cons (car l) r)))))

(tm-define (db-resource-remove t prop)
  (and (tm-func? t 'db-resource 4)
       (tm-func? (tm-ref t :last) 'document)
       (if (in? prop (list "rid" "type" "name"))
           t
           (with r (db-entry-remove (tm-children (tm-ref t 3)) prop)
             `(db-resource ,(tm-ref t 0) ,(tm-ref t 1) ,(tm-ref t 2)
                           (document ,@r))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bibliography hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bib-types)
  (list "article" "book" "booklet" "inbook" "incollection"
        "inproceedings" "conference" "manual" "mastersthesis" "misc"
        "phdthesis" "proceedings" "techreport" "unpublished"))

(tm-define (db-import-post t)
  (set! t (former t))
  (if (not (tm-equal? (db-resource-ref t "type") "bib-entry")) t
      (with x (db-resource-ref t "bib-type")
        (if (not x) t
            (with type (locase-all (tm->string x))
              (if (nin? type (bib-types)) t
                  (db-resource-set (db-resource-remove t "bib-type")
                                   "type" type)))))))

(tm-define (db-export-pre t)
  (set! t (former t))
  (with x (db-resource-ref t "type")
    (if x (set! x (tm->string x)))
    (if (not (in? x (bib-types))) t
        (db-resource-set (db-resource-set t "bib-type" x)
                         "type" "bib-type"))))
