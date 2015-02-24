
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-edit.scm
;; DESCRIPTION : editing TeXmacs databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-edit)
  (:use (database db-resource)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format for each type of resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-format
  ("user" (and "full-name" "email")))

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
                 (with r (db-entry-replace (tm-children (cadddr l))
                                           prop val)
                   (if (not (db-entry-find r prop))
                       (set! r (rcons r `(db-entry ,prop ,val))))
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
