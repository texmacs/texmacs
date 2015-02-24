
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
  (:use (database db-resource)
        (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-resource? t)
  (and (tm-func? t 'db-resource 4)
       (tm-func? (tm-ref t 3) 'document)))

(tm-define (db-entry? t) (tm-func? t 'db-entry 2))
(tm-define (db-entry-optional? t) (tm-func? t 'db-entry-optional 2))
(tm-define (db-entry-alternative? t) (tm-func? t 'db-entry-alternative 2))

(tm-define (db-entry-any? t)
  (or (tm-func? t 'db-entry 2)
      (tm-func? t 'db-entry-optional 2)
      (tm-func? t 'db-entry-alternative 2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for database markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-entry-find l prop)
  (and (nnull? l)
       (or (and (db-entry-any? (car l))
                (tm-equal? (tm-ref (car l) 0) prop)
                (tm-ref (car l) 1))
           (db-entry-find (cdr l) prop))))

(tm-define (db-resource-ref t prop)
  (and (db-resource? t)
       (cond ((== prop "rid") (tm-ref t 0))
             ((== prop "type") (tm-ref t 1))
             ((== prop "name") (tm-ref t 2))
             (else (db-entry-find (tm-children (tm-ref t 3)) prop)))))

(tm-define (db-entry-set l prop val)
  (if (null? l) l
      (cons (if (and (db-entry-any? (car l))
                     (tm-equal? (tm-ref (car l) 0) prop))
                `(db-entry ,prop ,val)
                (car l))
            (db-entry-set (cdr l) prop val))))

(tm-define (db-resource-set t prop val)
  (and (db-resource? t)
       (with l (tm-children t)
         (cond ((== prop "rid") `(db-resource ,val ,@(cdr l)))
               ((== prop "type") `(db-resource ,(car l) ,val ,@(cddr l)))
               ((== prop "name")
                `(db-resource ,(car l) ,(cadr l) ,val ,@(cdddr l)))
               (else
                 (with r (db-entry-set (tm-children (cadddr l)) prop val)
                   (if (not (db-entry-find r prop))
                       (set! r (rcons r `(db-entry ,prop ,val))))
                   `(db-resource ,(car l) ,(cadr l) ,(caddr l)
                                 (document ,@r))))))))

(tm-define (db-entry-remove l prop)
  (if (null? l) l
      (with r (db-entry-remove (cdr l) prop)
        (if (and (db-entry-any? (car l))
                 (tm-equal? (tm-ref (car l) 0) prop))
            r
            (cons (car l) r)))))

(tm-define (db-resource-remove t prop)
  (and (db-resource? t)
       (if (in? prop (list "rid" "type" "name"))
           t
           (with r (db-entry-remove (tm-children (tm-ref t 3)) prop)
             `(db-resource ,(tm-ref t 0) ,(tm-ref t 1) ,(tm-ref t 2)
                           (document ,@r))))))

(tm-define (db-entry-rename t a)
  (if (not (and (db-entry-any? t) (assoc-ref a (tm-ref t 0))))
      t
      `(db-entry ,(assoc-ref a (tm-ref t 0)) ,(tm-ref t 1))))

(tm-define (db-resource-rename t a)
  (if (not (db-resource? t))
      t
      `(db-resource ,(tm-ref t 0) ,(tm-ref t 1) ,(tm-ref t 2)
                    (document ,@(map (cut db-entry-rename <> a)
                                     (tm-children (tm-ref t 3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Format for each type of resource
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-format-table
  ("user" (and "full-name" "email")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completing the entries of a resource with empty fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (complete-entries-or l alt all)
  (cond ((null? alt)
         (with make (lambda (x) `(db-entry-alternative ,x ""))
           (map make all)))
        ((nin? (db-entry-find l (car alt)) (list "" #f))
         (with val (db-entry-find l (car alt))
           (list `(db-entry ,(car alt) ,val))))
        (else (complete-entries-or l (cdr alt) all))))

(define (complete-entries l fm)
  (cond ((func? fm 'and)
         (with r (map (cut complete-entries l <>) (cdr fm))
           (apply append r)))
        ((string? fm)
         (with val (or (db-entry-find l fm) "")
           (list `(db-entry ,fm ,val))))
        ((and (func? fm 'optional 1) (string? (cadr fm)))
         (let* ((val (or (db-entry-find l (cadr fm)) ""))
                (tag (if (== val "") 'db-entry-optional 'db-entry)))
           (list `(,tag ,(cadr fm) ,val))))
        ((and (func? fm 'or) (nnull? (cdr fm))
              (list-and (map string? (cdr fm))))
         (complete-entries-or l (cdr fm) (cdr fm)))
        (else (list))))

(tm-define (db-complete-entries t)
  (when (db-resource? t)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u 3))))
      (when fm
        (with l (complete-entries c fm)
          (tree-set (tree-ref t 3) `(document ,@l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filling out entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-enter t shift?)
  (:require (db-resource? t))
  (db-complete-entries t))

(define (db-next-entry t)
  (with u (tm-ref t :next)
    (and u (or (and (db-entry-any? u) u) (db-next-entry u)))))

(define (db-previous-entry t)
  (with u (tm-ref t :previous)
    (and u (or (and (db-entry-any? u) u) (db-previous-entry u)))))

(tm-define (kbd-enter t shift?)
  (:require (db-entry? t))
  (if (tree-empty? (tree-ref t 1))
      (set-message "Error: should fill out required field" "db-entry")
      (with next (db-next-entry t)
        (if next (tree-go-to next 1 :end)))))

(tm-define (kbd-enter t shift?)
  (:require (db-entry-optional? t))
  (if (tree-empty? (tree-ref t 1))
      (let* ((u t)
             (next (db-next-entry t))
             (prev (db-previous-entry t)))
        (when (or next prev)
          (tree-go-to (or next prev) 1 :end)
          (tree-remove (tree-ref u :up) (tree-index u) 1)))
      (with next (db-next-entry t)
        (tree-assign-node t 'db-entry)
        (if next (tree-go-to next 1 :end)))))
