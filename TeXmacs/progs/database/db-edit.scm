
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
;; Database kinds and formats of resources
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-kind-table)
(smart-table db-format-table)

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

(define (entry-not-among? e l)
  (and (db-entry-any? e)
       (or (null? l)
           (and (entry-not-among? e (cdr l))
                (or (not (db-entry-any? (car l)))
                    (!= (tm-ref e 0) (tm-ref (car l) 0)))))))

(tm-define (db-complete-entries t*)
  (and-with t (tree-search-upwards t* db-resource?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u 3))))
      (when fm
        (let* ((l (complete-entries c fm))
               (o (list-filter c (cut entry-not-among? <> l)))
               (m (append l o)))
          (tree-set (tree-ref t 3) `(document ,@m)))))))

(define (db-complete-entry? t)
  (and (db-entry-any? t)
       (or (db-entry-optional? t)
           (not (tree-empty? (tm->tree (tm-ref t 1)))))))

(tm-define (db-complete-entries? t*)
  (and-with t (tree-search-upwards t* db-resource?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u 3))))
      (and fm
           (not (tree-empty? (tm->tree (tm-ref u 2))))
           (list-and (map db-complete-entry? (complete-entries c fm)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding subsequent entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-next-entry t)
  (with u (tm-ref t :next)
    (and u (or (and (db-entry-any? u) u)
               (db-next-entry u)))))

(define (db-next-empty-entry t)
  (with u (tm-ref t :next)
    (and u (or (and (db-entry-any? u) (tree-empty? (tree-ref u 1)) u)
               (db-next-empty-entry u)))))

(define (db-previous-entry t)
  (with u (tm-ref t :previous)
    (and u (or (and (db-entry-any? u) u)
               (db-previous-entry u)))))

(define (db-previous-empty-entry t)
  (with u (tm-ref t :previous)
    (and u (or (and (db-entry-any? u) (tree-empty? (tree-ref u 1)) u)
               (db-previous-empty-entry u)))))

(define (db-first-field t all?)
  (and-with res (tree-search-upwards t db-resource?)
    (if all?
        (tree-ref t 2)
        (and (> (tree-arity (tree-ref res 3)) 0)
             (with e (tree-ref res 3 0)
               (with f (if (db-entry-any? e) e (db-next-entry e))
                 (and f (tree-ref f 1))))))))

(define (db-first-empty-field t all?)
  (and-with res (tree-search-upwards t db-resource?)
    (cond ((and all? (tree-empty? (tree-ref res 2)))
           (tree-ref res 2))
          ((> (tree-arity (tree-ref res 3)) 0)
           (with e (tree-ref res 3 0)
             (with f (if (and (db-entry-any? e)
                              (tree-empty? (tree-ref e 1)))
                         e (db-next-empty-entry e))
               (and f (tree-ref f 1)))))
          (else #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handling of alternative fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-alternatives l prop)
  (cond ((func? l 'and)
         (list-or (map (cut get-alternatives <> prop) (cdr l))))
        ((func? l 'or)
         (and (in? prop (cdr l)) (cdr l)))
        (else #f)))

(tm-define (db-format-alternatives type prop)
  (set! type (tm->string type))
  (set! prop (tm->string prop))
  (and-with l (smart-ref db-format-table type)
    (get-alternatives l prop)))

(tm-define (db-alternative-fields t)
  (and (db-entry-alternative? t)
       (and-with res (tree-search-upwards t db-resource?)
         (and-with l (db-format-alternatives (tree-ref res 1) (tree-ref t 0))
           (with r (map (cut db-resource-ref res <>) l)
             (and (list-and r) (map tree-up r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating new entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outer-document t)
  (or (and (tree-up t) (outer-document (tree-up t)))
      (and (tree-up t) (tm-func? t 'document) t)))

(tm-define (db-create-entry type)
  (with doc (outer-document (cursor-tree))
    (when doc
      (with i (tree-index (tree-down doc))
        (with res `(db-resource ,(create-unique-id) ,type "" (document))
          (tree-insert! doc (+ i 1) (list res))
          (tree-go-to doc (+ i 1) 2 :start)
          (db-complete-entries (tree-ref doc (+ i 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The enter key in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep-completing t)
  (cond ((db-first-empty-field t #t)
         (tree-go-to (db-first-empty-field t #t) :end))
        ((not (db-complete-entries? t))
         (and-with res (tree-search-upwards t db-resource?)
           (db-complete-entries res)
           (and-with e (db-first-empty-field res #t)
             (tree-go-to e :end))))
        (else (display* "Resource complete!\n"))))

(tm-define (kbd-enter t shift?)
  (:require (db-resource? t))
  (with u (tree-ref t :down)
    (cond ((tree-search-upwards t 'inactive)
           (former t shift?))
          ((and u (= (tree-index u) 2))
           (if (tree-empty? u)
               (set-message "Error: should fill out name for referencing"
                            "db-resource")
               (with d (tree-ref u :up 3)
                 (when (and (> (tm-arity d) 0) (db-entry-any? (tree-ref d 0)))
                   (tree-go-to d 0 1 :end)))))
          (else (keep-completing t)))))

(tm-define (kbd-enter t shift?)
  (:require (db-entry? t))
  (cond ((tree-empty? (tree-ref t 1))
         (set-message "Error: should fill out required field" "db-entry"))
        ((db-next-entry t)
         (tree-go-to (db-next-entry t) 1 :end))
        (else (keep-completing t))))

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

(tm-define (kbd-enter t shift?)
  (:require (db-entry-alternative? t))
  (let* ((alts (or (db-alternative-fields t) (list)))
         (pred? (lambda (e) (not (tree-empty? (tree-ref e 1)))))
         (ok (list-filter alts pred?)))
    (cond ((null? ok)
           (set-message "Error: one of the alternatives should be filled out"
                        "db-entry-alternative"))
          ((> (length ok) 1)
           (set-message "Error: only one alternatives should be filled out"
                        "db-entry-alternative"))
          (else
            (with a (car ok)
              (tree-go-to a 1 :end)
              (tree-assign-node! a 'db-entry)
              (for (e alts)
                (when (db-entry-alternative? e)
                  (tree-remove (tree-ref e :up) (tree-index e) 1)))
              (with next (db-next-entry a)
                (if next (tree-go-to next 1 :end))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The delete keys in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-remove t forwards?)
  (:require (db-resource? t))
  (cond ((and (cursor-inside? (tree-ref t 2))
              (tree-empty? (tree-ref t 2))
              (tree-empty? (tree-ref t 3)))
         (tree-cut t))
        ((and (tree-at-start? (tree-ref t 2)) (not forwards?))
         (tree-go-to t :start))
        ((and (tree-at-end? (tree-ref t 2)) forwards?)
         (with f (db-first-field t #f)
           (if f (tree-go-to f :start) (tree-go-to t :end))))
        ((cursor-inside? (tree-ref t 2))
         (former t forwards?))
        ((and (not (db-entry-any? (cursor-tree)))
              (with-innermost u db-entry-any? u))
         (former t forwards?))
        ((not forwards?) (kbd-left))
        (forwards? (kbd-right))))

(tm-define (kbd-remove t forwards?)
  (:require (db-entry-any? t))
  (cond ((and (cursor-inside? (tree-ref t 1)) (tree-empty? (tree-ref t 1)))
         (let* ((next (if forwards? (db-next-entry t) (db-previous-entry t)))
                (res (tree-search-upwards t 'db-resource)))
           (cond (next
                  (tree-go-to next 1 (if forwards? :start :end)))
                 ((and (not forwards?) res)
                  (tree-go-to res 2 :end))
                 ((and forwards? res)
                  (tree-go-to res 1)))
           (tree-remove (tree-up t) (tree-index t) 1)))
        ((and (tree-at-start? (tree-ref t 1)) (not forwards?))
         (tree-go-to t :start))
        ((and (tree-at-end? (tree-ref t 1)) forwards?)
         (tree-go-to t :end))
        (else (former t forwards?))))

