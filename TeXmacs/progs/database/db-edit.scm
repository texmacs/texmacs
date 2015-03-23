
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
  (:use (database db-format)
        (generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-entry? t)
  (and (tm-func? t 'db-entry 5)
       (tm-func? (tm-ref t :last) 'document)))

(tm-define (db-field? t) (tm-func? t 'db-field 2))
(tm-define (db-field-optional? t) (tm-func? t 'db-field-optional 2))
(tm-define (db-field-alternative? t) (tm-func? t 'db-field-alternative 2))

(tm-define (db-field-any? t)
  (or (tm-func? t 'db-field 2)
      (tm-func? t 'db-field-optional 2)
      (tm-func? t 'db-field-alternative 2)))

(tm-define (inside-db-field? which)
  (and-with t (tree-search-upwards (cursor-tree) db-field-any?)
    (and (tm-equal? (tm-ref t 0) which)
         (cursor-inside? (tm-ref t 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for database markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-field-find l prop)
  (and (nnull? l)
       (or (and (db-field-any? (car l))
                (tm-equal? (tm-ref (car l) 0) prop)
                (tm-ref (car l) 1))
           (db-field-find (cdr l) prop))))

(tm-define (db-entry-ref t prop)
  (and (db-entry? t)
       (cond ((== prop "id") (tm-ref t 0))
             ((== prop "type") (tm-ref t 1))
             ((== prop "name") (tm-ref t 2))
             (else (db-field-find (tm-children (tm-ref t :last)) prop)))))

(tm-define (db-field-set l prop val)
  (if (null? l) l
      (cons (if (and (db-field-any? (car l))
                     (tm-equal? (tm-ref (car l) 0) prop))
                `(db-field ,prop ,val)
                (car l))
            (db-field-set (cdr l) prop val))))

(tm-define (db-entry-set t prop val)
  (and (db-entry? t)
       (with l (tm-children t)
         (cond ((== prop "id") `(db-entry ,val ,@(cdr l)))
               ((== prop "type") `(db-entry ,(car l) ,val ,@(cddr l)))
               ((== prop "name")
                `(db-entry ,(car l) ,(cadr l) ,val ,@(cdddr l)))
               (else
                 (with r (db-field-set (tm-children (cAr l)) prop val)
                   (if (not (db-field-find r prop))
                       (set! r (rcons r `(db-field ,prop ,val))))
                   `(db-entry ,(car l) ,(cadr l) ,(caddr l) ,(cadddr l)
                              (document ,@r))))))))

(tm-define (db-field-remove l prop)
  (if (null? l) l
      (with r (db-field-remove (cdr l) prop)
        (if (and (db-field-any? (car l))
                 (tm-equal? (tm-ref (car l) 0) prop))
            r
            (cons (car l) r)))))

(tm-define (db-entry-remove t prop)
  (and (db-entry? t)
       (if (in? prop (list "id" "type" "name"))
           t
           (with r (db-field-remove (tm-children (tm-ref t :last)) prop)
             `(db-entry ,(tm-ref t 0) ,(tm-ref t 1)
                        ,(tm-ref t 2) ,(tm-ref t 3)
                        (document ,@r))))))

(tm-define (db-field-rename t a)
  (if (not (and (db-field-any? t) (assoc-ref a (tm-ref t 0))))
      t
      `(db-field ,(assoc-ref a (tm-ref t 0)) ,(tm-ref t 1))))

(tm-define (db-entry-rename t a)
  (if (not (db-entry? t))
      t
      `(db-entry ,(tm-ref t 0) ,(tm-ref t 1) ,(tm-ref t 2) ,(tm-ref t 3)
                 (document ,@(map (cut db-field-rename <> a)
                                  (tm-children (tm-ref t :last)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Completing the fields of an entry with empty fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (complete-fields-or l alt all)
  (cond ((null? alt)
         (with make (lambda (x) `(db-field-alternative ,x ""))
           (map make all)))
        ((nin? (db-field-find l (car alt)) (list "" #f))
         (with val (db-field-find l (car alt))
           (list `(db-field ,(car alt) ,val))))
        (else (complete-fields-or l (cdr alt) all))))

(define (complete-fields l fm)
  (cond ((func? fm 'and)
         (with r (map (cut complete-fields l <>) (cdr fm))
           (apply append r)))
        ((string? fm)
         (with val (or (db-field-find l fm) "")
           (list `(db-field ,fm ,val))))
        ((and (func? fm 'optional 1) (string? (cadr fm)))
         (let* ((val (or (db-field-find l (cadr fm)) ""))
                (tag (if (== val "") 'db-field-optional 'db-field)))
           (list `(,tag ,(cadr fm) ,val))))
        ((and (func? fm 'or) (nnull? (cdr fm))
              (list-and (map string? (cdr fm))))
         (complete-fields-or l (cdr fm) (cdr fm)))
        (else (list))))

(define (field-not-among? f l)
  (and (db-field-any? f)
       (or (null? l)
           (and (field-not-among? f (cdr l))
                (or (not (db-field-any? (car l)))
                    (!= (tm-ref f 0) (tm-ref (car l) 0)))))))

(tm-define (db-complete-fields t*)
  (and-with t (tree-search-upwards t* db-entry?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u :last))))
      (when fm
        (let* ((l (complete-fields c fm))
               (o (list-filter c (cut field-not-among? <> l)))
               (m (append l o)))
          (tree-set (tree-ref t :last) `(document ,@m)))))))

(define (db-complete-field? t)
  (and (db-field-any? t)
       (or (db-field-optional? t)
           (not (tree-empty? (tm->tree (tm-ref t 1)))))))

(tm-define (db-complete-fields? t*)
  (and-with t (tree-search-upwards t* db-entry?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u :last))))
      (and fm
           (not (tree-empty? (tm->tree (tm-ref u 2))))
           (list-and (map db-complete-field? (complete-fields c fm)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding subsequent fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-next-field t)
  (with u (tm-ref t :next)
    (and u (or (and (db-field-any? u) u)
               (db-next-field u)))))

(define (db-next-empty-field t)
  (with u (tm-ref t :next)
    (and u (or (and (db-field-any? u) (tree-empty? (tree-ref u 1)) u)
               (db-next-empty-field u)))))

(define (db-previous-field t)
  (with u (tm-ref t :previous)
    (and u (or (and (db-field-any? u) u)
               (db-previous-field u)))))

(define (db-previous-empty-field t)
  (with u (tm-ref t :previous)
    (and u (or (and (db-field-any? u) (tree-empty? (tree-ref u 1)) u)
               (db-previous-empty-field u)))))

(define (db-first-field t all?)
  (and-with res (tree-search-upwards t db-entry?)
    (if all?
        (tree-ref t 2)
        (and (> (tree-arity (tree-ref res :last)) 0)
             (with e (tree-ref res :last 0)
               (with f (if (db-field-any? e) e (db-next-field e))
                 (and f (tree-ref f 1))))))))

(define (db-first-empty-field t all?)
  (and-with res (tree-search-upwards t db-entry?)
    (cond ((and all? (tree-empty? (tree-ref res 2)))
           (tree-ref res 2))
          ((> (tree-arity (tree-ref res :last)) 0)
           (with e (tree-ref res :last 0)
             (with f (if (and (db-field-any? e)
                              (tree-empty? (tree-ref e 1)))
                         e (db-next-empty-field e))
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
  (and (db-field-alternative? t)
       (and-with res (tree-search-upwards t db-entry?)
         (and-with l (db-format-alternatives (tree-ref res 1) (tree-ref t 0))
           (with r (map (cut db-entry-ref res <>) l)
             (and (list-and r) (map tree-up r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating new fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outer-document t)
  (or (and (tree-up t) (outer-document (tree-up t)))
      (and (tree-up t) (tm-func? t 'document) t)))

(tm-define (db-create-field type)
  (with doc (outer-document (cursor-tree))
    (when doc
      (with i (tree-index (tree-down doc))
        (with res `(db-entry ,(create-unique-id) ,type "" (document))
          (tree-insert! doc (+ i 1) (list res))
          (tree-go-to doc (+ i 1) 2 :start)
          (db-complete-fields (tree-ref doc (+ i 1))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The enter key in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (keep-completing t)
  (cond ((db-first-empty-field t #t)
         (tree-go-to (db-first-empty-field t #t) :end))
        ((not (db-complete-fields? t))
         (and-with res (tree-search-upwards t db-entry?)
           (db-complete-fields res)
           (and-with f (db-first-empty-field res #t)
             (tree-go-to f :end))))
        (else (display* "Resource complete!\n"))))

(tm-define (kbd-enter t shift?)
  (:require (db-entry? t))
  (with u (tree-ref t :down)
    (cond ((tree-search-upwards t 'inactive)
           (former t shift?))
          ((and u (= (tree-index u) 2))
           (if (tree-empty? u)
               (set-message "Error: should fill out name for referencing"
                            "db-entry")
               (with d (tree-ref u :up :last)
                 (when (and (> (tm-arity d) 0) (db-field-any? (tree-ref d 0)))
                   (tree-go-to d 0 1 :end)))))
          (else (keep-completing t)))))

(tm-define (kbd-enter t shift?)
  (:require (db-field? t))
  (cond ((tree-empty? (tree-ref t 1))
         (set-message "Error: should fill out required field" "db-field"))
        ((db-next-field t)
         (tree-go-to (db-next-field t) 1 :end))
        (else (keep-completing t))))

(tm-define (kbd-enter t shift?)
  (:require (db-field-optional? t))
  (if (tree-empty? (tree-ref t 1))
      (let* ((u t)
             (next (db-next-field t))
             (prev (db-previous-field t)))
        (when (or next prev)
          (tree-go-to (or next prev) 1 :end)
          (tree-remove (tree-ref u :up) (tree-index u) 1)))
      (with next (db-next-field t)
        (tree-assign-node t 'db-field)
        (if next (tree-go-to next 1 :end)))))

(tm-define (kbd-enter t shift?)
  (:require (db-field-alternative? t))
  (let* ((alts (or (db-alternative-fields t) (list)))
         (pred? (lambda (f) (not (tree-empty? (tree-ref f 1)))))
         (ok (list-filter alts pred?)))
    (cond ((null? ok)
           (set-message "Error: one of the alternatives should be filled out"
                        "db-field-alternative"))
          ((> (length ok) 1)
           (set-message "Error: only one alternatives should be filled out"
                        "db-field-alternative"))
          (else
            (with a (car ok)
              (tree-go-to a 1 :end)
              (tree-assign-node! a 'db-field)
              (for (f alts)
                (when (db-field-alternative? f)
                  (tree-remove (tree-ref f :up) (tree-index f) 1)))
              (with next (db-next-field a)
                (if next (tree-go-to next 1 :end))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The delete keys in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-remove t forwards?)
  (:require (db-entry? t))
  (cond ((and (cursor-inside? (tree-ref t 2))
              (tree-empty? (tree-ref t 2))
              (tree-empty? (tree-ref t :last)))
         (tree-cut t))
        ((and (tree-at-start? (tree-ref t 2)) (not forwards?))
         (tree-go-to t :start))
        ((and (tree-at-end? (tree-ref t 2)) forwards?)
         (with f (db-first-field t #f)
           (if f (tree-go-to f :start) (tree-go-to t :end))))
        ((cursor-inside? (tree-ref t 2))
         (former t forwards?))
        ((and (tree-at-start? (tree-ref t :last)) (not forwards?))
         (tree-go-to t 2 :end))
        ((and (not (db-field-any? (cursor-tree)))
              (with-innermost u db-field-any? u))
         (former t forwards?))
        ((not forwards?) (kbd-left))
        (forwards? (kbd-right))))

(tm-define (kbd-remove t forwards?)
  (:require (db-field-any? t))
  (cond ((and (cursor-inside? (tree-ref t 1)) (tree-empty? (tree-ref t 1)))
         (let* ((next (if forwards? (db-next-field t) (db-previous-field t)))
                (res (tree-search-upwards t 'db-entry)))
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
