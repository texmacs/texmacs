
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
  (:use (database db-version)
        (generic generic-edit)
        (generic generic-kbd)
        (utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DRD properties and useful predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group db-entry-tag db-entry db-folded-entry db-pretty-entry)

(define-alternate db-folded-entry db-entry)

(define-group variant-tag (db-entry-tag))
(define-group similar-tag (db-entry-tag))

(tm-define (db-entry? t)
  (and (tm-func? t 'db-entry 5)
       (tm-func? (tm-ref t :last) 'document)))

(tm-define (db-entry-any? t)
  (and (tm-in? t (db-entry-tag-list))
       (== (tm-arity t) 5)
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

(tm-define (db-field-attr f)
  (and (db-field-any? f)
       (tm-atomic? (tm-ref f 0))
       (tm->string (tm-ref f 0))))

(tm-define (db-field-find l attr)
  (and (nnull? l)
       (or (and (db-field-any? (car l))
                (tm-equal? (tm-ref (car l) 0) attr)
                (tm-ref (car l) 1))
           (db-field-find (cdr l) attr))))

(tm-define (db-entry-ref t attr)
  (and (db-entry-any? t)
       (cond ((== attr "id") (tm-ref t 0))
             ((== attr "type") (tm-ref t 1))
             ((== attr "name") (tm-ref t 2))
             (else (db-field-find (tm-children (tm-ref t :last)) attr)))))

(tm-define (db-field-set l attr val)
  (if (null? l) l
      (cons (if (and (db-field-any? (car l))
                     (tm-equal? (tm-ref (car l) 0) attr))
                `(db-field ,attr ,val)
                (car l))
            (db-field-set (cdr l) attr val))))

(tm-define (db-entry-set t attr val)
  (and (db-entry-any? t)
       (let* ((lab (tm-label t))
              (l (tm-children t)))
         (cond ((== attr "id") `(,lab ,val ,@(cdr l)))
               ((== attr "type") `(,lab ,(car l) ,val ,@(cddr l)))
               ((== attr "name") `(,lab ,(car l) ,(cadr l) ,val ,@(cdddr l)))
               (else
                 (with r (db-field-set (tm-children (cAr l)) attr val)
                   (if (not (db-field-find r attr))
                       (set! r (rcons r `(db-field ,attr ,val))))
                   `(,lab ,(car l) ,(cadr l) ,(caddr l) ,(cadddr l)
                          (document ,@r))))))))

(tm-define (db-field-remove l attr)
  (if (null? l) l
      (with r (db-field-remove (cdr l) attr)
        (if (and (db-field-any? (car l))
                 (tm-equal? (tm-ref (car l) 0) attr))
            r
            (cons (car l) r)))))

(tm-define (db-entry-remove t attr)
  (and (db-entry-any? t)
       (if (in? attr (list "id" "type" "name"))
           t
           (let* ((lab (tm-label t))
                  (r (db-field-remove (tm-children (tm-ref t :last)) attr)))
             `(,lab ,(tm-ref t 0) ,(tm-ref t 1)
                    ,(tm-ref t 2) ,(tm-ref t 3)
                    (document ,@r))))))

(tm-define (db-field-rename t a)
  (if (not (and (db-field-any? t) (assoc-ref a (tm-ref t 0))))
      t
      `(db-field ,(assoc-ref a (tm-ref t 0)) ,(tm-ref t 1))))

(tm-define (db-entry-rename t a)
  (if (not (db-entry-any? t))
      t
      (with lab (tm-label t)
        `(,lab ,(tm-ref t 0) ,(tm-ref t 1) ,(tm-ref t 2) ,(tm-ref t 3)
               (document ,@(map (cut db-field-rename <> a)
                                (tm-children (tm-ref t :last))))))))

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

(define (complete-fields l fm opt?)
  (cond ((func? fm 'and)
         (with r (map (cut complete-fields l <> opt?) (cdr fm))
           (apply append r)))
        ((string? fm)
         (with val (or (db-field-find l fm) "")
           (list `(db-field ,fm ,val))))
        ((and (func? fm 'optional 1) (string? (cadr fm)))
         (let* ((val (or (db-field-find l (cadr fm)) ""))
                (tag (if (== val "") 'db-field-optional 'db-field)))
           (if (or opt? (!= val ""))
               (list `(,tag ,(cadr fm) ,val))
               (list))))
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

(tm-define (db-complete-fields t* opt?)
  (and-with t (tree-search-upwards t* db-entry-any?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u :last))))
      (when fm
        (let* ((l (complete-fields c fm opt?))
               (o (list-filter c (cut field-not-among? <> l)))
               (m (append l o)))
          (tree-set (tree-ref t :last) `(document ,@m)))))))

(define (db-complete-field? t)
  (and (db-field-any? t)
       (or (db-field-optional? t)
           (not (tree-empty? (tm->tree (tm-ref t 1)))))))

(tm-define (db-complete-fields? t*)
  (and-with t (tree-search-upwards t* db-entry-any?)
    (let* ((u (tm->stree t))
           (fm (smart-ref db-format-table (tm-ref u 1)))
           (c (tm-children (tm-ref u :last))))
      (and fm
           (not (tree-empty? (tm->tree (tm-ref u 2))))
           (list-and (map db-complete-field? (complete-fields c fm #f)))))))

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
  (and-with res (tree-search-upwards t db-entry-any?)
    (if all?
        (tree-ref t 2)
        (and (> (tree-arity (tree-ref res :last)) 0)
             (with e (tree-ref res :last 0)
               (with f (if (db-field-any? e) e (db-next-field e))
                 (and f (tree-ref f 1))))))))

(define (db-first-empty-field t all?)
  (and-with res (tree-search-upwards t db-entry-any?)
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

(define (get-alternatives l attr)
  (cond ((func? l 'and)
         (list-or (map (cut get-alternatives <> attr) (cdr l))))
        ((func? l 'or)
         (and (in? attr (cdr l)) (cdr l)))
        (else #f)))

(tm-define (db-format-alternatives type attr)
  (set! type (tm->string type))
  (set! attr (tm->string attr))
  (and-with l (smart-ref db-format-table type)
    (get-alternatives l attr)))

(tm-define (db-alternative-fields t)
  (and (db-field-alternative? t)
       (and-with res (tree-search-upwards t db-entry-any?)
         (and-with l (db-format-alternatives (tree-ref res 1) (tree-ref t 0))
           (with r (map (cut db-entry-ref res <>) l)
             (and (list-and r) (map tree-up r)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for dealing with collections of entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-selection-trees t p1 p2)
  (cond ((== p1 p2) (list))
        ((or (<= (length p1) 1) (<= (length p2) 1)) (list t))
        ((and (== (car p1) (car p2)) (tm-ref t (car p1)))
         (get-selection-trees (tm-ref t (car p1)) (cdr p1) (cdr p2)))
        ((< (car p1) (car p2))
         (let* ((i1 (car p1))
                (i2 (car p2))
                (q2 (path-end   (tm-ref t i1) (list)))
                (q1 (path-start (tm-ref t i2) (list)))
                (l1 (get-selection-trees (tm-ref t i1) (cdr p1) q2))
                (l2 (get-selection-trees (tm-ref t i2) q1 (cdr p2)))
                (lm (map (cut tm-ref t <>) (.. (+ i1 1) i2))))
           (append l1 lm l2)))
        (else (list))))

(define (selection-trees)
  (get-selection-trees (path->tree (list))
                       (selection-get-start)
                       (selection-get-end)))

(define (find-db-entries t)
  (cond ((tm-atomic? t) (list))
        ((db-entry-any? t) (list t))
        (else (append-map find-db-entries (tm-children t)))))

(define (selected-entries)
  (and (selection-active-any?)
       (append-map find-db-entries (selection-trees))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating new entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outer-document t)
  (or (and (tree-up t) (outer-document (tree-up t)))
      (and (tree-up t) (tm-func? t 'document) t)))

(tm-define (make-db-entry type)
  (:argument type "Entry type")
  (with doc (outer-document (cursor-tree))
    (when doc
      (let* ((i (tree-index (tree-down doc)))
             (id (with-database (user-database) (db-create-id)))
             (date (number->string (current-time)))
             (res `(db-entry ,id ,type ""
                             (document
                               (db-field "contributor" ,(get-default-user))
                               (db-field "modus" "manual")
                               (db-field "date" ,date))
                             (document))))
        (tree-insert! doc (+ i 1) (list res))
        (tree-go-to doc (+ i 1) 2 :start)
        (db-complete-fields (tree-ref doc (+ i 1)) #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding new fields to existing entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-db-field attr plus)
  (with-innermost t db-entry?
    (let* ((ins `(db-field ,(locase-all attr) ""))
           (pos 0))
      (when (cursor-inside? (tree-ref t :last))
        (set! pos (+ (tree-index (tree-ref t :down :down)) plus)))
      (tree-insert (tree-ref t :last) pos (list ins))
      (tree-go-to t :last pos 1 :start))))

(tm-define (db-field-possible-attributes u)
  (with t (tree-search-upwards u db-entry?)
    (if (not t) (list)
        (let* ((type (db-entry-ref (tm->stree t) "type"))
               (fm (smart-ref db-format-table type)))
          (format->attributes fm)))))

(define (db-this-field-possible-attributes)
  (with-innermost t db-entry?
    (cons "" (db-field-possible-attributes t))))

(tm-define (make-db-field-before attr)
  (:argument attr "Field attribute")
  (:proposals attr (db-this-field-possible-attributes))
  (make-db-field attr 0))
          
(tm-define (make-db-field-after attr)
  (:argument attr "Field attribute")
  (:proposals attr (db-this-field-possible-attributes))
  (make-db-field attr 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing fields from existing entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remove-db-field forwards?)
  (with-innermost t db-field?
    (when (and t (tree-ref t :up :up)
               (tree-is? t :up 'document)
               (db-entry? (tree-ref t :up :up)))
      (let* ((doc (tree-ref t :up))
             (i (tree-index t))
             (n (tree-arity doc))
             (j (if (< (+ i 1) n) (+ i 1) (- i 1))))
        (cond ((and (not forwards?) (> i 0))
               (tree-remove doc (- i 1) 1))
              ((not forwards?)
               (tree-go-to doc :up 2 :end))
              ((and (>= j 0) (db-field? (tree-ref doc j)))
               (tree-go-to doc j 1 :start)
               (tree-remove doc i 1))
              ((>= j 0)
               (tree-go-to doc j :start)
               (tree-remove doc i 1))
              (else
                (tree-go-to doc :up 2 :end)
                (tree-remove doc i 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assigning new identifiers to copied entries before commiting to database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-matches-entry? t as)
  (db-same-entries? (entry->assoc-list (tm->stree t)) as))

(define (detach-entries ts)
  (with h (make-ahash-table)
    (for (t ts)
      (let* ((key (db-entry-ref (tm->stree t) "id"))
             (old (or (ahash-ref h key) (list)))
             (new (rcons old t)))
        (ahash-set! h key new)))
    (for (id (map car (ahash-table->list h)))
      (with l (ahash-ref h id)
        (when (> (length l) 1)
          (with-database (user-database)
            (let* ((old (db-get-entry id))
                   (pred? (cut db-matches-entry? <> old)))
              (receive (l1 l2) (list-partition l pred?)
                (with r (cdr (append l1 l2))
                  (for (t r)
                    (with id (db-create-id)
                      (tree-set (tree-ref t 0) id))))))))))))

(tm-define (detach-buffer-entries)
  (with ts (find-db-entries (buffer-tree))
    (detach-entries ts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep on completing and confirm changes when done
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (confirm-entry t)
  (and (db-entry-any? t) (db-complete-fields? t)
       (with-database (user-database)
         (let* ((old-id (tm->string (db-entry-ref t "id")))
                (old (db-get-entry old-id))
                (new (entry->assoc-list (tm->stree t))))
           (with-extra-fields (list (list "contributor" (get-default-user))
                                    (list "modus" "manual")
                                    (list "origin"))
             (with-time-stamp #t
	       (with new-id (db-update-entry old-id new)
		 (if (== new-id old-id)
		     (begin
		       (set-message "Entry up to date in database"
				    "save entry")
		       :up-to-date)
		     (with new-t (db-load-entry new-id)
		       (tree-set (tree-ref t 0) new-id)
		       (tree-set (tree-ref t 3) (tm-ref new-t 3))
		       (set-message "Saved modifications in database"
				    "save entry")
		       :saved)))))))))

(define (keep-completing t opt?)
  (when (and (not opt?) (db-entry-any? t))
    (tree-go-to t 2 :end)
    (with u (tree-ref t :last)
      (for (i (reverse (.. 0 (tm-arity u))))
        (when (db-field-optional? (tree-ref u i))
          (tree-remove! u i 1)))))
  (cond ((db-first-empty-field t #t)
         (tree-go-to (db-first-empty-field t #t) :end))
        ((not (db-complete-fields? t))
         (and-with res (tree-search-upwards t db-entry-any?)
           (tree-assign-node! res 'db-entry)
           (db-complete-fields res opt?)
           (and-with f (db-first-empty-field res #t)
             (tree-go-to f :end))))
        ((db-url? (current-buffer))
         (and-with res (tree-search-upwards t db-entry-any?)
           (detach-buffer-entries)
           (confirm-entry res)))))

(define (confirm-entries l)
  (detach-buffer-entries)
  (receive (l1 l2) (list-partition l db-complete-fields?)
    (with r (map-in-order confirm-entry l1)
      (set-message (if (in? :saved r)
                       "Saved modifications in database"
                       "Entries up to date in database")
                   "save entries"))
    (when (nnull? l2)
      (keep-completing (car l) #f)
      (set-message "Incomplete entries found to be filled out"
                   "save entries"))
    (null? l2)))

(tm-define (db-confirm-entries-in t)
  (and-with l (find-db-entries t)
    (confirm-entries l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing one or more entries from the database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-entry t)
  (when (db-entry-any? t)
    (with-database (user-database)
      (with id (tm->string (db-entry-ref t "id"))
        (when (nnull? (db-get-entry id))
          (db-remove-entry id)
          (set-message "Removed entry from database" "remove entry"))))))

(define (remove-entries l)
  (for (t l)
    (remove-entry t))
  (when (nnull? l)
    (set-message "Removed entries from database" "remove entries")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The enter key in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
          (else (keep-completing t #t)))))

(tm-define (kbd-enter t shift?)
  (:require (db-field? t))
  (cond ((tree-empty? (tree-ref t 1))
         (set-message "Error: should fill out required field" "db-field"))
        ((db-next-field t)
         (tree-go-to (db-next-field t) 1 :end))
        (else (keep-completing t #t))))

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

(tm-define (kbd-alternate-enter t shift?)
  (:require (db-entry-any? t))
  (keep-completing t #f))

(tm-define (kbd-alternate-enter t shift?)
  (:require (and (in-database?) (pair? (selected-entries))))
  (when (db-url? (current-buffer))
    (confirm-entries (selected-entries))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The delete keys in databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-remove t forwards?)
  (:require (and (db-entry? t) (not (selection-active-any?))))
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
  (:require (and (db-field-any? t) (not (selection-active-any?))))
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

(define (adjust-cursor forwards?)
  (and-with t (cursor-tree)
    (and-with p (tree-up t)
      (with n (tree-arity p)
        (when (and (tm-equal? t "") (tree-is? p 'document) (> n 1))
          (let* ((i (tree-index t))
                 (j (cond ((and forwards? (< i (- n 1))) (+ i 1))
                          (forwards? (- i 1))
                          ((and (not forwards?) (> i 0)) (- i 1))
                          ((not forwards?) (+ i 1)))))
            (if (db-entry-any? (tree-ref p j))
                (tree-go-to p j 2 :end)
                (tree-go-to p j (if (> j i) :start :end)))
            (tree-remove! p i 1)))))))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (db-entry-any? t))
  (remove-entry t)
  (tree-select t)
  (clipboard-cut "nowhere")
  (adjust-cursor forwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (and (in-database?) (pair? (selected-entries))))
  (remove-entries (selected-entries))
  (clipboard-cut "nowhere")
  (adjust-cursor forwards?))

(kbd-map
  (:mode in-database?)
  ("A-delete" (structured-remove-right))
  ("A-backspace" (structured-remove-left)))
