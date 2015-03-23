
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
  (db-entry-rename t (list (cons "mid" "id")
                           (cons "mtype" "type")
                           (cons "mname" "name"))))

(tm-define (db-save-pre t)
  (db-entry-rename t (list (cons "id" "mid")
                           (cons "type" "mtype")
                           (cons "name" "mname"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Loading databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-load-fields l)
  ;; TODO: take into account string encodings
  (cond ((null? l) l)
        ((or (nlist? (car l)) (<= (length (car l)) 1))
         (db-load-fields (cdr l)))
        (else (cons `(db-field ,(caar l) ,(cadar l))
                    (db-load-fields (cons (cons (caar l) (cddar l))
                                          (cdr l)))))))

(tm-define (db-load-entry id)
  (let* ((l (db-get-all-decoded id))
         (type (assoc-ref l "type"))
         (name (assoc-ref l "name")))
    ;;(display* "Load " id " -> " l "\n")
    (set! type (if (pair? type) (car type) "?"))
    (set! name (if (pair? name) (car name) "?"))
    (set! l (assoc-remove! l "type"))
    (set! l (assoc-remove! l "name"))
    (set! l (db-load-fields l))
    (db-load-post `(db-entry ,id ,type ,name (document) (document ,@l)))))

(tm-define (db-load-type type)
  (let* ((l (db-search (list (list "type" type))))
         (i (map db-load-entry l)))
    `(document ,@i)))

(tm-define (db-load-types types)
  (with l (map cdr (map db-load-type types))
    `(document ,@(apply append l))))

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

(tm-define (db-save-selected-entry t pred?)
  (set! t (db-save-pre t))
  (when (and (db-entry? t) (pred? (tm-ref t 1)))
    (let* ((id (tm-ref t 0))
           (type (tm-ref t 1))
           (name (tm-ref t 2))
           (pairs (append-map db-save-field (tm-children (tm-ref t :last))))
           (all (cons* (list "type" type) (list "name" name) pairs))
           (prec (db-search (list (list "name" name)))))
      (if (and prec (nnull? prec))
          ;; TODO: entries might need to be updated
          (when db-duplicate-warning?
            (display* "Existing entry " name "\n"))
          (begin
            ;;(display* "Save " id " -> " all "\n")
            (db-set-field id "type" (list type))
            (db-set-all-encoded id all))))))

(tm-define (db-save-selected t pred?)
  (cond ((tm-func? t 'document)
         (for-each (cut db-save-selected <> pred?) (tm-children t)))
        ((or (tm-func? t 'db-entry 5) (tm-func? t 'bib-entry 3))
         (db-save-selected-entry (tm->stree t) pred?))
        ((and (tree? t) (tm-compound? t))
         (for-each (cut db-save-selected <> pred?)
                   (tree-accessible-children t)))))

(tm-define (db-save-types t types)
  (db-save-selected t (lambda (x) (in? x types))))

(tm-define (db-save t)
  (db-save-selected t (lambda (x) #t)))
