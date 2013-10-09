
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-style.scm
;; DESCRIPTION : management of global document style
;; COPYRIGHT   : (C) 2001--2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-style))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relations between style packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-category p) p)
(tm-define (style-category-overrides? p q) (== p q))
(tm-define (style-category-precedes? p q) #f)

(tm-define (style-includes? p q) #f)

(tm-define (style-overrides? p q)
  (style-category-overrides? (style-category p) (style-category q)))

(tm-define (style-precedes? p q)
  (style-category-precedes? (style-category p) (style-category q)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting and setting the list of style packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-style-list)
  (with t (tree->stree (get-style-tree))
    (cond ((string? t) (list t))
          ((and (pair? t) (== (car t) 'tuple)) (cdr t))
          (else (texmacs-error "get-style-list ""invalid style ~S" t)))))

(define (normalize-style-list* l)
  (cond ((null? l) l)
        ((list-find (cdr l) (cut style-overrides? <> (car l)))
         (normalize-style-list* (cdr l)))
        ((list-find (cdr l) (cut style-precedes? <> (car l)))
         (normalize-style-list* (cons (cadr l) (cons (car l) (cddr l)))))
        (else (cons (car l) (normalize-style-list* (cdr l))))))

(define (normalize-style-list** l before)
  (cond ((null? l) l)
        ((list-find before (cut style-includes? <> (car l)))
         (normalize-style-list** (cdr l) (cons (car l) before)))
        (else (cons (car l) (normalize-style-list** (cdr l)
                                                    (cons (car l) before))))))

(define (normalize-style-list l2)
  (with l (list-remove-duplicates l2)
    (if (null? l) l
        (cons (car l)
              (normalize-style-list** (normalize-style-list* (cdr l))
                                      (list (car l)))))))

(tm-define (set-style-list l)
  (set-style-tree (tm->tree `(tuple ,@(normalize-style-list l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level routines for style and style package management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (has-no-style?)
  (null? (get-style-list)))

(tm-define (set-no-style)
  (:check-mark "v" has-no-style?)
  (set-style-list '()))

(tm-define (has-main-style? style)
  (with l (get-style-list)
    (and (nnull? l) (== (car l) style))))

(tm-define (set-main-style style)
  (:argument style "Main document style")
  (:default  style "generic")
  (:check-mark "v" has-main-style?)
  (let* ((old (get-style-list))
         (new (if (null? old) (list style) (cons style (cdr old)))))
    (set-style-list new)))

(tm-define (has-style-package? pack)
  (or (in? pack (get-style-list))
      (and (list-find (get-style-list) (cut style-includes? <> pack))
           (not (list-find (get-style-list) (cut style-overrides? <> pack))))))

(tm-define (add-style-package pack)
  (:argument pack "Add package")
  (:check-mark "v" has-style-package?)
  (set-style-list (append (get-style-list) (list pack))))

(tm-define (remove-style-package pack)
  (:argument pack "Remove package")
  (:proposals pack (with l (get-style-list) (if (null? l) l (cdr l))))
  (set-style-list (list-difference (get-style-list) (list pack))))

(tm-define (toggle-style-package pack)
  (:argument pack "Toggle package")
  (:check-mark "v" has-style-package?)
  (if (has-style-package? pack)
      (remove-style-package pack)
      (add-style-package pack)))
