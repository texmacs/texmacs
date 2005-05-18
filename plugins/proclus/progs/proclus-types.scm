
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: e7837b86-4c46-410f-9370-e143ec1b3b1e
;;
;; MODULE      : proclus-types.scm
;; DESCRIPTION : Proclus link types
;; COPYRIGHT   : (C) 2003--2004  Alain Herreman, David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (proclus-types)
  (:use (proclus) ;; FIXME: circular dependence
        (proclus-list)
        (proclus-lib)
        (proclus-source)
	(proclus-locus)
	(ice-9 common-list))) ;; uniq et set-difference.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level access to type lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-types)
  (source-buffer-excursion
   (if (style-has? "proclus-type-list")
       (tuple->list (get-init-tree "proclus-type-list"))
       '())))

(tm-define (active-types)
  (source-buffer-excursion
   (if (style-has? "proclus-active-types")
       (tuple->list (get-init-tree "proclus-active-types"))
       (begin (set-active-types (list-types))
              (list-types)))))

(define (set-types types)
  (source-buffer-excursion
   (init-env-tree "proclus-type-list" (list->tuple types))))

(define (set-active-types types)
  (source-buffer-excursion
   (init-env-tree "proclus-active-types" (list->tuple types))))

(define (types-tree)
  (transform locus? (tree->stree (the-buffer))))

(define (type? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'locus)))

;;adds the list of  types ltypes to the  type list of the current doc.
(define (merge-types types)
  (set-types (uniq (append (list-types) types))))

;;adds the list of  types ltypes to the active  types list of the current doc.
(define (merge-active-types types)
  (set-active-types (uniq (append (active-types) types))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level type commands and menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deactivate-type type)
  (set-active-types
   (list-filter (active-types) (lambda (x) (!= x type)))))

(define (activate-type type)
  (deactivate-type type) ;; avoid duplicate entries
  (set-active-types (cons type (active-types))))

(define (activate-all-types)
  (set-active-types (list-types)))

(define (deactivate-all-types)
  (set-active-types '()))

(define (activate-negative-types)
  (let ((active (active-types)))
    (set-active-types (list-filter (list-types)
                                   (lambda (x) (not (in? x active)))))))


(define (type-active? type)
  (in? type (active-types)))

;; Caching to save on buffer switching when computing check marks

(define active-cache #f)

(define (clear-active-types-cache) (set! active-cache #f))

(define (type-active-cached? type)
  (if (not active-cache)
      (set! active-cache (active-types)))
  (in? type active-cache))

(tm-define (toggle-active-type type)
  (:check-mark "v" type-active-cached?)
  (if (type-active? type)
      (deactivate-type type)
      (activate-type type)))

(tm-define (type-menu-promise)
  ;; FIXME: texmacs dynamic menus are dead buggy. Joris assumed that macros
  ;; shall not be memoized and decided that all menu computation work should be
  ;; done in macros :-( Luckily, there _is_ a temporary workaround.
  (source-buffer-excursion
   (clear-active-types-cache)
   (menu-dynamic
     (-> "Types"
	 ("Import" (import-types))
	 ("Add" (new-types))
	 ("Remove" (delete-types))
	 ---
         ("Activate all" (activate-all-types))
         ("Disactivate all" (deactivate-all-types))
         ("Invert" (activate-negative-types))
         ---
         ,@(let ((types (list-types)))
             (if (null? types)
                 (menu-dynamic (when #f ("No type" (noop))))
                 (map type-menu-item types)))))))

(define (type-menu-item type)
  (eval (cons 'quasiquote
              (list (menu-pre `(,type (toggle-active-type ,type)))))))

(tm-define (import-types)
  (let ((from (get-strg-name-buffer)))
    (choose-file "Import types" "texmacs"
                 `(lambda (x) (import-types/sub x ,from)))))

(tm-define (import-types/sub u from)
  (switch-to-active-buffer u)
  (let ((imp-types (list-types)))
    (switch-to-active-buffer from)
    (merge-types imp-types)
    (merge-active-types imp-types)))

(tm-define list-types-tmp '())

(define (new-types)
  (set! list-types-tmp '())
  (new-types/sub))

(define (new-types/sub)
  (new-types/sub2 "Add type:"))

(define (new-types/sub2 msg)
  (interactive (list msg)
	       '(lambda(s)
		  (if (string-null? s)
		      (new-types-rec)
		      (begin (set-cons! list-types-tmp s)
			     (new-types/sub))))))

;;adds list-types-tmp to the list of types. New types are active.
(tm-define (new-types-rec)
  (merge-types list-types-tmp)
  (merge-active-types list-types-tmp))

(define (delete-types)
  (set! list-types-tmp '())
  (delete-types/sub))

(tm-define (delete-types/sub)
  (delete-types/sub2 "Remove type:"))

(define (delete-types/sub2 msg)
  (interactive (list msg)
	       '(lambda(s)
		  (if (string-null? s)
		      (delete-types-rec)
		      (begin (set-cons! list-types-tmp s)
			     (delete-types/sub))))))
(tm-define (delete-types-rec)
  (define (f x) (not (in? x list-types-tmp)))
  (set-types (list-filter (list-types) f))
  (set-active-types (list-filter (list-active-types) f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ask-types
  (case-lambda
    ((proc)
     (ask-types proc '() #f))
    ((proc types)
     (ask-types proc types #f))
    ((proc types error?)
     (interactive (if (not error?)
                      '("Link type:")
                      '("Incorrect type. Link type:"))
                  (cut ask-types/callback proc types <>)))))

(define (ask-types/callback proc types s)
  (cond ((and (string-null? s)
              (pair? types))
         (ask-reverse-types (cut proc types <...>)))
        ((in? s (list-types))
         (ask-types proc (cons s types)))
        (else (ask-types proc types #t))))

(define ask-reverse-types
  (case-lambda
    ((proc)
     (ask-reverse-types proc '() #f))
    ((proc types)
     (ask-reverse-types proc types #f))
    ((proc types error?)
     (interactive (if (not error?)
                      '("Inverse link type:")
                      '("Incorrect inverse type. Inverse link type:"))
                  (cut ask-reverse-types/callback proc types <>)))))

(define (ask-reverse-types/callback proc types s)
  (cond ((and (string-null? s)
              (pair? types))
         (proc types))
        ((in? s (list-types))
         (ask-reverse-types proc (cons s types)))
        (else (ask-reverse-types proc types #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing link types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ask-types-to-remove
  (case-lambda
    ((proc types) (ask-types-to-remove proc types #f))
    ((proc types error?)
     (interactive (if (not error?)
                      '("Remove link type:")
                      '("No such type. Remove link type:"))
                  (cut ask-types-to-remove/callback proc types <>)))))

(define (ask-types-to-remove/callback proc types s)
  (cond ((string-null? s) (proc types))
        ((in? s types)
         (ask-types-to-remove
          proc (list-filter types (lambda (x) (!= x s)))))
        (else (ask-types-to-remove proc types #t))))
