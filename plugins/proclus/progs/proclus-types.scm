
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
	(ice-9 common-list)) ;; uniq et set-difference.
  (:export type-menu-promise
           toggle-active-type ;; FIXME: for ugly menu workaround
           
           list-types 
           active-types

	   
	   import-types 
	   import-types/sub 
	   list-types-tmp
	   new-types/sub
	   new-types-rec
	   delete-types/sub
	   delete-types-rec

           
           ask-types
           
           ;; FIXME: for interactive
           ask-reverse-types ask-types/sub
           unproper-type unproper-reverse-type
           
           ;; FIXME: for interactive and proclus.scm
           list-direct-types-tmp list-reverse-types-tmp
           ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level access to type lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-types)
  (source-buffer-excursion
   (let ((x (tree->object (get-init-tree "proclus-type-list"))))
     (if (func? x 'tuple)
         (cdr x)
         '()))))

(define (active-types)
  (source-buffer-excursion
   (let ((x (tree->object (get-init-tree "proclus-active-types"))))
     (if (not (func? x 'tuple))
         (begin (set-active-types (list-types))
                (list-types))
         (cdr x)))))

(define (set-active-types types)
  (source-buffer-excursion
   (init-env-tree "proclus-active-types"
                  (object->tree (cons 'tuple types)))))

(define (types-tree)
  (transform locus? (tree->object (the-buffer))))

(define (type? x)
  (and (pair? x)
       (>= (length x) 3)
       (eq? (car x) 'locus)))

   

;;adds the list of  types ltypes to the  type list of the current doc.
(define (merge-types ltypes) 
  (init-env-tree "proclus-type-list"
		 (object->tree 
		  (cons 'tuple 
			(uniq 
			 (list-concatenate 
			  (list (list-types) ltypes)))))))

;;adds the list of  types ltypes to the active  types list of the current doc.
(define (merge-active-types ltypes) 
  (init-env-tree "proclus-active-types"
		 (object->tree 
		  (cons 'tuple 
			(uniq 
			 (list-concatenate 
			  (list (active-types) ltypes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High-level type commands and menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deactivate-type type)
  (set-active-types
   (list-filter (active-types) (lambda (x) (not (== x type))))))

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

(define (type-menu-promise)
  ;; FIXME: texmacs dynamic menus are dead buggy. Joris assumed that macros
  ;; shall not be memoized and decided that all menu computation work should be
  ;; done in macros :-( Luckily, there _is_ a temporary workaround.
  (source-buffer-excursion
   (clear-active-types-cache)
   (menu-dynamic
     (-> "Types"
	 ("Importer" (import-types))
	 ("Ajouter" (new-types))
	 ("Supprimer" (delete-types))
	 ---
         ("Activer tous" (activate-all-types))
         ("Désactiver tous" (deactivate-all-types))
         ("Inverser" (activate-negative-types))
	
         ---
         ,@(let ((types (list-types)))
             (if (null? types)
                 (menu-dynamic (when #f ("Pas de type" (noop))))
                 (map type-menu-item types)))))))

(define (type-menu-item type)
  (eval (cons 'quasiquote
              (list (menu-pre `(,type (toggle-active-type ,type)))))))


(define (import-types)
  (let ((from (get-strg-name-buffer)))
    (choose-file "Importer les types" "texmacs"
                 `(lambda (x) (import-types/sub x ,from)))))

(define (import-types/sub u from)
  (switch-to-active-buffer u)
  (let ((imp-types (list-types)))
    (switch-to-active-buffer from)
     (merge-types imp-types)))

(define list-types-tmp '())

(define (new-types)
  (set! list-types-tmp '())
  (new-types/sub))

(define (new-types/sub)
  (new-types/sub2 "Ajouter le type :"))

(define (new-types/sub2 msg)
  (interactive (list msg)
	       '(lambda(s)
		  (if (string-null? s)
		      (new-types-rec)
		      (begin (set-cons! list-types-tmp s)
			     (new-types/sub))))))

;;adds list-types-tmp to the list of types. New types are active.
(define (new-types-rec)
  (merge-types list-types-tmp)
  (merge-active-types list-types-tmp))

(define (delete-types)
  (set! list-types-tmp '())
  (delete-types/sub))

(define (delete-types/sub)
  (delete-types/sub2 "Supprimer le type :"))

(define (delete-types/sub2 msg)
  (interactive (list msg)
	       '(lambda(s)
		  (if (string-null? s)
		      (delete-types-rec)
		      (begin (set-cons! list-types-tmp s)
			     (delete-types/sub))))))
(define (delete-types-rec)
  (init-env-tree "proclus-type-list" 
		 (object->tree 
		  (cons 'tuple
			(list-filter 
			 (list-types) 
			 (lambda (x) (not (in? x list-types-tmp))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list-direct-types-tmp '())
(define list-reverse-types-tmp '())

(define (ask-types)
  (set! list-direct-types-tmp '())
  (set! list-reverse-types-tmp '())
  (ask-types/sub))

(define (ask-types/sub)
  (ask-types/sub2 "Type du lien:"))

(define (unproper-type s)
  (ask-types/sub2 "Type incorrect. Type de lien:"))

(define (ask-types/sub2 msg)
  (interactive (list msg)
	       '(lambda(s)
		  (cond ((and (string-null? s)
			      (pair? list-direct-types-tmp))
			 (ask-reverse-types))
			((in? s (list-types))
			 (begin (set-cons! list-direct-types-tmp s)
				(ask-types/sub)))
			(else (unproper-type s))))))

(define (ask-reverse-types)
  (ask-reverse-types/sub "Type du lien inverse:"))

(define (unproper-reverse-type s)
  (ask-reverse-types/sub "Type inverse incorrect. Type inverse du lien :"))

(define (ask-reverse-types/sub msg)
  (interactive (list msg)
	       '(lambda(s)
		  (cond ((and (string-null? s)
			      (pair? list-reverse-types-tmp))
			 (add-link))
			((in? s (list-types))
			 (begin (set-cons! list-reverse-types-tmp s)
				(ask-reverse-types)))
			(else  (unproper-reverse-type s))))))
