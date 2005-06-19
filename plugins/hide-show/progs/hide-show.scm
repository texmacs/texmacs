
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 737db109-97a5-496d-ac28-433eccf7a83e
;;
;; MODULE      : hide-show.scm
;; DESCRIPTION : Hiding and showing line structures
;; COPYRIGHT   : (C) 2004  David Allouche
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

(texmacs-module (hide-show)
  (:use (utils library tree)
	(buffer-replace)
	(interactive-proc)))

;; TODO: use package hide.ts when hide is not defined in the initial env

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiding and showing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hide tags)
  (buffer-replace-postorder
   (lambda (p t) (in? (tree-label t) tags))
   (lambda (p t)
     (path-insert-node (rcons p 0) '(hide)))))

(define (show tags)
  (buffer-replace-postorder
   (lambda (p t) (and (eq? 'hide (tree-label t))
		      (in? (tree-label (tree-ref t 0)) tags)))
   (lambda (p t) (path-remove-node (rcons p 0)))))

(tm-define (show-all)
  (buffer-replace-postorder
   (lambda (p t) (eq? 'hide (tree-label t)))
   (lambda (p t) (path-remove-node (rcons p 0)))))

(define (input->symbols s)
  (map string->symbol
       (list-filter (string-tokenize s #\space)
		    (lambda (s) (not (string-null? s))))))

(tm-define (hide-ia)
  (if (not (func? (tree->stree (get-init-tree "hide")) 'macro))
      (init-add-package "hide"))
  (interactive-proc (lambda (s) (hide (input->symbols s))) (list "Hide")))

(tm-define (show-ia)
  (interactive-proc (lambda(s) (show (input->symbols s))) (list "Show")))
