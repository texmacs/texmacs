
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
  (:use (buffer-replace)
	(interactive-proc))
  (:export hide-ia show-ia show-all))

;; TODO: use package hide.ts when hide is not defined in the initial env

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hiding and showing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (hide tags)
  (buffer-replace-postorder
   (lambda (p t) (in? (tree-get-label t) tags))
   (lambda (p t)
     (tm-ins-unary p 'hide))))

(define (show tags)
  (buffer-replace-postorder
   (lambda (p t) (and (eq? 'hide (tree-get-label t))
		      (in? (tree-get-label (tree-ref t 0)) tags)))
   (lambda (p t) (tm-rem-unary p))))

(define (show-all)
  (buffer-replace-postorder
   (lambda (p t) (eq? 'hide (tree-get-label t)))
   (lambda (p t) (tm-rem-unary p))))

(define (input->symbols s)
  (map string->symbol
       (list-filter (string-tokenize s #\space)
		    (lambda (s) (not (string-null? s))))))

(define (hide-ia)
  (if (not (func? (tree->object (get-init-tree "hide")) 'macro))
      (init-extra-style "hide"))
  (interactive-proc '("Hide:")
		    (lambda (s) (hide (input->symbols s)))))

(define (show-ia)
  (interactive-proc '("Show:")
		    (lambda(s) (show (input->symbols s)))))
