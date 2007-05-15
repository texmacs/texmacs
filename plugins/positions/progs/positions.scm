
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: be376a3f-243d-45aa-a4a5-e326e833f550
;;
;; MODULE      : positions.scm
;; DESCRIPTION : User interface for the position facility in texmacs.
;; COPYRIGHT   : (C) 2003, 2004  David Allouche
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

(texmacs-module (positions)
  (:use (interactive-proc)))

(define positions (make-ahash-table))

(define (recall-position name)
  (let ((pos (ahash-ref positions name)))
    (if (not pos) (set-message
		   (string-append "Error: no position by this name: " name)
		   "error"))
    pos))

(tm-define (position-remember-ia)
  (interactive-proc position-remember/callback (list "Remember position as")))

(define (position-remember/callback name)
  (let ((pos (position-new)))
    (position-set pos (cursor-path))
    (ahash-set! positions name pos)))

(tm-define (position-jump-ia)
  (interactive-proc position-jump/callback (list "Jump to position")))

(define (position-jump/callback name)
  (cond ((recall-position name) =>
	 (lambda (pos) (go-to (position-get pos))))))

(tm-define (position-forget-ia)
  (interactive-proc position-forget/callback (list "Forget position")))

(define (position-forget/callback name)
  (cond ((recall-position name) =>
	 (lambda (pos)
	   (position-delete pos)
	   (ahash-remove! positions name)))))
