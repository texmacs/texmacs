
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: fe816148-a328-4716-ac49-740d16bec52f
;;
;; MODULE      : proclus.scm
;; DESCRIPTION : The 'proclus' plugin core features
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

(texmacs-module (proclus)
  (:use (proclus-lib)
        (proclus-absname)
        (proclus-target)
        (proclus-list)
        (proclus-types)
        (proclus-distill)
        (proclus-source))
  (:export target
           initialisation
           active-source active-but
           add-link ;; FIXME: for proclus-types.scm (for interactive)
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (target)
  (if (in-proclus-editable?)
      (begin
        (register-buffer-absolute-name-maybe)
        (make-target (get-absolute-name) (next-target-num)))))

(define (next-target-num)
  (init-env "target-num" (number->string
                          (1+ (or (string->number (get-init-env "target-num"))
                                  0))))
  (string->number (get-init-env "target-num")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source and But selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-source '())
(define link-but '())

;; Initialize the link creation state machine.
;; Use global variables for communication across documents.
(define (initialisation)
  (set! link-source '())
  (set! link-but '()))

;; Designe la TARGET comme la source du prochain lien à créer.
(define (active-source)
  (if (and (in-proclus-editable?)
           (selection-active-any?))
      (target))
  (if (in-proclus-target?)
      (and-let* ((t (get-target)))
        (initialisation)
        (source-buffer-excursion
         (register-buffer-absolute-name-maybe))
        (set! link-source (target-self-link t)))))

;; Idem, désigne une destination.
(define (active-but)
  (if (and (in-proclus-editable?)
           (selection-active-any?))
      (target))
  (if (in-proclus-target?)
      (and-let* ((t (get-target)))
        (source-buffer-excursion
         (register-buffer-absolute-name-maybe))
        (set! link-but (target-self-link t))
        (if (pair? link-source) (make-link)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define active-buffer '())
(define active-pos-temp '())

(define (make-link)
  ;; Initiate footer interaction, control ends up in add-link, below.
  (set! active-buffer (get-strg-name-buffer))
  (set! active-pos-temp (the-path))
  (ask-types))

;; Creation of bidirectional links
(define (add-link)
  ;; This is called by interactive procs in proclus-types.scm
  (if (or (null? link-but) (null? link-source))
      #f (begin (add-link-end link-source link-but list-direct-types-tmp)
		(add-link-end link-but link-source list-reverse-types-tmp)
		(switch-to-active-buffer active-buffer)
		(tm-go-to active-pos-temp))))
