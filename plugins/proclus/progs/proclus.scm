
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
  (:use (proclus-lib) (proclus-absname) (proclus-locus) (proclus-list)
	(proclus-types) (proclus-distill) (proclus-source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Locus creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (locus)
  (if (in-proclus-editable?)
      (begin
        (register-buffer-absolute-name-maybe)
        (make-locus (get-absolute-name) (next-locus-num)))))

(define (next-locus-num)
  (init-env "locus-num" (number->string
                          (1+ (or (string->number (get-init-env "locus-num"))
                                  0))))
  (string->number (get-init-env "locus-num")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source and But selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define link-source '())
(define link-but '())

;; Initialize the link creation state machine.
;; Use global variables for communication across documents.
(tm-define (proclus-inactivate)
  (set! link-source '())
  (set! link-but '()))

(tm-define (has-active-source?)
  (pair? link-source))

;; Designe la LOCUS comme la source du prochain lien à créer.
(tm-define (active-source)
  (if (and (in-proclus-editable?)
           (selection-active-any?))
      (locus))
  (if (in-proclus-locus?)
      (and-let* ((t (get-locus)))
        (proclus-inactivate)
        (source-buffer-excursion
         (register-buffer-absolute-name-maybe))
        (set! link-source (locus-self-link t)))))

;; Idem, désigne une destination.
(tm-define (active-but)
  (if (and (in-proclus-editable?)
           (selection-active-any?))
      (locus))
  (if (in-proclus-locus?)
      (and-let* ((t (get-locus)))
        (source-buffer-excursion
         (register-buffer-absolute-name-maybe))
        (set! link-but (locus-self-link t))
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
  (ask-types add-link))

;; Creation of bidirectional links
(define (add-link direct-types reverse-types)
  ;; This is called by interactive procs in proclus-types.scm
  (if (or (null? link-but) (null? link-source))
      #f (begin (add-link-end link-source link-but direct-types)
		(add-link-end link-but link-source reverse-types)
		(switch-to-active-buffer active-buffer)
		(tm-go-to active-pos-temp))))
