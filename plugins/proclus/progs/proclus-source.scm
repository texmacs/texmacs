
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 13d08085-03f8-4d84-9513-4aaea35a3414
;;
;; MODULE      : proclus-source.scm
;; DESCRIPTION : Handling the source-buffer environment
;; COPYRIGHT   : (C) 2003--2004  David Allouche
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

(texmacs-module (proclus-source)
  (:use (proclus-lib)
        (proclus-locus))
  (:export has-last-locus?
           set-last-locus!
           go-to-last-locus
           has-source-buffer?
           set-source-buffer!
           go-to-source-buffer
           source-buffer-excursion
           source-buffer-excursion/sub))


(define last-locus '())

(define (has-last-locus?)
  (pair? last-locus))

(define (set-last-locus! lnk)
  (set! last-locus lnk))

(define (go-to-last-locus)
  (if (has-last-locus?)
   (go-to-locus last-locus)))

(define (has-source-buffer?)
  (not (string-null? (get-env "source-buffer"))))

(define (set-source-buffer! buf)
  (init-env "source-buffer" buf))

(define (go-to-source-buffer)
  (if (has-source-buffer?)
      (switch-to-active-buffer (get-env "source-buffer"))))

(define-macro (source-buffer-excursion . body)
  `(source-buffer-excursion/sub (lambda () ,@body)))

(define (source-buffer-excursion/sub thunk)
  (if (has-source-buffer?)
      (save-excursion
       (go-to-source-buffer)
       (thunk))
      (thunk)))
