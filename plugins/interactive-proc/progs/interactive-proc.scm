
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: b5ac5a9e-7863-4822-93a2-be7db53a4246
;;
;; MODULE      : interactive-proc.scm
;; DESCRIPTION : Better support for minibuffer (footer) interaction.
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

(texmacs-module (interactive-proc))

;; The implementation of "interactive-default" would be more elegant if
;; "interactive" took a procedure instead of a symbol. So let us use the
;; "interactive-proc" wrapper function until "interactive" is updated.

(tm-define (interactive-proc proc prompts)
  (set! interactive-proc/callback proc)
  (interactive interactive-proc/callback-wrapper prompts))

(define interactive-proc/callback #f)

(tm-define (interactive-proc/callback-wrapper . args)
  (let ((proc interactive-proc/callback))
    (set! interactive-proc/callback #f)
    (apply proc args)))


;; "interactive" primitive with a proc parameter and an optional default value

(tm-define (interactive-default proc prompts defaults)
  (interactive-proc
    (map interactive-default/prompt prompts defaults)
    (cut interactive-default/callback proc defaults <...>)))

(define (interactive-default/prompt p d)
  (if (not d) p (string-append p " [" d "]")))

(define (interactive-default/callback proc defaults . inputs)
  (apply proc (map (lambda (s d) (if (string-null? s) d s))
                   inputs defaults)))
