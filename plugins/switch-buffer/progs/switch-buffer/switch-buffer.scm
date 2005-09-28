
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 853fe506-9d43-4e72-b768-7677337bfb81
;;
;; MODULE      : switch-buffer/switch-buffer.scm
;; DESCRIPTION : Easy switching betwen editor buffers
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

(texmacs-module (switch-buffer switch-buffer)
  (:use (interactive-proc)))

;; Simple switch to the last used buffer

(tm-define (switch-to-recent-buffer)
  (cond ((buffer-menu-recent-item) => buffer-menu-item-switch-to)))

(define (buffer-menu-recent-item)
  (let ((buffer-menu (buffer-menu)))
    (and (< 1 (length buffer-menu))
         (second (buffer-menu)))))

(define (buffer-menu-item-switch-to x) ((second x)))

;; Switch to any buffer using the minibuffer with the last buffer as the
;; default value (like emacs...)

(tm-define (interactive-switch-to-buffer)
  (cond ((buffer-menu-recent-item) =>
         (lambda (x)
           (interactive-default interactive-switch-to-buffer/callback
				(list "Switch to buffer")
                                (list (first x)))))))

(define (interactive-switch-to-buffer/callback s)
  (cond ((buffer-menu-item-by-name s) => buffer-menu-item-switch-to)))

(define (buffer-menu-item-by-name s)
  (assoc s (buffer-menu)))
