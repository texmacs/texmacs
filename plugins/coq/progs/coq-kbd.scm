
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coq-kbd.scm
;; DESCRIPTION : keyboard shortcuts for coq
;; COPYRIGHT   : (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (coq-kbd)
  (:use (coq-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-coq-style?)
  ("$" (insert-coq-command))
  ("C-return" (coq-check))
  ("L e m m a" (insert-coq-enunciation "Lemma"))
  ("T h e o r e m" (insert-coq-enunciation "Theorem"))
  ("C-F12" (coq-remove-comments (buffer-tree))))
