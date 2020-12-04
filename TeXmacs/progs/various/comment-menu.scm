
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-menu.scm
;; DESCRIPTION : menus for commenting a text
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-menu)
  (:use (various comment-edit)))

(menu-bind comment-menu
  ("New comment" (make-comment))
  ("Edit comments" (open-comments-editor (current-buffer)))
  ---
  ("First comment" (go-to-comment :first))
  ("Previous comment" (go-to-comment :previous))
  ("Next comment" (go-to-comment :next))
  ("Last comment" (go-to-comment :last))
  ---
  ("Show comments" (operate-on-comments :show))
  ("Hide comments" (operate-on-comments :hide))
  ("Remove comments" (operate-on-comments :cut)))

(kbd-map
  (:mode in-comment?)
  ("std :" (make-comment))
  ("std [" (go-to-comment :previous))
  ("std ]" (go-to-comment :next))
  ("std {" (go-to-comment :first))
  ("std }" (go-to-comment :last)))
