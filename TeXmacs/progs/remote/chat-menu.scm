
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chat-menu.scm
;; DESCRIPTION : menus for chatting
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote chat-menu)
  (:use (remote chat-edit)))

(tm-menu (chat-administrated-menu)
  (with l (chat-list-administrated-rooms)
    (assuming l
      (with sorted-l (list-sort l string<=?)
        ---
        (for (name sorted-l) ((eval name) (chat-connect name)))))))

(tm-menu (chat-connect-menu)
  (with l (chat-list-rooms)
    (assuming l
      (with sorted-l (list-sort l string<=?)
        (for (name sorted-l) ((eval name) (chat-connect name)))
        ---))))

(menu-bind chat-menu
  (if (not (chat-session))
      (-> "Connect"
	  ("Create chatroom" (interactive chatroom-create))
	  (link chat-administrated-menu)
	  ---
	  (link chat-connect-menu)
	  ("Other" (interactive chat-connect))))
  (if (chat-session)
      (when (not (chat-connected?))
	("Catch up" (chat-catch-up))))
  (when (chat-connected?)
    ("Hang up" (interactive chat-hang-up))))
