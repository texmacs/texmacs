
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chat-menu.scm
;; DESCRIPTION : menus for chatting
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic chat-menu)
  (:use (dynamic chat-edit)))

(menu-bind chat-menu
  (if (not (chat-session))
      ("Connect" (interactive chat-connect)))
  (if (chat-session)
      (when (not (chat-connected?))
	("Catch up" (chat-catch-up))))
  (when (chat-connected?)
    ("Hang up" (interactive chat-hang-up))))
