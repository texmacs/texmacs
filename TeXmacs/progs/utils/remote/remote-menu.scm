
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : remote-menu.scm
;; DESCRIPTION : menus for user accounts on the TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils remote remote-menu)
  (:use (utils remote client)))

(menu-bind login-menu
  ("New account" (interactive remote-new-account))
  (when (not (remote-logged?))
    ("Login" (interactive remote-login)))
  (when (remote-logged?)
    ("Logout" (remote-logout))))

(menu-bind account-menu
  ("Nickname" (remote-interactive-set-user-property "Nickname"))
  ("Full name" (remote-interactive-set-user-property "Full name")))

(menu-bind remote-menu
  (-> "Login" (link login-menu))
  (when (remote-logged?)
    (-> "Account" (link account-menu))
    (-> "Chat" (link chat-menu))))
