
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : remote-menu.scm
;; DESCRIPTION : menus for user accounts on the TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote remote-menu)
  (:use (remote client)))

(menu-bind login-menu
  ("New account" (interactive remote-new-account))
  (when (not (remote-logged?))
    ("Login" (interactive remote-login)))
  (when (remote-logged?)
    ("Logout" (remote-logout))))

(menu-bind account-menu
  ("Nickname" (remote-interactive-set-user-property "Nickname"))
  ("Full name" (remote-interactive-set-user-property "Full name"))
  ("Email address" (remote-interactive-set-user-property "Email"))
  ("Home page" (remote-interactive-set-user-property "Web")))

(menu-bind remote-menu
  (-> "Login" (link login-menu))
  (when (remote-logged?)
    (-> "Account" (link account-menu))
    (-> "Chat" (link chat-menu))
    (-> "Files" (link remote-file-menu))))
