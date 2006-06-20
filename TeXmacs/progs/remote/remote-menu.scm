
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

(texmacs-module (remote remote-menu)
  (:use (remote tmfs)))

(menu-bind login-menu
  ("New account" (interactive remote-new-account))
  (when (not (remote-logged?))
    ("Login" (interactive remote-login)))
  (when (remote-logged?)
    ("Logout" (remote-logout))))

(menu-bind account-menu
  ("Nickname" (remote-interactive-set-user-property "Nickname"))
  ("Full name" (remote-interactive-set-user-property "Full name")))

;(menu-bind groups-menu
;  ("New group" (interactive remote-new-group))
;  (-> "Permissions"
;      ("Administration" (remote-interactive-set-group-permissions 'admin))
;      ("Read" (remote-interactive-set-group-permissions 'read))
;      ("Write" (remote-interactive-set-group-permissions 'write))
;      ("Other" (interactive remote-set-group-permissions))))

(menu-bind remote-file-menu
  ("New file" (remote-new-file))
  ---
  (when (remote-buffer?)
    (-> "Permissions"
	("Administration" (interactive-remote-set-property "owner"))
	("Read" (interactive-remote-set-property "readable"))
	("Write" (interactive-remote-set-property "writable")))
    (-> "Properties"
	("Set property" (interactive-remote-set-property-and-value))
	("Get property" (interactive remote-get-property)))))

(menu-bind remote-menu
  (-> "Login" (link login-menu))
  (when (remote-logged?)
    (-> "Account" (link account-menu))
;    (-> "Groups" (link groups-menu))
    (-> "Chat" (link chat-menu))
    (-> "Files" (link remote-file-menu))))
