
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : remote-remote-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote remote-texmacs-menu)
  (:use (remote texmacs-server)
	(remote texmacs-client)))

;;(menu-bind account-menu
;;  ("Nickname" (remote-interactive-set-user-property "Nickname"))
;;  ("Full name" (remote-interactive-set-user-property "Full name"))
;;  ("Email address" (remote-interactive-set-user-property "Email"))
;;  ("Home page" (remote-interactive-set-user-property "Web")))

(menu-bind texmacs-remote-menu
  (group "Local")
  ("Start server" (server-start))
  ("Set user information" (interactive server-set-user-information))
  (group "Remote")
  ("New account" (interactive client-new-account))
  ("Login" (interactive client-login)))

(menu-bind remote-menu
  ("Logout" (noop)))
