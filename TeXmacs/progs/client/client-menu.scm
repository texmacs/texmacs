
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-menu)
  (:use (client client-base)
        (client client-tmfs)))

;;(menu-bind account-menu
;;  ("Nickname" (remote-interactive-set-user-property "Nickname"))
;;  ("Full name" (remote-interactive-set-user-property "Full name"))
;;  ("Email address" (remote-interactive-set-user-property "Email"))
;;  ("Home page" (remote-interactive-set-user-property "Web")))

(menu-bind client-menu
  ("New account" (interactive client-new-account))
  ("Login" (interactive client-login)))

(tm-menu (remote-submenu server sname)
  (group sname)
  ("Logout" (client-logout server))
  ("Home directory" (load-buffer (remote-home-directory server)))
  ("New remote file" (remote-create-file-interactive server))
  ("New remote directory" (remote-create-dir-interactive server))
  (when (has-client-properties? (current-buffer))
    ("Properties" (open-client-properties-editor))))

(menu-bind remote-menu
  (for (server (client-active-servers))
    (dynamic (remote-submenu server (client-find-server-name server)))))
