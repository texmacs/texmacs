
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
        (client client-db)
        (client client-widgets)))

(menu-bind start-client-menu
  (with l (client-accounts)
    (if (null? l)
	("Login" (open-remote-login "" "")))
    (if (nnull? l)
	(for (x l)
	  (with (server-name pseudo) x
	    ((eval (string-append "Login as " pseudo "@" server-name))
	     (open-remote-login server-name pseudo))))
	("Other login" (open-remote-login "" "")))
    ("New account" (open-remote-account-creator))))

(tm-menu (remote-submenu server)
  ("Home" (load-document (remote-home-directory server)))
  ---
  (when (remote-file-name (current-buffer))
    ("New remote file" (remote-create-file-interactive server))
    ("New remote directory" (remote-create-dir-interactive server))
    (when (not (remote-home-directory? (current-buffer)))
      ("Rename" (remote-rename-interactive server)))
    ("Remove" (remote-remove-interactive server))
    ("Permissions" (open-file-permissions-editor server (current-buffer))))
  ---
  ("Upload" (remote-interactive-upload server))
  ("Download" (remote-interactive-download server))
  ("Synchronize" (remote-interactive-sync server))
  ---
  ("Logout" (client-logout server)))

(menu-bind client-menu
  (with l (client-active-servers)
    (assuming (null? l)
      (link start-client-menu))
    (assuming (== (length l) 1)
      (with server (car l)
        (dynamic (remote-submenu server))))
    (assuming (> (length l) 1)
      (for (server l)
        (-> (eval (client-find-server-name server))
            (dynamic (remote-submenu server)))))))
