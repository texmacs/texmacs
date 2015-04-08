
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
        (client client-tmfs)
        (client client-widgets)))

;;(menu-bind account-menu
;;  ("Nickname" (remote-interactive-set-user-property "Nickname"))
;;  ("Full name" (remote-interactive-set-user-property "Full name"))
;;  ("Email address" (remote-interactive-set-user-property "Email"))
;;  ("Home page" (remote-interactive-set-user-property "Web")))

(tm-define (client-login-home server-name pseudo passwd)
  (:argument server-name "Server")
  (:argument pseudo "User pseudo")
  (:argument passwd "password" "Password")
  (client-login-then server-name pseudo passwd
                     (lambda (ret)
                       (with server (client-find-server server-name)
                         (load-buffer (remote-home-directory server))))))

(menu-bind client-menu
  ("New account" (interactive client-new-account))
  ("Login" (interactive client-login-home)))

(tm-menu (remote-submenu server)
  ("Logout" (client-logout server))
  ---
  ("Home directory" (load-buffer (remote-home-directory server)))
  (when (remote-file-name (current-buffer))
    ("New remote file" (remote-create-file-interactive server))
    ("New remote directory" (remote-create-dir-interactive server))
    ;;("Browse files" (remote-browse server))
    ("Permissions" (open-file-permissions-editor server (current-buffer)))))

(menu-bind remote-menu
  (with l (client-active-servers)
    (assuming (null? l)
      (link client-menu))
    (assuming (== (length l) 1)
      (with server (car l)
        (group (client-find-server-name server))
        (dynamic (remote-submenu server))))
    (assuming (> (length l) 1)
      (for (server l)
        (-> (eval (client-find-server-name server))
            (dynamic (remote-submenu server)))))))
