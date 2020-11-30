
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote client submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (when (remote-home-directory server)
    ("Home" (load-document (remote-home-directory server))))
  ---
  (when (remote-file-name (current-buffer))
    ("New remote file" (remote-create-file-interactive server))
    ("New remote directory" (remote-create-dir-interactive server))
    (when (not (remote-home-directory? (current-buffer)))
      ("Rename" (remote-rename-interactive server)))
    ("Remove" (remote-remove-interactive server))
    ("Permissions" (open-file-permissions-editor server (current-buffer)))
    ("Share" (open-share-document-widget server (current-buffer))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main remote menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind remote-menu
  (if (and (null? remote-client-list) (not (server-started?)))
      (link start-client-menu)
      ;;---
      ;;(link start-server-menu)
      )
  (if (and (null? remote-client-list) (server-started?))
      (link server-menu))
  (if (nnull? remote-client-list)
      (link client-menu)))

(menu-bind remote-icons
  (let* ((servers (client-active-servers))
         (server (and (nnull? servers) (car servers))))
    (if (and (null? remote-client-list) (not (server-started?)))
        (=> (balloon (icon "tm_cloud.xpm") "Connect with server")
            (link start-client-menu)))
    (if (and (null? remote-client-list) (server-started?))
        (=> (balloon (icon "tm_cloud.xpm") "Server menu")
            (link server-menu)))
    (if (nnull? remote-client-list)
        (=> (balloon (icon "tm_cloud.xpm") "Connection with server")
            ("Logout" (client-logout server)))
        (=> (balloon (icon "tm_cloud_home.xpm") "Home directory on server")
            (when (remote-home-directory server)
              ("My documents" (load-document (remote-home-directory server))))
            ("My chat rooms" (load-document (list-chat-rooms server)))
            ("Shared resources" (load-document (list-shared server))))
        (if (and (remote-file-name (current-buffer))
                 (not (remote-home-directory? (current-buffer))))
            (=> (balloon (icon "tm_cloud_file.xpm") "Remote file")
                ("Rename" (remote-rename-interactive server))
                ("Remove" (remote-remove-interactive server))
                ("Permissions"
                 (open-file-permissions-editor server (current-buffer)))
                ("Share"
                 (open-share-document-widget server (current-buffer)))))
        (if (and (remote-file-name (current-buffer))
                 (remote-home-directory? (current-buffer)))
            (=> (balloon (icon "tm_cloud_dir.xpm") "Remote directory")
                ("New remote file" (remote-create-file-interactive server))
                ("New remote directory" (remote-create-dir-interactive server))
                ("Remove" (remote-remove-interactive server))
                ("Permissions"
                 (open-file-permissions-editor server (current-buffer)))
                ("Share"
                 (open-share-document-widget server (current-buffer)))))
        (=> (balloon (icon "tm_cloud_mail.xpm") "Messages")
            ("Incoming messages" (mail-box-open server))
            ("Send message" (open-message-editor server))
            ---
            ("Create chat room" (chat-room-create-interactive server))
            ("Join chat room" (chat-room-join-interactive server))
            (when (and (chat-room-url? (current-buffer))
                       (not (mail-box-url? (current-buffer))))
              ("Invite to chat room"
               (open-share-document-widget server (current-buffer))))))))
