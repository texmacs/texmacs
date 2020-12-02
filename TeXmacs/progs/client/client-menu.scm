
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

(tm-menu (remote-home-menu server)
  (when (remote-home-directory server)
    ("Home directory" (load-document (remote-home-directory server))))
  (when (list-chat-rooms server)
    ("Chat rooms" (load-document (list-chat-rooms server))))
  (when (list-live server)
    ("Live documents" (load-document (list-live server)))))

(tm-menu (remote-file-menu server)
  ("Rename" (remote-rename-interactive server))
  ("Remove" (remote-remove-interactive server))
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Share" (open-share-document-widget server (current-buffer))))

(tm-menu (remote-dir-menu server)
  ("New remote file" (remote-create-file-interactive server))
  ("New remote directory" (remote-create-dir-interactive server))
  ("Remove" (remote-remove-interactive server))
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Share" (open-share-document-widget server (current-buffer))))

(tm-menu (remote-chat-menu server)
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Invite" (open-share-document-widget server (current-buffer))))

(tm-menu (remote-chat-list-menu server)
  ("New chat room" (chat-room-create-interactive server))
  ("Join chat room" (chat-room-join-interactive server)))

(tm-menu (remote-live-menu server)
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Share" (open-share-document-widget server (current-buffer))))

(tm-menu (remote-live-list-menu server)
  ("New live document" (live-create-interactive server))
  ("Open live document" (live-open-interactive server)))

(tm-menu (remote-mail-menu server)
  ("Incoming messages" (mail-box-open server))
  ("Send message" (open-message-editor server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main remote menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (remote-submenu server)
  (dynamic (remote-home-menu server))
  (when (list-shared server)
    ("Shared resources" (load-document (list-shared server))))
  ---
  (if (and (remote-file-name (current-buffer))
           (not (remote-home-directory? (current-buffer))))
      (group "Remote file")
      (dynamic (remote-file-menu server)))
  (if (and (remote-file-name (current-buffer))
           (remote-home-directory? (current-buffer)))
      (group "Remote directory")
      (dynamic (remote-dir-menu server)))
  (if (and (chat-room-url? (current-buffer))
           (not (mail-box-url? (current-buffer))))
      (group "Chat room")
      (dynamic (remote-chat-menu server)))
  (if (chat-rooms-url? (current-buffer))
      (group "Chat rooms")
      (dynamic (remote-chat-list-menu server)))
  (if (live-url? (current-buffer))
      (group "Live document")
      (dynamic (remote-live-menu server)))
  (if (live-list-url? (current-buffer))
      (group "Live documents")
      (dynamic (remote-live-list-menu server)))
  ---
  ("Upload" (remote-interactive-upload server))
  ("Download" (remote-interactive-download server))
  ("Synchronize" (remote-interactive-sync server))
  ---
  (dynamic (remote-mail-menu server))
  ---
  ("Logout" (client-logout server)))

(menu-bind client-menu
  (invisible (client-active-servers))
  (with l (client-active-servers)
    (assuming (null? l)
      (link start-client-menu))
    (assuming (== (length l) 1)
      (dynamic (remote-submenu (car l))))
    (assuming (> (length l) 1)
      (for (server l)
        (-> (eval (client-find-server-name server))
            (dynamic (remote-submenu server)))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main remote icon menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (remote-subicons server)
  (=> (balloon (icon "tm_cloud.xpm") "Connection with server")
      ("Logout" (client-logout server)))
  (=> (balloon (icon "tm_cloud_home.xpm") "My resources on the server")
      (dynamic (remote-home-menu server))
      ---
      (when (list-shared server)
        ("Shared resources" (load-document (list-shared server)))))
  (if (and (remote-file-name (current-buffer))
           (not (remote-home-directory? (current-buffer))))
      (=> (balloon (icon "tm_cloud_file.xpm") "Remote file")
          (dynamic (remote-file-menu server))))
  (if (and (remote-file-name (current-buffer))
           (remote-home-directory? (current-buffer)))
      (=> (balloon (icon "tm_cloud_dir.xpm") "Remote directory")
          (dynamic (remote-dir-menu server))))
  (if (and (chat-room-url? (current-buffer))
           (not (mail-box-url? (current-buffer))))
      (=> (balloon (icon "tm_cloud_file.xpm") "Chat room")
          (dynamic (remote-chat-menu server))))
  (if (chat-rooms-url? (current-buffer))
      (=> (balloon (icon "tm_cloud_dir.xpm") "Chat rooms")
          (dynamic (remote-chat-list-menu server))))
  (if (live-url? (current-buffer))
      (=> (balloon (icon "tm_cloud_file.xpm") "Live document")
          (dynamic (remote-live-menu server))))
  (if (live-list-url? (current-buffer))
      (=> (balloon (icon "tm_cloud_dir.xpm") "Live documents")
          (dynamic (remote-live-list-menu server))))
  (=> (balloon (icon "tm_cloud_mail.xpm") "Messages")
      (dynamic (remote-mail-menu server))))

(menu-bind remote-icons
  (invisible (client-active-servers))
  (assuming (and (null? remote-client-list) (not (server-started?)))
    (=> (balloon (icon "tm_cloud.xpm") "Connect with server")
        (link start-client-menu)))
  (assuming (and (null? remote-client-list) (server-started?))
    (=> (balloon (icon "tm_cloud.xpm") "Server menu")
        (link server-menu)))
  (assuming (and (nnull? remote-client-list)
                 (nnull? (client-active-servers)))
    (dynamic (remote-subicons (car (client-active-servers))))))
