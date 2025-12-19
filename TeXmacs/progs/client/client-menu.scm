
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;                   2025  Gregoire Lecerf
;;                   2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-menu)
  (:use (client client-base)
        (client client-db)
        (client client-remote-config)
        (client client-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote client submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (server->string server)
  (if (client-find-server-port server)
      (with port (client-find-server-port server)
	(string-append (client-find-server-pseudo server) "@"
		       (client-find-server-name server)
		       (if (== port "6561") ""
			   (string-append ":" port))
		       "(" (number->string server) ")"))
      "Inactive connection"))

(define (account->string server-name port pseudo)
  (if (== server-name "")
      (account->string "localhost" port pseudo)
      (with x (string-append pseudo "@" server-name)
	(if (== port "6561") x
	    (string-append x ":" port)))))

(define-macro (with-post-reload . body) `(begin ,@body (revert-buffer)))

(menu-bind client-remove-account-menu
  (for (x (client-accounts))
    (with (server-name port pseudo authentications) x
      ((eval (account->string server-name port pseudo))
       (client-remove-account server-name port pseudo)))))

(menu-bind client-reset-credentials-menu
  ("Notify forgotten credentials"
   (open-remote-notify-reset-credentials "" "6561" "" `tls))
  ("Reset credentials from secret code"
   (open-remote-login-code "" "6561" "" `tls)))

(menu-bind client-start-menu
  (with l (client-accounts)
    (if (null? l)
	("Login" (open-remote-login "" "6561" "" `())))
    (if (nnull? l)
	(for (x l)
	  (with (server-name port pseudo authentications) x
	    ((eval (string-append
		    "Login as " (account->string server-name port pseudo)))
	     (open-remote-login server-name port pseudo authentications))))
	("Other login" (open-remote-login "" "6561" "" `())))
    ("New account" (open-remote-account-creator))
    (-> "Reset credentials" (link client-reset-credentials-menu))
    (if (nnull? l)
        (-> "Remove account" (link client-remove-account-menu)))))

(tm-menu (remote-home-menu server sep?)
  (when (remote-home-directory server)
    ("Home directory"
     (with-post-reload (load-document (remote-home-directory server)))))
  (when (list-chat-rooms server)
    ("Chat rooms" (with-post-reload (load-document (list-chat-rooms server)))))
  (when (list-live server)
    ("Live documents" (with-post-reload (load-document (list-live server)))))
  (assuming sep? ---)
  (when (list-shared server)
    ("Shared resources"
     (with-post-reload (load-document (list-shared server)))))
  ("Synchronize resources" (remote-interactive-sync server)))

(tm-menu (remote-file-menu server sep?)
  ("Rename" (remote-rename-interactive server))
  ("Remove" (remote-remove-interactive server))
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Share" (open-share-document-widget server (current-buffer)))
  (assuming sep? ---)
  ("Download" (simple-interactive-download server))
  ("Synchronize" (simple-interactive-synchronize server)))

(tm-menu (remote-dir-menu server sep?)
  ("New remote file" (remote-create-file-interactive server))
  ("New remote directory" (remote-create-dir-interactive server))
  ("Remove" (remote-remove-interactive server))
  ("Permissions" (open-permissions-editor server (current-buffer)))
  ("Share" (open-share-document-widget server (current-buffer)))
  (assuming sep? ---)
  ("Upload" (simple-interactive-upload server))
  ("Download" (simple-interactive-download server))
  ("Synchronize" (simple-interactive-synchronize server)))

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
  (eval (notifiable-entry
          'message "Incoming messages"
          (lambda () (with-post-reload (mail-box-open server)))))
  ("Send message" (open-message-editor server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main remote menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (remote-submenu server)
  (dynamic (remote-home-menu server #f))
  ---
  (if (and (remote-file-name (current-buffer))
           (not (remote-directory? (current-buffer))))
      (group "Remote file")
      (dynamic (remote-file-menu server #f)))
  (if (and (remote-file-name (current-buffer))
           (remote-directory? (current-buffer)))
      (group "Remote directory")
      (dynamic (remote-dir-menu server #f)))
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
  (dynamic (remote-mail-menu server))
  (assuming (server-connection-admin? server)
    ---
    (group "Server administration")
    ("Edit Server Preferences" (load-remote-config-form server))
    ("User Management" (open-admin-accounts-editor server)))
  ---
  ("Server infos" (open-public-preferences server))
  ("Edit account" (open-account-editor server))
  ---
  ("Logout" (client-logout server)))

(menu-bind client-menu
  (invisible (client-active-servers))
  (link client-start-menu)
  (with l (client-active-servers)
    ---
    (assuming (== (length l) 1)
     (dynamic (remote-submenu (car l))))
    (assuming (>= (length l) 2)
      (for (server l)
	(-> (eval (server->string server))
	    (dynamic (remote-submenu server)))))))

(menu-bind remote-menu
  (invisible (client-active-servers))
  (link client-menu)
  ---
  ("Client Preferences" (open-client-preferences))
  ---
  (assuming (not (server-started?))
    (link server-start-menu))
  (assuming (server-started?)
    (link server-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main remote icon menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-server #f)

(define (get-current-server)
  (with l (client-active-servers)
    (if (or (not current-server) (not (in? current-server l)))
	(if (null? l)
	    (set! current-server #f)
	    (set! current-server (car l)))))
  current-server)

(define (set-current-server s) (set! current-server s))

(menu-bind client-set-current-server-menu
  (invisible (client-active-servers))
  (with l (list-remove (client-active-servers) current-server)
    (for (server l)
      ((eval (server->string server)) (set-current-server server)))))

(tm-menu (remote-subicons server)
  (invisible (client-active-servers))
  (assuming (not (server-connection-admin? server))
    (=> (balloon (icon "tm_cloud.xpm") "Connection with server")
	("Edit account" (open-account-editor server))
	("Logout" (client-logout server))))
  (assuming (server-connection-admin? server)
    (=> (balloon (icon "tm_cloud_admin.xpm") "Connection with server")
	("Edit Server Preferences" (load-remote-config-form server))
	("User Management" (open-admin-accounts-editor server))
	("Edit account" (open-account-editor server))
	("Logout" (client-logout server))))
  (=> (balloon (icon "tm_cloud_home.xpm") "My resources on the server")
      (dynamic (remote-home-menu server #t)))
  (if (and (remote-file-name (current-buffer))
           (not (remote-directory? (current-buffer))))
      (=> (balloon (icon "tm_cloud_file.xpm") "Remote file")
	  (dynamic (remote-file-menu server #t))))
  (if (and (remote-file-name (current-buffer))
           (remote-directory? (current-buffer)))
      (=> (balloon (icon "tm_cloud_dir.xpm") "Remote directory")
	  (dynamic (remote-dir-menu server #t))))
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
  (=> (balloon (eval (notifiable-icon 'message
                                      "tm_cloud_mail.xpm"
                                      "tm_cloud_mail_new.xpm"))
               (eval (notif-count-label 'message "Messages")))
      (dynamic (remote-mail-menu server))))

(menu-bind remote-icons
  (invisible (client-active-servers))
  (invisible (get-current-server))
  (assuming (null? (client-active-servers))
    (=> (balloon (icon "tm_cloud.xpm") "Connect with server")
        (link client-start-menu)))
  (assuming (get-current-server)
    (dynamic (remote-subicons (get-current-server))))
  (assuming (>= (length (client-active-servers)) 2)
    (=> (balloon (eval (server->string (get-current-server))) "Connection")
	(link client-set-current-server-menu)))
  (assuming (server-started?)
    (=> (balloon (icon "tm_cloud_server.xpm") "Local server")
        (link server-menu))))
