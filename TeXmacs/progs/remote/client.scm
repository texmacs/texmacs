
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client.scm
;; DESCRIPTION : connections to a TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote client)
  (:use (remote crypt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (os-mingw?))
    (sigaction SIGPIPE (lambda (sig) #t)))
;; NOTE: mingw does not have SIGPIPE 
;; NOTE: Mandatory if we want to be able to catch the socket errors.

(define-macro (new-socket host port)
  `(with sock (socket AF_INET SOCK_STREAM 0)
     (with addr (car (vector-ref (gethostbyname ,host) 4))
       (connect sock AF_INET addr ,port)
       sock)))

(define-macro (socket-read sock)
  `(with s (read-line ,sock)
      (with-input-from-string s
         (lambda () (read)))))

(define-macro (socket-write sock obj)
  `(with vobj ,obj
     (with-output-to-port ,sock
       (lambda ()
	 (write vobj)
	 (newline)
	 (force-output)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow for connections to different servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-server #f)

(define (server-request cmd . opt)
  (with server (if (null? opt) current-server (car opt))
    ;;(display* "request: " cmd " for " server "\n")
    (catch #t
	   (lambda ()
	     (with sock (new-socket server 6561)
	       (socket-write sock cmd)
	       (socket-read sock)))
	   (lambda args #f))))

(tm-define (get-server)
  current-server)

(tm-define (set-server server)
  (set! current-server server))

(tm-define-macro (with-server server . body)
  `(with old-server (get-server)
     (set-server ,server)
     (with r (begin ,@body)
       (set-server old-server)
       r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure connections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define secure-connection (make-ahash-table))

(define (var-object->string x)
  (if (string? x)
      (string-append "s" x)
      (string-append "o" (object->string x))))

(define (var-string->object x)
  (with y (substring x 1 (string-length x))
    (if (== (string-ref x 0) #\s) y (string->object y))))

(tm-define (remote-connect)
  (and-let* ((private-key (rsa-generate))
	     (public-key (rsa-private->public private-key))
	     ;; build data base for remembering previous connections
	     ;; and increasing trust from the server side
	     ;; may also avoid need to log in
             (remote-64 (server-request `(get-public-key)))
	     (remote-key (base64->string remote-64))
	     ;; check with trusted database
	     ;; can also ask for some previously stored secret info
	     (challenge (secret-generate))
	     (enc-chal (rsa-encode challenge remote-key))
	     (public-key-64 (string->base64 public-key))
	     (enc-chal-64 (string->base64 enc-chal))
	     (msg `(connect ,public-key-64 ,enc-chal-64))
	     (r (server-request msg))
	     (decoded (rsa-decode (base64->string r) private-key))
	     (returned (remove-verification decoded))
	     (obj (var-string->object returned))
	     (final (and (== (car obj) challenge) (cdr obj))))
    (display* "TeXmacs] connected to " (get-server) "\n")
    (ahash-set! secure-connection (get-server) final)
    #t))

(tm-define (remote-request cmd)
  ;;(display* "Request: " cmd "\n")
  (and-let* ((server (get-server))
	     (info (ahash-ref secure-connection server)))
    (with (id key) info
      (and-let* ((message (var-object->string cmd))
		 (tagged (add-verification message))
		 (encoded (secret-encode tagged key))
		 (transmit (string->base64 encoded))
		 (reply-64 (server-request `(secure ,id ,transmit)))
		 (reply (base64->string reply-64))
		 (decoded (secret-decode reply key))
		 (untagged (remove-verification decoded))
		 (final (var-string->object untagged)))
	final))))

;;(tm-define (remote-hang-up)
;;  (remote-request '(hang-up)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding a default TeXmacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define default-texmacs-server #f)

(define (server-ping server)
  (server-request '(ping) server))

(define (first-server l)
  (cond ((null? l) #f)
	((server-ping (car l)) (car l))
	(else (first-server (cdr l)))))

(define (find-default-server)
  (let* ((local-sv "$TEXMACS_HOME_PATH/system/servers.scm")
	 (remote-sv "http://www.texmacs.org/Data/servers.scm")
	 (l (if (url-exists? local-sv) (load-object local-sv) '()))
	 (first (first-server l)))
    (or first
	(let* ((r (load-object remote-sv))
	       (m (list-union (cons "localhost" r) l)))
	  (if (!= (length m) (length l)) (save-object local-sv m))
	  (first-server m)))))

(tm-define (default-server)
  (or default-texmacs-server
      (with server (find-default-server)
	(set! default-texmacs-server server)
	(and (with-server server (remote-connect))
	     server))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging in and account management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-logged #f)
(define current-user #f)

(define (account-server)
  (or (get-server) (default-server)))

(tm-define (logged-server)
  current-logged)

(tm-define (remote-new-account user passwd)
  (:synopsis "Create a new user account on the remote server")
  (:argument user "User name")
  (:argument passwd "password" "Password")
  (with-server (account-server)
    (with encoded-passwd (secret-hash passwd)
      (remote-request `(new-user ,user ,encoded-passwd)))))

(tm-define (remote-login user passwd)
  (:synopsis "Login on the remote server")
  (:argument user "User name")
  (:argument passwd "password" "Password")
  (with-server (account-server)
    (with encoded-passwd (secret-hash passwd)
      (if (remote-request `(user-login ,user ,encoded-passwd))
	  (begin
	    (set! current-logged (get-server))
	    (set! current-user user))
	  (set-message "Wrong password" "Remote login")))))

(tm-define (remote-logout)
  (with-server (account-server)
    (when (remote-request `(user-logout))
      (set! current-logged #f)
      (set! current-user #f))))

(tm-define (remote-logged?)
  (not (not current-user)))

(tm-define (remote-user)
  current-user)

(tm-define (remote-set-user-property var val)
  (with-server (logged-server)
    (remote-request `(user-set-property ,var ,val))))

(tm-define (remote-get-user-property var)
  (with-server (logged-server)
    (remote-request `(user-get-property ,var))))

(tm-define (remote-interactive-set-user-property var)
  (:interactive #t)
  (let* ((old-val* (remote-get-user-property var))
	 (old-val (if (string? old-val*) old-val* "")))
    (interactive (lambda (val) (remote-set-user-property var val))
      (list var "string" old-val))))
