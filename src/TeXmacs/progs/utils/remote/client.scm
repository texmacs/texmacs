
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client.scm
;; DESCRIPTION : connections to a TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils remote client)
  (:use (utils remote crypt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sigaction SIGPIPE (lambda (sig) #t))
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

(tm-define (remote-connect)
  (display* "Connect to " (get-server) "\n")
  (and-let* ((private-key (rsa-generate))
	     (public-key (rsa-private->public private-key))
	     ;; build data base for remembering previous connections
	     ;; and increasing trust from the server side
	     ;; may also avoid need to log in
	     (remote-key (base64->string (server-request `(get-public-key))))
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
	     (obj (string->object returned))
	     (final (and (== (car obj) challenge) (cdr obj))))
    (ahash-set! secure-connection (get-server) final)
    #t))

(tm-define (remote-request cmd)
  (and-let* ((server (get-server))
	     (info (ahash-ref secure-connection server)))
    (with (id key) info
      (and-let* ((message (object->string cmd))
		 (tagged (add-verification message))
		 (encoded (secret-encode tagged key))
		 (transmit (string->base64 encoded))
		 (reply-64 (server-request `(secure ,id ,transmit)))
		 (reply (base64->string reply-64))
		 (decoded (secret-decode reply key))
		 (untagged (remove-verification decoded))
		 (final (string->object untagged)))
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
  (with-server (account-server)
    (with encoded-passwd (secret-encode passwd passwd)
      (remote-request server `(new-user ,user ,encoded-passwd)))))

(tm-define (remote-login user passwd)
  (with-server (account-server)
    (and-let* ((encoded-passwd (secret-encode passwd passwd))
	       (ok (remote-request `(user-login ,user ,encoded-passwd))))
      (set! current-logged (get-server))
      (set! current-user user))))

(tm-define (remote-set-user-property var val)
  (with-server (logged-server)
    (remote-request `(user-set-property ,var ,val))))

(tm-define (remote-get-user-property var)
  (with-server (logged-server)
    (remote-request `(user-get-property ,val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmfs-server)
  (or (logged-server) (get-server) (default-server)))

(tm-define (remote-save expr)
  (with-server (tmfs-server)
    (remote-request `(locus-new ,expr))))

(tm-define (remote-load nr)
  (with-server (tmfs-server)
    (remote-request `(locus-ref ,nr))))

(tm-define (remote-put nr expr)
  (with-server (tmfs-server)
    ;;(display* "remote-put " nr ", " expr "\n")
    (remote-request `(locus-set! ,nr ,expr))))

(tm-define (remote-get nr)
  ;;(display* "remote-get " nr "\n")
  (with r (remote-load nr)
    (if r r "")))
