
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client.scm
;; DESCRIPTION : connections to a TeXmacs server
;; COPYRIGHT   : (C) 2006  Henri Lesourd and Joris van der Hoeven
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
;; Finding a TeXmacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-server #f)

(define (server-request name cmd)
  ;;(display* "request: " name ", " cmd "\n")
  (catch #t
	 (lambda ()
	   (with sock (new-socket name 6561)
	     (socket-write sock cmd)
	     (socket-read sock)))
	 (lambda args #f)))

(define (server-ping name)
  (server-request name '(ping)))

(define (first-server l)
  (cond ((null? l) #f)
	((server-ping (car l)) (car l))
	(else (first-server (cdr l)))))

(define (find-server)
  (when (not current-server)
    (let* ((local-sv "$TEXMACS_HOME_PATH/system/servers.scm")
	   (remote-sv "http://www.texmacs.org/Data/servers.scm")
	   (l (if (url-exists? local-sv) (load-object local-sv) '())))
      (set! current-server (first-server l))
      (if (not current-server)
	  (let* ((r (load-object remote-sv))
		 (m (list-union (cons "localhost" r) l)))
	    (if (!= (length m) (length l))
		(begin
		  (save-object local-sv m)
		  (set! current-server (first-server m))))))
      current-server)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Secure connections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define secure-connection (make-ahash-table))

(tm-define (remote-connection-id server)
  (and-with r (ahash-ref secure-connection server)
    (car r)))

(tm-define (remote-connection-key server)
  (and-with r (ahash-ref secure-connection server)
    (cadr r)))

(tm-define (remote-connect server)
  (and-let* ((private-key (rsa-generate))
	     (public-key (rsa-private->public private-key))
	     ;; build data base for remembering previous connections
	     ;; and increasing trust from the server side
	     ;; may also avoid need to log in
	     (remote-key (server-request `(get-public-key)))
	     ;; check with trusted database
	     ;; can also ask for some previously stored secret info
	     (challenge (secret-generate))
	     (enc-chal (rsa-encode what remote-key))
	     (r (server-request `(connect ,public-key ,enc-chal)))
	     (decoded (rsa-decode (base64->string r) private-key))
	     (returned (remove-verification decoded))
	     (obj (string->object returned))
	     (final (and (== (car obj) challenge) (cdr obj))))
    (ahash-set! secure-connection server final)
    #t))

(tm-define (secure-request server cmd)
  (and-let* ((s (string->base64 (object->string cmd)))
	     (id (remote-connection-id server))
	     (key (remote-connection-key server))
	     (message (add-verification s))
	     (encoded (secret-encode message key))
	     (reply (remote-request `(secure ,id ,encoded)))
	     (decoded (secret-decode reply key))
	     (returned (remove-verification decoded))
	     (r (base64->string returned)))
    (string->object r)))

(tm-define (remote-hang-up server)
  (secure-request server '(hang-up)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging in and account management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define current-server #f)
(define current-user #f)

(tm-define (remote-new-account server user passwd)
  (with encoded-passwd (secret-encode passwd passwd)
    (secure-request server `(new-user ,user ,encoded-passwd))))

(tm-define (remote-login server user passwd)
  (and-let* ((encoded-passwd (secret-encode passwd passwd))
	     (ok (secure-request server `(user-login ,user ,encoded-passwd))))
    (set! current-server server)
    (set! current-user user)))

(tm-define (remote-set-user-property var val)
  (secure-request `(user-set-property ,var ,val)))

(tm-define (remote-get-user-property var)
  (secure-request `(user-get-property ,val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Saving and loading documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-request cmd)
  (and (find-server)
       (server-request current-server cmd)))

(tm-define (remote-save expr)
  (remote-request `(locus-new ,expr)))

(tm-define (remote-load nr)
  (remote-request `(locus-ref ,nr)))

(tm-define (remote-put nr expr)
  (if (string? expr) (set! expr (string-replace expr "\\" "\\\\")))
  (if (string? expr) (set! expr (string-replace expr "\n" "\\n")))
  ;;(display* "remote-put " nr ", " expr "\n")
  (remote-request `(locus-set! ,nr ,expr)))

(tm-define (remote-get nr)
  ;;(display* "remote-get " nr "\n")
  (with r (remote-load nr)
    (if (string? r) (set! r (string-replace r "\\\\" "\\")))
    (if (string? r) (set! r (string-replace r "\\n" "\n")))
    (if r r "")))
