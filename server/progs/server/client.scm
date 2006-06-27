
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client.scm
;; DESCRIPTION : a TeXmacs client for testing
;; COPYRIGHT   : (C) 2006  Henri Lesourd and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server client))
(read-set! keywords 'prefix)
(read-enable 'positions)
(debug-enable 'debug)
(debug-set! stack 1000000)
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools crypt)
	     (server socket))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands from the client
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-info #f)

(define-public (server-request msg)
  (define s (new-socket "localhost" 6561))
  ;;(display* "Send message " msg "\n")
  (socket-request s msg))

(define-public (request-connect)
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
    (set! connection-info final)
    #t))

(define-public (secure-request cmd)
  (when connection-info
    (with (id key) connection-info
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example session
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display* "ping= " (server-request `(ping)) "\n")
(display* "key = " (base64->string (server-request `(get-public-key))) "\n")
(request-connect)
(secure-request `(print-message "Hello world!"))
