
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : request.scm
;; DESCRIPTION : secure server requests
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server request))
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (tools file) (tools crypt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server identity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-identity-file (string-append (server-dir) "/system/keys.scm"))
(define server-public-key #f)
(define server-private-key #f)

(define-public (server-initialize)
  (if (not (access? server-identity-file R_OK))
      (let* ((private-key (rsa-generate))
	     (public-key (rsa-private->public private-key)))
	(save-object server-identity-file (list private-key public-key))
	(chmod server-identity-file #o600)))
  (with (private-key public-key) (load-object server-identity-file)
    (set! server-private-key private-key)
    (set! server-public-key public-key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unsecure requests and encryption / decryption
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-id 0)
(define secure-connection (make-ahash-table))
(define-public current-connection-id #f)

(define (var-object->string x)
  (if (string? x)
      (string-append "s" x)
      (string-append "o" (object->string x))))

(define (var-string->object x)
  (with y (substring x 1 (string-length x))
    (if (== (string-ref x 0) #\s) y (string->object y))))

(define-public (handle-unsecure-request cmd)
  ;;(display* "Received: " cmd "\n")
  (cond ((or (npair? cmd) (nlist? cmd)) #f)
	((== cmd '(ping))
	 #t)
	((== cmd '(get-public-key))
	 (string->base64 server-public-key))
	((and (== (car cmd) 'connect) (= (length cmd) 3))
	 (with (dummy remote-key-64 challenge-64) cmd
	   (and-let* ((remote-key (base64->string remote-key-64))
		      (challenge (base64->string challenge-64))
		      (decoded (rsa-decode challenge server-private-key))
		      (secret-key (secret-generate))
		      (ret (list decoded connection-id secret-key))
		      (s1 (var-object->string ret))
		      (s2 (add-verification s1))
		      (encoded (rsa-encode s2 remote-key)))
	     (ahash-set! secure-connection connection-id secret-key)
	     (set! connection-id (+ connection-id 1))
	     (string->base64 encoded))))
	((and (== (car cmd) 'secure) (= (length cmd) 3))
	 (with (dummy id msg-64) cmd
	   (and-let* ((msg (base64->string msg-64))
		      (key (ahash-ref secure-connection id))
		      (decoded (secret-decode msg key))
		      (untagged (remove-verification decoded))
		      (clean (var-string->object untagged)))
	     (set! current-connection-id id)
	     (with answer (handle-request clean)
	       (set! current-connection-id #f)
	       (and-let* ((reply (var-object->string answer))
			  (tagged (add-verification reply))
			  (encoded (secret-encode tagged key)))
		 (string->base64 encoded))))))
	(else #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add handlers for secure requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public request-table (make-ahash-table))

(define-public-macro (request-handler head . body)
  ;;(display* "Connected " head "\n")
  (let* ((s (car head))
	 (r `(lambda ,(cdr head) ,@body)))
    `(begin
       (ahash-set! request-table ',s ,r)
       (define-public ,head ,@body))))

(define-public (handle-request cmd)
  (display* "Request: " cmd "\n")
  (and (pair? cmd)
       (with r (ahash-ref request-table (car cmd))
	 (and r (apply r (cdr cmd))))))
