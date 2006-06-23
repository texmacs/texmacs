
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server.scm
;; DESCRIPTION : the TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server server))
(read-set! keywords 'prefix)
(read-enable 'positions)
(debug-enable 'debug)
(debug-set! stack 1000000)
(use-modules (ice-9 rdelim)
	     (tools base) (tools abbrevs) (tools ahash-table)
	     (tools file) (tools crypt)
	     (server socket) (server request)
	     (server atoms) (server permissions)
	     (tmfs tmfs) (chat chat))


(define server-finished? #f)

(request-handler (shutdown)
  (set! server-finished? #t)
  #t)

(request-handler (print-message message)
  (display* "Message: " message "\n")
  #t)

(define-public (server-request sock)
  (let* ((s (socket-read sock))
	 (r (handle-unsecure-request s)))
    (socket-write sock r)
    (cons s r)))

(define-public (server-start port)
  (let ((srv (new-socket "" port)))
    (ignore-errors 'system-error (mkdir (server-dir)))
    (ignore-errors 'system-error (mkdir (system-dir)))
    (chmod (server-dir) #o700)
    (chmod (system-dir) #o700)
    (server-initialize)
    (atom-initialize)
    (display* "Started TeXmacs daemon at port " port "\n")
    (do ((server-serial 0))
	(server-finished? (noop))
      (let* ((s (socket-accept srv))
	     (r (server-request s)))
	;;(display* server-serial ": " (car r) " -> " (cdr r) "\n")
	(noop))
      (set! server-serial (+ server-serial 1)))))

(server-start 6561)

;(let* ((key (rsa-generate))
;       (msg "Hallo allemaal")
;       (pub (rsa-private->public key))
;       (enc (rsa-encode msg pub))
;       (dec (rsa-decode enc key))
;       (Key (secret-generate))
;       (Enc (secret-encode msg Key))
;       (Dec (secret-decode Enc Key)))
;  (display* "private = " key "\n")
;  (display* "public  = " pub "\n")
;  (display* "1: " (string->base64 "hallo") "\n")
;  (display* "2: " (base64->string (string->base64 "hallo")) "\n")
;  (display* "original= " msg "\n")
;  (display* "encoded = " (string->base64 enc) "\n")
;  (display* "decoded = " dec "\n")
;  (display* "Encoded = " (string->base64 Enc) "\n")
;  (display* "Decoded = " Dec "\n"))
