
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server.scm
;; DESCRIPTION : the TeXmacs server
;; COPYRIGHT   : (C) 2006  Henri Lesourd and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server server))
(use-modules (ice-9 rdelim)
	     (tools base) (tools abbrevs) (tools ahash-table)
	     (tools file) (tools crypt)
	     (server socket) (server request) (server atoms)
	     (tmfs locus) (tmfs link) (chat chat))

(define server-finished? #f)

(request-handler (ping)
  #t)

(request-handler (shutdown)
  (set! server-finished? #t)
  #t)

(request-handler (print msg)
  (display* "Received message " msg "\n")
  #t)

(define-public (server-request sock)
  (let* ((s (socket-read sock))
	 (r (handle-request sock s)))
    (socket-write sock r)
    (cons s r)))

(define-public (server-start port)
  (let ((srv (new-socket "" port)))
    (ignore-errors 'system-error
      (mkdir (server-dir)))
    (ignore-errors 'system-error
      (mkdir (string-append (server-dir) "/system")))
    (atom-initialize)
    (display* "Started TeXmacs server at port " port "\n")
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
;       (dec (rsa-decode enc key)))
;  (display* "private = " key "\n")
;  (display* "public  = " pub "\n")
;  (display* "original= " msg "\n")
;  (display* "encoded = " (string->base64 enc) "\n")
;  (display* "1: " (string->base64 "hallo") "\n")
;  (display* "2: " (base64->string (string->base64 "hallo")) "\n")
;  (display* "decoded = " dec "\n"))
