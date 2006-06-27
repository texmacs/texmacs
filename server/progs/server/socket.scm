
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : socket.scm
;; DESCRIPTION : basic routines for the manipulation of sockets
;; COPYRIGHT   : (C) 2006  Henri Lesourd and Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server socket))
(use-modules (ice-9 rdelim)
	     (tools base) (tools abbrevs) (tools ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sigaction SIGPIPE (lambda (sig) #t))
;; NOTE: Mandatory if we want to be able to catch the socket errors.

(define-public-macro (new-socket host port)
  `(if (equal? ,host "")
       (with sock (socket AF_INET SOCK_STREAM 0)
	 (bind sock AF_INET INADDR_ANY ,port)
	 (listen sock 3)
	 sock)
       (with sock (socket AF_INET SOCK_STREAM 0)
	 (with addr (car (vector-ref (gethostbyname ,host) 4))
	   (connect sock AF_INET addr ,port)
	   sock))))

(define-public-macro (socket-accept sock)
  `(car (accept ,sock)))

(define-public-macro (socket-read sock)
  `(with s (read-line ,sock)
      (with-input-from-string s
         (lambda () (read)))))

(define-public-macro (socket-write sock obj)
  `(with vobj ,obj
     (with-output-to-port ,sock
       (lambda ()
	 (write vobj)
	 (newline)
	 (force-output)))))

(define-public-macro (socket-close sock)
  `(close-port ,sock))

(define-public (socket-request sock cmd)
  (socket-write sock cmd)
  (socket-read sock))
