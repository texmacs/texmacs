
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

(texmacs-module (utils remote client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sigaction SIGPIPE (lambda (sig) #t))
;; NOTE: Mandatory if we want to be able to catch the socket errors.

(define-macro (new-socket host port)
  `(if (equal? ,host "")
       (with sock (socket AF_INET SOCK_STREAM 0)
	 (bind sock AF_INET INADDR_ANY ,port)
	 (listen sock 3)
	 sock)
       (with sock (socket AF_INET SOCK_STREAM 0)
	 (with addr (car (vector-ref (gethostbyname ,host) 4))
	   (connect sock AF_INET addr ,port)
	   sock))))

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
;; Connection to the TeXmacs server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-request cmd)
  (with sock (new-socket "localhost" 6561)
    (socket-write sock cmd)
    (socket-read sock)))

(tm-define (remote-connect)
  (remote-request `(connect ,(getpid))))

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
