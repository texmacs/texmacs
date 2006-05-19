
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
	     (server socket) (server request)
	     (tmfs locus) (tmfs link) (chat chat))

(define server-finished? #f)

(request-handler (connect . l)
  ;;(display* "connect " l "\n")
  #t)

(request-handler (shutdown . l)
  ;;(display* "shut down " l "\n")
  (set! server-finished? #t)
  #t)

(request-handler (print . l)
  (display* "Received message " l "\n")
  #t)

(define-public (server-request sock)
  (let* ((s (socket-read sock))
	 (r (handle-request sock s)))
    (socket-write sock r)
    (cons s r)))

(define-public (server-start port)
  (let ((srv (new-socket "" port)))
    (display* "Started TeXmacs server at port " port "\n")
    (do ((server-serial 0))
	(server-finished? (noop))
      (let* ((s (socket-accept srv))
	     (r (server-request s)))
	;;(display* server-serial ": " (car r) " -> " (cdr r) "\n")
	(noop))
      (set! server-serial (+ server-serial 1)))))

(server-start 6561)
