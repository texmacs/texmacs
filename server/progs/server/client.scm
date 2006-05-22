
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
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (server socket))

(define-public (send-msg msg)
  (define s (new-socket "localhost" 6561))
  (socket-request s msg))

(display* (send-msg `(ping) "\n"))
(display* (send-msg `(print ,(list (getpid) 1 2 3))) "\n")
(do ((i 0)) ((>= i 10000000) 0) (set! i (+ i 1)))
(display* (send-msg `(print ,(list (getpid) 4 5 6))) "\n")
;;(display* (send-msg '(shutdown)) "\n")
