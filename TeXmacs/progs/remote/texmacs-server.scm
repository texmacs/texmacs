
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-server.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote texmacs-server))

(define server-client-active? (make-ahash-table))
(define server-client-waiting? (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchroneous servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-clients)
  (ahash-set->list server-client-active?))

(define (server-eval cmd)
  ;;(display* "Server command: " cmd "\n")
  (object->string* (eval (string->object cmd))))

(tm-define (server-add client)
  (ahash-set! server-client-active? client #t)
  (with wait 1
    (delayed
      (:while (ahash-ref server-client-active? client))
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.001 wait) 2500)))
      (when (not (ahash-ref server-client-waiting? client))
	(with cmd (server-read client)
	  (when (!= cmd "")
	    (with result (server-eval cmd)
	      (server-write client result)
	      (set! wait 1))))))))

(tm-define (server-remove client)
  (ahash-remove! server-client-active? client))

(define (server-remote-sub client cmd return)
  (when (not (ahash-ref server-client-waiting? client))
    (ahash-set! server-client-waiting? client #t)
    (server-write client (object->string* cmd))
    (with wait 1
      (delayed
	(:while (ahash-ref server-client-waiting? client))
	(:pause ((lambda () (inexact->exact wait))))
	(:do (set! wait (min (* 1.001 wait) 2500)))
	(with result (server-read client)
	  (when (!= result "")
	    (ahash-set! server-client-waiting? client #f)
	    (set! wait 1)
	    (return (string->object result))))))))

(tm-define (server-remote client cmd)
  (if dialogue-break
      (dialogue-user local-continue
	(with return (dialogue-machine local-continue)
	  (server-remote-sub client cmd return)))
      (texmacs-error "dialogue-ask" "Not in dialogue")))
