
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-client.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote texmacs-client))

(define client-active? #f)
(define client-waiting? #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchroneous clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (client-eval cmd)
  ;;(display* "Client command: " cmd "\n")
  (object->string* (eval (string->object cmd))))

(tm-define (client-add)
  (set! client-active? #t)
  (with wait 1
    (delayed
      (:while client-active?)
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.001 wait) 2500)))
      (when (not client-waiting?)
	(with cmd (client-read)
	  (when (!= cmd "")
	    (with result (client-eval cmd)
	      (client-write result)
	      (set! wait 1))))))))

(tm-define (client-remove)
  (set! client-active? #f))

(define (client-remote-sub cmd return)
  (when (not client-waiting?)
    (set! client-waiting? #t)
    (client-write (object->string* cmd))
    (with wait 1
      (delayed
	(:while client-waiting?)
	(:pause ((lambda () (inexact->exact wait))))
	(:do (set! wait (min (* 1.001 wait) 2500)))
	(with result (client-read)
	  (when (!= result "")
	    (set! client-waiting? #f)
	    (set! wait 1)
	    (return (string->object result))))))))

(tm-define (client-remote cmd)
  (if dialogue-break
      (dialogue-user local-continue
	(with return (dialogue-machine local-continue)
	  (client-remote-sub cmd return)))
      (texmacs-error "dialogue-ask" "Not in dialogue")))
