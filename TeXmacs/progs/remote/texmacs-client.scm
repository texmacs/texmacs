
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-client.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

(tm-define (client-remote cmd cont)
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
	    (cont (string->object result))))))))
