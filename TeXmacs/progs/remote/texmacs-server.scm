
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-server.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

(tm-define (server-remote client cmd cont)
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
	    (cont (string->object result))))))))
