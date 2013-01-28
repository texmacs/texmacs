
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-active? #f)
(define client-serial 0)

(tm-define (client-send cmd)
  (client-write (object->string* (list client-serial cmd)))
  (set! client-serial (+ client-serial 1)))

(define (client-return msg-id ret-val)
  (client-send `(server-remote-result ,msg-id ,ret-val)))

(define (client-eval msg-id cmd)
  ;;(display* "Client command [" msg-id "]: " cmd "\n")
  (with ret (object->string* (eval cmd))
    (when (not (func? cmd 'client-remote-result))
      (client-return msg-id ret))))

(tm-define (client-add)
  (set! client-active? #t)
  (with wait 1
    (delayed
      (:while client-active?)
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with msg (client-read)
        (when (!= msg "")
          (with (msg-id msg-cmd) (string->object msg)
            (client-eval msg-id msg-cmd)
            (set! wait 1)))))))

(tm-define (client-remove)
  (set! client-active? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-continuations (make-ahash-table))

(tm-define (client-remote-eval cmd cont)
  (ahash-set! client-continuations client-serial cont)
  (client-send cmd))

(tm-define (client-remote-result msg-id ret)
  (and-with cont (ahash-ref client-continuations msg-id)
    (ahash-remove! client-continuations msg-id)
    (cont ret)))
