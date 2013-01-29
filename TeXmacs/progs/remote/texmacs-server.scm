
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-client-active? (make-ahash-table))
(define server-serial 0)

(tm-define (server-clients)
  (ahash-set->list server-client-active?))

(tm-define (server-send client cmd)
  (server-write client (object->string* (list server-serial cmd)))
  (set! server-serial (+ server-serial 1)))

(define (server-return client msg-id ret-val)
  (server-send client `(client-remote-result ,msg-id ,ret-val)))

(define (server-eval client msg-id cmd)
  ;;(display* "Server command [" client ", " msg-id "]: " cmd "\n")
  (with ret (object->string* (eval cmd))
    (when (not (func? cmd 'server-remote-result))
      (server-return client msg-id ret))))

(tm-define (server-add client)
  (ahash-set! server-client-active? client #t)
  (with wait 1
    (delayed
      (:while (ahash-ref server-client-active? client))
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with msg (server-read client)
        (when (!= msg "")
          (with (msg-id msg-cmd) (string->object msg)
            (server-eval client msg-id msg-cmd)
            (set! wait 1)))))))

(tm-define (server-remove client)
  (ahash-remove! server-client-active? client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-continuations (make-ahash-table))

(tm-define (server-remote-eval client cmd cont)
  (ahash-set! server-continuations server-serial cont)
  (server-send client cmd))

(tm-define (server-remote-result msg-id ret)
  (and-with cont (ahash-ref server-continuations msg-id)
    (ahash-remove! server-continuations msg-id)
    (cont ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-users (make-ahash-table))

(define (server-load-users)
  (when (== (ahash-size server-users) 0)
    (with f "$TEXMACS_HOME_PATH/system/users.scm"
      (set! server-users
            (if (url-exists? f)
                (list->ahash-table (load-object f))
                (make-ahash-table))))))

(define (server-save-users)
  (with f "$TEXMACS_HOME_PATH/system/users.scm"
    (save-object f (ahash-table->list server-users))))

(tm-define (server-set-user-info id passwd email admin)
  (server-load-users)
  (ahash-set! server-users id (list passwd email admin))
  (server-save-users))

