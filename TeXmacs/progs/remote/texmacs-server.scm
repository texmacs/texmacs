
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-server.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote texmacs-server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public service-dispatch-table (make-ahash-table))

(tm-define-macro (tm-service proto . body)
  (if (npair? proto) '(noop)
      (with (fun . args) proto
        `(begin
           (tm-define (,fun envelope ,@args) ,@body)
           (ahash-set! service-dispatch-table ',fun ,fun)))))

(tm-define (server-eval envelope cmd)
  ;; (display* "server-eval " envelope ", " cmd "\n")
  (if (and (pair? cmd) (ahash-ref service-dispatch-table (car cmd)))
      (with (name . args) cmd
        (with fun (ahash-ref service-dispatch-table name)
          (apply fun (cons envelope args))))
      (server-error envelope "invalid command")))

(define (server-return envelope ret-val)
  (with (client msg-id) envelope
    (server-send client `(client-remote-result ,msg-id ,ret-val))))

(define (server-error envelope error-msg)
  (with (client msg-id) envelope
    (server-send client `(client-remote-error ,msg-id ,error-msg))))

(tm-service (remote-eval cmd)
  (with ret (eval cmd)
    ;; (display* "remote-eval " cmd " -> " ret "\n")
    (server-return envelope ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-client-active? (make-ahash-table))
(define server-serial 0)

(tm-define (active-clients)
  (ahash-set->list server-client-active?))

(tm-define (server-send client cmd)
  (server-write client (object->string* (list server-serial cmd)))
  (set! server-serial (+ server-serial 1)))

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
            (server-eval (list client msg-id) msg-cmd)
            (set! wait 1)))))))

(tm-define (server-remove client)
  (ahash-remove! server-client-active? client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-continuations (make-ahash-table))

(tm-define (server-remote-eval client cmd cont)
  (ahash-set! server-continuations server-serial (list client cont))
  (server-send client cmd))

(tm-service (server-remote-result msg-id ret)
  (with client (car envelope)
    (and-with val (ahash-ref server-continuations msg-id)
      (ahash-remove! server-continuations msg-id)
      (with (orig-client cont) val
        (when (== client orig-client)
          (cont ret))))))

(tm-service (server-remote-error msg-id err-msg)
  (with client (car envelope)
    (and-with val (ahash-ref server-continuations msg-id)
      (ahash-remove! server-continuations msg-id)
      (with (orig-client cont) val
        (when (== client orig-client)
          (texmacs-error "server-remote-error" "remote error ~S" err-msg))))))

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
