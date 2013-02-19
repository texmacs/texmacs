
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-base.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of call backs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public call-back-dispatch-table (make-ahash-table))

(tm-define-macro (tm-call-back proto . body)
  (if (npair? proto) '(noop)
      (with (fun . args) proto
        `(begin
           (tm-define (,fun envelope ,@args) ,@body)
           (ahash-set! call-back-dispatch-table ',fun ,fun)))))

(tm-define (client-eval envelope cmd)
  ;; (display* "client-eval " envelope ", " cmd "\n")
  (if (and (pair? cmd) (ahash-ref call-back-dispatch-table (car cmd)))
      (with (name . args) cmd
        (with fun (ahash-ref call-back-dispatch-table name)
          (apply fun (cons envelope args))))
      (client-error envelope "invalid command")))

(define (client-return envelope ret-val)
  (with (server msg-id) envelope
    (client-send server `(server-remote-result ,msg-id ,ret-val))))

(define (client-error envelope error-msg)
  (with (server msg-id) envelope
    (client-send server `(server-remote-error ,msg-id ,error-msg))))

(tm-call-back (local-eval cmd)
  (with ret (eval cmd)
    ;; (display* "local-eval " cmd " -> " ret "\n")
    (client-return envelope ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Establishing and finishing connections with servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-server-active? (make-ahash-table))
(define client-serial 0)

(tm-define (active-servers)
  (ahash-set->list client-server-active?))

(tm-define (client-send server cmd)
  (client-write server (object->string* (list client-serial cmd)))
  (set! client-serial (+ client-serial 1)))

(tm-define (client-add server)
  (ahash-set! client-server-active? server #t)
  (with wait 1
    (delayed
      (:while (ahash-ref client-server-active? server))
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with msg (client-read server)
        (when (!= msg "")
          (with (msg-id msg-cmd) (string->object msg)
            (client-eval (list server msg-id) msg-cmd)
            (set! wait 1)))))))

(tm-define (client-remove server)
  (ahash-remove! client-server-active? server))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-continuations (make-ahash-table))
(define client-error-handlers (make-ahash-table))

(define (std-client-error msg)
  (texmacs-error "client-remote-error" "remote error ~S" msg))

(tm-define (client-remote-eval server cmd cont . opt-err-handler)
  (with err-handler std-client-error
    (if (nnull? opt-err-handler) (set! err-handler (car opt-err-handler)))
    (ahash-set! client-continuations client-serial (list server cont))
    (ahash-set! client-error-handlers client-serial (list server err-handler))
    (client-send server cmd)))

(tm-define (client-remote-eval* server cmd cont)
  (client-remote-eval server cmd cont cont))

(tm-call-back (client-remote-result msg-id ret)
  (with server (car envelope)
    (and-with val (ahash-ref client-continuations msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server cont) val
        (when (== server orig-server)
          (cont ret))))))

(tm-call-back (client-remote-error msg-id err-msg)
  (with server (car envelope)
    (and-with val (ahash-ref client-error-handlers msg-id)
      (ahash-remove! client-continuations msg-id)
      (ahash-remove! client-error-handlers msg-id)
      (with (orig-server err-handler) val
        (when (== server orig-server)
          (err-handler err-msg))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (client-new-account server-name id passwd email)
  (:argument server-name "Server")
  (:argument id "User ID")
  (:argument passwd "password" "Password")
  (:argument email "Email address")
  (with server (client-start server-name)
    (when (!= server -1)
      (enter-secure-mode server)
      (client-remote-eval* server `(new-account ,id ,passwd ,email)
                           (lambda (msg)
                             (set-message msg "creating new account")
                             (client-stop server))))))

(tm-define (client-login server-name id passwd)
  (:argument server-name "Server")
  (:argument id "User ID")
  (:argument passwd "password" "Password")
  (with server (client-start server-name)
    (when (!= server -1)
      (enter-secure-mode server)
      (client-remote-eval* server `(remote-login ,id ,passwd)
                           (lambda (ret) (set-message ret "logging in"))))))
