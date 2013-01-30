
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : texmacs-client.scm
;; DESCRIPTION : clients of TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote texmacs-client))

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

(define client-active? #f)
(define client-serial 0)

(tm-define (active-servers)
  (list 0))

(tm-define (client-send server cmd)
  ;; FIXME: server ignored for the moment
  (client-write (object->string* (list client-serial cmd)))
  (set! client-serial (+ client-serial 1)))

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
            (client-eval (list 0 msg-id) msg-cmd)
            (set! wait 1)))))))

(tm-define (client-remove)
  (set! client-active? #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending asynchroneous commands to servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define client-continuations (make-ahash-table))

(tm-define (client-remote-eval server cmd cont)
  (ahash-set! client-continuations client-serial (list server cont))
  (client-send server cmd))

(tm-call-back (client-remote-result msg-id ret)
  (with server (car envelope)
    (and-with val (ahash-ref client-continuations msg-id)
      (ahash-remove! client-continuations msg-id)
      (with (orig-server cont) val
        (when (== server orig-server)
          (cont ret))))))

(tm-call-back (client-remote-error msg-id err-msg)
  (with server (car envelope)
    (and-with val (ahash-ref client-continuations msg-id)
      (ahash-remove! client-continuations msg-id)
      (with (orig-server cont) val
        (when (== server orig-server)
          (texmacs-error "client-remote-error" "remote error ~S" err-msg))))))
