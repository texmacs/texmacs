
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-base.scm
;; DESCRIPTION : TeXmacs servers
;; COPYRIGHT   : (C) 2007, 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaration of services
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define service-dispatch-table (make-ahash-table))

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

(tm-define (server-return envelope ret-val)
  (with (client msg-id) envelope
    (server-send client `(client-remote-result ,msg-id ,ret-val))))

(tm-define (server-error envelope error-msg)
  (with (client msg-id) envelope
    (server-send client `(client-remote-error ,msg-id ,error-msg))))

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
    (with f "$TEXMACS_HOME_PATH/server/users.scm"
      (set! server-users
            (if (url-exists? f)
                (list->ahash-table (load-object f))
                (make-ahash-table))))))

(define (server-save-users)
  (with f "$TEXMACS_HOME_PATH/server/users.scm"
    (save-object f (ahash-table->list server-users))))

(tm-define (server-set-user-info uid id fullname passwd email admin)
  (server-load-users)
  (ahash-set! server-users uid (list id fullname passwd email admin))
  (resource-set-user-info uid id fullname email)
  (server-save-users))

(tm-define (server-set-user-information id fullname passwd email admin)
  (:argument id "User ID")
  (:argument fullname "Full name")
  (:argument passwd "password" "Password")
  (:argument email "Email address")
  (:argument admin "Administrive rights?")
  (:proposals admin '("no" "yes"))
  (with uid (server-find-user id)
    (if (not uid) (set! uid (create-unique-id)))
    (server-set-user-info uid id fullname passwd email (== admin "yes"))))

(tm-define (server-find-user id)
  (server-load-users)
  (with l (ahash-table->list server-users)
    (with ok? (lambda (x) (== (cadr x) id))
      (and-with i (list-find-index l ok?)
	(car (list-ref l i))))))

(tm-define (server-create-user id fullname passwd email admin)
  (or (server-find-user id)
      (with uid (resource-create id "user" id)
        (server-set-user-info uid id fullname passwd email admin))))

(tm-service (new-account id fullname passwd email)
  (if (server-find-user id)
      (server-error envelope "user already exists")
      (with ret (server-create-user id fullname passwd email #f)
	(server-return envelope "done"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Logging in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define server-logged-table (make-ahash-table))

(tm-define (server-get-user envelope)
  (with client (car envelope)
    (and client (ahash-ref server-logged-table client))))

(tm-define (server-check-admin? envelope)
  (and-with uid (server-get-user envelope)
    (with (id fullname passwd email admin) (ahash-ref server-users uid)
      admin)))

(tm-service (remote-login id passwd)
  (with uid (server-find-user id)
    (if (not uid) (server-error envelope "user not found")
	(with (id2 fullname2 passwd2 email2 admin2) (ahash-ref server-users uid)
	  (if (!= passwd2 passwd) (server-error envelope "invalid password")
	      (with client (car envelope)
		(ahash-set! server-logged-table client uid)
		(server-return envelope "ready")))))))

(tm-service (remote-eval cmd)
  (if (server-check-admin? envelope)
      (with ret (eval cmd)
        ;; (display* "remote-eval " cmd " -> " ret "\n")
        (server-return envelope ret))
      (server-error envelope "execution of commands is not allowed")))
