;; Scenario: account lifecycle — create, login, query, update, logout
;; Services tested: new-account, remote-login, remote-logged?,
;;   remote-get-account, remote-set-account, remote-logout, remote-get-accounts

(define test-pseudo (string-append "acct-" test-seed))
(define test-name "Account Tester")
(define test-pass "TeXmacs123!")
(define test-email (string-append "acct-" test-seed "@localhost"))

;; Mutable state: set after login / admin-login
(define user-srv #f)

(define (cleanup-and-finish admin-srv)
  (with-server-eval (uid) admin-srv
    `(server-find-user ,test-pseudo)
    (client-remote-eval admin-srv
      (list 'remote-delete-account uid)
      (lambda (_)
        (display* "scenario-accounts DONE.\n")
        (quit-TeXmacs-code 0))
      (on-error "cleanup"))))

(define (step-admin-verify-and-cleanup)
  (client-login-then test-host test-port
    test-admin-user `(tls-password ,test-admin-pass)
    (lambda (admin-srv admin-ret)
      (if (!= admin-ret "ready")
        (fail! "admin-login" admin-ret)
        (client-remote-eval admin-srv '(remote-get-accounts 100 0)
          (lambda (accounts)
            (check! "get-accounts" (list? accounts)
              "expected list of accounts")
            (cleanup-and-finish admin-srv))
          (on-error "get-accounts"))))))

(define (step-logout)
  (client-remote-eval user-srv '(remote-logout)
    (lambda (bye)
      (check! "logout" (== bye "bye")
        (string-append "expected 'bye', got: " (object->string bye)))
      (step-admin-verify-and-cleanup))
    (on-error "logout")))

(define (step-update-name)
  (client-remote-eval user-srv
    `(remote-set-account "me" (("name" "Updated Tester")))
    (lambda (set-ret)
      (check! "set-account" (== set-ret "done")
        (string-append "expected 'done', got: " (object->string set-ret)))
      ;; Verify name was updated
      (client-remote-eval user-srv '(remote-get-account "me")
        (lambda (info2)
          (check! "name-updated"
            (and (list? info2)
                 (== (cadr (assoc "name" info2)) "Updated Tester"))
            (string-append "name not updated: " (object->string info2)))
          (step-logout))
        (on-error "get-account-2")))
    (on-error "set-account")))

(define (step-get-account)
  (client-remote-eval user-srv '(remote-get-account "me")
    (lambda (info)
      (check! "get-account-pseudo"
        (and (list? info) (== (cadr (assoc "pseudo" info)) test-pseudo))
        (string-append "unexpected info: " (object->string info)))
      (check! "get-account-name"
        (== (cadr (assoc "name" info)) test-name)
        (string-append "unexpected name: " (object->string info)))
      (check! "get-account-email"
        (== (cadr (assoc "email" info)) test-email)
        (string-append "unexpected email: " (object->string info)))
      (step-update-name))
    (on-error "get-account")))

(define (step-check-logged)
  (client-remote-eval user-srv '(remote-logged?)
    (lambda (logged)
      (check! "logged?" (== logged "yes")
        (string-append "expected 'yes', got: " (object->string logged)))
      (step-get-account))
    (on-error "logged?")))

(define (step-login srv sname port)
  (client-remote-eval srv
    `(remote-login ,test-pseudo (tls-password ,test-pass))
    (lambda (ret)
      (check! "login" (== ret "ready")
        (string-append "expected 'ready', got: " (object->string ret)))
      (add-active-connection srv sname port test-pseudo)
      (set! user-srv srv)
      (step-check-logged))
    (on-error "login")))

;; Entry point: anonymous connection to create a new account
(with server (anonymous-client-start test-host test-port)
  (if (< server 0) (fail! "anon-connect" "could not connect anonymously")
    (let ((infos (make-ahash-table)))
      (ahash-set! infos "server-name" test-host)
      (ahash-set! infos "port" test-port)
      (ahash-set! infos "pseudo" test-pseudo)
      (ahash-set! infos "name" test-name)
      (ahash-set! infos "creds" `((tls-password ,test-pass)))
      (ahash-set! infos "email" test-email)
      (ahash-set! infos "agreed" "on")
      (client-new-account server infos
        ;; cb-pending
        (lambda (srv sname port pseudo creds)
          (fail! "new-account" "got 'pending' — expected 'done'"))
        ;; cb-done
        (lambda (srv sname port pseudo creds)
          (pass! "new-account")
          (step-login srv sname port))
        ;; cb-err
        (lambda (msg)
          (fail! "new-account" msg))))))
