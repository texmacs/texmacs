(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (cond ((== ret "ready")
           (client-get-accounts-then server 10 0
             (lambda (users)
               (display* "got users = " users "\n")
               (quit-TeXmacs-code 0))))
          (else
            (display* "error: " ret "\n")
            (quit-TeXmacs-code -1)))))
