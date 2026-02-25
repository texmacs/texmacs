(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (cond ((== ret "ready")
           (with-remote-accounts users server 10 0
             (display* "got users = " users " and message = " msg "\n")
             (quit-TeXmacs-code 0)))
          (else
            (display* "error: " ret "\n")
            (quit-TeXmacs-code -1)))))
