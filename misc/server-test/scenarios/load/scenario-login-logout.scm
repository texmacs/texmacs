(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (cond ((== ret "ready")
           (sleep 1)
           (client-logout server)
           (quit-TeXmacs-code 0))
          (else (quit-TeXmacs-code -1)))))
