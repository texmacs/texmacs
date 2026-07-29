(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (cond ((== ret "ready")
           (client-remote-eval server `(remote-shared)
             (lambda (l)
               (display* "shared resources: " l "\n")
               (quit-TeXmacs-code 0))))
          (else (quit-TeXmacs-code -1)))))
