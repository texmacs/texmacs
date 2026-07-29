(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (cond ((== ret "ready")
           (client-public-preferences-then
             server
             (lambda (prefs)
               (display* "public preferences = " prefs "\n")
               (client-logout server)
               (quit-TeXmacs-code 0))))
          (else (quit-TeXmacs-code -1)))))
