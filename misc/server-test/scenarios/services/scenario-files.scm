;; Scenario: remote file and directory CRUD
;; Services tested: remote-dir-create, remote-dir-load, remote-dir-remove,
;;   remote-file-create, remote-file-load, remote-file-save,
;;   remote-file-remove, remote-identifier, remote-get-versions

(define file-pseudo (string-append "filer-" test-seed))
(define file-email (string-append "filer-" test-seed "@localhost"))
(define file-pass "TeXmacs123!")
(define dir-rname
  (string-append "localhost/~" file-pseudo "/test-dir-" test-seed))
(define file-rname
  (string-append "localhost/~" file-pseudo "/test-dir-" test-seed
    "/test-file-" test-seed ".tm"))
(define file-doc "(document \"hello world\")")
(define file-doc2 "(document \"updated content\")")

;; Mutable state: set after login
(define user-srv #f)
(define admin-srv #f)

(define (cleanup)
  (with-server-eval (uid) admin-srv
    `(server-find-user ,file-pseudo)
    (client-remote-eval admin-srv
      (list 'remote-delete-account uid)
      (lambda (_)
        (display* "scenario-files DONE.\n")
        (quit-TeXmacs-code 0))
      (on-error "cleanup"))))

(define (step-remove-file-and-dir)
  (client-remote-eval user-srv `(remote-file-remove ,file-rname)
    (lambda (rm-file)
      (pass! "file-remove")
      (client-remote-eval user-srv `(remote-dir-remove ,dir-rname)
        (lambda (rm-dir)
          (pass! "dir-remove")
          (cleanup))
        (on-error "dir-remove")))
    (on-error "file-remove")))

(define (step-dir-load)
  (client-remote-eval user-srv `(remote-dir-load ,dir-rname)
    (lambda (entries)
      (check! "dir-load"
        (and (list? entries) (> (length entries) 0))
        (string-append "expected non-empty dir, got: "
          (object->string entries)))
      (step-remove-file-and-dir))
    (on-error "dir-load")))

(define (step-identifier-and-versions)
  (client-remote-eval user-srv `(remote-identifier ,file-rname)
    (lambda (rid)
      (check! "identifier" (and rid (string? rid))
        (string-append "expected string rid, got: " (object->string rid)))
      (client-remote-eval user-srv `(remote-get-versions ,file-rname)
        (lambda (versions)
          (check! "versions"
            (and (list? versions) (>= (length versions) 2))
            (string-append "expected >= 2 versions, got: "
              (object->string versions)))
          (step-dir-load))
        (on-error "versions")))
    (on-error "identifier")))

(define (step-save-and-verify)
  (client-remote-eval user-srv
    `(remote-file-save ,file-rname ,file-doc2 "update")
    (lambda (saved)
      (pass! "file-save")
      (client-remote-eval user-srv `(remote-file-load ,file-rname)
        (lambda (loaded2)
          (check! "file-load-updated" (== loaded2 file-doc2)
            (string-append "expected updated doc, got: "
              (object->string loaded2)))
          (step-identifier-and-versions))
        (on-error "file-load-updated")))
    (on-error "file-save")))

(define (step-create-file-and-load)
  (client-remote-eval user-srv
    `(remote-file-create ,file-rname ,file-doc "initial")
    (lambda (create-result)
      (pass! "file-create")
      (client-remote-eval user-srv `(remote-file-load ,file-rname)
        (lambda (loaded)
          (check! "file-load" (== loaded file-doc)
            (string-append "expected doc, got: " (object->string loaded)))
          (step-save-and-verify))
        (on-error "file-load")))
    (on-error "file-create")))

(define (step-create-dir)
  (client-remote-eval user-srv `(remote-dir-create ,dir-rname)
    (lambda (dir-result)
      (pass! "dir-create")
      (step-create-file-and-load))
    (on-error "dir-create")))

;; Entry point
(client-login-then test-host test-port
  test-admin-user `(tls-password ,test-admin-pass)
  (lambda (asrv admin-ret)
    (if (!= admin-ret "ready")
      (fail! "admin-login" admin-ret)
      (begin
        (set! admin-srv asrv)
        (setup-test admin-srv
          (fixture-create-account ,file-pseudo "File Tester" ,file-pass
            ,file-email #f))
        (client-login-then test-host test-port
          file-pseudo `(tls-password ,file-pass)
          (lambda (srv ret)
            (if (!= ret "ready")
              (fail! "user-login" ret)
              (begin
                (set! user-srv srv)
                (step-create-dir)))))))))
