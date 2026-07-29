;; Scenario: database operations
;; Services tested: remote-create-entry, remote-get-entry, remote-set-entry,
;;   remote-get-field, remote-set-field, remote-get-attributes,
;;   remote-search, remote-search-user, remote-get-user-pseudo,
;;   remote-get-user-name

(define db-pseudo (string-append "dbtester-" test-seed))
(define db-email (string-append "dbtester-" test-seed "@localhost"))
(define db-name "DB Tester")
(define db-pass "TeXmacs123!")

;; Mutable state: set during test
(define user-srv #f)
(define admin-srv #f)
(define entry-id #f)

(define (cleanup uid)
  (client-remote-eval admin-srv
    (list 'remote-delete-account uid)
    (lambda (_)
      (display* "scenario-db DONE.\n")
      (quit-TeXmacs-code 0))
    (on-error "cleanup")))

(define (step-user-lookup)
  (with-remote-search-user users user-srv
    `(("pseudo" ,db-pseudo))
    (check! "search-user"
      (and (list? users) (> (length users) 0))
      (string-append "user not found: " (object->string users)))
    (let ((uid (car users)))
      (with-remote-get-user-pseudo pseudo user-srv uid
        (check! "get-user-pseudo" (== pseudo db-pseudo)
          (string-append "expected " db-pseudo
            ", got: " (object->string pseudo)))
        (with-remote-get-user-name uname user-srv uid
          (check! "get-user-name" (== uname db-name)
            (string-append "expected " db-name
              ", got: " (object->string uname)))
          (cleanup uid))))))

(define (step-update-and-search)
  (remote-set-entry user-srv entry-id
    `(("name" ,(string-append "updated-" test-seed))
      ("tag" "updated-tag")
      ("description" "updated desc")))
  ;; Verify update
  (with-remote-get-entry entry2 user-srv entry-id
    (check! "set-entry"
      (and (list? entry2)
           (== (cadr (assoc "name" entry2))
               (string-append "updated-" test-seed)))
      (string-append "entry not updated: " (object->string entry2)))
    ;; Search by type + tag
    (with-remote-search results user-srv
      `(("type" "file") ("tag" "updated-tag"))
      (check! "search"
        (and (list? results) (member entry-id results))
        (string-append "id not in search results: "
          (object->string results)))
      (step-user-lookup))))

(define (step-set-and-get-field)
  (remote-set-field user-srv entry-id "description"
    (list "test description"))
  (with-remote-get-field desc user-srv entry-id "description"
    (check! "get-field"
      (and (list? desc) (== (car desc) "test description"))
      (string-append "expected (\"test description\"), got: "
        (object->string desc)))
    (with-remote-get-attributes attrs user-srv entry-id
      (check! "get-attributes"
        (and (list? attrs) (member "name" attrs) (member "description" attrs))
        (string-append "unexpected attrs: " (object->string attrs)))
      (step-update-and-search))))

(define (step-create-and-get-entry)
  (with-remote-create-entry id user-srv
    `(("type" "file")
      ("name" ,(string-append "test-entry-" test-seed))
      ("tag" "test-tag"))
    (check! "create-entry" (and id (string? id))
      (string-append "expected string id, got: " (object->string id)))
    (set! entry-id id)
    (with-remote-get-entry entry user-srv id
      (check! "get-entry"
        (and (list? entry)
             (assoc "name" entry)
             (== (cadr (assoc "name" entry))
                 (string-append "test-entry-" test-seed)))
        (string-append "unexpected entry: " (object->string entry)))
      (step-set-and-get-field))))

;; Entry point
(client-login-then test-host test-port
  test-admin-user `(tls-password ,test-admin-pass)
  (lambda (asrv admin-ret)
    (if (!= admin-ret "ready")
      (fail! "admin-login" admin-ret)
      (begin
        (set! admin-srv asrv)
        (setup-test admin-srv
          (fixture-create-account ,db-pseudo ,db-name ,db-pass
            ,db-email #f))
        (client-login-then test-host test-port
          db-pseudo `(tls-password ,db-pass)
          (lambda (srv ret)
            (if (!= ret "ready")
              (fail! "user-login" ret)
              (begin
                (set! user-srv srv)
                (step-create-and-get-entry)))))))))
