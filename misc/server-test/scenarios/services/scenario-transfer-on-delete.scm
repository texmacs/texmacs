;; Scenario: delete a user account and verify shared resources transfer
;; to the remaining co-reader.
;;
;; Resources tested: file, chat-room, live document.
;; All fixture pseudo names embed the test seed so parallel runs don't collide.

(define alice-pseudo (string-append "alice-del-" test-seed))
(define bob-pseudo (string-append "bob-del-" test-seed))
(define alice-email (string-append "alice-del-" test-seed "@localhost"))
(define bob-email (string-append "bob-del-" test-seed "@localhost"))
(define live-name (string-append "alice-live-" test-seed))

;; Mutable state: set during test
(define admin-srv #f)
(define alice-uid #f)
(define bob-uid #f)
(define file-rid #f)
(define chat-rid #f)
(define live-rid #f)

(define (cleanup-bob)
  (client-remote-eval admin-srv
    (list 'remote-delete-account bob-uid)
    (lambda (_)
      (display* "scenario-transfer-on-delete DONE.\n")
      (quit-TeXmacs-code 0))
    (on-error "cleanup-bob")))

(define (step-verify-transfers)
  (with-server-eval
    (alice-after file-owners chat-owners live-owners) admin-srv
    `(with-user #t
       (list (server-find-user ,alice-pseudo)
             (db-get-field ,file-rid "owner")
             (db-get-field ,chat-rid "owner")
             (db-get-field ,live-rid "owner")))
    (check! "alice-deleted"
      (not alice-after)
      "alice account still exists after deletion")
    (check! "file-transferred-to-bob"
      (and (pair? file-owners) (== (car file-owners) bob-uid))
      (string-append "file not owned by bob: "
        (object->string file-owners)))
    (check! "chat-transferred-to-bob"
      (and (pair? chat-owners) (== (car chat-owners) bob-uid))
      (string-append "chat not owned by bob: "
        (object->string chat-owners)))
    (check! "live-transferred-to-bob"
      (and (pair? live-owners) (== (car live-owners) bob-uid))
      (string-append "live doc not owned by bob: "
        (object->string live-owners)))
    (cleanup-bob)))

(define (step-delete-alice)
  (client-remote-eval admin-srv
    (list 'remote-delete-account alice-uid)
    (lambda (result)
      (check! "delete-result" (== result "done")
        (string-append "expected 'done', got: " (object->string result)))
      (step-verify-transfers))
    (on-error "delete-account")))

(define (step-query-and-check)
  (with-server-eval (a-uid b-uid f-rid c-rid l-rid) admin-srv
    `(with-user #t
       (let* ((a     (server-find-user ,alice-pseudo))
              (b     (server-find-user ,bob-pseudo))
              (files (db-search (list (list "type" "file")      (list "owner" a))))
              (chats (db-search (list (list "type" "chat-room") (list "owner" a))))
              (lives (db-search (list (list "type" "live")      (list "owner" a)))))
         (list a b
               (and (pair? files) (car files))
               (and (pair? chats) (car chats))
               (and (pair? lives) (car lives)))))
    ;; Pre-condition checks
    (check! "alice-created" a-uid  "alice account not created")
    (check! "bob-created"   b-uid  "bob account not created")
    (check! "file-created"  f-rid  "alice's file not created")
    (check! "chat-created"  c-rid  "alice's chat room not created")
    (check! "live-created"  l-rid  "alice's live doc not created")
    ;; Store in mutable state for later steps
    (set! alice-uid a-uid)
    (set! bob-uid   b-uid)
    (set! file-rid  f-rid)
    (set! chat-rid  c-rid)
    (set! live-rid  l-rid)
    (step-delete-alice)))

;; Entry point
(client-login-then test-host test-port
  test-admin-user `(tls-password ,test-admin-pass)
  (lambda (srv ret)
    (if (!= ret "ready")
      (fail! "login" ret)
      (begin
        (set! admin-srv srv)
        (setup-test admin-srv
          (fixture-create-account ,alice-pseudo "Alice Delete" "TeXmacs123!"
            ,alice-email #f)
          (fixture-create-account ,bob-pseudo "Bob Delete" "TeXmacs123!"
            ,bob-email #f)
          (let ((fid (fixture-create-file ,alice-pseudo "alice-shared.tm"
                       (list (list ,bob-pseudo #t #f #f)))))
            (fixture-share ,alice-pseudo ,bob-pseudo fid "localhost"))
          (let ((cid (fixture-create-chat ,alice-pseudo "alice-room"
                       (list (list ,bob-pseudo #t #f #f)))))
            (fixture-share ,alice-pseudo ,bob-pseudo cid "localhost"))
          (let ((lid (fixture-create-live ,alice-pseudo ,live-name
                       (list (list ,bob-pseudo #t #f #f)))))
            (fixture-share ,alice-pseudo ,bob-pseudo lid "localhost")))
        (step-query-and-check)))))
