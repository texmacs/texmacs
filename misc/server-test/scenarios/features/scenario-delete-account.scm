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

(client-login-then
  test-host
  test-port
  test-admin-user
  `(tls-password ,test-admin-pass)
  (lambda (server ret)
    (if (!= ret "ready")
      (fail! "login" ret)

      ;; -- Step 1: create fixtures -------------------------------------------
      (begin
        (setup-test server
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

        ;; -- Step 2: query setup state ----------------------------------------
        (with-server-eval (alice-uid bob-uid file-rid chat-rid live-rid) server
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

          ;; -- Pre-condition checks -------------------------------------------
          (check! "alice-created" alice-uid  "alice account not created")
          (check! "bob-created"   bob-uid    "bob account not created")
          (check! "file-created"  file-rid   "alice's file not created")
          (check! "chat-created"  chat-rid   "alice's chat room not created")
          (check! "live-created"  live-rid   "alice's live doc not created")

          ;; -- Step 3: delete alice -------------------------------------------
          (client-remote-eval server
            (list 'remote-delete-account alice-uid)
            (lambda (result)
              (check! "delete-result" (== result "done")
                (string-append "expected 'done', got: " (object->string result)))

              ;; -- Step 4: verify post-deletion state -------------------------
              (with-server-eval
                (alice-after file-owners chat-owners live-owners) server
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

                ;; -- Step 5: clean up bob -------------------------------------
                (client-remote-eval server
                  (list 'remote-delete-account bob-uid)
                  (lambda (_)
                    (display* "scenario-delete-account DONE.\n")
                    (quit-TeXmacs-code 0))
                  (on-error "cleanup-bob"))))
            (on-error "delete-account")))))))
