;; Scenario: chat rooms and messaging
;; Services tested: remote-chat-room-create, remote-list-chat-rooms,
;;   remote-chat-room-open, remote-send-message, remote-mail-open,
;;   remote-shared

(define alice-pseudo (string-append "alice-chat-" test-seed))
(define bob-pseudo (string-append "bob-chat-" test-seed))
(define alice-email (string-append "alice-chat-" test-seed "@localhost"))
(define bob-email (string-append "bob-chat-" test-seed "@localhost"))
(define chat-pass "TeXmacs123!")
(define room-name (string-append "room-" test-seed))

;; Mutable state: set during test
(define admin-srv #f)
(define alice-srv #f)

(define (cleanup)
  (with-server-eval (alice-uid bob-uid) admin-srv
    `(with-user #t
       (list (server-find-user ,alice-pseudo)
             (server-find-user ,bob-pseudo)))
    (client-remote-eval admin-srv
      (list 'remote-delete-account alice-uid)
      (lambda (_)
        (client-remote-eval admin-srv
          (list 'remote-delete-account bob-uid)
          (lambda (_)
            (display* "scenario-chat DONE.\n")
            (quit-TeXmacs-code 0))
          (on-error "cleanup-bob")))
      (on-error "cleanup-alice"))))

(define (step-bob-mail-and-shared)
  (client-login-then test-host test-port
    bob-pseudo `(tls-password ,chat-pass)
    (lambda (bob-srv bob-ret)
      (if (!= bob-ret "ready")
        (fail! "bob-login" bob-ret)
        (client-remote-eval bob-srv '(remote-mail-open)
          (lambda (mail-msgs)
            (check! "mail-open"
              (and (list? mail-msgs) (>= (length mail-msgs) 1))
              (string-append "expected >= 1 mail msg, got: "
                (object->string mail-msgs)))
            (client-remote-eval bob-srv '(remote-shared)
              (lambda (shared)
                (check! "shared"
                  (and (list? shared) (>= (length shared) 1))
                  (string-append "expected >= 1 shared, got: "
                    (object->string shared)))
                (cleanup))
              (on-error "shared")))
          (on-error "mail-open"))))))

(define (step-send-and-verify)
  (client-remote-eval alice-srv
    `(remote-send-message ,room-name "send-document" "hello from alice")
    (lambda (sent)
      (check! "send-message" (== sent #t)
        (string-append "expected #t, got: " (object->string sent)))
      ;; Re-open room — verify message present
      (client-remote-eval alice-srv
        `(remote-chat-room-open ,room-name)
        (lambda (open-ret2)
          (let ((msgs (cadr open-ret2)))
            (check! "message-received"
              (and (list? msgs) (>= (length msgs) 1))
              (string-append "expected >= 1 msg, got: "
                (object->string msgs))))
          ;; Share a resource with bob via fixture
          (setup-test admin-srv
            (let ((fid (fixture-create-file
                         ,alice-pseudo "shared-for-bob.tm"
                         (list (list ,bob-pseudo #t #f #f)))))
              (fixture-share ,alice-pseudo ,bob-pseudo fid "localhost")))
          (step-bob-mail-and-shared))
        (on-error "chat-room-open-2")))
    (on-error "send-message")))

(define (step-create-and-open-room)
  (client-remote-eval alice-srv `(remote-chat-room-create ,room-name)
    (lambda (crid)
      (check! "chat-room-create" (and crid (string? crid))
        (string-append "expected string crid, got: " (object->string crid)))
      (client-remote-eval alice-srv '(remote-list-chat-rooms)
        (lambda (rooms)
          (check! "list-chat-rooms"
            (and (list? rooms)
                 (list-find rooms (lambda (r) (== (car r) room-name))))
            (string-append "room not in list: " (object->string rooms)))
          (client-remote-eval alice-srv
            `(remote-chat-room-open ,room-name)
            (lambda (open-ret)
              (check! "chat-room-open"
                (and (list? open-ret) (car open-ret))
                (string-append "expected (#t ...), got: "
                  (object->string open-ret)))
              (step-send-and-verify))
            (on-error "chat-room-open")))
        (on-error "list-chat-rooms")))
    (on-error "chat-room-create")))

;; Entry point
(client-login-then test-host test-port
  test-admin-user `(tls-password ,test-admin-pass)
  (lambda (asrv admin-ret)
    (if (!= admin-ret "ready")
      (fail! "admin-login" admin-ret)
      (begin
        (set! admin-srv asrv)
        (setup-test admin-srv
          (fixture-create-account ,alice-pseudo "Alice Chat" ,chat-pass
            ,alice-email #f)
          (fixture-create-account ,bob-pseudo "Bob Chat" ,chat-pass
            ,bob-email #f))
        (client-login-then test-host test-port
          alice-pseudo `(tls-password ,chat-pass)
          (lambda (asrv2 alice-ret)
            (if (!= alice-ret "ready")
              (fail! "alice-login" alice-ret)
              (begin
                (set! alice-srv asrv2)
                (step-create-and-open-room)))))))))
