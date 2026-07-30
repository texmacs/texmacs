
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-notifications-test.scm
;; DESCRIPTION : Tests for chat room and mail notification paths
;; COPYRIGHT   : (C) 2026  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-notifications-test)
  (:use (server server-chat)
        (server server-notifications)
        (server server-fixtures)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup / teardown
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notif-test-setup!)
  (fixture-create-account "alice" "Alice A" "AlicePass1!" "alice@test" #f)
  (fixture-create-account "bob"   "Bob B"   "BobPass1!"   "bob@test"   #f)
  (fixture-create-account "carol" "Carol C" "CarolPass1!" "carol@test" #f))

(define (notif-test-teardown!)
  (fixture-clear-notifications "alice" 'all)
  (fixture-clear-notifications "bob"   'all)
  (fixture-clear-notifications "carol" 'all)
  (chat-room-messages-reset))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: mail room notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-mail-notification)
  (integration-test-group
   "mail notification creates message kind" "mail-notif"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "sending to mail room creates 1 message notification"
     (with-database (server-database)
       (with-user #t
         (let* ((bob-uid (server-find-user "bob"))
                (mailbox (string-append "mail-bob"))
                (crid (or (chat-room-id mailbox)
                          (server-chat-room-create bob-uid mailbox))))
           (fixture-send-chat-message "alice" crid)
           (fixture-count-notifications "bob" 'message))))
     1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: chat room notification with invitation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-chat-notification-invited)
  (integration-test-group
   "chat notification for invited user" "chat-notif-invited"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "invited user gets 1 chat notification"
     (with-database (server-database)
       (with-user #t
         (let* ((crid (fixture-create-chat "alice" "test-room" '()))
                (dummy (fixture-share "alice" "bob" crid "localhost")))
           (fixture-send-chat-message "alice" crid)
           (fixture-count-notifications "bob" 'chat))))
     1)

   (test "uninvited user gets 0 notifications"
     (with-database (server-database)
       (with-user #t
         (let* ((crid (fixture-create-chat "alice" "test-room-2" '()))
                (dummy (fixture-share "alice" "bob" crid "localhost")))
           (fixture-send-chat-message "alice" crid)
           (fixture-count-notifications "carol" 'chat))))
     0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: no self-notification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-no-self-notification)
  (integration-test-group
   "owner does not notify self" "no-self-notif"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "owner gets 0 notifications in own room"
     (with-database (server-database)
       (with-user #t
         (let* ((crid (fixture-create-chat "alice" "own-room" '())))
           (fixture-send-chat-message "alice" crid)
           (fixture-count-notifications "alice" 'all))))
     0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: unshared room produces no notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-unshared-room)
  (integration-test-group
   "unshared room produces no notifications" "unshared-room"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "message in unshared room creates 0 notifications"
     (with-database (server-database)
       (with-user #t
         (let* ((crid (fixture-create-chat "alice" "empty-room" '())))
           (fixture-send-chat-message "alice" crid)
           (+ (fixture-count-notifications "alice" 'all)
              (fixture-count-notifications "bob"   'all)
              (fixture-count-notifications "carol" 'all)))))
     0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: cross-room isolation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-cross-room-isolation)
  (integration-test-group
   "sharing other resources does not trigger chat notification" "cross-room"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "file shared with bob, chat message in different room => 0 chat notifs"
     (with-database (server-database)
       (with-user #t
         (let* ((file-rid (fixture-create-file "alice" "doc.tm" '()))
                (crid (fixture-create-chat "alice" "isolated-room" '()))
                (dummy (fixture-share "alice" "bob" file-rid "localhost")))
           (fixture-send-chat-message "alice" crid)
           (fixture-count-notifications "bob" 'chat))))
     0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: cleanup removes notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-notification-cleanup)
  (integration-test-group
   "cleanup removes notifications" "notif-cleanup"
   (notif-test-setup!)
   (notif-test-teardown!)

   (test "clear removes all chat notifications"
     (with-database (server-database)
       (with-user #t
         (let* ((crid (fixture-create-chat "alice" "cleanup-room" '()))
                (dummy (fixture-share "alice" "bob" crid "localhost")))
           (fixture-send-chat-message "alice" crid)
           (fixture-send-chat-message "alice" crid)
           (fixture-clear-notifications "bob" 'chat)
           (fixture-count-notifications "bob" 'chat))))
     0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-server-notifications)
  (let ((n (+ (regtest-mail-notification)
              (regtest-chat-notification-invited)
              (regtest-no-self-notification)
              (regtest-unshared-room)
              (regtest-cross-room-isolation)
              (regtest-notification-cleanup))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of server-notifications: ok\n")))
