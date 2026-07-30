
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-tmfs-test.scm
;; DESCRIPTION : Tests for server tmfs operations
;; COPYRIGHT   : (C) 2026  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-tmfs-test)
  (:use (server server-tmfs)
        (server server-chat)
        (server server-fixtures)))

(define (create-accounts!)
  (display* "creating fixture accounts\n")
  (fixture-create-account "alice" "Alice A" "AlicePass1!" "alice@test" #f)
  (fixture-create-account "bob"   "Bob B"   "BobPass1!"   "bob@test"   #f)
  (fixture-create-account "carol" "Carol C" "CarolPass1!" "carol@test" #f)
  (fixture-create-dir "alice" "parent" '())
  (with rid (fixture-create-file "alice" "parent/shared" '(("bob" #t #t #f)))
    (generate-share-message "alice" "bob" rid "localhost"))
  (with rid (fixture-create-live "alice" "live-shared" '(("carol" #t #f #f)))
    (generate-share-message "alice" "carol" rid "localhost"))
  (with rid (fixture-create-chat "alice" "chat-shared" '(("carol" #t #f #f)))
    (generate-share-message "alice" "carol" rid "localhost"))
  (fixture-create-file "alice" "unshared" '())
  (fixture-create-live "alice" "live-unshared" '())
  (fixture-create-chat "alice" "chat-unshared" '())
  (fixture-send-chat-message "bob" "mail-alice")
  ;; Resources owned by others that alice participates in
  (fixture-create-file "bob"   "bob-doc"  '(("alice" #t #t #f)))
  (fixture-create-live "carol" "carol-live" '(("alice" #t #f #f))))

(define (delete-accounts!)
  (fixture-delete-user "alice")
  (display* "deleted alice\n")
  (fixture-delete-user "bob")
  (display* "deleted bob\n")
  (fixture-delete-user "carol")
  (display* "deleted carol\n"))

(tm-define (regtest-deletion-plan)
  (integration-test-group
    "deletion plan with shared and unshared resources" "deletion-plan"
    (create-accounts!)
    (delete-accounts!)
    (test "deletion plan"
      (with-database (server-database)
        (server-deletion-plan-entries "alice"))
      `((("mail-alice" "chat-room")
         ("chat-unshared" "chat-room")
         ("live-unshared" "live")
         ("unshared" "file"))
        (("chat-shared" "chat-room")
         ("live-shared" "live")
         ("shared" "file"))))
    (test "exec deletion plan"
      (with-database (server-database)
        (with mail-alice (db-search '(("name" "mail-alice")))
          (server-execute-deletion-plan "alice")
          (+ (length (db-search '(("type" "file") ("owner" "alice"))))
             (length (db-search '(("type" "dir") ("owner" "alice"))))
             (length (db-search '(("type" "chat-room") ("owner" "alice"))))
             (length (db-search '(("type" "live") ("owner" "alice"))))
             (length (db-search '(("type" "chat-message") ("to" ,@mail-alice)))))))
      5)
    (test "scrub removes alice from all ACLs"
      (with-database (server-database)
        (server-scrub-participations "alice")
        (+ (length (db-search '(("owner"    "alice"))))
           (length (db-search '(("readable" "alice"))))
           (length (db-search '(("writable" "alice"))))))
      0)))

