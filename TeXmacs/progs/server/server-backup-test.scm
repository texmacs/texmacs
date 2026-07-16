
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-backup-test.scm
;; DESCRIPTION : Integration tests for server-backup-prune
;; COPYRIGHT   : (C) 2026  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-backup-test)
  (:use (server server-backup)
        (server server-fixtures)))

(define test-backup-dir (string-append "/tmp/texmacs-backup-test-"
                                       (number->string (current-time))))

(define (create-snapshot! name)
  (system-mkdir (string-append test-backup-dir "/" name)))

(define (create-test-snapshots!)
  (system-mkdir test-backup-dir)
  ;; Day 1 (2025-01-15): 2 hourly snapshots
  (create-snapshot! "2025-01-15T08-00-00")
  (create-snapshot! "2025-01-15T09-00-00")
  ;; Day 2 (2025-01-16): 2 hourly snapshots
  (create-snapshot! "2025-01-16T10-00-00")
  (create-snapshot! "2025-01-16T11-00-00")
  ;; Day 3 (2025-01-17): 2 hourly snapshots
  (create-snapshot! "2025-01-17T12-00-00")
  (create-snapshot! "2025-01-17T13-00-00"))

(define (cleanup-test-snapshots!)
  (when (url-exists? test-backup-dir)
    (system-rmdir-recursive test-backup-dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: snapshot-name? predicate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-snapshot-name?)
  (regression-test-group
   "snapshot name validation" "snapshot-name"
   snapshot-name? :none
   (test "valid ISO snapshot"
     "2025-01-15T08-00-00" #t)
   (test "too short"
     "2025-01-15" #f)
   (test "missing T separator"
     "2025-01-15-08-00-00" #f)
   (test "non-numeric start"
     "backup-2025-01-15T08" #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: list-snapshots ordering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-list-snapshots)
  (integration-test-group
   "snapshot listing" "list-snapshots"
   (create-test-snapshots!)
   (cleanup-test-snapshots!)
   (test "returns all 6 snapshots sorted oldest first"
     (list-snapshots test-backup-dir)
     '("2025-01-15T08-00-00" "2025-01-15T09-00-00"
       "2025-01-16T10-00-00" "2025-01-16T11-00-00"
       "2025-01-17T12-00-00" "2025-01-17T13-00-00"))))

(define (regtest-list-snapshots-empty)
  (integration-test-group
   "snapshot listing empty" "list-snapshots-empty"
   (system-mkdir test-backup-dir)
   (cleanup-test-snapshots!)
   (test "empty dir returns empty list"
     (list-snapshots test-backup-dir)
     '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: server-backup-prune retention
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-backup-prune)
  (integration-test-group
   "backup prune retention" "backup-prune"
   (begin
     (create-test-snapshots!)
     (set-preference "server backup keep hourly"  "3")
     (set-preference "server backup keep daily"   "2")
     (set-preference "server backup keep monthly" "1")
     (set-preference "server backup keep yearly"  "0"))
   (cleanup-test-snapshots!)

   ;; Newest-first processing:
   ;;   13h -> hourly 1, daily 1 (17th), monthly 1
   ;;   12h -> hourly 2
   ;;   11h -> hourly 3, daily 2 (16th)
   ;;   10h -> not kept (hourly full, daily 16th seen)
   ;;   09h -> not kept
   ;;   08h -> not kept
   ;; + newest always kept (13h, already marked)
   (test "keeps 3 snapshots with hourly=3 daily=2 monthly=1"
     (begin
       (server-backup-prune test-backup-dir)
       (list-snapshots test-backup-dir))
     '("2025-01-16T11-00-00" "2025-01-17T12-00-00" "2025-01-17T13-00-00"))))

(define (regtest-backup-prune-safety-net)
  (integration-test-group
   "backup prune safety net" "backup-prune-safety"
   (begin
     (create-test-snapshots!)
     (set-preference "server backup keep hourly"  "0")
     (set-preference "server backup keep daily"   "0")
     (set-preference "server backup keep monthly" "0")
     (set-preference "server backup keep yearly"  "0"))
   (cleanup-test-snapshots!)

   (test "newest snapshot always preserved as safety net"
     (begin
       (server-backup-prune test-backup-dir)
       (list-snapshots test-backup-dir))
     '("2025-01-17T13-00-00"))))

(define (regtest-backup-prune-empty)
  (integration-test-group
   "backup prune empty dir" "backup-prune-empty"
   (system-mkdir test-backup-dir)
   (cleanup-test-snapshots!)

   (test "empty directory does not crash"
     (begin
       (server-backup-prune test-backup-dir)
       (list-snapshots test-backup-dir))
     '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: server-backup-run creates a valid snapshot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-backup-dest (string-append "/tmp/texmacs-backup-dest-"
                                        (number->string (current-time))))

(define (create-test-server-data!)
  (fixture-create-account "testuser" "Test User" "TestPass123!"
                          "test@localhost" #f)
  (fixture-create-file "testuser" "doc.tm" '()))

(define (cleanup-backup-dest!)
  (when (url-exists? test-backup-dest)
    (system-rmdir-recursive test-backup-dest)))

(define (regtest-backup-run)
  (integration-test-group
   "backup run" "backup-run"
   (begin
     (create-test-server-data!)
     (system-mkdir test-backup-dest)
     (set-preference "server backup destination" test-backup-dest)
     (set-preference "server backup keep hourly" "24")
     (set-preference "server backup keep daily" "0")
     (set-preference "server backup keep monthly" "0")
     (set-preference "server backup keep yearly" "0"))
   (cleanup-backup-dest!)

   (test "snapshot is created and matches source"
     (begin
       (server-backup-run)
       (let* ((snaps (list-snapshots test-backup-dest))
              (snap-dir (string-append test-backup-dest "/" (car snaps)))
              (src (url-concretize "$TEXMACS_HOME_PATH/server"))
              (diff-output (eval-system
                            (string-append "diff -r '" src "' '" snap-dir "'"))))
         (list (length snaps) (string-null? diff-output))))
     '(1 #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-server-backup)
  (let ((n (+ (regtest-snapshot-name?)
              (regtest-list-snapshots)
              (regtest-list-snapshots-empty)
              (regtest-backup-prune)
              (regtest-backup-prune-safety-net)
              (regtest-backup-prune-empty)
              (regtest-backup-run))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of server-backup: ok\n")))
