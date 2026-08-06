
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-backup.scm
;; DESCRIPTION : Periodic rsync backup of server data with retention policy
;; COPYRIGHT   : (C) 2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-backup)
  (:use (server server-authentication)
        (kernel texmacs tm-dialogue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-server-backup-destination var val)
  (when (server-started?)
    (if (string-null? val)
        (server-log-write `info "Server backup disabled (no destination set)")
        (server-log-write `info
          (string-append "Server backup destination set to " val)))))

(define (notify-server-backup-interval var val)
  (when (server-started?)
    (server-log-write `info
      (string-append "Server backup interval set to " val " hours"))))

(define (notify-server-backup-keep var val) #f)

(define (notify-server-service-backup var val)
  (when (server-started?)
    (server-log-write `info
                      (string-append "Allowing server periodic backup turned " val))))

(define-preferences
  ("server service backup" "off"
   notify-server-service-backup)
  ("server backup destination" ""
   notify-server-backup-destination)
  ("server backup interval" "24"
   notify-server-backup-interval)
  ("server backup keep hourly"  "24" notify-server-backup-keep)
  ("server backup keep daily"   "30" notify-server-backup-keep)
  ("server backup keep monthly" "12" notify-server-backup-keep)
  ("server backup keep yearly"  "5"  notify-server-backup-keep))

(tm-define (server-backup-enabled?)
  (== (get-preference "server service backup") "on"))

(tm-define (server-backup-set-enabled val)
  (set-preference "server service backup" val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Snapshot operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Snapshot directories are named YYYY-MM-DDTHH:MM:SS (19 chars, ISO 8601).
;; ISO timestamps sort lexicographically = chronologically.
;;
;; Directory listing goes through TeXmacs's own cross-platform URL layer.
;; Mode "d" = directories only; mode "dr" = directories + regular files.

;; Return snapshot names (strings) sorted oldest first.
(tm-define (snapshot-name? s)
  (and (== (string-length s) 19)
       (char-numeric? (string-ref s 0))
       (== (string-ref s 4) #\-)
       (== (string-ref s 10) #\T)))

(tm-define (list-snapshots dest)
  (if (not (url-exists? dest))
      '()
      (let* ((u    (string->url dest))
             (wc   (url-wildcard "*"))
             (hits (url->list (url-expand (url-complete (url-append u wc) "d"))))
             (names (filter snapshot-name? (map url-basename hits))))
        (sort names string<?))))

(define (server-backup-rm u) (system-rmdir-recursive u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retention
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For each time granularity, keep the newest snapshot that falls in each
;; distinct bucket, up to the configured count.  Snapshots are processed
;; newest-first so the first one seen per bucket is automatically the newest.
;;
;; Bucket keys are derived directly from the ISO name string:
;;   hourly  -> "YYYY-MM-DDTHH"   (first 13 chars)
;;   daily   -> "YYYY-MM-DD"      (first 10 chars)
;;   monthly -> "YYYY-MM"         (first  7 chars)
;;   yearly  -> "YYYY"            (first  4 chars)
;;
;; Weekly is omitted: keeping enough dailies (e.g. 30) covers
;; the same range
;;
;; Hard-link semantics mean deleting a snapshot never affects any other:
;; each file's inode is freed only when its last hard link is removed.

;; Apply retention and remove snapshots outside the policy.
;; The newest snapshot is always preserved as a safety net even if all
;; keep-* counts are zero.
(tm-define (server-backup-prune dest)
  (let* ((keep-h (or (string->number (get-preference "server backup keep hourly"))  0))
         (keep-d (or (string->number (get-preference "server backup keep daily"))   0))
         (keep-m (or (string->number (get-preference "server backup keep monthly")) 0))
         (keep-y (or (string->number (get-preference "server backup keep yearly"))  0))
         (snaps  (reverse (list-snapshots dest))) ;; newest first
         (marked (make-ahash-table))
         (seen-h (make-ahash-table))
         (seen-d (make-ahash-table))
         (seen-m (make-ahash-table))
         (seen-y (make-ahash-table))
         (cnt-h 0) (cnt-d 0) (cnt-m 0) (cnt-y 0))
    ;; always keep the most recent snapshot
    (when (pair? snaps)
      (ahash-set! marked (car snaps) #t))
    ;; Mark snapshots to keep under each retention bucket (newest first)
    (for-each
      (lambda (name)
        (let ((kh (substring name 0 13))
              (kd (substring name 0 10))
              (km (substring name 0 7))
              (ky (substring name 0 4)))
          (when (and (< cnt-h keep-h) (not (ahash-ref seen-h kh)))
            (ahash-set! seen-h kh #t) (set! cnt-h (+ cnt-h 1))
            (ahash-set! marked name #t))
          (when (and (< cnt-d keep-d) (not (ahash-ref seen-d kd)))
            (ahash-set! seen-d kd #t) (set! cnt-d (+ cnt-d 1))
            (ahash-set! marked name #t))
          (when (and (< cnt-m keep-m) (not (ahash-ref seen-m km)))
            (ahash-set! seen-m km #t) (set! cnt-m (+ cnt-m 1))
            (ahash-set! marked name #t))
          (when (and (< cnt-y keep-y) (not (ahash-ref seen-y ky)))
            (ahash-set! seen-y ky #t) (set! cnt-y (+ cnt-y 1))
            (ahash-set! marked name #t))))
      snaps)
    ;; Delete unmarked snapshots (oldest first)
    (for-each
      (lambda (name)
        (when (not (ahash-ref marked name))
          (server-log-write `info
            (string-append "Pruning backup snapshot: " name))
          (server-backup-rm
            (url-append (string->url dest) (string->url name)))))
      (list-snapshots dest))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rsync incremental snapshot backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Files to back up:
;;   $TEXMACS_HOME_PATH/server/
;;     global.tmdb          -- main DB (binary append-only log, crash-safe)
;;     users.scm            -- user accounts (Scheme, atomically rewritten)
;;     pending-users.scm    -- pending accounts
;;     reset-credentials-users.scm
;;     email-*.txt          -- email templates
;;     license.tm           -- server license document
;;     0-9/.../             -- hashed dirs with file content (write-once)
;;
;; Running rsync while the server is live is safe:
;;   - global.tmdb: sync_databases() is called every interpose cycle and
;;     runs through the full 30s idle window before this task fires, so
;;     pending is guaranteed empty and the file is fully consistent on disk.
;;     (The file is NOT purely append-only: large changes do a full atomic
;;     rename, and the replay reader does not tolerate truncated records.)
;;   - hashed file dirs are write-once (new version = new path)
;;   - users.scm is atomically rewritten via save-object
;;
;; Incremental snapshot strategy (hard-link deduplication):
;;   Each run creates DEST/YYYY-MM-DDTHH:MM:SS/ and hard-links unchanged
;;   files from the previous snapshot directory, so every snapshot is a
;;   self-contained full view of the data while only changed/new files use
;;   extra disk space.  No `latest` symlink is needed.
;;
;; --link-dest points at the previous snapshot directory (from list-snapshots),
;; so no `latest` symlink is needed.
;; Retention pruning runs at the end of each backup (server-backup-prune).

(define (has-rsync?) (== (system "rsync --version") 0))

(tm-define (has-rsync-ext?)
  (:secure #t)
  (if (has-rsync?) "true" "false"))


(tm-define (server-backup-run)
  (let* ((dest  (url-concretize (string->url (get-preference "server backup destination"))))
         (src   (url-concretize "$TEXMACS_HOME_PATH/server"))
         (ts    (string-replace (pretty-date (current-time) "iso8601") ":" "-"))
         (snaps (list-snapshots dest))
         (prev  (and (pair? snaps)
                     (string-append dest "/" (cAr snaps))))
         (new   (string-append dest "/" ts))
         (cmd   (string-append
                  "rsync -a --partial"
                  (if prev (string-append " --link-dest='" prev "'") "")
                  " '" src "/'"
                  " '" new "/'")))
    (cond
	  ((not (server-backup-enabled?))
	   (server-log-write `info "Server backup disabled"))
	  ((string-null? dest)
	   (server-log-write `notice "No backup destination configuration"))
	  ((not (has-rsync?))
	   (server-log-write `notice "rsync binary not found"))
	  (else
		(begin
		  (system-mkdir new)
		  (server-log-write `info (string-append "Launching backup: " cmd))
		  (eval-system cmd)
		  (server-log-write `info
							(string-append "Backup snapshot created: " new))
		  (server-backup-prune dest))))))

(tm-define (server-backup-register)
  (let* ((dest       (get-preference "server backup destination"))
         (interval-h (string->number (get-preference "server backup interval"))))
    (cond
	  ((not (server-mode?))
	   (server-log-write `info "Server backup not registered: not in server mode"))
	  ((not (server-backup-enabled?))
	   (server-log-write `info "Server backup disabled"))
	  ((string-null? dest)
	   (server-log-write `info "Server backup not registered: no destination"))
	  ((not (has-rsync?))
	   (server-log-write `notice "rsync binary not found"))
	  ((or (not interval-h) (<= interval-h 0))
	   (server-log-write `warning "Server backup not registered: invalid interval"))
	  (else
		(with interval (* interval-h 3600 1000)
			  (server-log-write
				`notice
				(string-append "Server backup registered: every "
							   (number->string interval-h) "h → " dest))
			  (delayed (:on-cpu-idle interval) (server-backup-run)))))))

(on-entry (server-backup-register))
