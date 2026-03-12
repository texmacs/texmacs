
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-fixtures.scm
;; DESCRIPTION : Test fixture helpers for creating server data
;; COPYRIGHT   : (C) 2026  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-fixtures)
  (:use (server server-base)
        (server server-tmfs)
        (server server-chat)
        (server server-notifications)
        (security password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Account creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-create-account pseudo name password email admin?)
  (let* ((credentials `((tls-password ,password)))
         (salt        (generate-salt))
         (credentials (server-add-salt credentials salt))
         (hiddens     (server-hide-credentials credentials)))
    (server-set-user-info #f pseudo name hiddens email admin?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-grant-permission rid pseudo attr)
  (let ((val (if (== pseudo "all") "all" (server-find-user pseudo))))
    (when val
      (db-set-field rid attr (cons val (db-get-field rid attr))))))

(tm-define (fixture-apply-permissions rid perms)
  (for-each
    (lambda (p)
       (let ((pseudo (first p))
             (r?     (second p))
             (w?     (third p))
             (o?     (fourth p))
             (share? (if (== (length p) 5) (fifth p)))
             (from (db-get-field-first rid "owner" #f)))
         (when r? (fixture-grant-permission rid pseudo "readable"))
         (when w? (fixture-grant-permission rid pseudo "writable"))
         (when o? (fixture-grant-permission rid pseudo "owner"))
         (when (and from share?) (generate-share-message from pseudo rid "localhost"))))
    perms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-create-file pseudo filename perms)
  (with-database (server-database)
    (with-user #t
      (let* ((uid    (server-find-user pseudo))
             (rname  (string-append "localhost/~" pseudo "/" filename))
             (result (server-file-create uid rname "(document \"\")" "fixture"))
             (rid    (and (pair? result) (== (car result) :created) (cadr result))))
        (when (and rid (pair? perms))
          (fixture-apply-permissions rid perms))
        rid))))

(tm-define (fixture-create-dir pseudo dirname perms)
  (with-database (server-database)
    (with-user #t
      (let* ((uid    (server-find-user pseudo))
             (rname  (string-append "localhost/~" pseudo "/" dirname))
             (result (server-dir-create uid rname))
             (rid    (and (pair? result) (== (car result) :created) (cadr result))))
        (when (and rid (pair? perms))
          (fixture-apply-permissions rid perms))
        rid))))

(tm-define (fixture-create-live pseudo name perms)
  (with-database (server-database)
    (with-time-stamp #t
      (with-user #t
        (let* ((uid (server-find-user pseudo))
               (rid (db-create-entry `(("type"  "live")
                                       ("name"  ,name)
                                       ("owner" ,uid)))))
          (repository-add rid "tm")
          (when (pair? perms)
            (fixture-apply-permissions rid perms))
          rid)))))

(tm-define (fixture-create-chat pseudo room-name perms)
  (with-database (server-database)
    (with-user #t
      (let* ((uid (server-find-user pseudo))
             (rid (server-chat-room-create uid room-name)))
        (when (and rid (pair? perms))
          (fixture-apply-permissions rid perms))
        rid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resource deletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-delete-user pseudo)
  (with-database (server-database)
    (let* ((uid (server-find-user pseudo))
           (children (db-search `(("owner" ,uid)))))
      (server-remove-user-chat-messages uid)
      (for (rid children)
           (let* ((rtype (db-get-field-first rid "type" "")))
             (cond
               ((== rtype "dir") (server-dir-remove-recursive rid))
               ((== rtype "file") (server-file-remove-complete rid))
               ((== rtype "live") (db-remove-entry rid)))))
      (server-mark-user-deleted uid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-send-chat-message from-pseudo dest)
  (with-database (server-database)
    (with-time-stamp #t
      (with-user #t
        (remote-send (server-find-user from-pseudo) dest "send-document"
                   '(document "test message"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-count-notifications pseudo kind)
  (with-database (server-database)
    (with-user #t
      (let* ((uid (server-find-user pseudo))
             (query `(("type" "notification")
                      ("owner" ,uid)
                      ,@(if (== kind 'all) '()
                            `(("kind" ,(symbol->string kind)))))))
        (length (db-search query))))))

(tm-define (fixture-clear-notifications pseudo kind)
  (with-database (server-database)
    (with-user #t
      (with uid (server-find-user pseudo)
        (server-clean-user-notifications uid kind)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sharing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fixture-resource-tmfs-url rid server-name)
  (let* ((rtype (db-get-field-first rid "type" #f))
         (name  (db-get-field-first rid "name" #f)))
    (cond ((== rtype "live")
           (string-append "tmfs://live/" server-name "/" name))
          ((== rtype "chat-room")
           (string-append "tmfs://chat/" server-name "/" name))
          ((== rtype "file")
           (string-append "tmfs://remote-file/" server-name "/" (resource->file-name rid)))
          ((== rtype "dir")
           (string-append "tmfs://remote-dir/" server-name "/" (resource->file-name rid)))
          (else #f))))

(tm-define (generate-share-message uid to rid server-name)
  "Create a share chat-message linking to a resource"
  (with-database (server-database)
    (with-time-stamp #t
      (with-user #t
        (remote-send uid (string-append "mail-" to) "share"
                     (fixture-resource-tmfs-url rid server-name))))))

(tm-define (fixture-share from-pseudo to-pseudo rid server-name)
 (generate-share-message from-pseudo to-pseudo rid server-name))
