
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-chat.scm
;; DESCRIPTION : Sending messages and chatting, server side
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-chat)
  (:use (server server-tmfs)
        (server server-notifications)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat rooms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (server-chat-room-create uid name)
  (with-time-stamp #t
    (db-create-entry `(("type" "chat-room")
                       ("name" ,name)
                       ("owner" ,uid)
                       ("readable" "all")
                       ("writable" "all")))))

(tm-define (server-chat-room-remove uid)
  (with msgs (db-search `(("type" "chat-message") ("to" ,uid)))
    (for-each db-remove-entry msgs))
  (db-remove-entry uid))

(tm-service (remote-chat-room-create name)
  ;; Create chat room and return its identifier
  ;;(display* "remote-chat-room-create " name "\n")
  (with uid (server-get-user envelope)
    (with crid (server-chat-room-create uid name)
      (server-return envelope crid))))

(tm-define (chat-room-id name)
  (with l (db-search `(("type" "chat-room")
                       ("name" ,name)))
    (and (nnull? l) (car l))))

(tm-define (search-remote-identifier u)
  (:require (string-starts? (url->string u) "tmfs://chat/"))
  (chat-room-id (url->string (url-tail u))))

(tm-service (remote-list-chat-rooms)
  ;; Return list of chat rooms owned by the user as (name date) pairs
  ;;(display* "remote-list-chat-rooms\n")
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (l (db-search `(("type" "chat-room")
                           ("owner" ,uid))))
           (get-entry (lambda (id)
                        (let* ((name (db-get-field-first id "name" #f))
                               (date (db-get-field-first id "date" "")))
                          (list name date))))
           (entries (map get-entry l))
           (r (list-filter entries
                           (lambda (e) (not (string-starts? (car e) "mail-"))))))
      (server-return envelope r))))

;; Find first active participant (excluding uid being deleted)
;; Priority: writable users > readable users > message senders
(define (find-active-participant crid exclude-uid)
  (let* ((writable (db-get-field crid "writable"))
         (readable (db-get-field crid "readable"))
         (msg-ids (db-search `(("type" "chat-message") ("to" ,crid))))
         (senders (map (lambda (mid)
                         (db-get-field-first mid "from" "")) msg-ids))
         (exclude (list exclude-uid "all" ""))
         (candidates (list-difference
                       (append writable readable senders)
                       exclude)))
    (list-find candidates
      (lambda (uid) (server-get-user-info uid)))))

;; Remove all chat messages sent by a user
(tm-define (server-remove-user-chat-messages uid)
  (let* ((msgs (db-search `(("type" "chat-message") ("from" ,uid)))))
    (for-each db-remove-entry msgs)))

(define chat-room-messages (make-ahash-table))
(define chat-room-present  (make-ahash-table))

(define (msg-is-shared? msg)
  (== (msg-action msg) "share"))

(define (chat-room-retrieve crid)
  (with l (db-search `(("type" "chat-message")
                       ("to" ,crid)
                       (:order "date" #t)))
    (map chat-message-retrieve l)))

(define (chat-room-initialize crid)
  (when (not (ahash-ref chat-room-messages crid))
    (ahash-set! chat-room-messages crid (chat-room-retrieve crid))
    (ahash-set! chat-room-present  crid (list))))

(define (ensure-chat-room client crid)
  (chat-room-initialize crid)
  (with l (ahash-ref chat-room-present crid)
    (when (nin? client l)
      (ahash-set! chat-room-present crid (cons client l)))))

(tm-define (chat-room-messages-reset)
  (set! chat-room-messages (make-ahash-table)))

(tm-service (remote-chat-room-messages-reset)
  (chat-room-messages-reset)
  (server-return envelope "ok"))

(tm-service (remote-chat-room-open name)
  ;; Connect client to a chat room and return list of past messages
  ;;(display* "remote-chat-room-open " name "\n")
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (crid (chat-room-id name)))
      (cond ((not crid)
             (server-error envelope "Error: unknown chat room"))
            ((string-starts? name "mail-")
             (server-error envelope "Error: invalid name of chat room"))
            ((not (db-allow? crid uid "readable"))
             (server-error envelope "Error: access to chat room denied"))
            (else
             (ensure-chat-room client crid)
             (let* ((ms (ahash-ref chat-room-messages crid))
                    (cached
                      (map (lambda (t)
                             (tree->stree
                               (server-handle-cache (tmfs-car name) uid (stree->tree t)))) ms))
                    (w? (db-allow? crid uid "writable")))
               (server-return envelope (list w? cached))))))))

(tm-service (remote-mail-open)
  ;; Open mail while creating mail box (a chat room) if necessary
  ;;(display* "remote-chat-room-create " name "\n")
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (pseudo (or (user->pseudo uid) uid))
           (name (string-append "mail-" pseudo))
           (crid (or (chat-room-id name) (server-chat-room-create uid name))))
      (ensure-chat-room client crid)
      (server-return envelope (ahash-ref chat-room-messages crid)))))

(tm-service (remote-shared)
  (with (client msg-id) envelope
    (let* ((uid (server-get-user envelope))
           (pseudo (or (user->pseudo uid) uid))
           (name (string-append "mail-" pseudo))
           (crid (or (chat-room-id name) (server-chat-room-create uid name))))
      (ensure-chat-room client crid)
      (server-return
        envelope
        (list-filter (ahash-ref chat-room-messages crid) msg-is-shared?)))))

(define (pseudo-present? pseudo crid)
  (and-let* ((uid (server-find-user pseudo))
             (client (uid-logged? uid))
             (present (ahash-ref chat-room-present crid)))
    (in? client present)))

(tm-define (chat-room-notify mid)
  ;; Notify the arrival of a new message to all participants
  (let* ((crid (db-get-field-first mid "to" "unknown"))
         (name (db-get-field-first crid "name" "unknown"))
         (owner (db-get-field-first crid "owner" "unknown"))
         (dummy (chat-room-initialize crid))
         (old-l (ahash-ref chat-room-messages crid))
         (new-m (chat-message-retrieve mid))
         (new-l (rcons old-l new-m))
         (users (make-ahash-table))
         (shared-with (resource-shared-with name owner)))
    (ahash-set! chat-room-messages crid new-l)
    (if (string-starts? name "mail-")
      (server-push-message (rcons new-m mid))
      (for (msg shared-with)
        (when (not (pseudo-present? (msg-to msg) crid))
          (server-push-chat-notification (msg-to msg) name new-m mid))))
    (for (client (ahash-ref chat-room-present crid))
      (server-remote-eval client `(chat-room-receive ,name ,new-m)
        (lambda (ok?) (noop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sending messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-create-message uid msg)
  (let* ((rid (with-time-stamp #t
                (db-create-entry `(("type" "message")
                                   ("owner" ,uid)))))
         (name (repository-add rid "tm"))
         (fname (repository-get rid))
         (doc (convert msg "texmacs-stree" "texmacs-snippet")))
    (string-save doc fname)
    rid))

(tm-define (remote-send uid dest action msg)
  (cond ((== action "send-document")
         (and-with msg* (remote-create-message uid msg)
           (remote-send uid dest "send" msg*)))
        ((list? dest)
         (for (did dest)
           (remote-send uid did action msg)))
        ((== (db-get-field-first dest "type" #f) "chat-room")
         (with-time-stamp #t
           ;; Look up resource ID from URL for "share" action
           (with rid (if (== action "share") (search-remote-identifier msg) #f)
             (with mid (db-create-entry `(("type" "chat-message")
                                          ("action" ,action)
                                          ("from" ,uid)
                                          ("to" ,dest)
                                          ("message" ,msg)
                                          ,@(if rid
                                              `(("resource-id" ,rid))
                                              '())))
               (chat-room-notify mid)
               mid))))
        ((chat-room-id dest)
         (with crid (chat-room-id dest)
           (when (db-allow? crid uid "writable")
             (remote-send uid (chat-room-id dest) action msg))))
        ((string-starts? dest "mail-")
         (let* ((pseudo (string-drop dest 5))
                (user (or (pseudo->user pseudo) pseudo))
                (crid (server-chat-room-create user dest)))
           (remote-send uid crid action msg)))))

(tm-service (remote-send-message dest action msg)
  ;;(display* "remote-send-message " dest ", " action ", " msg "\n")
  (with uid (server-get-user envelope)
    (remote-send uid dest action msg)
    (server-return envelope #t)))
