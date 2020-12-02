
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-chat.scm
;; DESCRIPTION : Sending messages and chatting, client side
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-chat)
  (:use (client client-tmfs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat room urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (chat-room-url? u)
  (string-starts? (url->string u) "tmfs://chat/"))

(tm-define (chat-rooms-url? u)
  (string-starts? (url->string u) "tmfs://chat-rooms/"))

(tm-define (mail-box-url? u)
  (and (chat-room-url? u)
       (string-starts? (url->string (url-tail u)) "mail-")))

(define (chat-room-name u)
  (cAr (tmfs->list (url->string u))))

(define (chat-room-server u)
  (and (chat-room-url? u)
       (and-let* ((name (string-drop (url->string u) 12))
                  (sname (tmfs-car name)))
         (client-find-server sname))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Receiving and sending messages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message->share u)
  (let* ((name (url->string (url-tail u)))
         (h `(hlink ,name ,(url->string u))))
    (if (chat-room-url? u)
        `(concat "You were invited to join the chat room " ,h ".")
        `(concat "The resource " ,h " has been shared with you."))))

(define (message->document msg)
  (with (action pseudo full-name date doc) msg
    (cond ((== action "share")
           (with doc* `(document ,(message->share doc))
             `(chat-output ,full-name ,pseudo "" ,date ,doc*)))
          (else `(chat-output ,full-name ,pseudo "" ,date ,doc)))))

(define (messages->document msgs name)
  `(document
     (section* "Messages")
     ,@(map message->document msgs)
     ,@(if (string-starts? name "mail-") (list)
           (list `(chat-input "")))))

(define (chat-document doc)
  `(document
     (TeXmacs ,(texmacs-version))
     (style (tuple "generic" "chat-room"))
     (body ,doc)))

(define (empty-document)
  (chat-document '(document "")))

(define (chat-room-modified fname)
  ;;(display* "Received message in " fname "\n")
  (noop))

(define chat-room-writable-table (make-ahash-table))

(define (chat-room-set-writable fname w?)
  (ahash-set! chat-room-writable-table fname w?))

(define (chat-room-writable? fname)
  (ahash-ref chat-room-writable-table fname))

(define (chat-room-set fname msgs)
  (with doc (messages->document msgs (chat-room-name fname))
    (buffer-set fname (chat-document doc))
    (buffer-pretend-saved fname)
    (chat-room-modified fname)))

(define (chat-room-insert fname msg)
  (and-let* ((doc (and (buffer-exists? fname) (buffer-get-body fname)))
             (outl (tree-search doc (cut tree-is? <> 'chat-output)))
             (inl (tree-search doc (cut tree-is? <> 'chat-input)))
             (pos (cond ((nnull? inl) (tree-index (car inl)))
                        ((nnull? outl) (+ (tree-index (cAr outl)) 1))
                        (else #f)))
             (p (if (nnull? inl) (tree-up (car inl)) (tree-up (cAr outl))))
             (ok? (tree-is? p 'document)))
    (tree-insert p pos (list (message->document msg)))))

(tm-call-back (chat-room-receive name msg)
  (with (server msg-id) envelope
    (and-let* ((sname (client-find-server-name server))
               (fname (string-append "tmfs://chat/" sname "/" name)))
      (chat-room-insert fname msg)
      (chat-room-modified fname)
      #t)))

(tm-define (chat-room-send)
  (and-let* ((t (tree-innermost 'chat-input))
             (mt (tm-ref t 0))
             (msg (tm->stree mt))
             (ok? (chat-room-url? (current-buffer)))
             (room (chat-room-name (current-buffer)))
             (server (chat-room-server (current-buffer)))
             (cmd `(remote-send-message ,room "send-document" ,msg))
             (sname (client-find-server-name server))
             (fname (string-append "tmfs://chat/" sname "/" room)))
    (if (chat-room-writable? fname)
        (begin
          (tree-set! mt `(document ""))
          (client-remote-eval server cmd ignore))
        (set-message "this chat room is read only" "send message"))))

(tm-define (kbd-control-return)
  (:require (inside? 'chat-input))
  (chat-room-send))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating and joining chat rooms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (chat-room-create server name)
  ;;(display* "chat-room-create " server ", " name "\n")
  (let* ((sname (client-find-server-name server))
         (fname (string-append "tmfs://chat/" sname "/" name)))
    (client-remote-eval server `(remote-chat-room-create ,name)
      (lambda (msg)
        (load-document fname))
      (lambda (err)
        (set-message err "create chat room")))))

(tm-define (chat-room-create-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (chat-room-create server name))
    (list "Name of the chat room" "string" '())))

(tm-define (chat-room-join server name)
  ;;(display* "chat-room-join " server ", " name "\n")
  (and-with sname (client-find-server-name server)
    (load-document (string-append "tmfs://chat/" sname "/" name))))

(tm-define (chat-room-join-interactive server)
  (:interactive #t)
  (interactive
      (lambda (name)
        (chat-room-join server name))
    (list "Join chat room" "string" '())))

(tm-define (mail-box-open server)
  ;;(display* "remote-mail-box-open " server "\n")
  (and-let* ((pseudo (client-find-server-pseudo server))
             (mbox (string-append "mail-" pseudo)))
    (chat-room-join server mbox)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of chat rooms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (chat-rooms name type)
  (in? type (list "read")))

(tmfs-load-handler (chat-rooms sname)
  (let* ((u (string-append "tmfs://chat-rooms/" sname))
         (base (string-append "tmfs://chat/" sname))
         (server (client-find-server sname)))
    (client-remote-eval server `(remote-list-chat-rooms)
      (lambda (l)
        (with hyp (lambda (c) `(hlink ,c ,(string-append base "/" c)))
          (with doc `(document (section* "My chat rooms") ,@(map hyp l))
            (buffer-set-body u doc)
            (buffer-pretend-saved u)
            (set-message "retrieved contents" "list of chat rooms"))))
      (lambda (err)
        (set-message err "list of chat rooms")))
    (set-message "loading..." "list of chat rooms")
    (empty-document)))

(tm-define (list-chat-rooms server)
  (and-with sname (client-find-server-name server)
    (string-append "tmfs://chat-rooms/" sname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List of shared documents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (shared sname type)
  (in? type (list "read")))

(define (list-shared? msg)
  (with (action pseudo full-name date doc) msg
    (== action "share")))

(define (list-shared-name msg)
  (with (action pseudo full-name date doc) msg
    full-name))

(define (list-shared-links l name)
  (with f (list-filter l (lambda (m) (== (list-shared-name m) name)))
    `((subsection* ,name)
      ,@(map (lambda (m)
               (with (action pseudo full-name date u) m
                 `(hlink ,(url->string (url-tail u))
                         ,(url->string u))))
             f))))

(define (list-shared-document l)
  (let* ((f (list-filter l list-shared?))
         (names (list-remove-duplicates (map list-shared-name f))))
    `(document
       (section* "Shared resources")
       ,@(append-map (cut list-shared-links f <>) names))))

(tmfs-load-handler (shared sname)
  (let* ((u (string-append "tmfs://shared/" sname))
         (server (client-find-server sname)))
    (client-remote-eval server `(remote-mail-open)
      (lambda (l)
        (with doc (list-shared-document l)
          (buffer-set-body u doc)
          (buffer-pretend-saved u)
          (set-message "retrieved contents" "list of shared resources")))
      (lambda (err)
        (set-message err "list of chat rooms")))
    (set-message "loading..." "list of shared resources")
    (empty-document)))

(tm-define (list-shared server)
  (and-with sname (client-find-server-name server)
    (string-append "tmfs://shared/" sname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chat rooms as files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-permission-handler (chat name type)
  (in? type (list "read")))

(tmfs-title-handler (chat name doc)
  (let* ((fname (string-append "tmfs://chat/" name))
         (room (chat-room-name fname))
         (title (string-append "Chat room - " room)))
    title))

(tmfs-load-handler (chat name)
  (let* ((fname (string-append "tmfs://chat/" name))
         (server (chat-room-server fname))
         (room (chat-room-name fname)))
    (cond ((not server)
           ;; FIXME: better error handling
           (texmacs-error "chat" "invalid server"))
          ((not (string-starts? room "mail-"))
           (client-remote-eval server `(remote-chat-room-open ,room)
             (lambda (ret)
               (chat-room-set fname (cadr ret))
               (chat-room-set-writable fname (car ret))
               (if (car ret)
                   (set-message "retrieved contents" "join chat room")
                   (set-message "joined in read only mode" "join chat room")))
             (lambda (err)
               (set-message err "join chat room")))
           (set-message "loading..." "joining chat room")
           (empty-document))
          ((string-starts? room "mail-")
           (client-remote-eval server `(remote-mail-open)
             (lambda (msgs)
               (chat-room-set fname msgs)
               (chat-room-set-writable fname #t)
               (set-message "retrieved contents" "open mail box"))
             (lambda (err)
               (set-message err "join chat room")))
           (set-message "loading..." "opening mail box")
           (empty-document)))))
