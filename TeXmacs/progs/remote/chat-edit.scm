
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chat-edit.scm
;; DESCRIPTION : editing routines for chatting
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote chat-edit)
  (:use (utils library tree)
	(utils library cursor)
	(remote client)))

(define chat-connected (make-ahash-table))
(define chat-last-modification (make-ahash-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic subroutines for chatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (chat-session)
  (:synopsis "Get the current chat session tree.")
  (tree-innermost 'chat-session))

(tm-define (chat-input)
  (and-with session (chat-session)
    (with last (tm-ref session 1 :last)
      (and (tree-is? last 'chat-input) last))))

(tm-define (chat-field)
  (or (tree-innermost 'chat-input)
      (tree-innermost 'chat-output)))

(tm-define (chat-room)
  (and-with session (chat-session)
    (tree->string (tree-ref session 0))))

(tm-define (chat-user)
  (and-with input (chat-input)
    (tree->string (tree-ref input 0))))

(tm-define (chat-user*)
  (and-with field (chat-field)
    (tree->string (tree-ref field 0))))

(tm-define (chat-connected?)
  (:synopsis "Are we inside a session and connected?")
  (and-with room (chat-room)
    (and-with user (chat-user)
      (ahash-ref chat-connected (cons room user)))))

(define (chat-convert field)
  (with (nr user contents) field
    `(chat-output ,user ,(stree->tree contents))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chatroom administration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (chat-server . opt-rooms)
  (or (get-server) (default-server)))

(tm-define (chat-list-administrated-rooms)
  (with-server (chat-server)
    (with l (remote-request '(chat-list-administrated-rooms))
      (and l (nnull? l) l))))

(tm-define (chatroom-create room)
  (:synopsis "Create a chat room.")
  (:argument room "Chat room")
  (when (chat-connect room)
    (with-server (chat-server room)
      (remote-request `(new-chatroom ,room)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for chatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (chat-list-rooms)
  (with-server (chat-server)
    (with l (remote-request '(chat-list-rooms))
      (and l (nnull? l) l))))

(tm-define (chat-connect room)
  (:synopsis "Connect to a chat room.")
  (:argument room "Chat room")
  (and-let* ((user (remote-user))
	     (can-insert (not (chat-session)))
	     (not-busy (not (ahash-ref chat-connected (cons user room)))))
    (with-server (chat-server room)
      (and-let* ((new (remote-request `(chat-connect ,room ,user)))
		 (out (map chat-convert new))
		 (in `(chat-input ,user (document ""))))
	(init-add-package "chat")
	(ahash-set! chat-connected (cons room user) #t)
	(insert-go-to `(chat-session ,room (document ,@out ,in))
		      (list 1 (length new) 1 0 0))
	(chat-refresh-handler room user (tree-innermost 'chat-session))
	#t))))

(tm-define (chat-catch-up)
  (:synopsis "Catch up with the discussion.")
  (and-let* ((room (chat-room))
	     (user (chat-user))
	     (session (chat-session))
	     (ok (not (chat-connected?))))
    (with-server (chat-server room)
      (and-let* ((new (remote-request `(chat-connect ,room ,user)))
		 (out (map chat-convert new))
		 (in `(chat-input ,user (document ""))))
	(ahash-set! chat-connected (cons room user) #t)
	(tree-set! session 1 `(document ,@out ,in))
	(tree-go-to session 1 :last 1 :end)
	(chat-refresh-handler room user (tree-innermost 'chat-session))))))

(tm-define (chat-hang-up)
  (:synopsis "Quit the chat room.")
  (and-let* ((room (chat-room))
	     (user (chat-user))
	     (ok (chat-connected?)))
    (with-server (chat-server room)
      (remote-request `(chat-hang-up ,room ,user))
      (ahash-remove! chat-connected (cons room user))
      (ahash-remove! chat-last-modification (cons room user)))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'chat-input))
  (and-let* ((room (chat-room))
	     (user (chat-user))
	     (input (chat-input))
	     (emit (tree->stree (tree-ref input 1)))
	     (ok (chat-connected?)))
    (with-server (chat-server room)
      (chat-wake-up room user #f)
      (tree-set! input 1 '(document ""))
      (remote-request `(chat-emit ,room ,user ,emit)))))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'chat-output))
  (and-let* ((room (chat-room))
	     (user (chat-user*))
	     (field (chat-field))
	     (nr (tree-index field))
	     (emit (tree->stree (tree-ref field 1)))
	     (ok (and (chat-connected?) (== user (chat-user)))))
    (with-server (chat-server room)
      (chat-wake-up room user #f)
      (remote-request `(chat-update ,room ,user ,nr ,emit)))))

(define (chat-update t field)
  ;;(display* "Update " field "\n")
  (with (nr user contents) field
    (while (>= nr (- (tree-arity t) 1))
      (tree-insert! t (- (tree-arity t) 1) '("")))
    (tree-set! t nr (chat-convert field))))

(define (chat-wake-up room user ring?)
  (with last (ahash-ref chat-last-modification (cons room user))
    (when (and ring? last (> (texmacs-time) (+ last 300000)))
      (delayed
	(:pause 500)
	(system-1 "play" "$TEXMACS_PATH/misc/sounds/phone.wav")
	(delayed
	  (:pause 2500)
	  (system-1 "play" "$TEXMACS_PATH/misc/sounds/phone.wav")
	  (delayed
	    (:pause 2500)
	    (system-1 "play" "$TEXMACS_PATH/misc/sounds/phone.wav")))))
    (ahash-set! chat-last-modification (cons room user) (texmacs-time))))

(tm-define (chat-refresh-handler room user t)
  (chat-wake-up room user #f)
  (with ptr (tree->tree-pointer t)
    (delayed
      (:pause 1000)
      (:every 2000)
      (:clean (tree-pointer-detach ptr))
      (:while (ahash-ref chat-connected (cons room user)))
      (with-server (chat-server room)
	(with new (remote-request `(chat-synchronize ,room ,user))
	  (if (and new (nnull? new))
	      (with u (tree-pointer->tree ptr)
		(when (tm-func? u 'chat-session)
		  (chat-wake-up room user #t)
		  (for-each (lambda (x) (chat-update (tree-ref u 1) x))
			    new)))))))))
