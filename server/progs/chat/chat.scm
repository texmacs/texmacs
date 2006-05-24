
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chat.scm
;; DESCRIPTION : chatting via the server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (chat chat))
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools list)
	     (server request) (server atoms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-remove l x)
  (cond ((null? l) l)
	((== (car l) x) (cdr l))
	(else (cons (car l) (list-remove (cdr l) x)))))

(define (ahash-init! t x init)
  (ahash-set! t x (ahash-ref* t x init)))
(define (ahash-add! t x plus)
  (ahash-set! t x (+ (ahash-ref* t x 0) plus)))
(define (ahash-list-insert! t x insert)
  (ahash-set! t x (append (ahash-ref* t x '()) (list insert))))
(define (ahash-list-remove! t x remove)
  (ahash-set! t x (list-remove (ahash-ref* t x '()) remove)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chatroom management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define chatroom-temporary-table (make-ahash-table))

(request-handler (new-chatroom room)
  (and-with user (current-user)
    (and (new-atom room 'chatroom '())
	 (atom-set-property room 'chatroom "administrator" user))))

(request-handler (chatroom-get-property room var)
  (if (atom-info room 'chatroom)
      (atom-get-property room 'chatroom var)
      (ahash-ref chatroom-temporary-table room var)))

(define (chatroom-administrator? room)
  (== (chatroom-get-property room "administrator")
      (current-user)))

(request-handler (chatroom-set-property room var val)
  (and (chatroom-administrator? room)
       (if (atom-info room 'chatroom)
	   (atom-set-property room 'chatroom var val)
	   (begin
	     (ahash-set! chatroom-temporary-table room var val)
	     #t))))

(define (chatroom-accepts? room user)
  #t)

(define (chatroom-write-access? room user)
  (== user (current-user)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define chat-connected (make-ahash-table))
(define chat-discussion (make-ahash-table))
(define chat-needs-update (make-ahash-table))

(request-handler (chat-list-administrated-rooms)
  (and-let* ((user (current-user))
	     (l1 (atom-list-of-type 'chatroom))
	     (l2 (list-filter l1 chatroom-administrator?)))
    (list-filter l2
      (lambda (room) (null? (ahash-ref* chat-connected room '()))))))

(request-handler (chat-list-rooms)
  (and-let* ((user (current-user))
	     (l1 (ahash-table->list chat-connected))
	     (l2 (list-filter l1 (lambda (x) (nnull? (cdr x)))))
	     (l3 (map car l2)))
    (list-filter l3 (lambda (room) (chatroom-accepts? room user)))))

(request-handler (chat-connect room user)
  (if (nin? user (ahash-ref* chat-connected room '()))
      (begin
	(ahash-list-insert! chat-connected room user)
	(ahash-init! chat-discussion room '())
	(ahash-set! chat-needs-update (cons room user) '())
	(ahash-ref chat-discussion room))
      (ahash-ref chat-discussion room)))

(request-handler (chat-hang-up room user)
  (ahash-list-remove! chat-connected room user)
  (ahash-remove! chat-needs-update (cons room user))
  (if (null? (ahash-ref chat-connected room))
      (ahash-set! chat-discussion room '()))
  #t)

(define (chat-invalidate room user nr)
  (ahash-list-insert! chat-needs-update (cons room user) nr)
  #t)

(request-handler (chat-emit room user what)
  (when (chatroom-write-access? room user)
    (with len (length (ahash-ref chat-discussion room))
      (ahash-list-insert! chat-discussion room (list len user what))
      (for-each (lambda (x) (chat-invalidate room x len))
		(ahash-ref chat-connected room))
      len)))

(define (list-set-safe! l nr x)
  (when (< nr (length l))
    (list-set! l nr x)))

(request-handler (chat-update room user nr what)
  (when (chatroom-write-access? room user)
    (list-set-safe! (ahash-ref chat-discussion room) nr (list nr user what))
    (for-each (lambda (x) (chat-invalidate room x nr))
	      (ahash-ref chat-connected room))
    #t))

(request-handler (chat-synchronize room user)
  (with r (map (lambda (nr) (list-ref (ahash-ref chat-discussion room) nr))
	       (ahash-ref* chat-needs-update (cons room user) '()))
    (ahash-set! chat-needs-update (cons room user) '())
    r))
