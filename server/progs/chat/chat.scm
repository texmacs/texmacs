
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
(use-modules (tools base) (tools abbrevs) (tools ahash-table)
	     (server request))

(define chat-connected (make-ahash-table))
(define chat-discussion (make-ahash-table))
(define chat-needs-update (make-ahash-table))

(define (list-remove l x)
  (cond ((null? l) l)
	((== (car l) x) (cdr x))
	(else (cons (car l) (list-remove (cdr l) x)))))

(define (ahash-init! t x init)
  (ahash-set! t x (ahash-ref* t x init)))
(define (ahash-add! t x plus)
  (ahash-set! t x (+ (ahash-ref* t x 0) plus)))
(define (ahash-list-insert! t x insert)
  (ahash-set! t x (append (ahash-ref* t x '()) (list insert))))
(define (ahash-list-remove! t x remove)
  (ahash-set! t x (list-remove (ahash-ref* t x '()) remove)))

(request-handler (chat-connect room user)
  (if (nin? user (ahash-ref* chat-connected room '()))
      (begin
	(ahash-list-insert! chat-connected room user)
	(ahash-init! chat-discussion room '())
	(ahash-set! chat-needs-update (cons room user) '())
	(ahash-ref chat-discussion room))
      (ahash-ref chat-discussion room)))

(request-handler (char-hang-up room user)
  (ahash-list-remove! chat-connected room user)
  (ahash-remove! chat-needs-update (cons room user))
  (if (null? (ahash-ref char-connected room))
      (ahash-set chat-discussion room '())))

(define-public (chat-invalidate room user nr)
  (ahash-list-insert! chat-needs-update (cons room user) nr)
  #t)

(request-handler (chat-emit room user what)
  (with len (length (ahash-ref chat-discussion room))
    (ahash-list-insert! chat-discussion room (list len user what))
    (for-each (lambda (x) (chat-invalidate room x len))
	      (ahash-ref chat-connected room))
    len))

(define (list-set-safe! l nr x)
  (when (< nr (length l))
    (list-set! l nr x)))

(request-handler (chat-update room user nr what)
  (list-set-safe! (ahash-ref chat-discussion room) nr (list nr user what))
  (for-each (lambda (x) (chat-invalidate room x nr))
	    (ahash-ref chat-connected room))
  #t)

(request-handler (chat-synchronize room user)
  (with r (map (lambda (nr) (list-ref (ahash-ref chat-discussion room) nr))
	       (ahash-ref chat-needs-update (cons room user)))
    (ahash-set! chat-needs-update (cons room user) '())
    r))
