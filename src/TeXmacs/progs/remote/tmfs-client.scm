
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-client.scm
;; DESCRIPTION : clients of remote TeXmacs file systems
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-client))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (object->string* obj)
  (cond ((null? obj) (object->string obj))
	((pair? obj) (object->string obj))
	((number? obj) (object->string obj))
	((string? obj) (object->string obj))
	((symbol? obj) (object->string obj))
	((tree? obj) (object->string (tree->stree obj)))
	(else (object->string #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchroneous servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmfs-server-client-active? (make-ahash-table))
(define tmfs-server-client-waiting? (make-ahash-table))

(tm-define (tmfs-server-clients)
  (ahash-set->list tmfs-server-client-active?))

(define (tmfs-server-eval cmd)
  (display* "Server command: " cmd "\n")
  (object->string* (eval (string->object cmd))))

(tm-define (tmfs-server-add client)
  (ahash-set! tmfs-server-client-active? client #t)
  (with wait 1
    (delayed
      (:while (ahash-ref tmfs-server-client-active? client))
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.001 wait) 2500)))
      (when (not (ahash-ref tmfs-server-client-waiting? client))
	(with cmd (tmfs-server-read client)
	  (when (!= cmd "")
	    (with result (tmfs-server-eval cmd)
	      (tmfs-server-write client result)
	      (set! wait 1))))))))

(tm-define (tmfs-server-remove client)
  (ahash-remove! tmfs-server-client-active? client))

(define (tmfs-server-remote-sub client cmd return)
  (when (not (ahash-ref tmfs-server-client-waiting? client))
    (ahash-set! tmfs-server-client-waiting? client #t)
    (tmfs-server-write client (object->string* cmd))
    (with wait 1
      (delayed
	(:while (ahash-ref tmfs-server-client-waiting? client))
	(:pause ((lambda () (inexact->exact wait))))
	(:do (set! wait (min (* 1.001 wait) 2500)))
	(with result (tmfs-server-read client)
	  (when (!= result "")
	    (ahash-set! tmfs-server-client-waiting? client #f)
	    (set! wait 1)
	    (return (string->object result))))))))

(tm-define (tmfs-server-remote client cmd)
  (if dialogue-break
      (dialogue-user local-continue
	(with return (dialogue-machine local-continue)
	  (tmfs-server-remote-sub client cmd return)))
      (texmacs-error "dialogue-ask" "Not in dialogue")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchroneous clients
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define tmfs-client-active? #f)
(define tmfs-client-waiting? #f)

(define (tmfs-client-eval cmd)
  (display* "Client command: " cmd "\n")
  (object->string* (eval (string->object cmd))))

(tm-define (tmfs-client-add)
  (set! tmfs-client-active? #t)
  (with wait 1
    (delayed
      (:while tmfs-client-active?)
      (:pause ((lambda () (inexact->exact wait))))
      (:do (set! wait (min (* 1.001 wait) 2500)))
      (when (not tmfs-client-waiting?)
	(with cmd (tmfs-client-read)
	  (when (!= cmd "")
	    (with result (tmfs-client-eval cmd)
	      (tmfs-client-write result)
	      (set! wait 1))))))))

(tm-define (tmfs-client-remove)
  (set! tmfs-client-active? #f))

(define (tmfs-client-remote-sub cmd return)
  (when (not tmfs-client-waiting?)
    (set! tmfs-client-waiting? #t)
    (tmfs-client-write (object->string* cmd))
    (with wait 1
      (delayed
	(:while tmfs-client-waiting?)
	(:pause ((lambda () (inexact->exact wait))))
	(:do (set! wait (min (* 1.001 wait) 2500)))
	(with result (tmfs-client-read)
	  (when (!= result "")
	    (set! tmfs-client-waiting? #f)
	    (set! wait 1)
	    (return (string->object result))))))))

(tm-define (tmfs-client-remote cmd)
  (if dialogue-break
      (dialogue-user local-continue
	(with return (dialogue-machine local-continue)
	  (tmfs-client-remote-sub cmd return)))
      (texmacs-error "dialogue-ask" "Not in dialogue")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commit and checkout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmfs-checkout l)
  (dialogue
    (let* ((new-closure (tmfs-client-remote `(tmfs-closure ,l)))
	   (old-closure (tmfs-closure l))
	   (diff-closure (list-difference new-closure old-closure))
	   (news (tmfs-client-remote `(tmfs-get-ressources ,diff-closure))))
      (tmfs-set-ressources news))))

(tm-define (tmfs-commit l)
  (dialogue
    (let* ((old-closure (tmfs-client-remote `(tmfs-closure ,l)))
	   (new-closure (tmfs-closure l))
	   (diff-closure (list-difference new-closure old-closure))
	   (news (tmfs-get-ressources diff-closure)))
      (tmfs-client-remote `(tmfs-set-ressources ,news)))))
