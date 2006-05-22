
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : atoms.scm
;; DESCRIPTION : important atomic objects which should be widely available,
;;               like servers, users, chatrooms, public keys, etc.
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server atoms))
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools file)
	     (server request))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General atom management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define atom-file (string-append (server-dir) "/system/atoms.scm"))
(define atom-table (make-ahash-table))
(define atom-list '())

(define-public (atom-initialize)
  (set! atom-list
	(if (access? atom-file R_OK) (load-object atom-file) '()))
  (for-each (lambda (x)
	      (ahash-set! atom-table (car x) (cdr x)))
	    atom-list))

(define-public (new-atom name type info)
  (display* "New atom: " name ", " type ", " info "\n")
  (and (not (ahash-ref atom-table (cons name type)))
       (begin
	 (set! atom-list (assoc-set! atom-list (cons name type) info))
	 (ahash-set! atom-table (cons name type) info)
	 (display* "atom-list= " atom-list "\n")
	 (save-object atom-file atom-list)
	 #t)))

(define-public (atom-info name type)
  (ahash-ref atom-table (cons name type)))

(define-public (atom-set-property name type var val)
  (with l (atom-info name type)
    (when l
      (set! l (assoc-set! l var val))
      (set! atom-list (assoc-set! atom-list (cons name type) l))
      (ahash-set! atom-table (cons name type) l)
      (save-object atom-file atom-list)
      #t)))

(define-public (atom-get-property name type var)
  (with l (atom-info name type)
    (assoc-ref l var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo: maintain list of available servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(request-handler (new-server name info)
;;  (new-atom name 'server info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maintain list of users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(request-handler (new-user name info)
  (new-atom name 'user info))

(request-handler (user-info name)
  (atom-info name 'user))

(request-handler (user-set-property name var val)
  (atom-set-property name 'user var val))

(request-handler (user-get-property name var)
  (atom-get-property name 'user var))

;;(request-handler (new-chatroom name info)
;;  (new-atom name 'chatroom info))
