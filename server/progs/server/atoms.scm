
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
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools list)
	     (tools file) (tools crypt)
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
  (and (not (ahash-ref atom-table (cons name type)))
       (begin
	 (set! atom-list (assoc-set! atom-list (cons name type) info))
	 (ahash-set! atom-table (cons name type) info)
	 (save-object atom-file atom-list)
	 (chmod atom-file #o600)
	 #t)))

(define-public (atom-list-of-type type)
  (map caar (list-filter atom-list (lambda (x) (== (cdar x) type)))))

(define-public (atom-info name type)
  (ahash-ref* atom-table (cons name type) '()))

(define-public (atom-set-property name type var val)
  (with l (atom-info name type)
    (set! l (assoc-set! l var val))
    (set! atom-list (assoc-set! atom-list (cons name type) l))
    (ahash-set! atom-table (cons name type) l)
    (save-object atom-file atom-list)
    (chmod atom-file #o600)
    #t))

(define-public (atom-get-property name type var)
  (with l (atom-info name type)
    (assoc-ref l var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: maintain list of other servers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (request-handler (new-server name info)
;;   (new-atom name 'server info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maintain list of users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-user (make-ahash-table))

(define-public (current-user)
  (ahash-ref connection-user current-connection-id))

(request-handler (new-user name passwd)
  (with encoded-passwd passwd
    (when (new-atom name 'user '())
      (atom-set-property name 'user "password" encoded-passwd))))

(request-handler (user-login name passwd)
  (and-let* ((encoded-passwd passwd)
	     (stored-passwd (atom-get-property name 'user "password"))
	     (ok (== encoded-passwd stored-passwd))
	     (id current-connection-id))
    (ahash-set! connection-user id name)
    #t))

(request-handler (user-logout)
  (and-let* ((id current-connection-id))
    (ahash-remove! connection-user id)
    #t))

(request-handler (user-set-property var val)
  (and (current-user)
       (atom-set-property (current-user) 'user var val)))

(request-handler (user-get-property var)
  (and (current-user) (!= var "password")
       (atom-get-property (current-user) 'user var)))
