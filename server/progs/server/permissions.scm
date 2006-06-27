
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : permissions.scm
;; DESCRIPTION : user and group permissions
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (server permissions))
(use-modules (tools base) (tools abbrevs) (tools ahash-table) (tools list)
	     (server request) (server atoms))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modification of permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (permissions-remove-user l user)
  (if (null? l) l
      (with r (permissions-remove-user (cdr l) user)
	(if (== (caaar l) user) r (cons (car l) r)))))

(define (permissions-remove-action l action)
  (if (null? l) l
      (with r (permissions-remove-action (cdr l) action)
	(if (== (caaar l) action) r (cons (car l) r)))))

(define (permissions-remove-entry l user action)
  (if (null? l) l
      (with r (permissions-remove-entry (cdr l) user action)
	(if (== (caar l) (cons user action)) r (cons (car l) r)))))

(define (permissions-change permissions user action to)
  (cond ((and (== user #t) (== action #t)) `(((#t . #t) . to)))
	((== action #t)
	 (with r (permissions-remove-user permissions user)
	   (acons user action r)))
	((== user #t)
	 (with r (permissions-remove-action permissions action)
	   (acons user action r)))
	(else
	 (with r (permissions-remove-entry permissions user action)
	   (acons user action r)))))

(define-public (permissions-add permissions user action)
  (permissions-change permissions user action #t))

(define-public (permissions-remove permissions user action)
  (permissions-change permissions user action #f))

(define-public (permissions-action-set permissions action users)
  (with r (permissions-remove-action permissions action)
    (append (map (lambda (user) (cons (cons user action) #t)) users) r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verification of permissions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define permissions-initialized? #f)
(define permissions-table (make-ahash-table))

(define (permissions-update-user-sub l user)
  (when (nnull? l)
    (ahash-set! permissions-table (list user (caar l) (cdar l)))
    (permissions-update-user-sub (cdr l) user)))

(define (permissions-update-user user)
  (permissions-update-user-sub
    (atom-get-property user 'user "permissions")
    user))

(define (permissions-update reset?)
  (if reset? (set! permissions-table (make-ahash-table)))
  (with old (ahash-size permissions-table)
    (for-each permissions-update-user (atom-list-of-type 'user))
    (with new (ahash-size permissions-table)
      (if (> new old) (permissions-update #f)))))

(define (permissions-initialize)
  (when (not permissions-initialized?)
    (set! permissions-initialized? #t)
    (permissions-update #t)))

(define (permission-match? p user action)
  (and (or (== (car p) #t)
	   (== (car p) user)
	   (ahash-ref permissions-table (list (car p) user action))
	   (ahash-ref permissions-table (list (car p) user #t))
	   (ahash-ref permissions-table (list (car p) #t action))
	   (ahash-ref permissions-table (list (car p) #t #t)))
       (or (== (cdr p) #t)
	   (== (cdr p) action))))

(define-public (permissions-match? l user action)
  (permissions-initialize)
  (cond ((null? l) #f)
	((permission-match? (caar l) user action) (cdar l))
	(else (permissions-match? (cdr l) user action))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for group management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (group-get-permissions group)
  (with l (atom-get-property group 'user "permissions")
    (if l l '())))

(define (group-set-permissions group l)
  (atom-set-property group 'user "permissions" l))

(define (group-administrator? group)
  (and-with user (current-user)
    (with l (get-permissions group)
      (permissions-match? l user 'admin))))

(request-handler (new-group group)
  (permissions-initialize)
  (and-with user (current-user)
    (when (new-atom group 'user '())
      (atom-set-property group 'user "group" #t)
      (group-set-permissions group `(((,user . admin) . #t)))
      (permissions-update #f))))

(request-handler (group-add-permission group user action)
  (permissions-initialize)
  (when (group-administrator?)
    (with l (group-get-permissions group)
      (group-set-permissions group (permissions-add l user action))
      (permissions-update #f))))

(request-handler (group-remove-permission group user action)
  (permissions-initialize)
  (when (group-administrator?)
    (with l (group-get-permissions group)
      (group-set-permissions group (permissions-remove l user action))
      (permissions-update #t))))

(request-handler (group-set-permission-action group action users)
  (permissions-initialize)
  (when (group-administrator?)
    (with l (group-get-permissions group)
      (group-set-permissions group (permissions-action-set l action users))
      (permissions-update #t))))
