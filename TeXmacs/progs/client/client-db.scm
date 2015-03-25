
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-db.scm
;; DESCRIPTION : remote databases, client side
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-db)
  (:use (client client-base)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote interface to basic database API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-get-field server id attr cont)
  ;;(display* "remote-get-field " server ", " id ", " attr "\n")
  (client-remote-eval server `(remote-get-field ,id ,attr) cont))

(tm-define-macro (with-remote-get-field r server id attr . body)
  `(remote-get-field ,server ,id ,attr
                     (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-set-field server id attr vals)
  ;;(display* "remote-set-field " server ", " id ", " attr ", " vals "\n")
  (client-remote-eval server `(remote-set-field ,id ,attr ,vals) ignore))

(tm-define (remote-get-attributes server id cont)
  ;;(display* "remote-get-attributes " server ", " id "\n")
  (client-remote-eval server `(remote-get-attributes ,id) cont))

(tm-define-macro (with-remote-get-attributes r server id . body)
  `(remote-get-attributes ,server ,id
                          (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-get-entry server id cont)
  ;;(display* "remote-get-entry " server ", " id "\n")
  (client-remote-eval server `(remote-get-entry ,id) cont))

(tm-define-macro (with-remote-get-entry r server id . body)
  `(remote-get-entry ,server ,id
                     (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-set-entry server id l)
  ;;(display* "remote-set-entry " server ", " id ", " l "\n")
  (client-remote-eval server `(remote-set-entry ,id ,l) ignore))

(tm-define (remote-create-entry server l cont)
  ;;(display* "remote-create-entry " server ", " l "\n")
  (client-remote-eval server `(remote-create-entry ,l) cont))

(tm-define-macro (with-remote-create-entry r server l . body)
  `(remote-create-entry ,server ,l
                        (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-search server q cont)
  ;;(display* "remote-search " server ", " q "\n")
  (client-remote-eval server `(remote-search ,q) cont))

(tm-define-macro (with-remote-search r server q . body)
  `(remote-search ,server ,q
                  (lambda (msg) (with ,r msg ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get limited information about other users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remote-search-user server q cont)
  ;;(display* "remote-search-user " server ", " q "\n")
  (client-remote-eval server `(remote-search-user ,q) cont))

(tm-define-macro (with-remote-search-user r server q . body)
  `(remote-search-user ,server ,q
                       (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-get-user-pseudo server uid cont)
  ;;(display* "remote-get-user-pseudo " server ", " uid "\n")
  (client-remote-eval server `(remote-get-user-pseudo ,uid) cont))

(tm-define-macro (with-remote-get-user-pseudo r server uid . body)
  `(remote-get-user-pseudo ,server ,uid
                           (lambda (msg) (with ,r msg ,@body))))

(tm-define (remote-get-user-name server uid cont)
  ;;(display* "remote-get-user-name " server ", " uid "\n")
  (client-remote-eval server `(remote-get-user-name ,uid) cont))

(tm-define-macro (with-remote-get-user-name r server uid . body)
  `(remote-get-user-name ,server ,uid
                           (lambda (msg) (with ,r msg ,@body))))
