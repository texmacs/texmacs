
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : db-users.scm
;; DESCRIPTION : Users and permissions
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database db-users)
  (:use (database db-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The current user
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define db-current-user #t)

(tm-define-macro (with-user uid . body)
  `(with-global db-current-user uid ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Important tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table db-encoding-table
  ((* "owner") :users)
  ((* "readable") :users)
  ((* "writable") :users))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encoding and decoding of lists of users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-encode-user user)
  (if (== user "all") user
      (with l (db-search (list (list "type" "user") (list "pseudo" user)))
        (and (pair? l) (car l)))))

(define (db-encode-users users)
  ;;(display* "Encode users " users "\n")
  (list-filter (map db-encode-user users) identity))

(define (db-decode-user id)
  (if (== id "all") id
      (db-get-field-first id "pseudo" #f)))

(define (db-decode-users ids)
  ;;(display* "Decode users " ids "\n")
  (list-filter (map db-decode-user ids) identity))

(smart-table db-encoder-table
  (,:users ,db-encode-users))

(smart-table db-decoder-table
  (,:users ,db-decode-users))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating new users
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (db-set-user-info uid pseudo name email)
  (with-transcode #f
    (db-set-field uid "pseudo" (list pseudo))
    (db-set-field uid "name" (list name))
    (db-set-field uid "type" (list "user"))
    (db-set-field uid "owner" (list uid))
    (db-set-field uid "email" (list email))
    (with home (string-append "~" pseudo)
      (when (null? (db-search (list (list "name" home)
                                    (list "type" "dir"))))
        (db-create home "dir" uid)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Access rights
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (db-allow-many? ids rdone uid udone attr)
  (and (nnull? ids)
       (or (db-allow-one? (car ids) rdone uid udone attr)
           (db-allow-many? (cdr ids) rdone uid udone attr))))

(define (db-allow-groups? id rdone uids udone attr)
  (and (nnull? uids)
       (or (db-allow-one? id rdone (car uids) udone attr)
           (db-allow-groups? id rdone (cdr uids) udone attr))))

(define (db-allow-one? id rdone uid udone attr)
  ;;(display* "Allow one " id ", " uid ", " attr "\n")
  (and (not (in? id rdone))
       (not (in? uid udone))
       (or (== id uid)
           (== id "all")
           (with ids (append (db-get-field id attr)
                             (db-get-field id "owner"))
             (set! ids (list-remove-duplicates ids))
             (set! ids (list-difference ids (cons id rdone)))
             (db-allow-many? ids (cons id rdone) uid udone attr))
           (with grs (db-get-field uid "member")
             (db-allow-groups? id rdone grs (cons uid udone) attr)))))

(tm-define (db-allow? id uid attr)
  (with-transcode #f
    ;;(display* "Allow " id ", " uid ", " attr "\n")
    (db-allow-one? id (list) uid (list) attr)))
