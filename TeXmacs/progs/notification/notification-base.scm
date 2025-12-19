
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : notification-base.scm
;; DESCRIPTION : GUI notifications of TeXmacs
;; COPYRIGHT   : (C) 2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (notification notification-base))

(define notifications-table (make-ahash-table))

(define (get-notif key) (ahash-ref notifications-table key))
(define (get-notif-act name type) (get-notif `(,name . ,type)))
(define (get-notif-cnt type) (get-notif type))

(define (set-notif key val) (ahash-set! notifications-table key val))
(define (set-notif-act name type act) (set-notif `(,name . ,type) act))
(define (inc-notif-count type)
  (with c (get-notif-cnt type)
    (if c (set-notif type (+ c 1)) (set-notif type 1))
    (get-notif-cnt type)))

(define (dec-notif-count type n)
  (with c (get-notif-cnt type)
    (if c (set-notif type (- c n)) (set-notif type 0))
    (get-notif-cnt type)))

(tm-define (has-notifications?) (positive? (ahash-size notifications-table)))

(tm-define (add-notification name type action)
  (if (get-notif-act name type) #f
    (begin
      (set-notif-act name type action)
      (inc-notif-count type))))

(tm-define (notification-action name type) (get-notif-act name type))
(tm-define (notification-count type)
  (with c (get-notif-cnt type)
    (if c c 0)))
(tm-define (notification-total-count)
  (ahash-fold
    (lambda (k v c) (if (symbol? k) (+ c (get-notif-cnt k)) c))
    0 notifications-table))

(tm-define (notification-counts)
  (ahash-fold
    (lambda (k v l) (if (symbol? k) (acons k (get-notif-cnt k) l) l))
    '() notifications-table))

(tm-define (get-notifications type)
  (ahash-fold
    (lambda (key value l)
      (if (and (pair? key) (== (cdr key) type)) (cons key l) l))
    '() notifications-table))

(tm-define (clear-notifications type)
  (with todelete (get-notifications type)
    (for (k todelete)
         (ahash-remove! notifications-table k))
    (dec-notif-count type (length todelete))))
