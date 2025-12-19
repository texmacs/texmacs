
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

;; server -> state
;; state:
;;  - payloads : nid -> notif-record
;;  - actions  : nid -> thunk
;;  - kinds    : nid -> kind(symbol)
;;  - counts   : kind(symbol) -> int
(define notifications-by-serv (make-ahash-table))

(define (make-notif-state)
  `((payloads . ,(make-ahash-table))
    (actions  . ,(make-ahash-table))
    (kinds    . ,(make-ahash-table))
    (counts   . ,(make-ahash-table))))

(define (state-ref st k) (cdr (assq k st)))

(define (get-or-make-state server)
  (with st (ahash-ref notifications-by-serv server)
    (if st st
      (begin
        (ahash-set! notifications-by-serv server (make-notif-state))
        (ahash-ref notifications-by-serv server)))))

(define (payloads-table server) (state-ref (get-or-make-state server) 'payloads))
(define (actions-table  server) (state-ref (get-or-make-state server) 'actions))
(define (kinds-table    server) (state-ref (get-or-make-state server) 'kinds))
(define (counts-table   server) (state-ref (get-or-make-state server) 'counts))

(define (inc-count! server kind n)
  (let* ((counts (counts-table server))
         (c      (ahash-ref counts kind)))
    (ahash-set! counts kind (+ n (or c 0)))
    (ahash-ref counts kind)))

(define (dec-count! server kind n)
  (let* ((counts (counts-table server))
         (c      (ahash-ref counts kind)))
    (ahash-set! counts kind (max 0 (- (or c 0) n)))
    (ahash-ref counts kind)))

(tm-define (notification-count server kind)
  (or (ahash-ref (counts-table server) kind) 0))

(tm-define (notification-total-count server)
  (ahash-fold (lambda (k v acc) (+ acc (or v 0))) 0 (counts-table server)))

(tm-define (notification-counts server)
  (ahash-fold (lambda (k v l) (acons k (or v 0) l)) '() (counts-table server)))

(tm-define (has-notifications? server kind)
  (> (notification-count server kind) 0))

(tm-define (add-notification server nid kind action payload)
  (let* ((actions  (actions-table server))
         (payloads (payloads-table server))
         (kinds    (kinds-table server)))
    (if (ahash-ref actions nid) #f
      (begin
        (ahash-set! actions  nid action)
        (ahash-set! payloads nid payload)
        (ahash-set! kinds    nid kind)
        (inc-count! server kind 1)
        #t))))

(tm-define (notification-action server nid)
  (ahash-ref (actions-table server) nid))

(tm-define (notification-payload server nid)
  (ahash-ref (payloads-table server) nid))

(tm-define (notification-kind server nid)
  (ahash-ref (kinds-table server) nid))

(tm-define (get-notifications server kind)
  (ahash-fold
    (lambda (nid k acc) (if (== k kind) (cons nid acc) acc))
    '() (kinds-table server)))

(tm-define (clear-notifications server kind)
  (with todel (get-notifications server kind)
    (for (nid todel)
      (ahash-remove! (actions-table server) nid)
      (ahash-remove! (payloads-table server) nid)
      (ahash-remove! (kinds-table server) nid))
    (dec-count! server kind (length todel))))
