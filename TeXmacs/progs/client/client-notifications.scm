
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : client-notifications.scm
;; DESCRIPTION : Remote notification helpers
;; COPYRIGHT   : (C) 2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (client client-notifications)
  (:use (client client-base))
        (notification notification-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (notifiable-icon server kind normal has-notifs)
  (if (has-notifications? server kind) `(icon ,has-notifs) `(icon ,normal)))

(tm-define (notif-count-label server kind label)
  (with c (notification-count server kind)
    (cond
      ((> c 0)  (string-append label " (" (number->string c) ")"))
      ((> c 99) (string-append label " (99+)"))
      (else label))))

(tm-define (notifiable-entry server kind label action)
  (if (> (notification-count server kind) 0)
      `((style ,widget-style-bold ,(notif-count-label server kind label))
        ,(lambda ()
           (action)
           (clear-notifications server kind)
           (client-ack-remote-notifications server kind)))
      `(,label ,action)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Remote wiring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-notification-from-record server nid notif)
  (with kind (string->symbol (car (assoc-ref notif "kind")))
    (cond
      ((eq? kind 'message)
       (add-notification server nid 'message
                         (lambda () (mail-box-open server)) notif))
      (else #f))))


(tm-call-back (client-push-notifications nid notif)
  (with (server msg-id) envelope
    (add-notification-from-record server nid notif)))

(tm-define (client-pending-notifications-then server cb)
  (client-remote-then server `(remote-pending-notifications) cb
                      "Cannot get pending notifications: "))

(tm-define (client-sync-remote-notifications server)
  (client-pending-notifications-then server
    (lambda (notifs)
      (for-each
        (lambda (n) (add-notification-from-record server (car n) (cadr n)))
        notifs))))

(tm-define (client-ack-remote-notifications server kind)
  (client-remote-eval* server `(remote-ack-notifications ,kind)
    (lambda (ret)
      (when (!= ret "done")
        (client-open-error
          (string-append "Cannot ack notifications: " ret))))))
