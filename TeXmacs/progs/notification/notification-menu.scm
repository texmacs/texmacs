
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : notification-menu.scm
;; DESCRIPTION : menus for remote TeXmacs services
;; COPYRIGHT   : (C) 2025  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (notification notification-menu)
  (:use (notification notification-base)))

(tm-define (notifiable-icon notif-type normal has-notifs)
  (if (> (notification-count notif-type) 0)
    `(icon ,has-notifs)
    `(icon ,normal)))

(tm-define (notif-count-label type label)
  (with c (notification-count type)
    (if (> c 0)
      (string-append label " (" (number->string (notification-count type)) ")")
      label)))

(tm-define (notifiable-entry type label action)
  (if (> (notification-count 'message) 0)
    `((style ,widget-style-bold ,(notif-count-label type label))
      ,(lambda () (action) (clear-notifications type)))
    `(,label ,action)))
