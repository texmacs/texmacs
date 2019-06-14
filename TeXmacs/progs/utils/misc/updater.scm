
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : updater.scm
;; DESCRIPTION : support utilities for tm_updater
;; COPYRIGHT   : (C) 2013 Miguel de Benito Delgado
;;               2019 modified by Gregoire Lecerf 
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc updater))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preference management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (updater-initialize)
  (when (updater-supported?)
	(with n (get-preference "updater:interval")
	      (when (string-number? n)
		    (updater-set-interval (string->number n))
		    (updater-check-background)))))

(define-preferences
  ("updater:interval" "null" noop))

;; Uncomment to allow automatic updates
;(when (== (get-preference "updater:interval" "null"))
;    (set-preference "updater:interval" "168"))

