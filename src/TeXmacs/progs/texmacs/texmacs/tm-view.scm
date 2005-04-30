
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-view.scm
;; DESCRIPTION : setting the view preferences and properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-view)
  (:export
    toggle-visible-header toggle-visible-footer toggle-visible-icon-bar
    toggle-full-screen-mode toggle-full-screen-edit-mode
    other-shrinking-factor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; View preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-header var val)
  (show-header (== val "on")))

(define (notify-icon-bar var val)
  (cond ((== var "main icon bar")
	 (show-icon-bar 0 (== val "on")))
	((== var "context dependent icons")
	 (show-icon-bar 1 (== val "on")))
	((== var "user provided icons")
	 (show-icon-bar 2 (== val "on")))))

(define (notify-status-bar var val)
  (show-footer (== val "on")))

(define (notify-shrinking-factor var val)
  (set-default-shrinking-factor (string->number val))
  (set-shrinking-factor (string->number val)))

(define-preferences
  ("header" "on" notify-header)
  ("main icon bar" "on" notify-icon-bar)
  ("context dependent icons" "on" notify-icon-bar)
  ("user provided icons" "off" notify-icon-bar)
  ("status bar" "on" notify-status-bar)
  ("shrinking factor" "5" notify-shrinking-factor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the view properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-visible-header)
  (:synopsis "Toggle the visibility of the window's header.")
  (:check-mark "v" visible-header?)
  (show-header (not (visible-header?))))

(tm-define (toggle-visible-footer)
  (:synopsis "Toggle the visibility of the window's footer.")
  (:check-mark "v" visible-footer?)
  (show-footer (not (visible-footer?))))

(tm-define (toggle-visible-icon-bar n)
  (:synopsis "Toggle the visibility of the @n-th icon bar.")
  (:check-mark "v" visible-icon-bar?)
  (show-icon-bar n (not (visible-icon-bar? n))))

(tm-define (toggle-full-screen-mode)
  (:synopsis "Toggle full screen mode.")
  (:check-mark "v" full-screen?)
  (full-screen-mode (not (full-screen?)) #f))

(tm-define (toggle-full-screen-edit-mode)
  (:synopsis "Toggle full screen edit mode.")
  (:check-mark "v" full-screen-edit?)
  (full-screen-mode (not (full-screen-edit?)) (not (full-screen-edit?))))

(define (test-shrinking-factor? n)
  (= (get-shrinking-factor) n))

(tm-property (set-shrinking-factor n)
  (:check-mark "*" test-shrinking-factor?))

(define (other-shrinking-factor s)
  (set-shrinking-factor (string->number s)))
