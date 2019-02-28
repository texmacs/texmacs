
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : view-widgets.scm
;; DESCRIPTION : the view widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus view-widgets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retina settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-retina-preference which)
  (if (cpp-has-preference? which)
      (get-preference which)
      (cond ((== which "retina-scale") (number->string (get-retina-scale)))
            (else ""))))

(tm-define (set-retina-preference which val)
  (set-preference which val))

(tm-define (get-retina-boolean-preference which)
  (if (cpp-has-preference? which)
      (preference-on? which)
      (cond ((== which "retina-factor") (== (get-retina-factor) 2))
            ((== which "retina-icons") (== (get-retina-icons) 2))
            (else #f))))

(tm-define (set-retina-boolean-preference which on?)
  (set-retina-preference which (if on? "on" "off")))

(tm-define (reset-retina-preferences)
  (reset-preference "retina-factor")
  (reset-preference "retina-icons")
  (reset-preference "retina-scale"))

(tm-widget (retina-settings-widget cmd)
  (centered
    (assuming (os-macos?)
      (centered
	(aligned
	  (meti (hlist // (text "Use retina fonts"))
	    (toggle (set-retina-boolean-preference "retina-factor" answer)
		    (get-retina-boolean-preference "retina-factor")))
	  (meti (hlist // (text "Use retina icons"))
	    (toggle (set-retina-boolean-preference "retina-icons" answer)
		    (get-retina-boolean-preference "retina-icons")))
	  (meti (hlist // (text "Use unified toolbars"))
            (toggle (set-boolean-preference "use unified toolbar" answer)
                    (get-boolean-preference "use unified toolbar"))))))
    (assuming (not (os-macos?))
      (centered
	(aligned
	  (meti (hlist // (text "Use high resolution fonts"))
	    (toggle (set-retina-boolean-preference "retina-factor" answer)
		    (get-retina-boolean-preference "retina-factor")))
	  (meti (hlist // (text "Use high resolution icons"))
	    (toggle (set-retina-boolean-preference "retina-icons" answer)
		    (get-retina-boolean-preference "retina-icons"))))))
    ===
    (aligned
      (item (text "Graphical interface font scale:")
        (enum (set-retina-preference "retina-scale" answer)
              '("1" "1.2" "1.4" "1.6" "1.8" "")
              (get-retina-preference "retina-scale")
              "5em")))
    === ===
    (bottom-buttons
      ("Cancel" (cmd "cancel")) >>
      ("Reset" (begin (reset-retina-preferences) (cmd "ok"))) //
      ("Ok" (cmd "ok")))))


(tm-widget (retina-settings-notify cmd)
  (padded
    (text "Restart TeXmacs in order to let changes take effect")
    ===
    (bottom-buttons
      >>
      ("Ok" (cmd "Ok"))
      >>)))

(tm-define (open-retina-settings)
  (:interactive #t)
  (dialogue-window retina-settings-widget
    (lambda (answer)
      (when (== answer "ok")
        (delayed
          (:idle 1)
	  (dialogue-window retina-settings-notify noop "Notification"))))
    (if (os-macos?)
	"Retina screen settings"
	"High resolution screen settings")))
