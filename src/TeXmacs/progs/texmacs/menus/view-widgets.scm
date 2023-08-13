
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
      (cond ((== which "retina-scale")
             (cond ((== (get-retina-scale) 1.0) "1")
                   ((== (get-retina-scale) 2.0) "2")
                   (else (number->string (get-retina-scale)))))
            (else ""))))

(tm-define (set-retina-preference which val)
  (set-preference which val))

(tm-define (get-retina-boolean-preference which)
  (if (cpp-has-preference? which)
      (preference-on? which)
      (cond ((== which "retina-factor") (== (get-retina-factor) 2))
            ((== which "retina-zoom") (== (get-retina-zoom) 2))
            ((== which "retina-icons") (== (get-retina-icons) 2))
            (else #f))))

(tm-define (set-retina-boolean-preference which on?)
  (set-retina-preference which (if on? "on" "off")))

(tm-define (reset-retina-preferences)
  (reset-preference "retina-factor")
  (reset-preference "retina-zoom")
  (reset-preference "retina-icons")
  (reset-preference "retina-scale"))

(tm-widget (retina-settings-widget cmd)
  (centered
    (assuming (and (os-macos?) (qt4-gui?))
      (centered
	(aligned
	  (item (text "Use retina fonts:")
	    (toggle (set-retina-boolean-preference "retina-factor" answer)
		    (get-retina-boolean-preference "retina-factor")))
	  (item (text "Use retina icons:")
	    (toggle (set-retina-boolean-preference "retina-icons" answer)
		    (get-retina-boolean-preference "retina-icons")))
          (item (text "Scale graphical interface:")
            (enum (set-retina-preference "retina-scale" answer)
                  '("1" "1.2" "1.4" "1.6" "1.8" "2" "")
                  (get-retina-preference "retina-scale")
                  "5em")))))
    (assuming (and (os-macos?) (qt5-or-later-gui?))
      (centered
        (aligned
	  (item (text "Use retina fonts:")
	    (toggle (set-retina-boolean-preference "retina-factor" answer)
		    (get-retina-boolean-preference "retina-factor")))
          (assuming (!= (get-preference "gui theme") "")
            (item (text "Scale graphical interface:")
              (enum (set-retina-preference "retina-scale" answer)
                    '("1" "1.2" "1.5" "2" "")
                    (get-retina-preference "retina-scale")
                    "5em"))))))
    (assuming (not (os-macos?))
      (centered
	(aligned
	  (item (text "Double the zoom factor for TeXmacs documents:")
	    (toggle (set-retina-boolean-preference "retina-zoom" answer)
		    (get-retina-boolean-preference "retina-zoom")))
	  (item (text "Use high resolution icons:")
	    (toggle (set-retina-boolean-preference "retina-icons" answer)
		    (get-retina-boolean-preference "retina-icons")))
          (assuming (!= (get-preference "gui theme") "")
            (item (text "Scale of the graphical user interface:")
              (enum (set-retina-preference "retina-scale" answer)
                    '("1" "1.2" "1.5" "2" "")
                    (get-retina-preference "retina-scale")
                    "5em"))))))
    === ===
    (bottom-buttons
      ("Cancel" (cmd "cancel")) >>
      ("Reset" (begin (reset-retina-preferences) (cmd "ok"))) // //
      ("Ok" (cmd "ok")))))

(tm-define (open-retina-settings-window)
  (:interactive #t)
  (dialogue-window retina-settings-widget
    (lambda (answer)
      (when (== answer "ok")
        (notify-restart)))
    (if (os-macos?)
	"Retina screen settings"
	"High resolution screen settings")))

(tm-define (open-retina-settings)
  (:interactive #t)
  (if (side-tools?)
      (tool-select :transient-right 'retina-settings-tool)
      (open-retina-settings-window)))
