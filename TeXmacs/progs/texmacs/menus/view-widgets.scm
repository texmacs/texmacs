
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

(tm-widget (retina-settings-widget)
  (centered
    (centered
      (aligned
        (meti (hlist // (text "Use retina fonts"))
          (toggle (set-retina-factor (if answer 2 1))
                  (== (get-retina-factor) 2)))
        (meti (hlist // (text "Use retina icons"))
          (toggle (set-retina-icons (if answer 2 1))
                  (== (get-retina-icons) 2)))))
    ===
    (aligned
      (item (text "Graphical interface font scale:")
        (enum (begin
                (set-preference "retina-scale" answer)
                (set-retina-scale (string->number answer)))
              '("1" "1.2" "1.4" "1.6" "1.8" "")
              (number->string (get-retina-scale))
              "5em")))))

(tm-define (open-retina-settings)
  (:interactive #t)
  (top-window retina-settings-widget "Retina screen settings"))
