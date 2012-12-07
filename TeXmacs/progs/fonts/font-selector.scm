
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-selector.scm
;; DESCRIPTION : Widget for font selection
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-selector)
  (:use (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-font-family "roman")
(tm-define selector-font-base-size "10")
(tm-define selector-font-series "medium")
(tm-define selector-font-shape "right")

(tm-widget (font-sample-text)
  (texmacs-output
    `(with "bg-color" "white"
       (document
         (with
           "font" ,selector-font-family
           "font-base-size" ,selector-font-base-size
           "font-series" ,selector-font-series
           "font-shape" ,selector-font-shape
           "abcdefghij, ABCDEFGHIJ, 0123456789")))
    '(style "generic")))

(tm-define (make-multi-with . l)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(with ,@l ,t) (cons (length l) (path-end t '())))))

(tm-define (font-select-font)
  (with l '()
    (when (!= selector-font-family (get-env "font"))
      (set! l (cons* "font" selector-font-family l)))
    (when (!= selector-font-base-size (get-env "font-base-size"))
      (set! l (cons* "font-base-size" selector-font-base-size l)))
    (when (!= selector-font-series (get-env "font-series"))
      (set! l (cons* "font-series" selector-font-series l)))
    (when (!= selector-font-shape (get-env "font-shape"))
      (set! l (cons* "font-shape" selector-font-shape l)))
    (apply make-multi-with l)))

(tm-widget (font-selector quit)
  (padded
    (bold (text "Font family and properties"))
    ===
    (horizontal
      (vertical
        (resize ("300px" "300px" "2000px") ("200px" "200px" "2000px")
          (scrollable
            (choice (set! selector-font-family answer)
                    (font-database-families)
                    selector-font-family))))
      ///
      (vertical
        (aligned
          (item (text "Size:")
            (enum (set! selector-font-base-size answer)
                  '("5" "8" "9" "10" "11" "12" "14" "17" "20" "40")
                  selector-font-base-size "100px"))
          (item (text "Weight:")
            (enum (set! selector-font-series answer)
                  '("light" "medium" "bold")
                  selector-font-series "100px"))
          (item (text "Slant:")
            (enum (set! selector-font-shape answer)
                  '("right" "italic" "slanted")
                  selector-font-shape "100px")))
        (horizontal (glue #f #t 0 0))))
    === ===
    (bold (text "Font sample"))
    ===
    (resize ("300px" "300px" "2000px") ("100px" "100px" "100px")
      (scrollable
        (refresh font-sample-text)))
    === ===
    (explicit-buttons
      (hlist >>> ("Ok" (begin (font-select-font) (quit)))))))

(tm-define (open-font-selector)
  (set! selector-font-family (get-env "font"))
  (set! selector-font-base-size (get-env "font-base-size"))
  (set! selector-font-series (get-env "font-series"))
  (set! selector-font-shape (get-env "font-shape"))
  (dialogue-window font-selector noop "Font selector"))
