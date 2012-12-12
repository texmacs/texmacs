
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

;; (tm-define selector-font-size "10")
;; (tm-define selector-font-weight "Medium")
;; (tm-define selector-font-slant "Normal")
;; (tm-define selector-font-stretch "Unstretched")
;; (tm-define selector-font-serif "Serif")
;; (tm-define selector-font-aspect "Proportional")
;; (tm-define selector-font-imitate "Printed")
;; (tm-define selector-font-case "Mixed")
;; (tm-define selector-font-purpose "Generic")


;; (horizontal (glue #f #f 0 0) (bold (text "Shape")))
;; ===
;; (aligned
;;   (item (text "Size:")
;;     (enum (set! selector-font-size answer)
;;           '("5" "7" "8" "9" "10" "11" "12" "14" "18" "24"
;;             "30" "36" "48" "64")
;;           selector-font-size "120px"))
;;   (item (text "Weight:")
;;     (enum (set! selector-font-weight answer)
;;           '("Light" "Medium" "Bold")
;;           selector-font-weight "120px"))
;;   (item (text "Slant:")
;;     (enum (set! selector-font-slant answer)
;;           '("Normal" "Italic" "Oblique")
;;           selector-font-slant "120px")))
;; === === === ===
;; (horizontal (glue #f #f 0 0) (bold (text "Features")))
;; ===
;; (aligned
;;   (item (text "Serif:")
;;     (enum (set! selector-font-serif answer)
;;           '("Serif" "Sans Serif")
;;           selector-font-serif "120px"))
;;   (item (text "Aspect:")
;;     (enum (set! selector-font-aspect answer)
;;           '("Proportional" "Monospaced")
;;           selector-font-aspect "120px"))
;;   (item (text "Case:")
;;     (enum (set! selector-font-case answer)
;;           '("Mixed" "Small Capitals")
;;           selector-font-case "120px")))
;; (horizontal (glue #f #t 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state of font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-font-family "TeXmacs Computer Modern")
(tm-define selector-font-style "Regular")
(tm-define selector-font-size "10")

(tm-define (selector-get-font)
  (logical-font-public selector-font-family selector-font-style))

(define (selector-initialize-font)
  (let* ((fam (get-env "font"))
         (var (get-env "font-family"))
         (ser (get-env "font-series"))
         (sh  (get-env "font-shape"))
         (sz  (get-env "font-base-size"))
         (lf  (logical-font-private fam var ser sh))
         (fn  (logical-font-search lf #t)))
    ;;(display* "lf= " lf "\n")
    ;;(display* "fn= " fn "\n")
    (set! selector-font-family (car fn))
    (set! selector-font-style (cadr fn))
    (set! selector-font-size sz)))

(tm-define (make-multi-with . l)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(with ,@l ,t) (cons (length l) (path-end t '())))))

(tm-define (selector-apply-font)
  (when (!= selector-font-style "Unknown")
    (with fn (selector-get-font)
      (with l '()
        (when (!= selector-font-size (get-env "font-base-size"))
          (set! l (cons* "font-base-size" selector-font-size l)))
        (when (!= (logical-font-shape fn) (get-env "font-shape"))
          (set! l (cons* "font-shape" (logical-font-shape fn) l)))
        (when (!= (logical-font-series fn) (get-env "font-series"))
          (set! l (cons* "font-series" (logical-font-series fn) l)))
        (when (!= (logical-font-variant fn) (get-env "font-family"))
          (set! l (cons* "font-family" (logical-font-variant fn) l)))
        (when (!= (logical-font-family fn) (get-env "font"))
          (set! l (cons* "font" (logical-font-family fn) l)))
        (apply make-multi-with l)))))

(define (selector-font-demo-text)
  (with fn (selector-get-font)
    ;;(display* "Font: " fn "\n")
    ;;(display* "Internal font: " (logical-font-family fn)
    ;;          ", " (logical-font-variant fn)
    ;;          ", " (logical-font-series fn)
    ;;          ", " (logical-font-shape fn)
    ;;          ", " selector-font-size "\n")
    `(document
       (with
         "font" ,(logical-font-family fn)
         "font-family" ,(logical-font-variant fn)
         "font-series" ,(logical-font-series fn)
         "font-shape" ,(logical-font-shape fn)
         "font-base-size" ,selector-font-size
         "abcdefghij, ABCDEFGHIJ, 0123456789"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (font-default-sizes)
  '("5" "6" "7" "8" "9" "10" "11" "12" "14" "16" "18" "20"
    "24" "28" "32" "36" "40" "48" "64" "72" "96"
    "128" "144" "192"))

(tm-widget (font-style-selector)
  (vertical
    (bold (text "Style"))
    ===
    (resize ("200px" "200px" "400px") ("250px" "250px" "2000px")
      (scrollable
        (choice (set! selector-font-style answer)
                (font-database-styles selector-font-family)
                selector-font-style)))))

(tm-widget (font-sample-text)
  (texmacs-output
    `(with "bg-color" "white"
       ,(selector-font-demo-text))
    '(style "generic")))

(tm-widget (font-selector quit)
  (padded
    (horizontal
      (vertical
        (bold (text "Family"))
         ===
        (resize ("300px" "300px" "2000px") ("250px" "250px" "2000px")
          (scrollable
            (choice (set! selector-font-family answer)
                    (font-database-families)
                    selector-font-family))))
      ///
      (refresh font-style-selector)
      ///
      (vertical
        (bold (text "Size"))
         ===
        (resize ("75px" "75px" "75px") ("250px" "250px" "2000px")
          (scrollable
            (choice (set! selector-font-size answer)
                    (font-default-sizes)
                    selector-font-size)))))
    === === ===
    (bold (text "Sample text"))
    ===
    (resize ("300px" "300px" "2000px") ("100px" "100px" "100px")
      (scrollable
        (refresh font-sample-text)))
    === ===
    (explicit-buttons
      (hlist >>> ("Ok" (begin (selector-apply-font) (quit)))))))

(tm-define (open-font-selector)
  (selector-initialize-font)
  (dialogue-window font-selector noop "Font selector"))
