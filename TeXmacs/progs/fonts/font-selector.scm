
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
;; From old style to new style font properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define old-family first)
(define old-variant second)
(define old-series third)
(define old-shape fourth)
(define old-size fifth)

(define (decode-family fn)
  ;; Font family
  (with f (old-family fn)
    (cond ((== f "roman") "Computer Modern")
          ((== f "concrete") "Concrete")
          ;;
          ((== f "duerer") "Duerer")
          ((== f "pandora") "Pandora")
          ((== f "punk") "Punk")
          (else f))))

(define (decode-size fn)
  ;; Font name in points
  (with s (old-size fn)
    s))

(define (decode-weight fn)
  ;; Font weight
  (with s (old-series fn)
    (cond ((== s "light") "Light")
          ((== s "bold") "Bold")
          ((== s "medium") "Medium")
          (else "Medium"))))

(define (decode-slant fn)
  ;; Font slant
  (with s (old-shape fn)
    (cond ((== s "condensed") "Normal")
          ((== s "flat") "Normal")
          ((== s "italic") "Italic")
          ((== s "italic-flat") "Italic")
          ((== s "italic-right") "Upright Italic")
          ((== s "italic-small-caps") "Italic")
          ((== s "long") "Normal")
          ((== s "slanted-flat") "Oblique")
          ((== s "slanted-small-caps") "Oblique")
          ((== s "small-caps") "Normal")
          (else "Normal"))))

(define (decode-stretch fn)
  ;; Font stretching
  (with s (old-shape fn)
    (cond ((== s "condensed") "Condensed")
          ((== s "flat") "Unstretched")
          ((== s "italic") "Unstretched")
          ((== s "italic-flat") "Unstretched")
          ((== s "italic-right") "Unstretched")
          ((== s "italic-small-caps") "Unstretched")
          ((== s "long") "Unstretched")
          ((== s "slanted-flat") "Unstretched")
          ((== s "slanted-small-caps") "Unstretched")
          ((== s "small-caps") "Unstretched")
          (else "Unstretched"))))

(define (decode-serif fn)
  ;; Hint whether we should use a serif font
  (with v (old-variant fn)
    (cond ((== v "rm") "Serif")
          ((== v "ss") "Sans Serif")
          ((== v "tt") "Serif")
          (else "Serif"))))

(define (decode-aspect fn)
  ;; Hint whether we should use a monospaced font
  (with v (old-variant fn)
    (cond ((== v "rm") "Proportional")
          ((== v "ss") "Proportional")
          ((== v "tt") "Monospaced")
          ((== v "hw") "Proportional")
          (else "Proportional"))))

(define (decode-imitate fn)
  ;; Hint whether the font should imitate some writing device
  (with v (old-variant fn)
    (cond ((== v "rm") "Printed")
          ((== v "ss") "Printed")
          ((== v "tt") "Typewriter")
          ((== v "hw") "Handwriting")
          (else "Printed"))))

(define (decode-case fn)
  ;; Hint on how words should be capitalized
  (with s (old-shape fn)
    (cond ((== s "condensed") "Mixed")
          ((== s "flat") "Mixed")
          ((== s "italic") "Mixed")
          ((== s "italic-flat") "Mixed")
          ((== s "italic-right") "Mixed")
          ((== s "italic-small-caps") "Small Capitals")
          ((== s "long") "Mixed")
          ((== s "slanted-flat") "Mixed")
          ((== s "slanted-small-caps") "Small Capitals")
          ((== s "small-caps") "Small Capitals")
          (else "Mixed"))))

(define (decode-purpose fn)
  ;; Hint concerning the purpose of a font
  ;; Not really implemented yet; possible future values
  ;; include 'Title' and 'Decorative'
  (with s (old-shape fn)
    (cond ((== s "condensed") "Generic")
          ((== s "flat") "Flat")
          ((== s "italic") "Generic")
          ((== s "italic-flat") "Flat")
          ((== s "italic-right") "Generic")
          ((== s "italic-small-caps") "Generic")
          ((== s "long") "Long")
          ((== s "slanted-flat") "Flat")
          ((== s "slanted-small-caps") "Generic")
          ((== s "small-caps") "Generic")
          (else "Generic"))))

(define (decode-font fn)
  (list (decode-family fn)
        (decode-size fn)
        (decode-weight fn)
        (decode-slant fn)
        (decode-stretch fn)
        (decode-serif fn)
        (decode-aspect fn)
        (decode-imitate fn)
        (decode-case fn)
        (decode-purpose fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From new style to old style font properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define new-family first)
(define new-size second)
(define new-weight third)
(define new-slant fourth)
(define new-stretch fifth)
(define new-serif sixth)
(define new-aspect seventh)
(define new-imitate eighth)
(define new-case ninth)
(define new-purpose tenth)

(define (encode-family fn)
  (with f (new-family fn)
    (cond ((== f "Computer Modern") "roman")
          ((== f "Concrete") "concrete")
          ;;
          ((== f "Duerer") "duerer")
          ((== f "Pandora") "pandora")
          ((== f "Punk") "punk")
          (else f))))

(define (encode-variant fn)
  (let* ((serif (new-serif fn))
         (aspect (new-aspect fn))
         (imitate (new-imitate fn)))
    (cond ((== serif "Sans Serif") "ss")
          ((== aspect "Monospaced") "tt")
          ((== imitate "Typewriter") "tt")
          ((== imitate "Handwriting") "hw")
          (else "rm"))))


(define (encode-series fn)
  (with w (new-weight fn)
    (cond ((== w "Light") "light")
          ((== w "Medium") "medium")
          ((== w "Bold") "bold")
          (else "medium"))))

(define (encode-shape fn)
  (let* ((slant (new-slant fn))
         (stretch (new-stretch fn))
         (caps (new-case fn))
         (purpose (new-purpose fn)))
    (cond ((and (== purpose "Flat") (== slant "Italic"))
           "italic-flat")
          ((and (== purpose "Flat") (== slant "Oblique"))
           "slanted-flat")
          ((and (== caps "Small Capitals") (== slant "Italic"))
           "italic-small-caps")
          ((and (== caps "Small Capitals") (== slant "Oblique"))
           "slanted-small-caps")
          ((== purpose "Flat") "flat")
          ((== purpose "Long") "long")
          ((== stretch "Condensed") "condensed")
          ((== caps "Small Capitals") "small-caps")
          ((== slant "Normal") "right")
          ((== slant "Italic") "italic")
          ((== slant "Oblique") "slanted")
          (else "right"))))

(define (encode-size fn)
  (with s (new-size fn)
    s))

(define (encode-font fn)
  (list (encode-family fn)
        (encode-variant fn)
        (encode-series fn)
        (encode-shape fn)
        (encode-size fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state of font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-font-family "Computer Modern")
(tm-define selector-font-size "10")
(tm-define selector-font-weight "Medium")
(tm-define selector-font-slant "Normal")
(tm-define selector-font-stretch "Unstretched")
(tm-define selector-font-serif "Serif")
(tm-define selector-font-aspect "Proportional")
(tm-define selector-font-imitate "Printed")
(tm-define selector-font-case "Mixed")
(tm-define selector-font-purpose "Generic")

(define (selector-set-font fn)
  (set! selector-font-family (new-family fn))
  (set! selector-font-size (new-size fn))
  (set! selector-font-weight (new-weight fn))
  (set! selector-font-slant (new-slant fn))
  (set! selector-font-stretch (new-stretch fn))
  (set! selector-font-serif (new-serif fn))
  (set! selector-font-aspect (new-aspect fn))
  (set! selector-font-imitate (new-imitate fn))
  (set! selector-font-case (new-case fn))
  (set! selector-font-purpose (new-purpose fn)))

(define (selector-get-font)
  (list selector-font-family
        selector-font-size
        selector-font-weight
        selector-font-slant
        selector-font-stretch
        selector-font-serif
        selector-font-aspect
        selector-font-imitate
        selector-font-case
        selector-font-purpose))

(define (selector-initialize-font)
  (with fn (list (get-env "font")
                 (get-env "font-family")
                 (get-env "font-series")
                 (get-env "font-shape")
                 (get-env "font-base-size"))
    (selector-set-font (decode-font fn))))

(tm-define (make-multi-with . l)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(with ,@l ,t) (cons (length l) (path-end t '())))))

(tm-define (selector-apply-font)
  (with fn (encode-font (selector-get-font))
    (with l '()
      (when (!= (old-family fn) (get-env "font"))
        (set! l (cons* "font" (old-family fn) l)))
      (when (!= (old-size fn) (get-env "font-base-size"))
        (set! l (cons* "font-base-size" (old-size fn) l)))
      (when (!= (old-series fn) (get-env "font-series"))
        (set! l (cons* "font-series" (old-series fn) l)))
      (when (!= (old-shape fn) (get-env "font-shape"))
        (set! l (cons* "font-shape" (old-shape fn) l)))
      (when (!= (old-variant fn) (get-env "font-family"))
        (set! l (cons* "font-family" (old-variant fn) l)))
      (apply make-multi-with l))))

(define (selector-font-demo-text)
  (with fn (encode-font (selector-get-font))
    ;;(display* "Font: " fn "\n")
    `(document
       (with
         "font" ,(old-family fn)
         "font-base-size" ,(old-size fn)
         "font-series" ,(old-series fn)
         "font-shape" ,(old-shape fn)
         "font-family" ,(old-variant fn)
         "abcdefghij, ABCDEFGHIJ, 0123456789"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (all-available-fonts)
  (with l (cons* "Computer Modern"
                 "Concrete"
                 (font-database-families))
    (list-sort l string<=?)))

(tm-widget (font-sample-text)
  (texmacs-output
    `(with "bg-color" "white"
       ,(selector-font-demo-text))
    '(style "generic")))

(tm-widget (font-selector quit)
  (padded
    (bold (text "Font"))
    ===
    (horizontal
      (vertical
        (resize ("400px" "400px" "2000px") ("250px" "250px" "2000px")
          (scrollable
            (choice (set! selector-font-family answer)
                    (all-available-fonts)
                    selector-font-family))))
      ///
      (vertical
        (horizontal (glue #f #f 0 0) (bold (text "Shape")))
        ===
        (aligned
          (item (text "Size:")
            (enum (set! selector-font-size answer)
                  '("5" "7" "8" "9" "10" "11" "12" "14" "18" "24"
                    "30" "36" "48" "64")
                  selector-font-size "100px"))
          (item (text "Weight:")
            (enum (set! selector-font-weight answer)
                  '("Light" "Medium" "Bold")
                  selector-font-weight "100px"))
          (item (text "Slant:")
            (enum (set! selector-font-slant answer)
                  '("Normal" "Italic" "Oblique")
                  selector-font-slant "100px")))
        === === === ===
        (horizontal (glue #f #f 0 0) (bold (text "Features")))
        ===
        (aligned
          (item (text "Serif:")
            (enum (set! selector-font-serif answer)
                  '("Serif" "Sans Serif")
                  selector-font-serif "100px"))
          (item (text "Aspect:")
            (enum (set! selector-font-aspect answer)
                  '("Proportional" "Monospaced")
                  selector-font-aspect "100px"))
          (item (text "Case:")
            (enum (set! selector-font-case answer)
                  '("Mixed" "Small Capitals")
                  selector-font-case "100px")))
        (horizontal (glue #f #t 0 0))))
    === === ===
    (bold (text "Sample text"))
    ===
    (resize ("400px" "400px" "2000px") ("100px" "100px" "100px")
      (scrollable
        (refresh font-sample-text)))
    === ===
    (explicit-buttons
      (hlist >>> ("Ok" (begin (selector-apply-font) (quit)))))))

(tm-define (open-font-selector)
  (selector-initialize-font)
  (dialogue-window font-selector noop "Font selector"))
