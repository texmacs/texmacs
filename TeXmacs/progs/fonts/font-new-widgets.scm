
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

(texmacs-module (fonts font-new-widgets)
  (:use (kernel gui menu-widget)
        (fonts font-sample)
        (generic format-edit)
        (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font samples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (standard-selector-text)
  `(with "par-par-sep" "0.2em"
     (document
       "Lowercase: abcdefghijklmnopqrstuvwxyz"
       "Uppercase: ABCDEFGHIJKLMNOPQRSTUVWXYZ"
       "Numbers: 0123456789 +-*/^=<less><gtr>"
       "Special: ([{|}]) \"`,.:;!?'\" @#$%&_\\~"
       "Accented: ‡·‰‚„ËÈÎÍÏÌÔÓÚÛˆÙı˘˙¸˚"
       ,(string-append
         "Greek: <alpha><beta><gamma><delta><varepsilon><zeta><eta><theta>"
         "<iota><kappa><lambda><mu><nu><xi><omicron><pi>"
         "<rho><sigma><tau><upsilon><varphi><psi><chi><omega>")
       ,(string-append
         "Cyrillic: <#430><#431><#432><#433><#434><#435><#436><#437>"
         "<#438><#439><#43A><#43B><#43C><#43D><#43E><#43F>"
         "<#440><#441><#442><#443><#444><#445><#446><#447>"
         "<#448><#449><#44A><#44B><#44C><#44D><#44E><#44F>")
       ,(string-append
         "Mathematics: <leq><geq><leqslant><geqslant><prec><succ> "
         "<leftarrow><rightarrow><Leftarrow><Rightarrow><mapsto> "
         "<times><cdot><oplus><otimes>"))))

(define-public sample-text (standard-selector-text))
(define-public sample-kind "Standard")

(tm-define (set-font-sample-range hexa-start hexa-end)
  (:argument hexa-start "First unicode character in hexadecimal")
  (:argument hexa-end "Last unicode character in hexadecimal")
  (set! sample-text
        (build-character-table (hexadecimal->integer hexa-start)
                               (hexadecimal->integer hexa-end))))

(define (set-font-sample-kind kind)
  (set! sample-kind kind)
  (cond ((== kind "ASCII")
         (set-font-sample-range "20" "7f"))
        ((== kind "Latin")
         (set-font-sample-range "80" "ff"))
        ((== kind "Greek")
         (set-font-sample-range "380" "3ff"))
        ((== kind "Cyrillic")
         (set-font-sample-range "400" "4ff"))
        ((== kind "CJK")
         (set-font-sample-range "4e00" "9fcc"))
        ((== kind "Hangul")
         (set-font-sample-range "ac00" "d7af"))
        ((== kind "Math Symbols")
         (set-font-sample-range "2000" "23ff"))
        ((== kind "Math Extra")
         (set-font-sample-range "2900" "2e7f"))
        ((== kind "Math Letters")
         (set-font-sample-range "1d400" "1d7ff"))
        ((== kind "Unicode 0000-0fff")
         (set-font-sample-range "0000" "0fff"))
        ((== kind "Unicode 1000-1fff")
         (set-font-sample-range "1000" "1fff"))
        ((== kind "Unicode 2000-2fff")
         (set-font-sample-range "2000" "2fff"))
        ((== kind "Unicode 3000-3fff")
         (set-font-sample-range "3000" "3fff"))
        ((== kind "Unicode 4000-4fff")
         (set-font-sample-range "4000" "4fff"))
        ((and (== kind "Selection") (selection-active-any?))
         (set! sample-text (tree->stree (selection-tree))))
        (else
         (set! sample-text (standard-selector-text)))))

(define (get-font-sample-kind)
  sample-kind)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state of font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-font-family "TeXmacs Computer Modern")
(tm-define selector-font-style "Regular")
(tm-define selector-font-size "10")

(tm-define (selector-get-font)
  (logical-font-patch
    (logical-font-public selector-font-family selector-font-style)
    (selected-properties)))

(define (selector-initialize-font getter)
  (let* ((fam (font-family-main (getter "font")))
         (var (getter "font-family"))
         (ser (getter "font-series"))
         (sh  (getter "font-shape"))
         (sz  (getter "font-base-size"))
         (lf  (logical-font-private fam var ser sh))
         (fn  (logical-font-search-exact lf)))
    ;;(display* "lf= " lf "\n")
    ;;(display* "fn= " fn "\n")
    (set! selector-font-family (car fn))
    (set! selector-font-style (cadr fn))
    (set! selector-font-size sz)
    (selector-initialize-search)))

(tm-define (selector-get-changes getter)
  (if (== selector-font-style "Unknown")
      (list)
      (with fn (selector-get-font)
        (with l '()
          (when (!= selector-font-size (getter "font-base-size"))
            (set! l (cons* "font-base-size" selector-font-size l)))
          (when (!= (logical-font-shape fn) (getter "font-shape"))
            (set! l (cons* "font-shape" (logical-font-shape fn) l)))
          (when (!= (logical-font-series fn) (getter "font-series"))
            (set! l (cons* "font-series" (logical-font-series fn) l)))
          (when (!= (logical-font-variant fn) (getter "font-family"))
            (set! l (cons* "font-family" (logical-font-variant fn) l)))
          (when (!= (logical-font-family fn) (getter "font"))
            (set! l (cons* "font" (logical-font-family fn) l)))
          l))))

(define (selector-font-simulate-comment)
  (let* ((fn  (selector-get-font))
	 (fam (logical-font-family fn))
         (var (logical-font-variant fn))
         (ser (logical-font-series fn))
         (sh  (logical-font-shape fn))
         (lf  (logical-font-private fam var ser sh))
         (fn2 (logical-font-search lf))
         (sel (string-recompose (selected-properties) " ")))
    ;;(display* "fn = " fn "\n")
    ;;(display* "lf = " lf "\n")
    ;;(display* "fn2= " fn2 "\n")
    (if (and (== selector-font-family (car fn2))
             (== selector-font-style (cadr fn2))
             (== sel ""))
        ""
        (string-append "  (" selector-font-family " " selector-font-style
                       (if (== sel "") "" " + ") sel
                       " -> " (car fn2) " " (cadr fn2) ")"))))

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
         ,sample-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state for font searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-search-weight "Any")
(tm-define selector-search-slant "Any")
(tm-define selector-search-stretch "Any")
(tm-define selector-search-serif "Any")
(tm-define selector-search-spacing "Any")
(tm-define selector-search-case "Any")
(tm-define selector-search-device "Any")
(tm-define selector-search-category "Any")
(tm-define selector-search-glyphs "Any")

(define (selector-initialize-search)
  (set! selector-search-weight "Any")
  (set! selector-search-slant "Any")
  (set! selector-search-stretch "Any")
  (set! selector-search-serif "Any")
  (set! selector-search-spacing "Any")
  (set! selector-search-case "Any")
  (set! selector-search-device "Any")
  (set! selector-search-category "Any")
  (set! selector-search-glyphs "Any"))

(define (selector-search-glyphs-decoded)
  (with s selector-search-glyphs
    (cond ((== s "ASCII") "Ascii")
          ((== s "Math Symbols") "MathSymbols")
          ((== s "Math Extra") "MathExtra")
          ((== s "Math Letters") "MathLetters")
          (else s))))

(define (selected-properties)
  (with l (list selector-search-weight
                selector-search-slant
                selector-search-stretch
                selector-search-serif
                selector-search-spacing
                selector-search-case
                selector-search-device
                selector-search-category
                (selector-search-glyphs-decoded))
    (list-filter l (cut != <> "Any"))))

(tm-define-macro (selector-search-set! var val)
  `(begin
     (set! ,var ,val)
     (delayed
       (refresh-now "font-family-selector"))))

(tm-define (selected-families)
  (search-font-families (selected-properties)))

(tm-define (selected-styles family)
  (search-font-styles family (selected-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (font-default-sizes)
  '("5" "6" "7" "8" "9" "10" "11" "12" "14" "16" "18" "20"
    "24" "28" "32" "36" "40" "48" "64" "72" "96"
    "128" "144" "192"))

(tm-widget (font-family-selector)
  (vertical
    (bold (text "Family"))
    ===
    (resize ("300px" "300px" "2000px") ("325px" "325px" "2000px")
      (scrollable
        (choice (set! selector-font-family answer)
                (selected-families)
                selector-font-family)))))

(tm-widget (font-style-selector)
  (vertical
    (bold (text "Style"))
    ===
    (resize ("200px" "200px" "400px") ("325px" "325px" "2000px")
      (scrollable
        (choice (set! selector-font-style answer)
                (selected-styles selector-font-family)
                selector-font-style)))))

(tm-widget (font-size-selector)
  (vertical
    (bold (text "Size"))
    ===
    (resize ("75px" "75px" "75px") ("325px" "325px" "2000px")
      (scrollable
        (choice (set! selector-font-size answer)
                (font-default-sizes)
                selector-font-size)))))

(tm-widget (font-sample-text)
  (texmacs-output
    `(with "bg-color" "white"
       ,(selector-font-demo-text))
    '(style "generic")))

(tm-widget (font-properties-selector)
  (vertical
    (horizontal
      (glue #f #f 0 0)
      (bold (text "Filter"))
      (glue #f #f 0 0))
    ===
    (aligned
      ;;(item (text "Base family:")
      ;;  (enum (set! selector-font-family answer)
      ;;        (font-database-families)
      ;;        selector-font-family "120px"))
      ;;(item (text "Base style:")
      ;;  (enum (set! selector-font-style answer)
      ;;        (font-database-styles selector-font-family)
      ;;        selector-font-style "120px"))
      ;;(item ====== ======)
      (item (text "Weight:")
        (enum (selector-search-set! selector-search-weight answer)
              '("Any" "Thin" "Light" "Medium" "Bold" "Black")
              selector-search-weight "120px"))
      (item (text "Slant:")
        (enum (selector-search-set! selector-search-slant answer)
              '("Any" "Normal" "Italic" "Oblique")
              selector-search-slant "120px"))
      (item (text "Stretch:")
        (enum (selector-search-set! selector-search-stretch answer)
              '("Any" "Condensed" "Unextended" "Wide")
              selector-search-stretch "120px"))
      (item (text "Case:")
        (enum (selector-search-set! selector-search-case answer)
              '("Any" "Mixed" "Small Capitals")
              selector-search-case "120px"))
      (item ====== ======)
      (item (text "Serif:")
        (enum (selector-search-set! selector-search-serif answer)
              '("Any" "Serif" "Sans Serif")
              selector-search-serif "120px"))
      (item (text "Spacing:")
        (enum (selector-search-set! selector-search-spacing answer)
              '("Any" "Proportional" "Monospaced")
              selector-search-spacing "120px"))
      (item (text "Device:")
        (enum (selector-search-set! selector-search-device answer)
              '("Any" "Print" "Typewriter" "Digital"
		"Pen" "Art Pen" "Chalk" "Marker")
              selector-search-device "120px"))
      (item (text "Category:")
        (enum (selector-search-set! selector-search-category answer)
              '("Any" "Ancient" "Attached" "Calligraphic" "Comic"
                "Decorative" "Distorted" "Gothic" "Handwritten" "Initials"
                "Medieval" "Miscellaneous" "Outline" "Retro" "Scifi" "Title")
              selector-search-category "120px"))
      (item ====== ======)
      (item (text "Glyphs:")
        (enum (selector-search-set! selector-search-glyphs answer)
              '("Any" "ASCII" "Latin" "Greek" "Cyrillic"
                "CJK" "Hangul" "Math Symbols" "Math Extra" "Math Letters")
              selector-search-glyphs "120px")))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-selector-demo)
  (hlist
    (bold (text "Sample text"))
    (text (selector-font-simulate-comment))
    >>>)
  ===
  (resize ("300px" "300px" "2000px") ("225px" "225px" "225px")
    (scrollable
      (link font-sample-text))))

(tm-widget ((font-selector flag?) quit)
  (padded
    (horizontal
      (refreshable "font-family-selector"
        (link font-family-selector))
      ///
      (refresh font-style-selector auto)
      ///
      (refreshable "font-size-selector"
        (link font-size-selector))
      ///
      (link font-properties-selector))
    === === ===
    (refresh font-selector-demo auto)
    === ===
    (explicit-buttons
      (hlist
        (enum (set-font-sample-kind answer)
              '("Standard" "Selection"
                "ASCII" "Latin" "Greek" "Cyrillic" "CJK" "Hangul"
                "Math Symbols" "Math Extra" "Math Letters"
                "Unicode 0000-0fff" "Unicode 1000-1fff"
                "Unicode 2000-2fff" "Unicode 3000-3fff"
                "Unicode 4000-4fff")
              (get-font-sample-kind) "120px")
        >>>
        (if flag?
            ("Reset"
             (begin
               (init-default "font" "font-base-size" "math-font" "prog-font"
                             "font-family" "font-series" "font-shape")
               (selector-initialize-font get-init)
               (refresh-now "font-family-selector")
               (refresh-now "font-size-selector")))
            // //
            ("Ok" (quit (selector-get-changes get-init))))
        (if (not flag?)
            ("Ok" (quit (selector-get-changes get-env))))))))

(tm-define (open-font-selector)
  (:interactive #t)
  (selector-initialize-font get-env)
  (dialogue-window (font-selector #f) make-multi-with "Font selector"))

(tm-define (open-document-font-selector)
  (:interactive #t)
  (selector-initialize-font get-init)
  (dialogue-window (font-selector #t) init-multi "Document font selector"))
