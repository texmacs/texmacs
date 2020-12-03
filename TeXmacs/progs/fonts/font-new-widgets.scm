
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
         "<times><cdot><oplus><otimes>")
       (concat "Variants: " (strong "Bold") "  " (em "Italic")
               "  " (name "Small Capitals") "  " (samp "Sans Serif")
               "  " (kbd "Typewriter")))))

(define (math-selector-text)
  `(with "par-par-sep" "0.2em"
     (document
       (concat "Lowercase Roman: "
         (math "a b c d e f g h i j k l m n o p q r s t u v w x y z"))
       (concat "Uppercase Roman: "
         (math "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z"))
       (concat "Lowercase Greek: "
         (math ,(string-append
                 "<alpha> <beta> <gamma> <delta> <varepsilon> <zeta> <eta>"
                 " <theta> <iota> <kappa> <lambda> <mu> <nu> <xi> <omicron>"
                 " <pi> <rho> <sigma> <tau> <upsilon> <varphi> <psi>"
                 " <chi> <omega>")))
       (concat "Uppercase Greek: "
         (math ,(string-append
                 "<Alpha> <Beta> <Gamma> <Delta> <Epsilon> <Zeta> <Eta>"
                 " <Theta> <Iota> <Kappa> <Lambda> <Mu> <Nu> <Xi> <Omicron>"
                 " <Pi> <Rho> <Sigma> <Tau> <Upsilon> <Phi> <Psi>"
                 " <Chi> <Omega>")))
       (concat "Blackboard bold: "
         (math ,(string-append
                 "<bbb-A> <bbb-B> <bbb-C> <bbb-D> <bbb-E> <bbb-F> <bbb-G>"
                 " <bbb-H> <bbb-I> <bbb-J> <bbb-K> <bbb-L> <bbb-M> <bbb-N>"
                 " <bbb-O> <bbb-P> <bbb-Q> <bbb-R> <bbb-S> <bbb-T> <bbb-U>"
                 " <bbb-V> <bbb-W> <bbb-X> <bbb-Y> <bbb-Z>")))
       (concat "Calligraphic: "
         (math ,(string-append
                 "<cal-A> <cal-B> <cal-C> <cal-D> <cal-E> <cal-F> <cal-G>"
                 " <cal-H> <cal-I> <cal-J> <cal-K> <cal-L> <cal-M> <cal-N>"
                 " <cal-O> <cal-P> <cal-Q> <cal-R> <cal-S> <cal-T> <cal-U>"
                 " <cal-V> <cal-W> <cal-X> <cal-Y> <cal-Z>")))
       (concat "Fraktur: "
         (math ,(string-append
                 "<frak-A> <frak-B> <frak-C> <frak-D> <frak-E> <frak-F>"
                 " <frak-G> <frak-H> <frak-I> <frak-J> <frak-K> <frak-L>"
                 " <frak-M> <frak-N> <frak-O> <frak-P> <frak-Q> <frak-R>"
                 " <frak-S> <frak-T> <frak-U> <frak-V> <frak-W> <frak-X>"
                 " <frak-Y> <frak-Z>"))))))

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
  (cond ((== kind "Mathematics")
         (set! sample-text (math-selector-text)))
        ((== kind "ASCII")
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
    (selector-initialize-search)
    (selector-initialize-customize getter)))

(tm-define (selector-get-changes getter)
  (if (== selector-font-style "Unknown")
      (list)
      (with fn (selector-get-font)
        (with l '()
          (when (!= (selector-font-effects) (getter "font-effects"))
            (set! l (cons* "font-effects" (selector-font-effects) l)))
          (when (!= selector-font-size (getter "font-base-size"))
            (set! l (cons* "font-base-size" selector-font-size l)))
          (when (!= (logical-font-shape fn) (getter "font-shape"))
            (set! l (cons* "font-shape" (logical-font-shape fn) l)))
          (when (!= (logical-font-series fn) (getter "font-series"))
            (set! l (cons* "font-series" (logical-font-series fn) l)))
          (when (!= (logical-font-variant fn) (getter "font-family"))
            (set! l (cons* "font-family" (logical-font-variant fn) l)))
          (when (!= (logical-font-family* fn) (getter "font"))
            (set! l (cons* "font" (logical-font-family* fn) l)))
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
    ;;(display* "Internal font: " (logical-font-family* fn)
    ;;          ", " (logical-font-variant fn)
    ;;          ", " (logical-font-series fn)
    ;;          ", " (logical-font-shape fn)
    ;;          ", " selector-font-size "\n")
    `(document
       (with
         "font" ,(logical-font-family* fn)
         "font-family" ,(logical-font-variant fn)
         "font-series" ,(logical-font-series fn)
         "font-shape" ,(logical-font-shape fn)
         "font-base-size" ,selector-font-size
         "font-effects" ,(selector-font-effects)
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
;; Global state for font customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (selector-customize?)
  (== (get-preference "advanced font customization") "on"))

(tm-define (selector-customize! on?)
  (if on?
      (set-preference "advanced font customization" "on")
      (reset-preference "advanced font customization"))
  (refresh-now "font-customized-selector"))

(tm-define selector-customize-table (make-ahash-table))

(tm-define (selector-customize-get which default)
  (or (ahash-ref selector-customize-table which) default))

(tm-define (selector-customize-set! which val)
  (if (or (== val "") (== val "default") (== val "Default")
	  (== val (font-effect-default which)))
      (ahash-remove! selector-customize-table which)
      (ahash-set! selector-customize-table which val)))

(tm-define (selector-customize-get* which default)
  (with val (selector-customize-get which default)
    (cond ((== val "roman") "TeXmacs Computer Modern")
          ((== val "bonum") "TeX Gyre Bonum")
          ((== val "pagella") "TeX Gyre Pagella")
          ((== val "schola") "TeX Gyre Schola")
          ((== val "termes") "TeX Gyre Termes")
          (else val))))

(tm-define (selector-customize-set!* which val)
  (cond ((== val "TeXmacs Computer Modern") (set! val "roman"))
        ((== val "TeX Gyre Bonum") (set! val "bonum"))
        ((== val "TeX Gyre Pagella") (set! val "pagella"))
        ((== val "TeX Gyre Schola") (set! val "schola"))
        ((== val "TeX Gyre Termes") (set! val "termes")))
  (selector-customize-set! which val))

(define (selector-initialize-customize getter)
  (let* ((fam  (getter "font"))
         (effs (getter "font-effects")))
    (set! selector-customize-table (make-ahash-table))
    (for (kv (string-tokenize-by-char fam #\,))
      (with l (string-tokenize-by-char kv #\=)
	(when (== (length l) 2)
	  (with (var val) l
	    (when (in? var '("bold" "italic" "smallcaps" "sansserif"
			     "typewriter" "math" "greek" "bbb" "cal" "frak"))
	      (ahash-set! selector-customize-table var val))))))
    (for (kv (string-tokenize-by-char effs #\,))
      (with l (string-tokenize-by-char kv #\=)
	(when (== (length l) 2)
	  (with (var val) l
	    (cond ((== var "bold")
		   (ahash-set! selector-customize-table "embold" val))
		  ((== var "bbb")
		   (ahash-set! selector-customize-table "embbb" val))
		  ((in? var '("slant" "hmagnify" "vmagnify"
			      "hextended" "vextended"))
		   (ahash-set! selector-customize-table var val)))))))))

(define (logical-font-family* fn)
  (let* ((fam   (logical-font-family fn))
         (bf    (selector-customize-get "bold" #f))
         (it    (selector-customize-get "italic" #f))
         (sc    (selector-customize-get "smallcaps" #f))
         (ss    (selector-customize-get "sansserif" #f))
         (tt    (selector-customize-get "typewriter" #f))
         (math  (selector-customize-get "math" #f))
         (greek (selector-customize-get "greek" #f))
         (bbb   (selector-customize-get "bbb" #f))
         (cal   (selector-customize-get "cal" #f))
         (frak  (selector-customize-get "frak" #f)))
    (if bf    (set! fam (string-append "bold=" bf "," fam)))
    (if it    (set! fam (string-append "italic=" it "," fam)))
    (if sc    (set! fam (string-append "smallcaps=" sc "," fam)))
    (if ss    (set! fam (string-append "sansserif=" ss "," fam)))
    (if tt    (set! fam (string-append "typewriter=" tt "," fam)))
    (if math  (set! fam (string-append "math=" math "," fam)))
    (if greek (set! fam (string-append "greek=" greek "," fam)))
    (if bbb   (set! fam (string-append "bbb=" bbb "," fam)))
    (if cal   (set! fam (string-append "cal=" cal "," fam)))
    (if frak  (set! fam (string-append "frak="frak  "," fam)))
    fam))

(define (selector-font-effects)
  (let* ((effs   (list))
         (embold (selector-customize-get "embold" #f))
         (embbb  (selector-customize-get "embbb" #f))
         (slant  (selector-customize-get "slant" #f))
         (hmag   (selector-customize-get "hmagnify" #f))
         (vmag   (selector-customize-get "vmagnify" #f))
         (hext   (selector-customize-get "hextended" #f))
         (vext   (selector-customize-get "vextended" #f)))
    (with add (lambda (var val)
		(when val
		  (set! effs (rcons effs (string-append var "=" val)))))
      (add "hmagnify" hmag)
      (add "vmagnify" vmag)
      (add "hextended" hext)
      (add "vextended" vext)
      (add "bold" embold)
      (add "bbb" embbb)
      (add "slant" slant)
      (string-recompose effs ","))))

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
    (resize "300px" "350px"
      (scrollable
        (choice (set! selector-font-family answer)
                (selected-families)
                selector-font-family)))))

(tm-widget (font-style-selector)
  (vertical
    (bold (text "Style"))
    ===
    (resize "200px" "350px"
      (scrollable
        (choice (set! selector-font-style answer)
                (selected-styles selector-font-family)
                selector-font-style)))))

(tm-widget (font-size-selector)
  (vertical
    (bold (text "Size"))
    ===
    (resize "75px" "350px"
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
    (horizontal (glue #f #t 0 0))
    ;;(horizontal
    ;;  >>>
    ;;  (toggle (selector-customize! answer) (selector-customize?)) ///
    ;;  (text "Advanced customizations")
    ;;  >>>)
    ))

(define (font-effect-defaults which)
  (cond ((== which "embold")
	 '("1" "1.25" "1.5" "2" "2.5" "3" "3.5" "4" ""))
	((== which "embbb")
	 '("1" "1.5" "2" "2.5" "3" "3.5" "4" "4.5" "5" ""))
	((== which "slant")
	 '("-0.5" "-0.25" "-0.1" "0"
	   "0.1" "0.2" "0.25" "0.3" "0.4" "0.5" "0.75" "1" ""))
	(else
	  '("0.5" "0.6" "0.7" "0.8" "0.9" "1"
	    "1.1" "1.2" "1.3" "1.4" "1.5" "1.6" "1.8" "2" ""))))

(define (font-effect-default which)
  (cond ((== which "slant") "0")
	(else "1")))

(tm-widget (font-effect-selector which)
  (enum (selector-customize-set! which answer)
        (font-effect-defaults which)
        (selector-customize-get which (font-effect-default which)) "50px"))

(tm-widget (font-effects-selector)
  (vertical
    (aligned
      (item (text "Slant:")
        (dynamic (font-effect-selector "slant")))
      (item (text "Embold:")
        (dynamic (font-effect-selector "embold")))
      (item (text "Double stroke:")
        (dynamic (font-effect-selector "embbb")))
      (item (text "Extended:")
        (dynamic (font-effect-selector "hextended")))
      ;;(item (text "Extend vertically:")
      ;;  (dynamic (font-effect-selector "vextended")))
      (item (text "Magnify horizontally:")
        (dynamic (font-effect-selector "hmagnify")))
      (item (text "Magnify vertically:")
        (dynamic (font-effect-selector "vmagnify"))))
    (horizontal (glue #f #t 0 0))))

(define (default-subfonts-list which)
  '("TeXmacs Computer Modern" "Stix"
    "TeX Gyre Bonum" "TeX Gyre Pagella"
    "TeX Gyre Schola" "TeX Gyre Termes"))

(define (default-subfonts which)
  (with l (cons "Default" (default-subfonts-list which))
    (if (in? which l)
	(append l (list ""))
	(append l (list which "")))))

(tm-widget (subfont-selector which)
  (enum (selector-customize-set!* which answer)
	(default-subfonts (selector-customize-get* which "Default"))
        (selector-customize-get* which "Default") "160px"))

(tm-widget (font-variant-selector)
  (vertical
    (aligned
      (item (text "Bold:")
        (dynamic (subfont-selector "bold")))
      (item (text "Italic:")
        (dynamic (subfont-selector "italic")))
      (item (text "Small capitals:")
        (dynamic (subfont-selector "smallcaps")))
      (item (text "Sans serif:")
        (dynamic (subfont-selector "sansserif")))
      (item (text "Typewriter:")
        (dynamic (subfont-selector "typewriter"))))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-math-selector)
  (vertical
    (aligned
      (item (text "Mathematics:")
        (dynamic (subfont-selector "math")))
      (item (text "Greek:")
        (dynamic (subfont-selector "greek")))
      (item (text "Blackboard bold:")
        (dynamic (subfont-selector "bbb")))
      (item (text "Calligraphic:")
        (dynamic (subfont-selector "cal")))
      (item (text "Fraktur:")
        (dynamic (subfont-selector "frak"))))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-customized-selector)
  (assuming (selector-customize?)
    === === ===
    (hlist
      (bold (text "Font customization"))
      >>>)
    ===
    (horizontal
      (link font-effects-selector)
      >>>
      (link font-variant-selector)
      >>>
      (link font-math-selector))
    === === ===)
  (assuming (not (selector-customize?))
    === === ===))

(tm-widget (font-customization-dialog quit)
  (padded
    === === ===
    (hlist
      (bold (text "Font customization"))
      >>>)
    ===
    (horizontal
      (link font-effects-selector)
      >>>
      (link font-variant-selector)
      >>>
      (link font-math-selector))
    === === ===
    (explicit-buttons (hlist >>> ("Done" (quit))))))

(tm-widget (font-selector-demo)
  (hlist
    (bold (text "Sample text"))
    (text (selector-font-simulate-comment))
    >>>)
  ===
  (resize "850px" "225px"
    (scrollable
      (link font-sample-text))))

(tm-define (font-import name)
  (font-database-extend-local name)
  (refresh-now "font-family-selector")
  (refresh-now "font-size-selector"))

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
    (refreshable "font-customized-selector"
      (link font-customized-selector))
    (refresh font-selector-demo auto)
    === ===
    (explicit-buttons
      (hlist
        (enum (set-font-sample-kind answer)
              '("Standard" "Mathematics" "Selection"
                "ASCII" "Latin" "Greek" "Cyrillic" "CJK" "Hangul"
                "Math Symbols" "Math Extra" "Math Letters"
                "Unicode 0000-0fff" "Unicode 1000-1fff"
                "Unicode 2000-2fff" "Unicode 3000-3fff"
                "Unicode 4000-4fff")
              (get-font-sample-kind) "120px")
        >>>
        (assuming (not (selector-customize?))
          ("Advanced"
           (dialogue-window font-customization-dialog
                            noop "Advanced font selector")) // //)
        ("Import" (choose-file font-import "Import font" "")) // //
        (if flag?
            ("Reset"
             (begin
               (init-default "font" "font-base-size" "math-font" "prog-font"
                             "font-family" "font-series" "font-shape"
                             "font-effects")
               (selector-initialize-font get-init)
               (refresh-now "font-family-selector")
               (refresh-now "font-size-selector")
               (refresh-now "font-customized-selector")))
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

(define ((prefixed-get-init prefix) var)
  (if (init-has? (string-append prefix var))
      (get-init (string-append prefix var))
      (get-init var)))

(define ((prefixed-init-multi prefix) l)
  (when (and (nnull? l) (nnull? (cdr l)))
    (init-env (string-append prefix (car l)) (cadr l))
    ((prefixed-init-multi prefix) (cddr l))))

(tm-define (open-document-other-font-selector prefix)
  (let* ((getter (prefixed-get-init prefix))
         (setter (prefixed-init-multi prefix)))
    (selector-initialize-font getter)
    (dialogue-window (font-selector #t) setter "Font selector")))
