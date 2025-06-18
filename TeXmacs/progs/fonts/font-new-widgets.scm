
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
        (generic document-edit)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define selector-table (make-ahash-table))

(define (selkey specs var)
  (with win (if (list-4? specs) (cadddr specs) (current-window))
    (list specs var (window->buffer win))))

(tm-define (selector-set* specs var val)
  ;;(display* "Set " specs ", " var " <- " val "\n")
  (ahash-set! selector-table (selkey specs var) val)
  (refresh-now "font-style-selector")
  (refresh-now "font-selector-demo"))

(tm-define (selector-notify specs)
  (with (getter setter . other) specs
    (with changes (selector-get-changes specs getter)
      (when (nnull? changes)
        (setter changes)
        ;;(with-window win (update-menus))
        (keyboard-focus-on "canvas")
        ))))

(tm-define (selector-set specs var val)
  ;;(display* "Set " specs ", " var " <- " val "\n")
  (when (!= val (selector-get specs var))
    (ahash-set! selector-table (selkey specs var) val)
    (selector-notify specs)
    (refresh-now "font-style-selector")
    (refresh-now "font-selector-demo")))    

(tm-define (selector-reset* specs var)
  ;;(display* "Reset " specs ", " var "\n")
  (ahash-remove! selector-table (selkey specs var))
  (refresh-now "font-style-selector")
  (refresh-now "font-selector-demo"))

(tm-define (selector-reset specs var)
  ;;(display* "Reset " specs ", " var "\n")
  (ahash-remove! selector-table (selkey specs var))
  (selector-notify specs)
  (refresh-now "font-style-selector")
  (refresh-now "font-selector-demo"))

(define font-vars
  (list :family :style :size))

(define filter-vars
  (list :weight :slant :stretch
        :serif :spacing :case
        :device :category :glyphs))

(define customize-vars
  (list "bold" "italic" "smallcaps" "sansserif"
        "typewriter" "math" "greek" "bbb" "cal" "frak"
        "embold" "embbb"
        "slant" "hmagnify" "vmagnify" "hextended" "vextended"))

(define all-vars (append font-vars filter-vars customize-vars))

(tm-define (selector-get* specs var)
  ;;(display* "Get " specs ", " var "\n")
  (or (ahash-ref selector-table (selkey specs var))
      (cond ((== var :family) (car (initial-font-data specs)))
            ((== var :style) (cadr (initial-font-data specs)))
            ((== var :size) (caddr (initial-font-data specs)))
            ((in? var filter-vars) "Any")
            ((in? var customize-vars) (initial-customize-get specs var))
            (else #f))))

(tm-define (selector-get specs var)
  (with r (selector-get* specs var)
    ;;(display* "Get " specs ", " var " -> " r "\n")
    r))

(tm-define (selector-clean specs)
  (with (getter setter . other) specs
    (for (var all-vars)
      (ahash-remove! selector-table (selkey specs var)))))

(tm-define (selector-restore specs global?)
  (when global? ;; NOTE: non global => ':default' values not yet implemented
    (with vars (list "font" "font-base-size" "math-font" "prog-font"
                     "font-family" "font-series" "font-shape"
                     "font-effects")
      (with (getter setter . other) specs
        (for (var all-vars)
          (ahash-remove! selector-table (selkey specs var)))
        (for (var vars)
          (setter (list var :default))))
      (keyboard-focus-on "canvas")
      (delayed
        (:pause 250)
        (refresh-now "font-family-selector")
        (refresh-now "font-style-selector")
        (refresh-now "font-size-selector")
        (refresh-now "font-customized-selector")
        (refresh-now "font-selector-demo")))))

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

(define (initial-font-data specs)
  (let* ((getter (car specs))
         (fam (font-family-main (getter "font")))
         (var (getter "font-family"))
         (ser (getter "font-series"))
         (sh  (getter "font-shape"))
         (sz  (getter "font-base-size"))
         (lf  (logical-font-private fam var ser sh))
         (fn  (logical-font-search-exact lf)))
    (list (or (car fn) "TeXmacs Computer Modern")
          (or (cadr fn) "Regular")
          (or sz "10"))))

(tm-define (selector-get-font specs)
  (logical-font-patch
    (logical-font-public (selector-get specs :family)
                         (selector-get specs :style))
    (selected-properties specs)))

(tm-define (selector-get-changes specs getter)
  (if (== (selector-get specs :style) "Unknown")
      (list)
      (with fn (selector-get-font specs)
        (with l '()
          (when (!= (selector-font-effects specs) (getter "font-effects"))
            (set! l (cons* "font-effects" (selector-font-effects specs) l)))
          (when (!= (selector-get specs :size) (getter "font-base-size"))
            (set! l (cons* "font-base-size" (selector-get specs :size) l)))
          (when (!= (logical-font-shape fn) (getter "font-shape"))
            (set! l (cons* "font-shape" (logical-font-shape fn) l)))
          (when (!= (logical-font-series fn) (getter "font-series"))
            (set! l (cons* "font-series" (logical-font-series fn) l)))
          (when (!= (logical-font-variant fn) (getter "font-family"))
            (set! l (cons* "font-family" (logical-font-variant fn) l)))
          (when (!= (logical-font-family* specs fn) (getter "font"))
            (set! l (cons* "font" (logical-font-family* specs fn) l)))
          l))))

(define (selector-font-simulate-data specs)
  (let* ((fn  (selector-get-font specs))
	 (fam (logical-font-family fn))
         (var (logical-font-variant fn))
         (ser (logical-font-series fn))
         (sh  (logical-font-shape fn))
         (lf  (logical-font-private fam var ser sh))
         (fn2 (logical-font-search lf))
         (fn1 (list (selector-get specs :family) (selector-get specs :style)))
         (sel (string-recompose (selected-properties specs) " "))
         (nm1 (string-append (car fn1) " " (cadr fn1)))
         (nm2 (string-append (car fn2) " " (cadr fn2)))
         (nm+ (if (== sel "") nm1 (string-append nm1 " + " sel))))
    ;;(display* "fn = " fn "\n")
    ;;(display* "lf = " lf "\n")
    ;;(display* "fn2= " fn2 "\n")
    (list nm1 nm+ nm2)))

(define (selector-font-simulate-comment specs)
  (with (fn1 fn1+ fn2) (selector-font-simulate-data specs)
    (if (and (== fn1 fn1+) (== fn1 fn2)) ""
        (string-append "  (" fn1+ " -> " fn2 ")"))))

(tm-widget (selector-font-simulate-widget specs)
  (with (fn1 fn1+ fn2) (selector-font-simulate-data specs)
    (assuming (or (!= fn1 fn1+) (!= fn1 fn2))
      (division "discrete"
        (aligned
          (item (bold (text "Requested:")) (text fn1+))
          (item (bold (text "Replaced by:")) (text fn2)))))))

(define (selector-font-demo-text specs)
  (with fn (selector-get-font specs)
    ;;(display* "Font: " fn "\n")
    ;;(display* "Internal font: " (logical-font-family* specs fn)
    ;;          ", " (logical-font-variant fn)
    ;;          ", " (logical-font-series fn)
    ;;          ", " (logical-font-shape fn)
    ;;          ", " (selector-get specs :size) "\n")
    `(document
       (with
         "font" ,(logical-font-family* specs fn)
         "font-family" ,(logical-font-variant fn)
         "font-series" ,(logical-font-series fn)
         "font-shape" ,(logical-font-shape fn)
         "font-base-size" ,(selector-get specs :size)
         "font-effects" ,(selector-font-effects specs)
         ,sample-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state for font searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (selector-search-glyphs-decoded specs)
  (with s (selector-get specs :glyphs)
    (cond ((== s "ASCII") "Ascii")
          ((== s "Math Symbols") "MathSymbols")
          ((== s "Math Extra") "MathExtra")
          ((== s "Math Letters") "MathLetters")
          (else s))))

(define (selected-properties specs)
  (with l (list (selector-get specs :weight)
                (selector-get specs :slant)
                (selector-get specs :stretch)
                (selector-get specs :serif)
                (selector-get specs :spacing)
                (selector-get specs :case)
                (selector-get specs :device)
                (selector-get specs :category)
                (selector-search-glyphs-decoded specs))
    (list-filter l (cut != <> "Any"))))

(tm-define-macro (selector-set* specs var val)
  `(begin
     (selector-set ,specs ,var ,val)
     (delayed
       (refresh-now "font-family-selector"))))

(tm-define (selected-families specs)
  (search-font-families (selected-properties specs)))

(tm-define (selected-styles specs family)
  (search-font-styles family (selected-properties specs)))

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

(tm-define (selector-customize-get specs which default)
  (or (selector-get specs which) default))

(tm-define (selector-customize-set! specs which val)
  (if (or (== val "") (== val "default") (== val "Default")
	  (== val (font-effect-default which)))
      (selector-reset specs which)
      (selector-set specs which val)))

(tm-define (selector-customize-get* specs which default)
  (with val (selector-customize-get specs which default)
    (cond ((== val "roman") "TeXmacs Computer Modern")
          ((== val "bonum") "TeX Gyre Bonum")
          ((== val "pagella") "TeX Gyre Pagella")
          ((== val "schola") "TeX Gyre Schola")
          ((== val "termes") "TeX Gyre Termes")
          (else val))))

(tm-define (selector-customize-set!* specs which val)
  (cond ((== val "TeXmacs Computer Modern") (set! val "roman"))
        ((== val "TeX Gyre Bonum") (set! val "bonum"))
        ((== val "TeX Gyre Pagella") (set! val "pagella"))
        ((== val "TeX Gyre Schola") (set! val "schola"))
        ((== val "TeX Gyre Termes") (set! val "termes")))
  (selector-customize-set! specs which val))

(define (initial-customize-get specs var)
  (let* ((getter (car specs))
         (fam    (getter "font"))
         (effs   (getter "font-effects"))
         (rval   #f))
    (for (kv (string-tokenize-by-char fam #\,))
      (with l (string-tokenize-by-char kv #\=)
	(when (== (length l) 2)
	  (with (var2 val) l
            (when (== var var2)
              (set! rval val))))))
    (for (kv (string-tokenize-by-char effs #\,))
      (with l (string-tokenize-by-char kv #\=)
	(when (== (length l) 2)
	  (with (var2 val) l
	    (cond ((== var2 "bold") (if (== var "embold") (set! rval val)))
		  ((== var2 "bbb") (if (== var "embbb") (set! rval val)))
		  ((== var2 var) (set! rval val)))))))
    rval))

(define (logical-font-family* specs fn)
  (let* ((fam   (logical-font-family fn))
         (bf    (selector-customize-get specs "bold" #f))
         (it    (selector-customize-get specs "italic" #f))
         (sc    (selector-customize-get specs "smallcaps" #f))
         (ss    (selector-customize-get specs "sansserif" #f))
         (tt    (selector-customize-get specs "typewriter" #f))
         (math  (selector-customize-get specs "math" #f))
         (greek (selector-customize-get specs "greek" #f))
         (bbb   (selector-customize-get specs "bbb" #f))
         (cal   (selector-customize-get specs "cal" #f))
         (frak  (selector-customize-get specs "frak" #f)))
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

(define (selector-font-effects specs)
  (let* ((effs   (list))
         (embold (selector-customize-get specs "embold" #f))
         (embbb  (selector-customize-get specs "embbb" #f))
         (slant  (selector-customize-get specs "slant" #f))
         (hmag   (selector-customize-get specs "hmagnify" #f))
         (vmag   (selector-customize-get specs "vmagnify" #f))
         (hext   (selector-customize-get specs "hextended" #f))
         (vext   (selector-customize-get specs "vextended" #f)))
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

(tm-define (font-default-sizes*)
  '("5" "6" "7" "8" "9" "10" "11" "12" "14" "16" "18" "20" "24" ""))

(tm-widget (font-family-selector* specs)
  (resize "300px" "350px"
    (scrollable
      (choice (selector-set specs :family answer)
              (selected-families specs)
              (selector-get specs :family)))))

(tm-widget (font-family-selector specs)
  (vertical
    (bold (text "Family"))
    ===
    (dynamic (font-family-selector* specs))))

(tm-widget (font-style-selector specs)
  (vertical
    (bold (text "Style"))
    ===
    (resize "200px" "350px"
      (scrollable
        (choice (selector-set specs :style answer)
                (selected-styles specs (selector-get specs :family))
                (selector-get specs :style))))))

(tm-widget (font-style-selector* specs)
  (hlist
    (bold (text "Style")) // //
    (enum (selector-set specs :style answer)
          (selected-styles specs (selector-get specs :family))
          (selector-get specs :style) "10em")))

(tm-widget (font-size-selector specs)
  (vertical
    (bold (text "Size"))
    ===
    (resize "75px" "350px"
      (scrollable
        (choice (selector-set specs :size answer)
                (font-default-sizes)
                (selector-get specs :size))))))

(tm-widget (font-size-selector* specs)
  (hlist
    (bold (text "Size")) // //
    (enum (selector-set specs :size answer)
          (font-default-sizes*)
          (selector-get specs :size) "3em")))

(tm-widget (font-sample-text specs)
  (texmacs-output
    `(with "bg-color" "white"
       ,(selector-font-demo-text specs))
    '(style "generic")))

(tm-widget (font-properties-selector* specs)
  (aligned
    ;;(item (text "Base family:")
    ;;  (enum (selector-set specs :family answer)
    ;;        (font-database-families)
    ;;        (selector-get specs :family) "150px"))
    ;;(item (text "Base style:")
    ;;  (enum (selector-set specs :style answer)
    ;;        (font-database-styles (selector-get specs :family))
    ;;        (selector-get specs :style) "150px"))
    ;;(item ====== ======)
    (item (text "Weight:")
      (enum (selector-set* specs :weight answer)
            '("Any" "Thin" "Light" "Medium" "Bold" "Black")
            (selector-get specs :weight) "150px"))
    (item (text "Slant:")
      (enum (selector-set* specs :slant answer)
            '("Any" "Normal" "Italic" "Oblique")
            (selector-get specs :slant) "150px"))
    (item (text "Stretch:")
      (enum (selector-set* specs :stretch answer)
            '("Any" "Condensed" "Unextended" "Wide")
            (selector-get specs :stretch) "150px"))
    (item (text "Case:")
      (enum (selector-set* specs :case answer)
            '("Any" "Mixed" "Small Capitals")
            (selector-get specs :case) "150px"))
    (item ====== ======)
    (item (text "Serif:")
      (enum (selector-set* specs :serif answer)
            '("Any" "Serif" "Sans Serif")
            (selector-get specs :serif) "150px"))
    (item (text "Spacing:")
      (enum (selector-set* specs :spacing answer)
            '("Any" "Proportional" "Monospaced")
            (selector-get specs :spacing) "150px"))
    (item (text "Device:")
      (enum (selector-set* specs :device answer)
            '("Any" "Print" "Typewriter" "Digital"
              "Pen" "Art Pen" "Chalk" "Marker")
            (selector-get specs :device) "150px"))
    (item (text "Category:")
      (enum (selector-set* specs :category answer)
            '("Any" "Ancient" "Attached" "Calligraphic" "Comic"
              "Decorative" "Distorted" "Gothic" "Handwritten" "Initials"
              "Medieval" "Miscellaneous" "Outline" "Retro" "Scifi" "Title")
            (selector-get specs :category) "150px"))
    (item ====== ======)
    (item (text "Glyphs:")
      (enum (selector-set* specs :glyphs answer)
            '("Any" "ASCII" "Latin" "Greek" "Cyrillic"
              "CJK" "Hangul" "Math Symbols" "Math Extra" "Math Letters")
            (selector-get specs :glyphs) "150px")))
  (horizontal (glue #f #t 0 0))
  ;;(horizontal
  ;;  >>>
  ;;  (toggle (selector-customize! answer) (selector-customize?)) ///
  ;;  (text "Advanced customizations")
  ;;  >>>)
  )

(tm-widget (font-properties-selector specs)
  (vertical
    (horizontal
      (glue #f #f 0 0)
      (bold (text "Filter"))
      (glue #f #f 0 0))
    ===
    (dynamic (font-properties-selector* specs))))

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

(tm-widget (font-effect-selector specs which)
  (enum (selector-customize-set! specs which answer)
        (font-effect-defaults which)
        (selector-customize-get
         specs which (font-effect-default which)) "50px"))

(tm-widget (font-effects-selector specs)
  (vertical
    (aligned
      (item (text "Slant:")
        (dynamic (font-effect-selector specs "slant")))
      (item (text "Embold:")
        (dynamic (font-effect-selector specs "embold")))
      (item (text "Double stroke:")
        (dynamic (font-effect-selector specs "embbb")))
      (item (text "Extended:")
        (dynamic (font-effect-selector specs "hextended")))
      ;;(item (text "Extend vertically:")
      ;;  (dynamic (font-effect-selector specs "vextended")))
      (item (text "Magnify horizontally:")
        (dynamic (font-effect-selector specs "hmagnify")))
      (item (text "Magnify vertically:")
        (dynamic (font-effect-selector specs "vmagnify"))))
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

(tm-widget (subfont-selector specs which)
  (enum (selector-customize-set!* specs which answer)
	(default-subfonts (selector-customize-get* specs which "Default"))
        (selector-customize-get* specs which "Default") "160px"))

(tm-widget (font-variant-selector specs)
  (vertical
    (aligned
      (item (text "Bold:")
        (dynamic (subfont-selector specs "bold")))
      (item (text "Italic:")
        (dynamic (subfont-selector specs "italic")))
      (item (text "Small capitals:")
        (dynamic (subfont-selector specs "smallcaps")))
      (item (text "Sans serif:")
        (dynamic (subfont-selector specs "sansserif")))
      (item (text "Typewriter:")
        (dynamic (subfont-selector specs "typewriter"))))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-math-selector specs)
  (vertical
    (aligned
      (item (text "Mathematics:")
        (dynamic (subfont-selector specs "math")))
      (item (text "Greek:")
        (dynamic (subfont-selector specs "greek")))
      (item (text "Blackboard bold:")
        (dynamic (subfont-selector specs "bbb")))
      (item (text "Calligraphic:")
        (dynamic (subfont-selector specs "cal")))
      (item (text "Fraktur:")
        (dynamic (subfont-selector specs "frak"))))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-customized-selector specs)
  (assuming (selector-customize?)
    === === ===
    (hlist
      (bold (text "Font customization"))
      >>>)
    ===
    (horizontal
      (dynamic (font-effects-selector specs))
      >>>
      (dynamic (font-variant-selector specs))
      >>>
      (dynamic (font-math-selector specs)))
    === === ===)
  (assuming (not (selector-customize?))
    === === ===))

(tm-widget ((font-customization-dialog specs) quit)
  (padded
    === === ===
    (hlist
      (bold (text "Font customization"))
      >>>)
    ===
    (horizontal
      (dynamic (font-effects-selector specs))
      >>>
      (dynamic (font-variant-selector specs))
      >>>
      (dynamic (font-math-selector specs)))
    === === ===
    (explicit-buttons (hlist >>> ("Done" (quit))))))

(tm-widget (font-selector-demo specs)
  (hlist
    (bold (text "Sample text"))
    (text (selector-font-simulate-comment specs))
    >>>)
  ===
  (resize "880px" "225px"
    (scrollable
      (dynamic (font-sample-text specs)))))

(tm-define (font-import name)
  (font-database-extend-local name)
  (refresh-now "font-family-selector")
  (refresh-now "font-style-selector")
  (refresh-now "font-size-selector")
  (refresh-now "font-selector-demo"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((font-selector specs global?) quit)
  (padded
    (horizontal
      (refreshable "font-family-selector"
        (dynamic (font-family-selector specs)))
      ///
      (refreshable "font-style-selector"
        (dynamic (font-style-selector specs)))
      ///
      (refreshable "font-size-selector"
        (dynamic (font-size-selector specs)))
      ///
      (dynamic (font-properties-selector specs)))
    (refreshable "font-customized-selector"
      (dynamic (font-customized-selector specs)))
    (refreshable "font-selector-demo"
      (promise (menu-dynamic (dynamic (font-selector-demo specs)))))
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
              (get-font-sample-kind) "20em")
        >>>
        (assuming (not (selector-customize?))
          ("Advanced"
           (dialogue-window (font-customization-dialog specs)
                            noop "Advanced font selector")) // //)
        ("Import" (choose-file font-import "Import font" "")) // //
        (if global?
            ("Reset" (selector-restore specs global?))
            // //
            ("Ok" (quit (selector-get-changes specs get-init))))
        (if (not global?)
            ("Ok" (quit (selector-get-changes specs get-env))))))))

(tm-tool* (font-tool win name getter setter global?)
  (:name name)
  (with tool `(font-tool ,name ,getter ,setter ,global?)
    (with specs (list getter setter global? win)
      (with wide? (tool-bottom? tool win)
        (centered
          (vertical
            ===
            (refreshable "font-family-selector"
              (dynamic (font-family-selector* specs)))
            ===
            (horizontal
              (refreshable "font-style-selector"
                (dynamic (font-style-selector* specs)))
              >>>
              (refreshable "font-size-selector"
                (dynamic (font-size-selector* specs))))))
        (assuming global?
          (division "discrete"
            (hlist
              >> ("Restore defaults" (selector-restore specs global?)))
            ===))
        ======
        (section-tabs "font-tool-tabs" win
          (section-tab "Filters"
            (centered (dynamic (font-properties-selector* specs)))
            ======
            (refreshable "font-selector-demo"
              (dynamic (selector-font-simulate-widget specs))))
          (section-tab "Effects"
            (centered (dynamic (font-effects-selector specs))))
         (section-tab "Variants"
            (centered (dynamic (font-variant-selector specs))))
          (section-tab "Mathematics"
            (centered (dynamic (font-math-selector specs))))
          (section-tab "More"
            (division "plain"
              (padded
                ("Import font" (choose-file font-import "Import font" ""))
                ("Scan disk for more fonts" (scan-disk-for-fonts))
                ("Clear font cache" (clear-font-cache))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level window interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-font-selector-window)
  (:interactive #t)
  (with specs (list get-env make-multi-with #f)
    (selector-clean specs)
    (dialogue-window (font-selector specs #f)
                     make-multi-with "Font selector")))

(tm-define (open-document-font-selector-window)
  (:interactive #t)
  (with specs (list get-init init-multi #t)
    (selector-clean specs)
    (dialogue-window (font-selector specs #t)
                     init-multi "Document font selector")))

(define ((prefixed-get-init prefix) var)
  (if (init-has? (string-append prefix var))
      (get-init (string-append prefix var))
      (get-init var)))

(define ((prefixed-init-multi prefix) l)
  (when (and (nnull? l) (nnull? (cdr l)))
    (init-env (string-append prefix (car l)) (cadr l))
    ((prefixed-init-multi prefix) (cddr l))))

(tm-define (open-document-other-font-selector-window prefix)
  (let* ((getter (prefixed-get-init prefix))
         (setter (prefixed-init-multi prefix))
         (specs (list getter setter #t)))
    (selector-clean specs)
    (dialogue-window (font-selector specs #t)
                     setter "Font selector")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level tool interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-font-tool name getter setter global?)
  (let* ((win (current-window))
         (specs (list getter setter global? win))
         (tool `(font-tool ,name ,getter ,setter ,global?)))
    (selector-clean specs)
    (tool-select :right tool win)))

(tm-define (open-font-selector)
  (:interactive #t)
  (if (side-tools?)
      (open-font-tool "Font" get-env make-multi-with #f)
      (open-font-selector-window)))

(tm-define (open-document-font-selector)
  (:interactive #t)
  (if (side-tools?)
      (open-font-tool "Document font" get-init init-multi #t)
      (open-document-font-selector-window)))

(tm-define (open-document-other-font-selector prefix)
  (:interactive #t)
  (if (side-tools?)
      (let* ((getter (prefixed-get-init prefix))
             (setter (prefixed-init-multi prefix)))
        (open-font-tool "Font selector" getter setter #t))
      (open-document-other-font-selector prefix-window)))
