
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-widgets.scm
;; DESCRIPTION : the preferences widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus preferences-widgets)
  (:use (texmacs menus preferences-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Validation Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (define-preference-names-and-validate pref . options)
  (let ((allowed (map car options)))
    `(begin
       (define-preference-names ,pref ,@options)
       (when (nin? (get-preference ,pref) ',allowed)
         (reset-preference ,pref)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (set-pretty-preference* which pretty-val)
  (let* ((old (get-preference which))
         (act (set-pretty-preference which pretty-val))
         (new (get-preference which)))
    (when (!= new old)
      (notify-restart))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Appearance preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names-and-validate "look and feel"
  ("default" "Default")
  ("emacs" "Emacs")
  ("gnome" "Gnome")
  ("kde" "KDE")
  ("macos" "Mac OS")
  ("windows" "Windows"))

(for (l supported-languages)
  (set-preference-name "language" l (upcase-first l)))

(when (nin? (get-preference "language") supported-languages)
  (reset-preference "language"))

(define-preference-names-and-validate "complex actions"
  ("menus" "Through the menus")
  ("popups" "Through popup windows"))

(define-preference-names-and-validate "interactive questions"
  ("footer" "On the footer")
  ("popup" "In popup windows"))

(define-preference-names-and-validate "detailed menus"
  ("simple" "Simplified menus")
  ("detailed" "Detailed menus"))

(define-preference-names-and-validate "buffer management"
  ("separate" "Documents in separate windows")
  ("shared" "Multiple documents share window"))

(define-preference-names-and-validate "gui theme"
      ("default" "Default")
      ("light" "Bright")
      ("dark" "Dark"))

(define-preference-names-and-validate "gui density"
  ("compact" "Compact")
  ("normal" "Normal")
  ("large" "Large"))

(define-preference-names-and-validate "gui:responsive tab mode"
  ("top" "Top tabs")
  ("side" "Side tabs")
  ("mobile" "Mobile list"))

(when (not qt6-or-later-gui?)
  (when (in? (get-preference "gui theme") '("light" "dark" "default"))
    (set-preference "gui theme" "default")))

(tm-widget (general-preferences-widget)
  (setting-group "General"
    (setting-enum (set-pretty-preference* "look and feel" answer)
                  "Look and feel"
                  '("Default" "Emacs" "Gnome" "KDE" "Mac OS" "Windows")
                  (get-pretty-preference "look and feel")
                  "18em")
    (setting-enum (set-pretty-preference "language" answer)
                  "User interface language"
                  (map upcase-first supported-languages)
                  (get-pretty-preference "language")
                  "18em")
    (setting-enum (set-pretty-preference "complex actions" answer)
                  "Complex actions"
                  '("Through the menus" "Through popup windows")
                  (get-pretty-preference "complex actions")
                  "18em")
    (setting-enum (set-pretty-preference "interactive questions" answer)
                  "Interactive questions"
                  '("On the footer" "In popup windows")
                  (get-pretty-preference "interactive questions")
                  "18em")
    (setting-enum (set-pretty-preference "detailed menus" answer)
                  "Details in menus"
                  '("Simplified menus" "Detailed menus")
                  (get-pretty-preference "detailed menus")
                  "18em")
    (setting-enum (set-pretty-preference "buffer management" answer)
                  "Buffer management"
                  '("Documents in separate windows"
                    "Multiple documents share window")
                  (get-pretty-preference "buffer management")
                  "18em")
    (setting-enum (set-pretty-preference* "gui theme" answer)
                  "User interface theme"
                  (if qt6-or-later-gui?
                      '("Default" "Bright" "Dark" "")
                      '("Default" "Bright" "Dark" "Native" "Legacy" ""))
                  (get-pretty-preference "gui theme")
            "18em")
    (if (support-functionality? "density")
      (setting-enum (set-pretty-preference "gui density" answer)
              "Interface density"
              '("Compact" "Normal" "Large")
              (get-pretty-preference "gui density")
            "18em")
    )
    (setting-enum (set-pretty-preference "gui:responsive tab mode" answer)
      "Responsive tabs default mode"
      '("Top tabs" "Side tabs" "Mobile list")
      (get-pretty-preference "gui:responsive tab mode")
              "18em")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names-and-validate "text spacebar"
  ("default" "Default")
  ("allow multiple spaces" "Allow multiple spaces")
  ("glue multiple spaces" "Glue multiple spaces")
  ("no multiple spaces" "No multiple spaces"))

(define-preference-names-and-validate "math spacebar"
  ("default" "Default")
  ("allow spurious spaces" "Allow spurious spaces")
  ("avoid spurious spaces" "Avoid spurious spaces")
  ("no spurious spaces" "No spurious spaces"))

(define-preference-names-and-validate "automatic quotes"
  ("default" "Default")
  ("none" "Disabled")
  ("dutch" "Dutch")
  ("english" "English")
  ("french" "French")
  ("german" "German")
  ("spanish" "Spanish")
  ("swiss" "Swiss"))

(define-preference-names-and-validate "automatic brackets"
  ("off" "Disabled")
  ("mathematics" "Inside mathematics" "mathematics")
  ("on" "Enabled"))

(define-preference-names-and-validate "cyrillic input method"
  ("none" "None")
  ("translit" "Translit")
  ("jcuken" "Jcuken")
  ("yawerty" "Yawerty"))

(tm-widget (keyboard-preferences-widget)
  (setting-group "Keyboard"
    (setting-enum (set-pretty-preference "text spacebar" answer)
                  "Space bar in text mode"
                  '("Default" "No multiple spaces"
                    "Glue multiple spaces" "Allow multiple spaces")
                  (get-pretty-preference "text spacebar")
                  "15em")
    (setting-enum (set-pretty-preference "math spacebar" answer)
                  "Space bar in math mode"
                  '("Default" "No spurious spaces"
                    "Avoid spurious spaces" "Allow spurious spaces")
                  (get-pretty-preference "math spacebar")
                  "15em")
    (setting-enum (set-pretty-preference "automatic quotes" answer)
                  "Automatic quotes"
                  '("Default" "Disabled" "Dutch" "English" "French" "German" "Spanish" "Swiss")
                  (get-pretty-preference "automatic quotes")
                  "15em")
    (setting-enum (set-pretty-preference "automatic brackets" answer)
                  "Automatic brackets"
                  '("Disabled" "Enabled" "Inside mathematics")
                  (get-pretty-preference "automatic brackets")
                  "15em")
    (setting-enum (set-pretty-preference "cyrillic input method" answer)
                  "Cyrillic input method"
                  '("None" "Translit" "Jcuken" "Yawerty")
                  (get-pretty-preference "cyrillic input method")
                  "15em"))
  (setting-group "Remote controllers with keyboard simulation"
    (hlist
      (vlist
        (setting-enum (set-preference "ir-left" answer) "Left" '("pageup" "")
                      (get-preference "ir-left") "8em")
        (setting-enum (set-preference "ir-right" answer) "Right" '("pagedown" "")
                      (get-preference "ir-right") "8em")
        (setting-enum (set-preference "ir-up" answer) "Up" '("home" "")
                      (get-preference "ir-up") "8em")
        (setting-enum (set-preference "ir-down" answer) "Down" '("end" "")
                      (get-preference "ir-down") "8em"))
      ///
      (vlist
        (setting-enum (set-preference "ir-center" answer) "Center" '("return" "S-return" "")
                      (get-preference "ir-center") "8em")
        (setting-enum (set-preference "ir-play" answer) "Play" '("F5" "")
                      (get-preference "ir-play") "8em")
        (setting-enum (set-preference "ir-pause" answer) "Pause" '("escape" "")
                      (get-preference "ir-pause") "8em")
        (setting-enum (set-preference "ir-menu" answer) "Menu" '("." "")
                      (get-preference "ir-menu") "8em")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics preferences widget
;; FIXME: - "assuming" has no effect in refreshable widgets
;;        - Too much alignment tweaking      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (math-keyboard-preferences-widget)
  (setting-group "Keyboard"
    (setting-toggle (set-boolean-preference "use large brackets" answer)
                    "Use extensible brackets"
                    (get-boolean-preference "use large brackets"))))

(tm-widget (math-hints-preferences-widget)
  (setting-group "Contextual hints"
    (refreshable "math-pref-context"
      (setting-toggle (set-boolean-preference "show full context" answer)
                      "Show full context"
                      (get-boolean-preference "show full context"))
      (setting-toggle (set-boolean-preference "show table cells" answer)
                      "Show table cells"
                      (get-boolean-preference "show table cells"))
      (setting-toggle (set-boolean-preference "show focus" answer)
                      "Show current focus"
                      (get-boolean-preference "show focus"))
      (assuming (get-boolean-preference "semantic editing")
        (setting-toggle (set-boolean-preference "show only semantic focus" answer)
                        "Only show semantic focus"
                        (get-boolean-preference "show only semantic focus"))))))

(tm-widget (math-semantics-preferences-widget)
  (setting-group "Semantics"
    (refreshable "math-pref-semantic-selections"
      (setting-toggle (and (set-boolean-preference "semantic editing" answer)
                           (refresh-now "math-pref-semantic-selections")
                           (refresh-now "math-pref-context"))
                      "Semantic editing"
                      (get-boolean-preference "semantic editing"))
      (assuming (get-boolean-preference "semantic editing")
        (setting-toggle (set-boolean-preference "semantic selections" answer)
                        "Semantic selections"
                        (get-boolean-preference "semantic selections")))
      (assuming #f
        (setting-toggle (set-boolean-preference "semantic correctness" answer)
                        "Semantic correctness"
                        (get-boolean-preference "semantic correctness"))))))

(tm-widget (math-correction-preferences-widget)
  (setting-group "Correction"
    (setting-toggle (set-boolean-preference "manual remove superfluous invisible" answer)
                    "Remove superfluous invisible operators"
                    (get-boolean-preference "manual remove superfluous invisible"))
    (setting-toggle (set-boolean-preference "manual insert missing invisible" answer)
                    "Insert missing invisible operators"
                    (get-boolean-preference "manual insert missing invisible"))
    (setting-toggle (set-boolean-preference "manual homoglyph correct" answer)
                    "Homoglyph substitutions"
                    (get-boolean-preference "manual homoglyph correct"))))

(tm-widget (math-preferences-widget)
  (dynamic (math-keyboard-preferences-widget))
  (dynamic (math-hints-preferences-widget))
  (dynamic (math-semantics-preferences-widget))
  (dynamic (math-correction-preferences-widget)))

(tm-widget (math-preferences-widget*)
  (dynamic (math-keyboard-preferences-widget))
  ====== ======
  (dynamic (math-hints-preferences-widget))
  ====== ======
  (dynamic (math-semantics-preferences-widget))
  ====== ======
  (dynamic (math-correction-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Html ----------

(define (export-formulas-as-mathjax on?)
  (set-boolean-preference "texmacs->html:mathjax" on?)
  (when on?
    (set-boolean-preference "texmacs->html:mathml" #f)
    (set-boolean-preference "texmacs->html:images" #f)
    (refresh-now "texmacs to html")))

(define (export-formulas-as-mathml on?)
  (set-boolean-preference "texmacs->html:mathml" on?)
  (when on?
    (set-boolean-preference "texmacs->html:mathjax" #f)
    (set-boolean-preference "texmacs->html:images" #f)
    (refresh-now "texmacs to html")))

(define (export-formulas-as-images on?)
  (set-boolean-preference "texmacs->html:images" on?)
  (when on?
    (set-boolean-preference "texmacs->html:mathjax" #f)
    (set-boolean-preference "texmacs->html:mathml" #f)
    (refresh-now "texmacs to html")))

(tm-widget (html-preferences-widget)
  (setting-group "TeXmacs -> Html"
    (refreshable "texmacs to html"
      (setting-toggle (set-boolean-preference "texmacs->html:css" answer)
                      "Use CSS for more advanced formatting"
                      (get-boolean-preference "texmacs->html:css"))
      (setting-toggle (export-formulas-as-mathjax answer)
                      "Export mathematical formulas as MathJax"
                      (get-boolean-preference "texmacs->html:mathjax"))
      (setting-toggle (export-formulas-as-mathml answer)
                      "Export mathematical formulas as MathML"
                      (get-boolean-preference "texmacs->html:mathml"))
      (setting-toggle (export-formulas-as-images answer)
                      "Export mathematical formulas as images"
                      (get-boolean-preference "texmacs->html:images"))
      (setting-enum (set-preference "texmacs->html:css-stylesheet" answer)
                    "CSS stylesheet"
                    '("---"
                      "https://www.texmacs.org/css/web-article.css"
                      "https://www.texmacs.org/css/web-article-dark.css"
                      "https://www.texmacs.org/css/web-article-colored.css"
                      "https://www.texmacs.org/css/web-article-dark-colored.css"
                      "")
                    (get-preference "texmacs->html:css-stylesheet") "18em")))
  (setting-group "Html -> TeXmacs"
    (refreshable "html -> texmacs"
      (setting-toggle (set-boolean-preference "mathml->texmacs:latex-annotations" answer)
                      "Try to import formulas using LaTeX annotations"
                      (get-boolean-preference "mathml->texmacs:latex-annotations")))))

;; LaTeX ----------

(define-preference-names-and-validate "texmacs->latex:encoding"
  ("ascii" "Ascii")
  ("cork"  "Cork with catcodes")
  ("utf-8" "Utf-8 with inputenc"))

(define (get-latex-source-tracking)
  (or (get-boolean-preference "latex->texmacs:source-tracking")
      (get-boolean-preference "texmacs->latex:source-tracking")))

(define (set-latex-source-tracking on?)
  (set-boolean-preference "latex->texmacs:source-tracking" on?)
  (set-boolean-preference "texmacs->latex:source-tracking" on?)
  (refresh-now "source-tracking"))

(define (get-latex-conservative)
  (and (get-boolean-preference "latex->texmacs:conservative")
       (get-boolean-preference "texmacs->latex:conservative")))

(define (set-latex-conservative on?)
  (set-boolean-preference "latex->texmacs:conservative" on?)
  (set-boolean-preference "texmacs->latex:conservative" on?)
  (refresh-now "source-tracking"))

(define (get-latex-transparent-source-tracking)
  (or (get-boolean-preference "latex->texmacs:transparent-source-tracking")
      (get-boolean-preference "texmacs->latex:transparent-source-tracking")))

(define (set-latex-transparent-source-tracking on?)
  (set-boolean-preference "latex->texmacs:transparent-source-tracking" on?)
  (set-boolean-preference "texmacs->latex:transparent-source-tracking" on?))

(tm-widget (latex-preferences-widget)
  (setting-group "LaTeX -> TeXmacs"
    (setting-toggle (set-boolean-preference "latex->texmacs:fallback-on-pictures" answer)
                    "Import sophisticated objects as pictures"
                    (get-boolean-preference "latex->texmacs:fallback-on-pictures")))
  (setting-group "TeXmacs -> LaTeX"
    (setting-toggle (set-boolean-preference "texmacs->latex:replace-style" answer)
                    "Replace TeXmacs styles with no LaTeX equivalents"
                    (get-boolean-preference "texmacs->latex:replace-style"))
    (setting-toggle (set-boolean-preference "texmacs->latex:expand-macros" answer)
                    "Expand TeXmacs macros with no LaTeX equivalents"
                    (get-boolean-preference "texmacs->latex:expand-macros"))
    (setting-toggle (set-boolean-preference "texmacs->latex:expand-user-macros" answer)
                    "Expand user-defined macros"
                    (get-boolean-preference "texmacs->latex:expand-user-macros"))
    (setting-toggle (set-boolean-preference "texmacs->latex:indirect-bib" answer)
                    "Export bibliographies as links"
                    (get-boolean-preference "texmacs->latex:indirect-bib"))
    (setting-toggle (set-boolean-preference "texmacs->latex:use-macros" answer)
                    "Allow for macro definitions in preamble"
                    (get-boolean-preference "texmacs->latex:use-macros"))
    (setting-enum (set-pretty-preference "texmacs->latex:encoding" answer)
                  "Character encoding"
                  '("Ascii" "Cork with catcodes" "Utf-8 with inputenc")
                  (get-pretty-preference "texmacs->latex:encoding")
                  "15em"))
  (setting-group "Conservative conversion options"
    (refreshable "source-tracking"
      (setting-toggle (set-latex-source-tracking answer)
                      "Keep track of source code"
                      (get-latex-source-tracking))
      (setting-toggle (set-latex-conservative answer)
                      "Only convert changes with respect to tracked version"
                      (get-latex-conservative))
      (assuming (get-latex-source-tracking)
        (setting-toggle (set-latex-transparent-source-tracking answer)
                        "Guarantee transparent source tracking"
                        (get-latex-transparent-source-tracking)))
      (assuming (get-latex-source-tracking)
        (setting-toggle (set-boolean-preference "texmacs->latex:attach-tracking-info" answer)
                        "Store tracking information in LaTeX files"
                        (get-boolean-preference "texmacs->latex:attach-tracking-info"))))))

;; BibTeX ----------

(define (get-bibtm-conservative)
  (get-boolean-preference "bibtex->texmacs:conservative"))

(define (set-bibtm-conservative on?)
  (set-boolean-preference "bibtex->texmacs:conservative" on?))

(define (get-tmbib-conservative)
  (get-boolean-preference "texmacs->bibtex:conservative"))

(define (set-tmbib-conservative on?)
  (set-boolean-preference "texmacs->bibtex:conservative" on?))

(tm-widget (bibtex-preferences-widget)
  (setting-group "BibTeX -> TeXmacs"
    (setting-enum (set-pretty-preference "bibtex command" answer)
                  "BibTeX command"
                  '("bibtex" "biber" "biblatex" "rubibtex" "")
                  (get-pretty-preference "bibtex command")
                  "15em")
    (setting-toggle (set-bibtm-conservative answer)
                    "Only convert changes when re-importing"
                    (get-bibtm-conservative)))
  (setting-group "TeXmacs -> BibTeX"
    (setting-toggle (set-tmbib-conservative answer)
                    "Only convert changes with respect to imported version"
                    (get-tmbib-conservative))))

;; Verbatim ----------

(define-preference-names-and-validate "texmacs->verbatim:encoding"
  ("auto" "Automatic")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("iso-8859-2" "Iso-8859-2")
  ("utf-8" "Utf-8"))

(define-preference-names-and-validate "verbatim->texmacs:encoding"
  ("auto" "Automatic")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("iso-8859-2" "Iso-8859-2")
  ("utf-8" "Utf-8"))

(tm-widget (verbatim-preferences-widget)
  (setting-group "TeXmacs -> Verbatim"
    (setting-toggle (set-boolean-preference "texmacs->verbatim:wrap" answer)
                    "Use line wrapping for lines which are longer than 80 characters"
                    (get-boolean-preference "texmacs->verbatim:wrap"))
    (setting-enum (set-pretty-preference "texmacs->verbatim:encoding" answer)
                  "Character encoding"
                  '("Automatic" "Cork" "Iso-8859-1" "Iso-8859-2" "Utf-8")
                  (get-pretty-preference "texmacs->verbatim:encoding")
                  "12em"))
  (setting-group "Verbatim -> TeXmacs"
    (setting-toggle (set-boolean-preference "verbatim->texmacs:wrap" answer)
                    "Merge lines into paragraphs unless separated by blank lines"
                    (get-boolean-preference "verbatim->texmacs:wrap"))
    (setting-enum (set-pretty-preference "verbatim->texmacs:encoding" answer)
                  "Character encoding"
                  '("Automatic" "Cork" "Iso-8859-1" "Iso-8859-2" "Utf-8")
                  (get-pretty-preference "verbatim->texmacs:encoding")
                  "12em")))

;; Pdf ----------
(define-preference-names-and-validate "texmacs->pdf:version"
  ("Default" "default")
  ("1.4" "1.4")
  ("1.5" "1.5")
  ("1.6" "1.6")
  ("1.7" "1.7"))

(tm-widget (pdf-preferences-widget)
  (setting-group "TeXmacs -> Pdf/Postscript"
    (assuming (supports-native-pdf?)
      (setting-toggle (set-boolean-preference "native pdf" answer)
                      "Produce Pdf using native export filter"
                      (get-boolean-preference "native pdf")))
    (assuming (supports-ghostscript?)
      (setting-toggle (set-boolean-preference "native postscript" answer)
                      "Produce Postscript using native export filter"
                      (get-boolean-preference "native postscript")))
    (setting-toggle (set-boolean-preference "texmacs->pdf:expand slides" answer)
                    "Expand beamer slides"
                    (get-boolean-preference "texmacs->pdf:expand slides"))
    (assuming (supports-native-pdf?)
      (setting-toggle (set-boolean-preference "texmacs->pdf:distill inclusion" answer)
                      "Distill encapsulated Pdf files"
                      (get-boolean-preference "texmacs->pdf:distill inclusion"))
      (setting-toggle (set-boolean-preference "texmacs->pdf:check" answer)
                      "Check exported Pdf files for correctness"
                      (get-boolean-preference "texmacs->pdf:check"))
      (setting-enum (set-preference "texmacs->pdf:version" answer)
                    "Pdf version number"
                    '("default" "1.4" "1.5" "1.6" "1.7")
                    (get-preference "texmacs->pdf:version") "12em"))))

;; Images ----------

(define (pretty-format-list)
  (let* ((desired-image-format-list '(("svg" "Svg")  ("eps" "Eps")
           ("png" "Png")("tif" "Tiff") ("jpg" "Jpeg") ("pdf" "Pdf")))
         (valid-image-format-list 
           (filter (lambda (x) (file-converter-exists? "x.pdf" (string-append "x." (car x))))
             desired-image-format-list)))
   (eval `(define-preference-names "texmacs->image:format" ,@valid-image-format-list))
   (cadr (apply map list valid-image-format-list))))

(define (supports-inkscape?) (url-exists-in-path? "inkscape"))

(tm-widget (image-preferences-widget)
  (setting-group "TeXmacs -> Image"
    (setting-enum (set-preference "texmacs->image:raster-resolution" answer)
                  "Bitmap export resolution (dpi)"
                  '("1200" "600" "300" "150" "")
                  (get-preference "texmacs->image:raster-resolution")
                  "8em")
    (setting-enum (set-pretty-preference "texmacs->image:format" answer)
                  "Clipboard image format"
                  (pretty-format-list)
                  (get-pretty-preference "texmacs->image:format")
                  "8em"))
  (setting-group "Image -> TeXmacs"
    (assuming (supports-inkscape?)
      (setting-toggle (set-boolean-preference "image->texmacs:svg-prefer-inkscape" answer)
                      "Use Inkscape for conversion from SVG"
                      (get-boolean-preference "image->texmacs:svg-prefer-inkscape")))))

(tm-widget (ai-preferences-widget)
  (setting-group "AI corrections"
    (setting-toggle (set-boolean-preference "ai-correct show differences" answer)
                    "Show differences after text corrections"
                    (get-boolean-preference "ai-correct show differences"))
    (setting-toggle (set-boolean-preference "ai-correct explain" answer)
                    "Explain text corrections"
                    (get-boolean-preference "ai-correct explain"))))

;; All converters ----------

(tm-widget (conversion-preferences-widget)
    (responsive-tabs
      (responsive-tab (text "Html")
          (dynamic (html-preferences-widget)))
      (responsive-tab (text "LaTeX")
          (dynamic (latex-preferences-widget)))
      (responsive-tab (text "BibTeX")
          (dynamic (bibtex-preferences-widget)))
      (responsive-tab (text "Verbatim")
          (dynamic (verbatim-preferences-widget)))
      (assuming (or (supports-native-pdf?) (supports-ghostscript?))
        (responsive-tab (text "Pdf")
            (dynamic (pdf-preferences-widget))))
      (responsive-tab (text "Image")
          (dynamic (image-preferences-widget)))
      (responsive-tab (text "AI")
          (dynamic (ai-preferences-widget))))
  ===)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names-and-validate "autosave"
  ("5" "5 sec")
  ("30" "30 sec")
  ("120" "120 sec")
  ("300" "300 sec")
  ("0" "Disable"))

(define-preference-names-and-validate "security"
  ("accept no scripts" "Accept no scripts")
  ("prompt on scripts" "Prompt on scripts")
  ("accept all scripts" "Accept all scripts"))

(define-preference-names-and-validate "updater:interval"
  ("0" "Never")
  ("0" "Unsupported")
  ("24" "Once a day")
  ("168" "Once a week")
  ("720" "Once a month"))

(define-preference-names-and-validate "document update times"
  ("1" "Once")
  ("2" "Twice")
  ("3" "Three times"))

(define-preference-names-and-validate "scripting language"
  ("none" "None"))

(define (updater-last-check-formatted)
  "Time since last update check formatted for use in the preferences dialog"
  (with c (updater-last-check)
    (if (<= c 0) 
        "Never"
        (with h (ceiling (/ (- (current-time) c) 3600))
          (cond ((< h 24) (replace "Less than %1 hour(s) ago" h))
                ((< h 720) (replace "%1 days ago" (ceiling (/ h 24))))
                (else (translate "More than 1 month ago")))))))

(define (last-check-string)
  (if (updater-supported?)
      (updater-last-check-formatted)
      "Never (unsupported)"))

(define (automatic-checks-choices)
  (if (updater-supported?)
      '("Never" "Once a day" "Once a week" "Once a month")
      '("Unsupported")))

(tm-define (scripts-preferences-list)
  (lazy-plugin-force)
  (with l (scripts-list)
    (for (x l) (set-preference-name "scripting language" x (scripts-name x)))
    (cons "None" (map scripts-name l))))

(tm-widget (script-preferences-widget)
  (setting-enum (set-pretty-preference "security" answer)
                "Execution of scripts"
                '("Accept no scripts" "Prompt on scripts" "Accept all scripts")
                (get-pretty-preference "security")
                "15em"))

(tm-widget (security-preferences-widget)
  (refreshable "security-preferences-refresher"
    (padded
      ======
      (bold (text "Wallet"))
      ===
      (dynamic (wallet-preferences-widget))
      ====== ======
      (bold (text "Encryption"))
      ===
      (dynamic (gpg-preferences-widget))
      ;;====== ======
      ;;(bold (text "Scripts")) 
      ;;===
      ;;(dynamic (script-preferences-widget))
      )))

(tm-widget (misc-preferences-widget)
  (setting-group "Other"
    (setting-enum (set-pretty-preference "autosave" answer)
                  "Automatically save"
                  '("5 sec" "30 sec" "120 sec" "300 sec" "Disable")
                  (get-pretty-preference "autosave")
                  "12em")
    (setting-enum (set-pretty-preference "security" answer)
                  "Security"
                  '("Accept no scripts" "Prompt on scripts" "Accept all scripts")
                  (get-pretty-preference "security")
                  "12em")
    (setting-enum (set-pretty-preference "scripting language" answer)
                  "Scripting language"
                  (scripts-preferences-list)
                  (get-pretty-preference "scripting language")
                  "12em")
    (setting-enum (set-pretty-preference "document update times" answer)
                  "Document updates run"
                  '("Once" "Twice" "Three times")
                  (get-pretty-preference "document update times") 
                  "12em")
    (assuming (updater-supported?)
      (setting-enum (set-pretty-preference "updater:interval" answer)
                    "Check for automatic updates"
                    (automatic-checks-choices)
                    (get-pretty-preference "updater:interval")
                    "12em"))
    (assuming (updater-supported?)
      (item (text "Last check:") (text (last-check-string))))))

(tm-widget (experimental-preferences-widget)
    (setting-group "Experimental features"
      (setting-toggle (set-boolean-preference "experimental encryption" answer)
                      "Encryption" (get-boolean-preference "experimental encryption"))
      (setting-toggle (set-boolean-preference "fast environments" answer)
                      "Fast environments" (get-boolean-preference "fast environments"))
      (setting-toggle (set-boolean-preference "experimental alpha" answer)
                      "Alpha transparency" (get-boolean-preference "experimental alpha"))
      (setting-toggle (set-boolean-preference "new style fonts" answer)
                      "New style fonts" (get-boolean-preference "new style fonts"))
      (setting-toggle (set-boolean-preference "advanced font customization" answer)
                      "Advanced font customization" (get-boolean-preference "advanced font customization"))
      (setting-toggle (set-boolean-preference "new style page breaking" answer)
                      "New style page breaking" (get-boolean-preference "new style page breaking"))
      (assuming (os-macos?)
        (setting-toggle (set-boolean-preference "use native menubar" answer)
                        "Use native menubar" (get-boolean-preference "use native menubar")))
      (setting-toggle (set-boolean-preference "gui:new bibliography dialogue" answer)
                      "New bibliography dialogue" (get-boolean-preference "gui:new bibliography dialogue"))
      (setting-toggle (set-boolean-preference "prog:highlight brackets" answer)
                      "Program bracket matching" (get-boolean-preference "prog:highlight brackets"))
      (setting-toggle (set-boolean-preference "prog:automatic brackets" answer)
                      "Automatic program brackets" (get-boolean-preference "prog:automatic brackets"))
      (setting-toggle (set-boolean-preference "prog:select brackets" answer)
                      "Program bracket selections" (get-boolean-preference "prog:select brackets"))
      (setting-toggle (set-boolean-preference "case-insensitive-match" answer)
                      "Case-insensitive search" (get-boolean-preference "case-insensitive-match"))
      (assuming (qt-gui?)  ; TODO: recode the dialogue in scheme
        (setting-toggle (set-boolean-preference "gui:print dialogue" answer)
                        "Use print dialogue" (get-boolean-preference "gui:print dialogue")))
      (assuming (os-macos?)
        (setting-toggle (set-boolean-preference "use unified toolbar" answer)
                        "Use unified toolbars" (get-boolean-preference "use unified toolbar")))
      (assuming (qt6-or-later-gui?)
        (setting-toggle (set-boolean-preference "new toolbar" answer)
                        "Use new toolbar" (get-boolean-preference "new toolbar"))
        (setting-toggle (set-boolean-preference "use experimental keyboard patches" answer)
                        "Use experimental keyboard patches" (get-boolean-preference "use experimental keyboard patches")))
      (setting-toggle (set-boolean-preference "disable texmacs window positioning" answer)
                      "Disable texmacs window positioning" (get-boolean-preference "disable texmacs window positioning"))
      (glue #f #t 0 0)))

(tm-widget (experimental-preferences-widget*)
  (setting-toggle (set-boolean-preference "experimental encryption" answer)
                  "Encryption" (get-boolean-preference "experimental encryption"))
  (setting-toggle (set-boolean-preference "fast environments" answer)
                  "Fast environments" (get-boolean-preference "fast environments"))
  ;;(setting-toggle (set-boolean-preference "experimental alpha" answer)
  ;;                "Alpha transparency" (get-boolean-preference "experimental alpha"))
  ;;(setting-toggle (set-boolean-preference "new style fonts" answer)
  ;;                "New style fonts" (get-boolean-preference "new style fonts"))
  ;;(setting-toggle (set-boolean-preference "advanced font customization" answer)
  ;;                "Advanced font customization" (get-boolean-preference "advanced font customization"))
  (setting-toggle (set-boolean-preference "new style page breaking" answer)
                  "New style page breaking" (get-boolean-preference "new style page breaking"))
  ;;(setting-toggle (set-boolean-preference "gui:new bibliography dialogue" answer)
  ;;                "New bibliography dialogue" (get-boolean-preference "gui:new bibliography dialogue"))
  (setting-toggle (set-boolean-preference "prog:highlight brackets" answer)
                  "Program bracket matching" (get-boolean-preference "prog:highlight brackets"))
  (setting-toggle (set-boolean-preference "prog:automatic brackets" answer)
                  "Automatic program brackets" (get-boolean-preference "prog:automatic brackets"))
  (setting-toggle (set-boolean-preference "prog:select brackets" answer)
                  "Program bracket selections" (get-boolean-preference "prog:select brackets"))
  ;;(setting-toggle (set-boolean-preference "case-insensitive-match" answer)
  ;;                "Case-insensitive search" (get-boolean-preference "case-insensitive-match"))
  (assuming (qt-gui?)  ; TODO: recode the dialogue in scheme
    (setting-toggle (set-boolean-preference "gui:print dialogue" answer)
                    "Use print dialogue" (get-boolean-preference "gui:print dialogue")))
  (assuming (os-macos?)
    (setting-toggle (set-boolean-preference "use native menubar" answer)
                    "Use native menubar" (get-boolean-preference "use native menubar"))
    (setting-toggle (set-boolean-preference "use unified toolbar" answer)
                    "Use unified toolbars" (get-boolean-preference "use unified toolbar"))))

(tm-widget (other-preferences-widget)
    (dynamic (misc-preferences-widget))
    ======
    (bold (text "Experimental features (to be used with care)"))
    ======
    (dynamic (experimental-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define prefs-plugin-table (make-ahash-table))

(tm-define (prefs-plugin-get)
  (or (ahash-ref prefs-plugin-table :current) "scheme"))

(tm-define (prefs-plugin-set name)
  (ahash-set! prefs-plugin-table :current name)
  (refresh-now "plugin-prefs")
  (update-menus))

(tm-widget (plugin-preferences-list)
  (scrollable
    (choice (prefs-plugin-set (name->plugin answer))
            (map plugin->name (plugins-with-preferences))
            (plugin->name (prefs-plugin-get)))))

(tm-widget (plugin-preferences-widget*)
    (dynamic (plugin-preferences-widget (prefs-plugin-get))))

(tm-widget (plugins-preferences-widget)
  (padded
    (horizontal
      (vertical
        (resize "150px" "300px"
          (dynamic (plugin-preferences-list)))
        (glue #f #t 0 0))
      ///
      (vertical
        (refreshable "plugin-prefs"
          (promise (menu-dynamic
                     (dynamic (plugin-preferences-widget (prefs-plugin-get))))))
        (glue #f #t 400 0)))))

(tm-tool* (plugin-preferences-tool win)
  (:name (string-append (plugin->name (prefs-plugin-get)) " preferences"))
    (dynamic (plugin-preferences-widget (prefs-plugin-get))))

(tm-widget (plugin-titled-preferences-widget name)
  (division "title"
    (text (string-append (plugin->name name) " preferences")) >>)
  (padded
    (dynamic (plugin-preferences-widget name))))

(tm-tool* (plugins-preferences-tool win)
  (:name "Plugin preferences")
  (centered
    (resize "250px" "200px"
      (dynamic (plugin-preferences-list))))
  === ===
  (refreshable "plugin-prefs"
    (dynamic (plugin-titled-preferences-widget (prefs-plugin-get)))))

(tm-define (open-plugin-preferences name)
  (:interactive #t)
  (prefs-plugin-set name)
  (if (side-tools?)
      (tool-select :right 'plugin-preferences-tool)
      (top-window plugin-preferences-widget*
                  (string-append (plugin->name name) " preferences"))))

(tm-define (open-plugins-preferences)
  (:interactive #t)
  (and-with l (plugins-with-preferences)
    (prefs-plugin-set (car l)))
  (if (side-tools?)
      (tool-select :right 'plugins-preferences-tool)
      (top-window plugins-preferences-widget "Plugin preferences")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (preferences-widget)
    (responsive-icon-tabs
      (responsive-icon-tab "tm_prefs_general.xpm" (text "General")
          (dynamic (general-preferences-widget)))
      (responsive-icon-tab "tm_prefs_keyboard.xpm" (text "Keyboard")
          (dynamic (keyboard-preferences-widget)))
      ;; TODO: please implement nice icon tabs first before
      ;; adding new tabs in the preferences widget
      ;; The tabs currently take too much horizontal space
      (responsive-icon-tab "tm_math_preferences.xpm" (text "Maths")
          (dynamic (math-preferences-widget)))
      (responsive-icon-tab "tm_prefs_convert.xpm" (text "Convert")
        (dynamic (conversion-preferences-widget)))
      (assuming (== (get-preference "experimental encryption") "on")
        (responsive-icon-tab "tm_prefs_security.xpm" (text "Security")
            (dynamic (security-preferences-widget))))
      (responsive-icon-tab "tm_prefs_other.xpm" (text "Other")
          (dynamic (other-preferences-widget)))))

(tm-define (open-preferences-window)
  (:interactive #t)
  (top-window preferences-widget "User preferences"))

(tm-define (open-preferences)
  (:interactive #t)
  (if (side-tools?)
      (tool-select :right 'preferences-tool)
      (open-preferences-window)))
