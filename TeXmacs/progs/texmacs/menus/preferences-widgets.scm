
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
;; Appearance preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "look and feel"
  ("default" "Default")
  ("emacs" "Emacs")
  ("gnome" "Gnome")
  ("kde" "KDE")
  ("macos" "Mac OS")
  ("windows" "Windows"))

(for (l supported-languages)
  (set-preference-name "language" l (upcase-first l)))

(define-preference-names "complex actions"
  ("menus" "Through the menus")
  ("popups" "Through popup windows"))

(define-preference-names "interactive questions"
  ("footer" "On the footer")
  ("popup" "In popup windows"))

(define-preference-names "detailed menus"
  ("simple ""Simplified menus")
  ("detailed" "Detailed menus"))

(define-preference-names "buffer management"
  ("separate" "Documents in separate windows")
  ("shared" "Multiple documents share window"))

(tm-widget (general-preferences-widget)
  (aligned
    (item (text "Look and feel:")
      (enum (set-pretty-preference "look and feel" answer)
            '("Default" "Emacs" "Gnome" "KDE" "Mac OS" "Windows")
            (get-pretty-preference "look and feel")
            "21em"))
    (item (text "User interface language:")
      (enum (set-pretty-preference "language" answer)
            (map upcase-first supported-languages)
            (get-pretty-preference "language")
            "21em"))
    (item (text "Complex actions:")
      (enum (set-pretty-preference "complex actions" answer)
            '("Through the menus" "Through popup windows")
            (get-pretty-preference "complex actions")
            "21em"))
    (item (text "Interactive questions:")
      (enum (set-pretty-preference "interactive questions" answer)
            '("On the footer" "In popup windows")
            (get-pretty-preference "interactive questions")
            "21em"))
    (item (text "Details in menus:")
      (enum (set-pretty-preference "detailed menus" answer)
            '("Simplified menus" "Detailed menus")
            (get-pretty-preference "detailed menus")
            "21em"))
    (item (text "Buffer management:")
      (enum (set-pretty-preference "buffer management" answer)
            '("Documents in separate windows"
              "Multiple documents share window")
            (get-pretty-preference "buffer management")
            "21em"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "text spacebar"
  ("default" "Default")
  ("allow multiple spaces" "Allow multiple spaces")
  ("glue multiple spaces" "Glue multiple spaces")
  ("no multiple spaces" "No multiple spaces"))

(define-preference-names "math spacebar"
  ("default" "Default")
  ("allow spurious spaces" "Allow spurious spaces")
  ("avoid spurious spaces" "Avoid spurious spaces")
  ("no spurious spaces" "No spurious spaces"))

(define-preference-names "automatic quotes"
  ("default" "Default")
  ("none" "Disabled")
  ("dutch" "Dutch")
  ("english" "English")
  ("french" "French")
  ("german" "German")
  ("spanish" "Spanish")
  ("swiss" "Swiss"))

(define-preference-names "automatic brackets"
  ("off" "Disabled")
  ("mathematics" "Inside mathematics" "mathematics")
  ("on" "Enabled"))

(define-preference-names "cyrillic input method"
  ("none" "None")
  ("translit" "Translit")
  ("jcuken" "Jcuken")
  ("yawerty" "Yawerty"))

(tm-widget (keyboard-preferences-widget)
  (aligned
    (item (text "Space bar in text mode:")
      (enum (set-pretty-preference "text spacebar" answer)
            '("Default" "No multiple spaces"
	      "Glue multiple spaces" "Allow multiple spaces")
            (get-pretty-preference "text spacebar")
            "15em"))
    (item (text "Space bar in math mode:")
      (enum (set-pretty-preference "math spacebar" answer)
            '("Default" "No spurious spaces"
	      "Avoid spurious spaces" "Allow spurious spaces")
            (get-pretty-preference "math spacebar")
            "15em"))
    (item (text "Automatic quotes:")
      (enum (set-pretty-preference "automatic quotes" answer)
            '("Default" "Disabled" "Dutch" "English" "French" "German" "Spanish" "Swiss")
            (get-pretty-preference "automatic quotes")
            "15em"))
    (item (text "Automatic brackets:")
      (enum (set-pretty-preference "automatic brackets" answer)
            '("Disabled" "Enabled" "Inside mathematics")
            (get-pretty-preference "automatic brackets")
            "15em"))
    (item (text "Cyrillic input method:")
      (enum (set-pretty-preference "cyrillic input method" answer)
            '("None" "Translit" "Jcuken" "Yawerty")
            (get-pretty-preference "cyrillic input method")
            "15em")))
  ======
  (bold (text "Remote controllers with keyboard simulation"))
  ===
  (hlist
    (aligned
      (item (text "Left:")
        (enum (set-preference "ir-left" answer) '("pageup" "")
              (get-preference "ir-left") "8em"))
      (item (text "Right:")
        (enum (set-preference "ir-right" answer) '("pagedown" "")
              (get-preference "ir-right") "8em"))
      (item (text "Up:")
        (enum (set-preference "ir-up" answer) '("home" "")
              (get-preference "ir-up") "8em"))
      (item (text "Down:")
        (enum (set-preference "ir-down" answer) '("end" "")
              (get-preference "ir-down") "8em")))
    ///
    (aligned
      (item (text "Center:")
        (enum (set-preference "ir-center" answer) '("return" "S-return" "")
              (get-preference "ir-center") "8em"))
      (item (text "Play:")
        (enum (set-preference "ir-play" answer) '("F5" "")
              (get-preference "ir-play") "8em"))
      (item (text "Pause:")
        (enum (set-preference "ir-pause" answer) '("escape" "")
              (get-preference "ir-pause") "8em"))
      (item (text "Menu:")
        (enum (set-preference "ir-menu" answer) '("." "")
              (get-preference "ir-menu") "8em")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematics preferences widget
;; FIXME: - "assuming" has no effect in refreshable widgets
;;        - Too much alignment tweaking      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (math-preferences-widget)
  (padded
  (hlist
    (glue #t #f 5 0)
    (vlist
      (vlist (hlist (glue #t #f 5 0) (bold (text "Keyboard"))) ---
        (hlist
          (glue #t #f 5 0)
          (aligned
            (item (text "Use extensible brackets")
              (toggle (set-boolean-preference "use large brackets"
                                              answer)
                      (get-boolean-preference "use large brackets")))))
        (glue #f #t 0 5))
      === ===
      (vlist (hlist (glue #t #f 5 0) (bold (text "Contextual hints"))) ---
        (hlist 
          (glue #t #f 5 0)
          (refreshable "math-pref-context"
            (aligned
              (item (text "Show full context")
                (toggle (set-boolean-preference "show full context" answer)
                        (get-boolean-preference "show full context")))
              (item (text "Show table cells")
                (toggle (set-boolean-preference "show table cells" answer)
                        (get-boolean-preference "show table cells")))
              (item (text "Show current focus")
                (toggle (set-boolean-preference "show focus" answer)
                        (get-boolean-preference "show focus")))
              (assuming (get-boolean-preference "semantic editing")
                (item (text "Only show semantic focus")
                  (toggle (set-boolean-preference
                           "show only semantic focus" answer)
                          (get-boolean-preference
                           "show only semantic focus")))))))))
    /// ///
    (vlist
      (vlist (bold (text "Semantics")) ---
        (refreshable "math-pref-semantic-selections"
          (aligned
            (meti (hlist // (text "Semantic editing"))
              (toggle (and (set-boolean-preference "semantic editing"
                                                   answer)
                           (refresh-now "math-pref-semantic-selections")
                           (refresh-now "math-pref-context"))
                      (get-boolean-preference "semantic editing")))
            (assuming (get-boolean-preference "semantic editing")
              (meti (hlist // (text "Semantic selections"))
                (toggle (set-boolean-preference "semantic selections"
                                                answer)
                        (get-boolean-preference
                         "semantic selections"))))
            (if #f
                (meti (hlist // (text "Semantic correctness"))
                  (toggle (set-boolean-preference "semantic correctness"
                                                  answer)
                          (get-boolean-preference
                           "semantic correctness"))))))
          (glue #f #t 0 5))
      === ===
      (vlist (bold (text "Correction")) ---
        (aligned
          (meti (hlist // (text "Remove superfluous invisible operators"))
            (toggle (set-boolean-preference
                     "manual remove superfluous invisible" answer)
                    (get-boolean-preference
                     "manual remove superfluous invisible")))
          (meti (hlist // (text "Insert missing invisible operators"))
            (toggle (set-boolean-preference
                     "manual insert missing invisible" answer)
                    (get-boolean-preference
                     "manual insert missing invisible")))
          (meti (hlist // (text "Homoglyph substitutions"))
            (toggle (set-boolean-preference
                     "manual homoglyph correct" answer)
                    (get-boolean-preference
                     "manual homoglyph correct"))))
        (glue #f #t 0 5)))
    (glue #t #f 5 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Html ----------

(tm-widget (html-preferences-widget)
  ===
  (bold (text "TeXmacs -> Html"))
  ===
  (aligned
    (meti (hlist // (text "Use CSS for more advanced formatting"))
      (toggle (set-boolean-preference "texmacs->html:css" answer)
              (get-boolean-preference "texmacs->html:css")))
    (meti (hlist // (text "Export mathematical formulas as MathML"))
      (toggle (set-boolean-preference "texmacs->html:mathml" answer)
              (get-boolean-preference "texmacs->html:mathml")))
    (meti (hlist // (text "Export mathematical formulas as images"))
      (toggle (set-boolean-preference "texmacs->html:images" answer)
              (get-boolean-preference "texmacs->html:images")))))

;; LaTeX ----------

(define-preference-names "texmacs->latex:encoding"
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
  (or (get-boolean-preference "latex->texmacs:conservative")
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
  ===
  (bold (text "LaTeX -> TeXmacs"))
  ===
  (aligned
    (meti (hlist // (text "Import sophisticated objects as pictures"))
      (toggle
        (set-boolean-preference "latex->texmacs:fallback-on-pictures" answer)
        (get-boolean-preference "latex->texmacs:fallback-on-pictures"))))
  ======
  (bold (text "TeXmacs -> LaTeX"))
  ===
  (aligned
    (meti (hlist // (text "Replace TeXmacs styles with no LaTeX equivalents"))
      (toggle (set-boolean-preference "texmacs->latex:replace-style" answer)
              (get-boolean-preference "texmacs->latex:replace-style")))
    (meti (hlist // (text "Expand TeXmacs macros with no LaTeX equivalents"))
      (toggle (set-boolean-preference "texmacs->latex:expand-macros" answer)
              (get-boolean-preference "texmacs->latex:expand-macros")))
    (meti (hlist // (text "Expand user-defined macros"))
      (toggle (set-boolean-preference "texmacs->latex:expand-user-macros" answer)
              (get-boolean-preference "texmacs->latex:expand-user-macros")))
    (meti (hlist // (text "Export bibliographies as links"))
      (toggle (set-boolean-preference "texmacs->latex:indirect-bib" answer)
              (get-boolean-preference "texmacs->latex:indirect-bib")))
    (meti (hlist // (text "Allow for macro definitions in preamble"))
      (toggle (set-boolean-preference "texmacs->latex:use-macros" answer)
              (get-boolean-preference "texmacs->latex:use-macros"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "texmacs->latex:encoding" answer)
            '("Ascii" "Cork with catcodes" "Utf-8 with inputenc")
            (get-pretty-preference "texmacs->latex:encoding")
            "15em")))
  ======
  (bold (text "Conservative conversion options"))
  ===
  (refreshable "source-tracking"
    (aligned
      (meti (hlist // (text "Keep track of source code"))
        (toggle
         (set-latex-source-tracking answer)
         (get-latex-source-tracking)))
      (meti (hlist // (text "Only convert changes with respect to tracked version"))
        (toggle
         (set-latex-conservative answer)
         (get-latex-conservative)))
      (meti (when (get-latex-source-tracking)
              (hlist // (text "Guarantee transparent source tracking")))
        (when (get-latex-source-tracking)
          (toggle
           (set-latex-transparent-source-tracking answer)
           (get-latex-transparent-source-tracking))))
      (meti (when (get-latex-source-tracking)
              (hlist // (text "Store tracking information in LaTeX files")))
        (when (get-latex-source-tracking)
          (toggle
           (set-boolean-preference "texmacs->latex:attach-tracking-info" answer)
           (get-boolean-preference "texmacs->latex:attach-tracking-info")))))))

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
  ===
  (bold (text "BibTeX -> TeXmacs"))
  ===
  (aligned
    (item (text "BibTeX command:")
      (enum (set-pretty-preference "bibtex command" answer)
            '("bibtex" "biber" "biblatex" "rubibtex" "")
            (get-pretty-preference "bibtex command")
            "15em")))
  ===
  (aligned
    (meti (hlist // (text "Only convert changes when re-importing"))
      (toggle (set-bibtm-conservative answer)
              (get-bibtm-conservative))))
  ======
  (bold (text "TeXmacs -> BibTeX"))
  ===
  (aligned
    (meti (hlist // (text "Only convert changes with respect to imported version"))
      (toggle (set-tmbib-conservative answer)
              (get-tmbib-conservative)))))

;; Verbatim ----------

(define-preference-names "texmacs->verbatim:encoding"
  ("auto" "Automatic")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("utf-8" "Utf-8"))

(define-preference-names "verbatim->texmacs:encoding"
  ("auto" "Auto")
  ("cork" "Cork")
  ("iso-8859-1" "Iso-8859-1")
  ("utf-8" "Utf-8"))

(tm-widget (verbatim-preferences-widget)
  ===
  (bold (text "TeXmacs -> Verbatim"))
  ===
  (aligned
    (meti (hlist // (text "Use line wrapping for lines which are longer than 80 characters"))
      (toggle (set-boolean-preference "texmacs->verbatim:wrap" answer)
              (get-boolean-preference "texmacs->verbatim:wrap"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "texmacs->verbatim:encoding" answer)
            '("Automatic" "Cork" "Iso-8859-1" "Utf-8")
            (get-pretty-preference "texmacs->verbatim:encoding")
            "5em")))
  ======
  (bold (text "Verbatim -> TeXmacs"))
  ===
  (aligned
    (meti (hlist // (text "Merge lines into paragraphs unless separated by blank lines"))
      (toggle (set-boolean-preference "verbatim->texmacs:wrap" answer)
              (get-boolean-preference "verbatim->texmacs:wrap"))))
  ===
  (aligned
    (item (text "Character encoding:")
      (enum (set-pretty-preference "verbatim->texmacs:encoding" answer)
            '("Auto" "Cork" "Iso-8859-1" "Utf-8")
            (get-pretty-preference "verbatim->texmacs:encoding")
            "5em"))))

;; Pdf ----------
(define-preference-names "texmacs->pdf:version"
  ("Default" "default")
  ("1.4" "1.4")
  ("1.5" "1.5")
  ("1.6" "1.6")
  ("1.7" "1.7"))

(tm-widget (pdf-preferences-widget)
  ===
  (bold (text "TeXmacs -> Pdf/Postscript"))
  ===
  (aligned
    (assuming (supports-native-pdf?)
      (meti (hlist // (text "Produce Pdf using native export filter"))
	(toggle (set-boolean-preference "native pdf" answer)
		(get-boolean-preference "native pdf"))))
    (assuming (supports-ghostscript?)
      (meti (hlist // (text "Produce Postscript using native export filter"))
	(toggle (set-boolean-preference "native postscript" answer)
		(get-boolean-preference "native postscript"))))
   (meti (hlist // (text "Expand beamer slides"))
      (toggle (set-boolean-preference "texmacs->pdf:expand slides" answer)
	      (get-boolean-preference "texmacs->pdf:expand slides"))))
    (assuming (supports-native-pdf?)
      (aligned (meti (hlist // (text "Distill encapsulated Pdf files"))
	(toggle (set-boolean-preference "texmacs->pdf:distill inclusion" answer)
		(get-boolean-preference "texmacs->pdf:distill inclusion"))))
      (aligned (meti (hlist // (text "Check exported Pdf files for correctness"))
	(toggle (set-boolean-preference "texmacs->pdf:check" answer)
		(get-boolean-preference "texmacs->pdf:check"))))
      (aligned (item (text "Pdf version number:")
        (enum (set-preference "texmacs->pdf:version" answer)
	      '("default" "1.4" "1.5" "1.6" "1.7")
	      (get-preference "texmacs->pdf:version") "8em")))))

;; Images ----------

(define (pretty-format-list)
  (let* ((desired-image-format-list '(("svg" "Svg")  ("eps" "Eps")
           ("png" "Png")("tif" "Tiff") ("jpg" "Jpeg") ("pdf" "Pdf")))
         (valid-image-format-list 
           (filter (lambda (x) (file-converter-exists? "x.pdf" (string-append "x." (car x))))
             desired-image-format-list)))
   (eval `(define-preference-names "texmacs->image:format" ,@valid-image-format-list))
   (cadr (apply map list valid-image-format-list))))

(tm-widget (image-preferences-widget)
  ===
  (bold (text "TeXmacs -> Image"))
  ===
  (aligned
      (item (text "Bitmap resolution (dpi):")
        (enum (set-preference "texmacs->image:raster-resolution" answer)
	      '("1200" "600" "300" "150" "")
	      (get-preference "texmacs->image:raster-resolution")
	      "5em"))
      (item (text "Clipboard image format:")
        (enum (set-pretty-preference "texmacs->image:format" answer)
	      (pretty-format-list)
	      (get-pretty-preference "texmacs->image:format")
	      "5em"))))

;; All converters ----------

(tm-widget (conversion-preferences-widget)
  ===
  (padded
    (tabs
      (tab (text "Html")
        (centered
          (dynamic (html-preferences-widget))))
      (tab (text "LaTeX")
        (centered
          (dynamic (latex-preferences-widget))))
      (tab (text "BibTeX")
        (centered
          (dynamic (bibtex-preferences-widget))))
      (tab (text "Verbatim")
        (centered
          (dynamic (verbatim-preferences-widget))))
      (assuming (or (supports-native-pdf?) (supports-ghostscript?))
        (tab (text "Pdf")
          (centered
            (dynamic (pdf-preferences-widget)))))
      (tab (text "Image")
        (centered
          (dynamic (image-preferences-widget))))))
  ===)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preference-names "autosave"
  ("5" "5 sec")
  ("30" "30 sec")
  ("120" "120 sec")
  ("300" "300 sec")
  ("0" "Disable"))

(define-preference-names "security"
  ("accept no scripts" "Accept no scripts")
  ("prompt on scripts" "Prompt on scripts")
  ("accept all scripts" "Accept all scripts"))

(define-preference-names "updater:interval"
  ("0" "Never")
  ("0" "Unsupported")
  ("24" "Once a day")
  ("168" "Once a week")
  ("720" "Once a month"))

(define-preference-names "document update times"
  ("1" "Once")
  ("2" "Twice")
  ("3" "Three times"))

(define-preference-names "scripting language"
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
  (aligned
    (item (text "Execution of scripts:")
      (enum (set-pretty-preference "security" answer)
            '("Accept no scripts" "Prompt on scripts" "Accept all scripts")
            (get-pretty-preference "security")
            "15em"))))

(tm-widget (security-preferences-widget)
  (refreshable "security-preferences-refresher"
    (padded
      (tabs
        (tab (text "Wallet")
          (centered
            (dynamic (wallet-preferences-widget))))
        (tab (text "Encryption")
          (centered
            (dynamic (gpg-preferences-widget))))
        ;;(tab (text "Scripts")
        ;;  (centered
        ;;    (dynamic (script-preferences-widget))))
        ))))

(tm-widget (experimental-preferences-widget)
  (hlist
    (aligned
      (meti (hlist // (text "Encryption"))
        (toggle (set-boolean-preference "experimental encryption" answer)
                (get-boolean-preference "experimental encryption")))
      (meti (hlist // (text "Fast environments"))
        (toggle (set-boolean-preference "fast environments" answer)
                (get-boolean-preference "fast environments")))
      (meti (hlist // (text "Alpha transparency"))
        (toggle (set-boolean-preference "experimental alpha" answer)
                (get-boolean-preference "experimental alpha")))
      (meti (hlist // (text "New style fonts"))
        (toggle (set-boolean-preference "new style fonts" answer)
                (get-boolean-preference "new style fonts")))
      (meti (hlist // (text "Advanced font customization"))
        (toggle (set-boolean-preference "advanced font customization" answer)
                (get-boolean-preference "advanced font customization")))
      (meti (hlist // (text "New style page breaking"))
        (toggle (set-boolean-preference "new style page breaking" answer)
                (get-boolean-preference "new style page breaking"))))
    /// ///
    (vlist
      (aligned
        (meti (hlist // (text "New bibliography dialogue"))
          (toggle
           (set-boolean-preference "gui:new bibliography dialogue" answer)
           (get-boolean-preference "gui:new bibliography dialogue")))
        (meti (hlist // (text "Program bracket matching"))
          (toggle (set-boolean-preference "prog:highlight brackets" answer)
                  (get-boolean-preference "prog:highlight brackets")))
        (meti (hlist // (text "Automatic program brackets"))
          (toggle (set-boolean-preference "prog:automatic brackets" answer)
                  (get-boolean-preference "prog:automatic brackets")))
        (meti (hlist // (text "Program bracket selections"))
          (toggle (set-boolean-preference "prog:select brackets" answer)
                  (get-boolean-preference "prog:select brackets")))
        (meti (hlist // (text "Case-insensitive search"))
          (toggle (set-boolean-preference "case-insensitive-match" answer)
                  (get-boolean-preference "case-insensitive-match")))
        (assuming (qt-gui?)  ; TODO: recode the dialogue in scheme
          (meti (hlist // (text "Use print dialogue"))
            (toggle (set-boolean-preference "gui:print dialogue" answer)
                    (get-boolean-preference "gui:print dialogue"))))
        (assuming (os-macos?)
          (meti (hlist // (text "Use unified toolbars"))
            (toggle (set-boolean-preference "use unified toolbar" answer)
                    (get-boolean-preference "use unified toolbar")))))
      (glue #f #t 0 0))))

(tm-widget (other-preferences-widget)
  (centered
    (aligned
      (item (text "Automatically save:")
        (enum (set-pretty-preference "autosave" answer)
              '("5 sec" "30 sec" "120 sec" "300 sec" "Disable")
              (get-pretty-preference "autosave")
              "15em"))
      (item (text "Security:")
        (enum (set-pretty-preference "security" answer)
              '("Accept no scripts" "Prompt on scripts" "Accept all scripts")
              (get-pretty-preference "security")
              "15em"))
      (item (text "Scripting language:")
        (enum (set-pretty-preference "scripting language" answer)
              (scripts-preferences-list)
              (get-pretty-preference "scripting language")
              "15em"))
      (item (text "Document updates run:")
	(enum (set-pretty-preference "document update times" answer)
	      '("Once" "Twice" "Three times")
	      (get-pretty-preference "document update times") 
	      "15em"))
      (assuming (updater-supported?)
        (item (text "Check for automatic updates:")
          (enum (set-pretty-preference "updater:interval" answer)
                (automatic-checks-choices)
                (get-pretty-preference "updater:interval")
                "15em")))
      (assuming (updater-supported?)
        (item (text "Last check:") (text (last-check-string))))))
  ======
  (bold (text "Experimental features (to be used with care)"))
  ======
  (dynamic (experimental-preferences-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (preferences-widget)
  (icon-tabs
    (icon-tab "tm_prefs_general.xpm" (text "General")
      (centered
        (dynamic (general-preferences-widget))))
    (icon-tab "tm_prefs_keyboard.xpm" (text "Keyboard")
      (centered
        (dynamic (keyboard-preferences-widget))))
    ;; TODO: please implement nice icon tabs first before
    ;; adding new tabs in the preferences widget
    ;; The tabs currently take too much horizontal space
    ;;(icon-tab "tm_prefs_other.xpm" (text "Mathematics") ; TODO: icon
    ;;  (centered
    ;;    (dynamic (math-preferences-widget))))
    (icon-tab "tm_prefs_convert.xpm" (text "Convert")
      (dynamic (conversion-preferences-widget)))
    (assuming (== (get-preference "experimental encryption") "on")
      (icon-tab "tm_prefs_security.xpm" (text "Security")
        (centered
          (dynamic (security-preferences-widget)))))
    (icon-tab "tm_prefs_other.xpm" (text "Other")
      (centered
        (dynamic (other-preferences-widget))))))

(tm-define (open-preferences)
  (:interactive #t)
  (top-window preferences-widget "User preferences"))
