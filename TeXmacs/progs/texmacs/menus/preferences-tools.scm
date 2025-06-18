
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preferences-tools.scm
;; DESCRIPTION : the preferences widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus preferences-tools)
  (:use (texmacs menus preferences-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alternative presentation of experimental preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (experimental-preferences-widget*)
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
              (get-boolean-preference "new style page breaking")))
    (assuming (os-macos?)
      (meti (hlist // (text "Use native menubar"))
        (toggle (set-boolean-preference "use native menubar" answer)
                (get-boolean-preference "use native menubar"))))
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
                (get-boolean-preference "use unified toolbar"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences tool
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-tool* (preferences-tool win)
  (:name "Preferences")
  (section-tabs "preferences-tabs" win
    (section-tab "General" ======
      (centered (dynamic (general-preferences-widget))))
    (section-tab "Keyboard" ===
      (centered (dynamic (keyboard-preferences-widget))))
    ;;(section-tab "Mathematics"
    ;;  (dynamic (math-preferences-widget)))
    (section-tab "Convert"
      (section-tabs "convert-preferences-tabs" win
        (section-tab "Html" ===
          (centered (dynamic (html-preferences-widget))))
        (section-tab "LaTeX" ===
          (centered (dynamic (latex-preferences-widget))))
        (section-tab "BibTeX" ======
          (centered (dynamic (bibtex-preferences-widget))))
        (section-tab "Verbatim" ===
          (centered (dynamic (verbatim-preferences-widget))))
        (section-tab "Pdf" ===
          (centered (dynamic (pdf-preferences-widget))))
        (section-tab "Image" ===
          (centered (dynamic (image-preferences-widget))))))
    (section-tab "Security" ===
      (dynamic (security-preferences-widget)))
    (section-tab "Other" ===
      (centered
        ======
        (bold (text "Miscellaneous preferences"))
        === ===
        (dynamic (misc-preferences-widget))
        ====== ======
        (bold (text "Experimental features"))
        === ===
        (dynamic (experimental-preferences-widget*))))))
