
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-menu.scm
;; DESCRIPTION : menus for setting global document properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-menu)
  (:use (generic document-edit)
        (generic generic-menu)
        (texmacs menus file-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Project menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (include-list base t)
  (cond ((tree-is? t 'document)
         (apply append (map (cut include-list base <>) (tree-children t))))
        ((and (tree-is? t 'include) (tree-atomic? (tree-ref t 0)))
         (list (url-relative base (tree->string (tree-ref t 0)))))
        (else (list))))

(tm-define (project-file-list)
  (if (project-attached?)
      (let* ((prj (project-get))
             (t (buffer->tree prj)))
        (include-list prj t))
      (list)))

(tm-define (project-menu)
  (buffer-list-menu (project-file-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document style
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-style-menu
  ("No style" (set-no-style))
  ---
  (link style-menu)
  ---
  ("Other style" (interactive set-main-style))
  ---
  (group "Customizations")
  (with l (get-style-list)
    (for (pack (if (null? l) l (cdr l)))
      (-> (eval (upcase-first pack))
          ("Remove package" (remove-style-package pack)))))
  (-> "Add package"
      (link toggle-package-menu)
      ---
      ("Add other package" (interactive add-style-package))))

(menu-bind document-style-extra-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-view-menu
  ("Edit source tree" (toggle-preamble))
  (-> "Informative flags"
      ("Default" (init-default "info-flag"))
      ---
      ("None" (init-env "info-flag" "none"))
      ("Minimal" (init-env "info-flag" "minimal"))
      ("Short" (init-env "info-flag" "short"))
      ("Detailed" (init-env "info-flag" "detailed"))
      ("Also on paper" (init-env "info-flag" "paper")))
  (-> "Page layout"
      ("Default" (init-default "page-screen-margin" "page-show-hf"
			       "page-screen-left" "page-screen-right"
			       "page-screen-top" "page-screen-bot"))
      ---
      ("Show header and footer" (toggle-visible-header-and-footer))
      ("Margins as on paper" (toggle-page-screen-margin))
      ---
      (when (test-env? "page-screen-margin" "true")
	("Left margin" (init-interactive-env "page-screen-left"))
	("Right margin" (init-interactive-env "page-screen-right"))
	("Top margin" (init-interactive-env "page-screen-top"))
	("Bottom margin" (init-interactive-env "page-screen-bot"))))
  ---
  (group "Source tags")
  (-> "Style"
      ("Default" (init-default "src-style"))
      ---
      ("Angular" (init-env "src-style" "angular"))
      ("Scheme" (init-env "src-style" "scheme"))
      ("Functional" (init-env "src-style" "functional"))
      ("Latex" (init-env "src-style" "latex")))
  (-> "Special"
      ("Default" (init-default "src-special"))
      ---
      ("None" (init-env "src-special" "raw"))
      ("Formatting" (init-env "src-special" "format"))
      ("Normal" (init-env "src-special" "normal"))
      ("Maximal" (init-env "src-special" "maximal")))
  (-> "Compactification"
      ("Default" (init-default "src-compact"))
      ---
      ("Minimal" (init-env "src-compact" "none"))
      ("Only inline tags" (init-env "src-compact" "inline"))
      ("Normal" (init-env "src-compact" "normal"))
      ("Inline arguments" (init-env "src-compact" "inline args"))
      ("Maximal" (init-env "src-compact" "all")))
  (-> "Closing style"
      ("Default" (init-default "src-close"))
      ---
      ("Repeat" (init-env "src-close" "repeat"))
      ("Stretched" (init-env "src-close" "long"))
      ("Compact" (init-env "src-close" "compact"))
      ("Minimal" (init-env "src-close" "minimal"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global and document language
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind global-language-menu
  ("British"
   (begin
     (init-language "british")
     (set-output-language "british")))
  ("Bulgarian"
   (begin
     (init-language "bulgarian")
     (set-output-language "bulgarian")))
  (when (supports-chinese?)
    ("Chinese"
     (begin
       (init-language "chinese")
       (set-output-language "chinese"))))
  ("Czech"
   (begin
     (init-language "czech")
     (set-output-language "czech")))
  ("Danish"
   (begin
     (init-language "danish")
     (set-output-language "danish")))
  ("Dutch"
   (begin
     (init-language "dutch")
     (set-output-language "dutch")))
  ("English"
   (begin
     (init-language "english")
     (set-output-language "english")))
  ("Finnish"
   (begin
     (init-language "finnish")
     (set-output-language "finnish")))
  ("French"
   (begin
     (init-language "french")
     (set-output-language "french")))
  ("German"
   (begin
     (init-language "german")
     (set-output-language "german")))
  ("Hungarian"
   (begin
     (init-language "hungarian")
     (set-output-language "hungarian")))
  ("Italian"
   (begin
     (init-language "italian")
     (set-output-language "italian")))
  (when (supports-japanese?)
    ("Japanese"
     (begin
       (init-language "japanese")
       (set-output-language "japanese"))))
  (when (supports-korean?)
    ("Korean"
     (begin
       (init-language "korean")
       (set-output-language "korean"))))
  ("Polish"
   (begin
     (init-language "polish")
     (set-output-language "polish")))
  ("Portuguese"
   (begin
     (init-language "portuguese")
     (set-output-language "portuguese")))
  ("Romanian"
   (begin
     (init-language "romanian")
     (set-output-language "romanian")))
  ("Russian"
   (begin
     (init-language "russian")
     (set-output-language "russian")))
  ("Slovene"
   (begin
     (init-language "slovene")
     (set-output-language "slovene")))
  ("Spanish"
   (begin
     (init-language "spanish")
     (set-output-language "spanish")))
  ("Swedish"
   (begin
     (init-language "swedish")
     (set-output-language "swedish")))
  (when (supports-chinese?)
    ("Taiwanese"
     (begin
       (init-language "taiwanese")
       (set-output-language "taiwanese"))))
  ("Ukrainian"
   (begin
     (init-language "ukrainian")
     (set-output-language "ukrainian"))))

(menu-bind document-language-menu
  ("Default" (init-default "language"))
  ---
  ("British" (init-language "british"))
  ("Bulgarian" (init-language "bulgarian"))
  (when (supports-chinese?)
    ("Chinese" (init-language "chinese")))
  ("Czech" (init-language "czech"))
  ("Danish" (init-language "danish"))
  ("Dutch" (init-language "dutch"))
  ("English" (init-language "english"))
  ("Finnish" (init-language "finnish"))
  ("French" (init-language "french"))
  ("German" (init-language "german"))
  ("Hungarian" (init-language "hungarian"))
  ("Italian" (init-language "italian"))
  (when (supports-japanese?)
    ("Japanese" (init-language "japanese")))
  (when (supports-korean?)
    ("Korean" (init-language "korean")))
  ("Polish" (init-language "polish"))
  ("Portuguese" (init-language "portuguese"))
  ("Romanian" (init-language "romanian"))
  ("Russian" (init-language "russian"))
  ("Slovene" (init-language "slovene"))
  ("Spanish" (init-language "spanish"))
  ("Swedish" (init-language "swedish"))
  (when (supports-chinese?)
    ("Taiwanese" (init-language "taiwanese")))
  ("Ukrainian" (init-language "ukrainian")))

(tm-define (current-language-icon)
  (cond ((test-init? "language" "british") "tm_british.xpm")
        ((test-init? "language" "bulgarian") "tm_bulgarian.xpm")
        ((test-init? "language" "chinese") "tm_chinese.xpm")
        ((test-init? "language" "czech") "tm_czech.xpm")
        ((test-init? "language" "danish") "tm_danish.xpm")
        ((test-init? "language" "dutch") "tm_dutch.xpm")
        ((test-init? "language" "english") "tm_english.xpm")
        ((test-init? "language" "finnish") "tm_finnish.xpm")
        ((test-init? "language" "french") "tm_french.xpm")
        ((test-init? "language" "german") "tm_german.xpm")
        ((test-init? "language" "hungarian") "tm_hungarian.xpm")
        ((test-init? "language" "italian") "tm_italian.xpm")
        ((test-init? "language" "japanese") "tm_japanese.xpm")
        ((test-init? "language" "korean") "tm_korean.xpm")
        ((test-init? "language" "polish") "tm_polish.xpm")
        ((test-init? "language" "portuguese") "tm_portuguese.xpm")
        ((test-init? "language" "romanian") "tm_romanian.xpm")
        ((test-init? "language" "russian") "tm_russian.xpm")
        ((test-init? "language" "slovene") "tm_slovene.xpm")
        ((test-init? "language" "spanish") "tm_spanish.xpm")
        ((test-init? "language" "swedish") "tm_swedish.xpm")
        ((test-init? "language" "taiwanese") "tm_taiwanese.xpm")
        ((test-init? "language" "ukrainian") "tm_ukrainian.xpm")
        (else "tm_stateless.xpm")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page sizes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-page-size-menu)
  ("Default"
   (init-default "page-type" "page-width" "page-height")
   (notify-page-change))
  ---
  (group "Common formats")
  ("A3" (init-page-type "a3"))
  ("A4" (init-page-type "a4"))
  ("A5" (init-page-type "a5"))
  ("B4" (init-page-type "b4"))
  ("B5" (init-page-type "b5"))
  ("Letter" (init-page-type "letter"))
  ("Legal" (init-page-type "legal"))
  ("Executive" (init-page-type "executive"))
  ---
  (group "Standard formats")
  (-> "A series"
      ("A0" (init-page-type "a0"))
      ("A1" (init-page-type "a1"))
      ("A2" (init-page-type "a2"))
      ("A3" (init-page-type "a3"))
      ("A4" (init-page-type "a4"))
      ("A5" (init-page-type "a5"))
      ("A6" (init-page-type "a6"))
      ("A7" (init-page-type "a7"))
      ("A8" (init-page-type "a8"))
      ("A9" (init-page-type "a9")))
  (-> "B series"
      ("B0" (init-page-type "b0"))
      ("B1" (init-page-type "b1"))
      ("B2" (init-page-type "b2"))
      ("B3" (init-page-type "b3"))
      ("B4" (init-page-type "b4"))
      ("B5" (init-page-type "b5"))
      ("B6" (init-page-type "b6"))
      ("B7" (init-page-type "b7"))
      ("B8" (init-page-type "b8"))
      ("B9" (init-page-type "b9")))
  (-> "Arch series"
      ("ArchA" (init-page-type "archA"))
      ("ArchB" (init-page-type "archB"))
      ("ArchC" (init-page-type "archC"))
      ("ArchD" (init-page-type "archD"))
      ("ArchE" (init-page-type "archE")))
  (-> "American"
      ("10x14" (init-page-type "10x14"))
      ("11x17" (init-page-type "11x17"))
      ("C5" (init-page-type "C5"))
      ("Comm10" (init-page-type "Comm10"))
      ("DL" (init-page-type "DL"))
      ("Executive" (init-page-type "executive"))
      ("Half letter" (init-page-type "halfletter"))
      ("Half executive" (init-page-type "halfexecutive"))
      ("Ledger" (init-page-type "ledger"))
      ("Legal" (init-page-type "legal"))
      ("Letter" (init-page-type "letter"))
      ("Monarch" (init-page-type "Monarch")))
  (-> "Miscellaneous"
      ("C sheet" (init-page-type "csheet"))
      ("D sheet" (init-page-type "dsheet"))
      ("E sheet" (init-page-type "esheet"))
      ("Flsa" (init-page-type "flsa"))
      ("Flse" (init-page-type "flse"))
      ("Folio" (init-page-type "folio"))
      ("Lecture note" (init-page-type "lecture note"))
      ("Note" (init-page-type "note"))
      ("Quarto" (init-page-type "quarto"))
      ("Statement" (init-page-type "statement"))
      ("Tabloid" (init-page-type "tabloid")))
  (-> "Beamer"
      ("Widescreen 16:9" (init-page-type "16:9"))
      ("Widescreen 8:5" (init-page-type "8:5"))
      ;;("Widescreen 3:2" (init-page-type "3:2"))
      ("Standard 4:3" (init-page-type "4:3"))
      ("Standard 5:4" (init-page-type "5:4")))
  ---
  ("Other" (interactive init-page-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other document submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-font-base-size-menu)
  ("Default" (init-default "font-base-size"))
  ---
  ("8" (init-env "font-base-size" "8"))
  ("9" (init-env "font-base-size" "9"))
  ("10" (init-env "font-base-size" "10"))
  ("11" (init-env "font-base-size" "11"))
  ("12" (init-env "font-base-size" "12"))
  ("14" (init-env "font-base-size" "14"))
  ---
  ("Other" (init-interactive-env "font-base-size")))

(tm-menu (supported-scripts-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (scripts-list)))
    (for (name l)
      ((eval (scripts-name name))
       (noop) ;; NOTE: inhibit segfault due to property searching?
       (init-env "prog-scripts" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (wait-update-current-buffer)
  (system-wait "Updating current buffer, " "please wait")
  (update-current-buffer))

(menu-bind document-menu
  (-> "Style" (link document-style-menu))
  (link document-style-extra-menu)
  (if (detailed-menus?)
      ;;(-> "Add package"
      ;;(link add-package-menu)
      ;;---
      ;;("Other" (interactive add-style-package)))
      ;;(-> "Remove package"
      ;;(link remove-package-menu)
      ;;---
      ;;("Other" (interactive remove-style-package)))
      (if (!= (get-init-tree "sectional-short-style") (tree 'macro "false"))
	  (-> "Part" (link document-part-menu)))
      (-> "View" (link document-view-menu)))
  ---
  (if (new-fonts?)
      ("Font" (interactive open-document-font-selector)))
  (if (not (new-fonts?))
      (-> "Font"
          (-> "Text font"
              ("Default" (init-default "font"))
              ---
              ("Concrete" (init-env "font" "concrete"))
              (if (url-exists-in-tex? "pnr10.mf")
                  ("Pandora" (init-env "font" "pandora")))
              ("Roman" (init-env "font" "roman"))
              (if (font-exists-in-tt? "STIX-Regular")
                  ("Stix" (init-env "font" "stix")))
              ---
              ("Bookman" (init-env "font" "bookman"))
              ("Courier" (init-env "font" "courier"))
              ("Helvetica" (init-env "font" "helvetica"))
              ("N.C. Schoolbook" (init-env "font" "new-century-schoolbook"))
              ("Palatino" (init-env "font" "palatino"))
              ("Times" (init-env "font" "times"))
              ---
              (if (font-exists-in-tt? "DejaVuSerif")
                  ("Dejavu" (init-env "font" "dejavu")))
              ("Lucida" (init-env "font" "x-lucida"))
              (if (font-exists-in-tt? "luxirr")
                  ("Luxi" (init-env "font" "luxi")))
              ("Utopia" (init-env "font" "x-utopia"))
              (if (or (supports-chinese?)
                      (supports-japanese?)
                      (supports-korean?))
                  ---
                  (if (font-exists-in-tt? "Batang")
                      ("Batang" (init-env "font" "batang")))
                  (if (font-exists-in-tt? "fireflysung")
                      ("Fireflysung" (init-env "font" "fireflysung")))
                  (if (font-exists-in-tt? "AppleGothic")
                      ("Gothic" (init-env "font" "apple-gothic")))
                  (if (font-exists-in-tt? "Gulim")
                      ("Gulim" (init-env "font" "gulim")))
                  (if (font-exists-in-tt? "华文细黑")
                      ("HeiTi" (init-env "font" "heiti")))
                  (if (font-exists-in-tt? "ヒラギノ明朝 ProN W6")
                      ("Hiragino Kaku" (init-env "font" "kaku")))
                  (if (font-exists-in-tt? "ipam")
                      ("Ipa" (init-env "font" "ipa")))
                  (if (font-exists-in-tt? "ttf-japanese-gothic")
                      ("Japanese" (init-env "font" "ttf-japanese")))
                  (if (font-exists-in-tt? "kochi-mincho")
                      ("Kochi" (init-env "font" "kochi")))
                  (if (font-exists-in-tt? "儷黑 Pro")
                      ("LiHei" (init-env "font" "lihei")))
                  (if (font-exists-in-tt? "wqy-microhei")
                      ("MicroHei" (init-env "font" "wqy-microhei")))
                  (if (font-exists-in-tt? "mingliu")
                      ("MingLiU" (init-env "font" "mingliu")))
                  (if (font-exists-in-tt? "PMingLiU")
                      ("MingLiU" (init-env "font" "pmingliu")))	      
                  (if (font-exists-in-tt? "MS Gothic")
                      ("MS Gothic" (init-env "font" "ms-gothic")))
                  (if (font-exists-in-tt? "MS Mincho")
                      ("MS Mincho" (init-env "font" "ms-mincho")))
                  (if (font-exists-in-tt? "sazanami-gothic")
                      ("Sazanami" (init-env "font" "sazanami")))
                  (if (font-exists-in-tt? "simfang")
                      ("SimFang" (init-env "font" "simfang")))
                  (if (font-exists-in-tt? "simhei")
                      ("SimHei" (init-env "font" "simhei")))
                  (if (font-exists-in-tt? "simkai")
                      ("SimKai" (init-env "font" "simkai")))
                  (if (font-exists-in-tt? "simli")
                      ("SimLi" (init-env "font" "simli")))
                  (if (font-exists-in-tt? "simsun")
                      ("SimSun" (init-env "font" "simsun")))
                  (if (and (font-exists-in-tt? "SimSun")
                           (not (font-exists-in-tt? "simsun")))
                      ("SimSun" (init-env "font" "apple-simsun")))
                  (if (font-exists-in-tt? "simyou")
                      ("SimYou" (init-env "font" "simyou")))
                  (if (font-exists-in-tt? "ukai")
                      ("UKai" (init-env "font" "ukai")))
                  (if (font-exists-in-tt? "unbatang")
                      ("Unbatang" (init-env "font" "unbatang")))
                  (if (font-exists-in-tt? "uming")
                      ("UMing" (init-env "font" "uming")))
                  (if (font-exists-in-tt? "wqy-zenhei")
                      ("ZenHei" (init-env "font" "wqy-zenhei")))))
          (-> "Mathematical font"
              ("Default" (init-default "math-font"))
              ---
              ("Adobe" (init-env "math-font" "adobe"))
              (if (font-exists-in-tt? "Apple Symbols")
                  ("Apple symbols" (init-env "math-font" "math-apple")))
              (if (font-exists-in-tt? "Asana-Math")
                  ("Asana" (init-env "math-font" "math-asana")))
              ("Concrete" (init-env "math-font" "concrete"))
              (if (font-exists-in-tt? "DejaVuSerif")
                  ("Dejavu" (init-env "math-font" "math-dejavu")))
              ("Euler new roman" (init-env "math-font" "ENR"))
              (if (font-exists-in-tt? "LucidaGrande")
                  ("Lucida" (init-env "math-font" "math-lucida")))
              (if (font-exists-in-tt? "texgyrepagella-math")
                  ("Pagella" (init-env "math-font" "math-pagella")))
              ("Roman" (init-env "math-font" "roman"))
              (if (font-exists-in-tt? "STIX-Regular")
                  ("Stix" (init-env "math-font" "math-stix")))
              (if (font-exists-in-tt? "texgyretermes-math")
                  ("Termes" (init-env "math-font" "math-termes"))))
          (-> "Program font"
              ("Default" (init-default "prog-font"))
              ---
              ("Concrete" (init-env "prog-font" "concrete"))
              (if (url-exists-in-tex? "pnr10.mf")
                  ("Pandora" (init-env "prog-font" "pandora")))
              ("Roman" (init-env "prog-font" "roman"))
              ("Times" (init-env "prog-font" "times")))
          ---
          (-> "Size"
              (link document-font-base-size-menu))
          (-> "Dpi"
              ("Default" (init-default "dpi"))
              ---
              ("150" (init-env "dpi" "150"))
              ("200" (init-env "dpi" "200"))
              ("300" (init-env "dpi" "300"))
              ("400" (init-env "dpi" "400"))
              ("600" (init-env "dpi" "600"))
              ("800" (init-env "dpi" "800"))
              ("1200" (init-env "dpi" "1200"))
              ---
              ("Other" (init-interactive-env "dpi")))))
  (-> "Magnification"
      ("Default" (init-default "magnification"))
      ---
      ("0.7" (init-env "magnification" "0.7"))
      ("0.8" (init-env "magnification" "0.8"))
      ("1" (init-env "magnification" "1"))
      ("1.2" (init-env "magnification" "1.2"))
      ("1.4" (init-env "magnification" "1.4"))
      ("1.7" (init-env "magnification" "1.7"))
      ("2" (init-env "magnification" "2"))
      ---
      ("Other" (init-interactive-env "magnification")))
  (-> "Color"
      (-> "Foreground"
	  ("Default" (init-default "color"))
	  ---
	  (pick-color (init-env "color" answer))
	  ---
          ("Palette" (interactive-color
                      (lambda (col) (init-env "color" col)) '()))
	  ("Other" (init-interactive-env "color")))
      (-> "Background"
	  ("Default" (init-default "bg-color"))
	  ---
	  (pick-background "" (init-env-tree "bg-color" answer))
	  ---
          ("Palette" (interactive-background
                      (lambda (col) (init-env "bg-color" col)) '()))
	  ("Other" (init-interactive-env "bg-color"))))
  (if (detailed-menus?)
      (-> "Language"
          (link document-language-menu)))
  (-> "Scripts"
      ("Default" (init-default "prog-scripts"))
      ---
      (link supported-scripts-menu))
  (-> "Paragraph"
      (-> "Style"
	  ("Default" (init-default "par-mode"))
	  ---
	  ("Justified" (init-env "par-mode" "justify"))
	  ("Left aligned" (init-env "par-mode" "left"))
	  ("Centered" (init-env "par-mode" "center"))
	  ("Right aligned" (init-env "par-mode" "right")))
      (-> "Hyphenation"
	  ("Default" (init-default "par-hyphen"))
	  ---
	  ("Normal" (init-env "par-hyphen" "normal"))
	  ("Professional" (init-env "par-hyphen" "professional")))
      (-> "Margins"
	  ("Default" (init-default "par-first"))
	  ---
	  ("First indentation" (init-interactive-env "par-first")))
      (-> "Spacing"
	  ("Default" (init-default "par-sep" "par-line-sep"
				   "interpargraph space"))
	  ---
	  ("Interline separation" (init-interactive-env "par-sep"))
	  ("Interline space" (init-interactive-env "par-line-sep"))
	  ("Interparagraph space" (init-interactive-env "par-par-sep")))
      (-> "Number of columns"
	  ("Default" (init-default "par-columns"))
	  ---
	  ("1" (init-env "par-columns" "1"))
	  ("2" (init-env "par-columns" "2"))
	  ("3" (init-env "par-columns" "3"))))
  (-> "Page"
      (-> "Type"
	  ("Default" (init-default-page-medium))
	  ---
	  ("Paper" (init-page-medium "paper"))
	  ("Papyrus" (init-page-medium "papyrus"))
	  ("Automatic" (init-page-medium "automatic"))
	  ("Beamer" (init-page-medium "beamer")))
      (-> "Size"
	  (link document-page-size-menu))
      (-> "Orientation"
	  ("Default" (init-default-page-orientation))
	  ---
	  ("Portrait" (init-page-orientation "portrait"))
	  ("Landscape" (init-page-orientation "landscape")))
      (-> "Margins"
	  ("Default" (init-default "page-width-margin" "page-height-margin"
				   "page-odd" "page-even" "page-right"
				   "par-width" "page-odd-shift"
				   "page-even-shift" "page-top" "page-bot"
				   "page-height-margin"))
	  ---
	  (when (test-env? "page-width-margin" "false")
		("Odd page left margin" (init-interactive-env "page-odd"))
		("Odd page right margin" (init-interactive-env "page-right"))
		("Even page left margin" (init-interactive-env "page-even")))
	  (when (test-env? "page-height-margin" "false")
		("Top margin" (init-interactive-env "page-top"))
		("Bottom margin" (init-interactive-env "page-bot")))
	  ---
	  ("Alternative specification" (toggle-page-width-margin))
	  (when (test-env? "page-width-margin" "true")
		("Paragraph width" (init-interactive-env "par-width"))
		("Odd page shift" (init-interactive-env "page-odd-shift"))
		("Even page shift" (init-interactive-env "page-even-shift"))))
      (if (detailed-menus?)
	  ---
	  (group "Breaking")
	  (-> "Algorithm"
	      ("Default" (init-default "page-breaking"))
	      ---
	      ("Sloppy" (init-env "page-breaking" "sloppy"))
	      ("Medium" (init-env "page-breaking" "medium"))
	      ("Professional" (init-env "page-breaking" "optimal")))
	  (-> "Limits"
	      ("Allowed reduction" (init-interactive-env "page-shrink"))
	      ("Allowed extension" (init-interactive-env "page-extend")))
	  (-> "Flexibility"
	      ("Default" (init-default "page-flexibility"))
	      ---
	      ("0" (init-env "page-flexibility" "0.0"))
	      ("1/4" (init-env "page-flexibility" "0.25"))
	      ("1/2" (init-env "page-flexibility" "0.5"))
	      ("3/4" (init-env "page-flexibility" "0.75"))
	      ("1" (init-env "page-flexibility" "1.0"))
	      ---
	      ("Other" (init-interactive-env "page-flexibility")))))
  ---
  (-> "Update"
      ("All" (generate-all-aux) (inclusions-gc) (wait-update-current-buffer))
      ---
      ("Buffer" (wait-update-current-buffer))
      ("Bibliography" (generate-all-aux) (wait-update-current-buffer))
;;    ("Bibliography" (generate-aux "bibliography"))
      ("Table of contents" (generate-aux "table-of-contents"))
      ("Index" (generate-aux "index"))
      ("Glossary" (generate-aux "glossary"))
      (if (project-attached?)
	  ---
	  ("Clear local information" (clear-local-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document focus menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-document-extra-menu t))
(tm-menu (focus-style-extra-menu t))

(tm-menu (focus-style-menu t)
  (group "Style")
  (let* ((st* (get-style-list))
         (st (if (null? st*) (list "no style") st*)))
    ;;((eval (upcase-first (car st)))
    ;;(open-style-selector))
    (-> (eval (upcase-first (car st)))
        (link style-menu)
        ---
        ("Other style" (interactive set-main-style)))
    (dynamic (focus-style-extra-menu t))
    (for (pack (list-filter (cdr st) (negate hidden-package?)))
      (-> (eval (upcase-first pack))
          ("Remove package" (remove-style-package pack)))))
  (-> "Add style package"
      (link add-package-menu)
      ---
      ("Other package" (interactive add-style-package))))

(tm-menu (focus-document-menu t)
  (group "Document")
  (-> (eval (upcase-first (get-init "page-type")))
      (link document-page-size-menu))
  (-> (eval (string-append (get-init "font-base-size") " pt"))
      (link document-font-base-size-menu)))

(tm-menu (standard-focus-menu t)
  (:require (tree-is-buffer? t))
  (dynamic (focus-style-menu t))
  ---
  (dynamic (focus-document-menu t))
  (dynamic (focus-document-extra-menu t))
  ---
  ("Help" (focus-help)))

(tm-menu (focus-document-extra-icons t))
(tm-menu (focus-style-extra-icons t))

(tm-menu (focus-style-icons t)
  (minibar
    (let* ((st* (get-style-list))
           (st (if (null? st*) (list "no style") st*)))
      ;;((balloon (eval (upcase-first (car st))) "Document style")
      ;; (open-style-selector))
      (=> (balloon (eval (upcase-first (car st))) "Document style")
          (link style-menu)
          ---
          ("Other style" (interactive set-main-style)))
      (dynamic (focus-style-extra-icons t))
      (for (pack (list-filter (cdr st) (negate hidden-package?)))
        (=> (eval pack)
            ("Remove package" (remove-style-package pack)))))
    (=> (balloon (icon "tm_add.xpm") "Add style package")
        (link add-package-menu)
        ---
        ("Other package" (interactive add-style-package)))
    (assuming (tree-is-buffer? t)
      ((balloon (icon "tm_focus_help.xpm") "Describe tag")
       (focus-help)))))

(tm-define (current-page-icon)
  (cond ((test-init? "page-orientation" "landscape")
         (cond ((test-init? "par-columns" "2") "tm_landscape_2col.xpm")
               (else "tm_landscape.xpm")))
        (else
         (cond ((test-init? "par-columns" "2") "tm_portrait_2col.xpm")
               (else "tm_portrait.xpm")))))

(tm-menu (focus-document-icons t)
  (minibar
    (=> (balloon (eval (upcase-first (get-init "page-type")))
                 "Paper size")
        (link document-page-size-menu))
    (=> (balloon (eval (string-append (get-init "font-base-size") "pt"))
                 "Font size")
        (link document-font-base-size-menu))
    (=> (balloon (icon (eval (current-page-icon))) "Page layout")
        ("Portrait" (init-page-orientation "portrait"))
        ("Landscape" (init-page-orientation "landscape"))
        ---
        ("One column" (init-env "par-columns" "1"))
        ("Two columns" (init-env "par-columns" "2")))
    (glue #f #f 0 0)
    (=> (balloon (icon (eval (current-language-icon))) "Document language")
        (link document-language-menu))))

(tm-menu (standard-focus-icons t)
  (:require (tree-is-buffer? t))
  (dynamic (focus-style-icons t))  
  //
  (dynamic (focus-document-icons t))  
  //   
  (dynamic (focus-document-extra-icons t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style chooser widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (select-style-among-widget l)
  (resize ("300px" "300px" "300px") ("200px" "300px" "1000px")
    (scrollable
      (choice (set-main-style answer) l "generic"))))

(tm-widget (select-common-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "beamer" "book" "browser" "exam"
                  "generic" "letter" "manual" "seminar" "source"))))

(tm-widget (select-education-style-widget)
  (dynamic (select-style-among-widget
            (list "exam" "lycee-examen" "lycee-tp"))))

(tm-widget (select-article-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "tmarticle"))))

(tm-widget (select-any-style-widget)
  (dynamic (select-style-among-widget
            (list "article" "tmarticle"))))

(tm-widget (select-style-widget)
  (tabs
    (tab (text "Common")
      (dynamic (select-common-style-widget)))
    (tab (text "Education")
      (dynamic (select-education-style-widget)))
    (tab (text "Article")
      (dynamic (select-article-style-widget)))
    (tab (text "Any")
      (dynamic (select-any-style-widget)))))

(tm-define (open-style-selector)
  (:interactive #t)
  (top-window select-style-widget "Select document style"))
