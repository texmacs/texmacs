
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
  (:use (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus for the Document menu and the iconbars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind project-menu
  (link project-buffer-menu))

(menu-bind document-style-menu
  ("No style" (init-style "none"))
  ---
  (link style-menu)
  ---
  ("Other style" (interactive init-style)))

(menu-bind document-view-menu
  ("Edit source tree" (toggle-preamble))
  (-> "Informative flags"
      ("Default" (init-default "info-flag"))
      ---
      ("None" (init-env "info-flag" "none"))
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
     (init-language <"czech")
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

(tm-menu (supported-scripts-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (list-sort supported-scripts-list string<=?)))
    (for (name l)
      ((eval (ahash-ref supported-scripts-table name))
       (noop) ;; NOTE: inhibit segfault due to property searching?
       (init-env "prog-scripts" name)))))

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
  ---
  ("Other" (interactive init-page-size)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-menu
  (-> "Style" (link document-style-menu))
  (if (detailed-menus?)
      (-> "Add package"
	  (link add-package-menu)
	  ---
	  ("Other" (interactive init-add-package)))
      (-> "Remove package"
	  (link remove-package-menu)
	  ---
	  ("Other" (interactive init-remove-package)))
      (if (!= (get-init-tree "sectional-short-style") (tree 'macro "false"))
	  (-> "Part" (link document-part-menu)))
      (-> "View" (link document-view-menu)))
  ---
  (-> "Font"
      (-> "Text font"
	  ("Default" (init-default "font"))
	  ---
	  ("Concrete" (init-env "font" "concrete"))
	  ("Cyrillic" (init-env "font" "cyrillic"))
	  ("Pandora" (init-env "font" "pandora"))
	  ("Roman" (init-env "font" "roman"))
	  ---
	  ("Bookman" (init-env "font" "bookman"))
	  ("Courier" (init-env "font" "courier"))
	  ("Helvetica" (init-env "font" "helvetica"))
	  ("Palatino" (init-env "font" "palatino"))
	  ("Times" (init-env "font" "times"))
	  ---
	  (if (font-exists-in-tt? "DejaVuSerif")
	      ("Dejavu" (init-env "font" "dejavu")))
	  ("Lucida" (init-env "font" "x-lucida"))
	  (if (> (get-font-type) 0)
	      (if (font-exists-in-tt? "luxirr")
		  ("Luxi" (init-env "font" "luxi"))))
	  (if (font-exists-in-tt? "STIXGeneral")
	      ("Stix" (init-env "font" "stix")))
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
	  ("Roman" (init-env "math-font" "roman"))
	  (if (font-exists-in-tt? "STIXGeneral")
	      ("Stix" (init-env "math-font" "math-stix"))))
      (-> "Program font"
	  ("Default" (init-default "prog-font"))
	  ---
	  ("Concrete" (init-env "prog-font" "concrete"))
	  ("Pandora" (init-env "prog-font" "pandora"))
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
	  ("Other" (init-interactive-env "dpi"))))
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
	  (pick-color (init-env "color" (tree->stree answer)))
	  ---
	  ("Other" (init-interactive-env "color")))
      (-> "Background"
	  ("Default" (init-default "bg-color"))
	  ---
	  (pick-background (init-env-tree "bg-color" (tree->stree answer)))
	  ---
	  ("Other" (init-interactive-env "bg-color"))))
  (if (detailed-menus?)
      (-> "Language"
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
	  ("Ukrainian" (init-language "ukrainian"))))
  (-> "Scripts"
      ("Default" (init-default "prog-scripts"))
      ---
      (link supported-scripts-menu))
  (-> "Paragraph"
      (-> "Style"
	  ("Default" (init-default "par-mode"))
	  ---
	  ("Justified" (init-env "par-mode" "justify"))
	  ("Left ragged" (init-env "par-mode" "left"))
	  ("Centered" (init-env "par-mode" "center"))
	  ("Right ragged" (init-env "par-mode" "right")))
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
	  ("Default" (check "*" (test-default? "page-medium"))
	   (init-default "page-medium")
	   (notify-page-change))
	  ---
	  ("Paper" (init-page-medium "paper"))
	  ("Papyrus" (init-page-medium "papyrus"))
	  ("Automatic" (init-page-medium "automatic")))
      (-> "Size"
	  (link document-page-size-menu))
      (-> "Orientation"
	  ("Default" (check "*" (test-default? "page-orientation"))
	   (init-default "page-orientation")
	   (notify-page-change))
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
      ("All" (generate-all-aux) (update-buffer))
      ---
      ("Buffer" (update-buffer))
      ("Bibliography" (generate-all-aux) (update-buffer))
;;    ("Bibliography" (generate-aux "bibliography"))
      ("Table of contents" (generate-aux "table-of-contents"))
      ("Index" (generate-aux "index"))
      ("Glossary" (generate-aux "glossary"))
      (if (project-attached?)
	  ---
	  ("Clear local information" (clear-local-info)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document focus icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (standard-focus-icons t)
  (:require (== (tree->path t) (buffer-path)))
  (minibar
   (let* ((st* (tree->stree (get-style-tree)))
	  (st (if (== st* '(tuple)) '(generic) (cdr st*))))
     (=> (balloon (eval (upcase-first (car st))) "Document style")
	 (link style-menu))
     (for (pack (cdr st))
       (=> (eval pack)
	   ("Remove package" (init-remove-package pack)))))
   (=> (balloon (icon "tm_add.xpm") "Add style package")
       (link add-package-menu)))

  (glue #f #f 5 0)
  (minibar
   (=> (balloon (eval (upcase-first (get-init "page-type")))
		"Paper size")
       (link document-page-size-menu))
   (=> (balloon (eval (string-append (get-init "font-base-size") " pt"))
		"Font size")
       (link document-font-base-size-menu))))
