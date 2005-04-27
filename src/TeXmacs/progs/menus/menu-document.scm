
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-document.scm
;; DESCRIPTION : menus for setting global document properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-document)
  (:use (texmacs texmacs tm-document)))

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
  ("Other style" ... (interactive '("Document style:") 'init-style)))

(menu-bind global-language-menu
  ("British"
   (begin
     (init-language "british")
     (set-output-language "british")))
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
  ("Ukrainian"
   (begin
     (init-language "ukrainian")
     (set-output-language "ukrainian"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Document menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind document-menu
  (-> "Style" (link document-style-menu))
  (-> "Add package"
      (link add-package-menu)
      ---
      ("Other" ... (interactive '("Use package:") 'init-add-package)))
  (-> "Remove package"
      (link remove-package-menu)
      ---
      ("Other" ... (interactive '("Use package:") 'init-remove-package)))
  (-> "Master"
      ("Attach" (interactive '("Master file:") 'project-attach))
      ("Detach" (project-detach)))
  (-> "View"
      ("Edit source tree" (toggle-preamble))
      (-> "Informative flags"
	  ("Default" (init-default "info-flag"))
	  ---
	  ("None" (init-env "info-flag" "none"))
	  ("Short" (init-env "info-flag" "short"))
	  ("Detailed" (init-env "info-flag" "detailed")))
      (-> "Page layout"
	  ("Default" (init-default "page-screen-margin" "page-show-hf"
				   "page-screen-left" "page-screen-right"
				   "page-screen-top" "page-screen-bot"))
	  ---
	  ("Show header and footer" (toggle-visible-header-and-footer))
	  ("Margins as on paper" (toggle-page-screen-margin))
	  ---
	  (when (test-env? "page-screen-margin" "true")
		("Left margin" ...
		 (interactive '("Left margin:")
			      '(lambda (s) (init-env "page-screen-left" s))))
		("Right margin" ...
		 (interactive '("Right margin:")
			      '(lambda (s) (init-env "page-screen-right" s))))
		("Top margin" ...
		 (interactive '("Top margin:")
			      '(lambda (s) (init-env "page-screen-top" s))))
		("Bottom margin" ...
		 (interactive '("Bottom margin:")
			      '(lambda (s) (init-env "page-screen-bot" s))))))
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
  ---
  (-> "Font"
      (-> "Text font"
	  ("Default" (init-default "font"))
	  ---
	  ("Roman" (init-env "font" "roman"))
	  ("Pandora" (init-env "font" "pandora"))
	  ("Concrete" (init-env "font" "concrete"))
	  ("Cyrillic" (init-env "font" "cyrillic"))
	  ---
	  ("Bookman" (init-env "font" "bookman"))
	  ("Courier" (init-env "font" "courier"))
	  ("Helvetica" (init-env "font" "helvetica"))
	  ("Palatino" (init-env "font" "palatino"))
	  ("Times" (init-env "font" "times"))
	  ---
	  (if (support-tt-fonts?)
	      (if (font-exists-in-tt? "luxirr")
		  ("Luxi" (init-env "font" "luxi"))))
	  ("Lucida" (init-env "font" "x-lucida"))
	  ("Utopia" (init-env "font" "x-utopia")))
      (-> "Mathematical font"
	  ("Default" (init-default "math-font"))
	  ---
	  ("Roman" (init-env "math-font" "roman"))
	  ("Concrete" (init-env "math-font" "concrete"))
	  ("Euler new roman" (init-env "math-font" "ENR"))
	  ("Adobe" (init-env "math-font" "adobe")))
      (-> "Program font"
	  ("Default" (init-default "prog-font"))
	  ---
	  ("Roman" (init-env "prog-font" "roman"))
	  ("Pandora" (init-env "prog-font" "pandora"))
	  ("Concrete" (init-env "prog-font" "concrete"))
	  ("Times" (init-env "prog-font" "times")))
      ---
      (-> "Size"
	  ("Default" (init-default "font-base-size"))
	  ---
	  ("8" (init-env "font-base-size" "8"))
	  ("9" (init-env "font-base-size" "9"))
	  ("10" (init-env "font-base-size" "10"))
	  ("11" (init-env "font-base-size" "11"))
	  ("12" (init-env "font-base-size" "12"))
	  ("14" (init-env "font-base-size" "14"))
	  ---
	  ("Other" ... (interactive '("Font size:") 'init-font-size)))
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
	  ("Other" ... (interactive '("Font dpi:") 'init-dpi))))
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
      ("Other" ... (interactive '("Magnification:") 'init-magn)))
  (-> "Color"
      (-> "Foreground"
	  ("Default" (init-default "color"))
	  ---
	  ("Black" (init-env "color" "black"))
	  ("White" (init-env "color" "white"))
	  ("Grey" (init-env "color" "grey"))
	  ("Red" (init-env "color" "red"))
	  ("Blue" (init-env "color" "blue"))
	  ("Yellow" (init-env "color" "yellow"))
	  ("Green" (init-env "color" "green"))
	  ("Orange" (init-env "color" "orange"))
	  ("Magenta" (init-env "color" "magenta"))
	  ("Brown" (init-env "color" "brown"))
	  ("Pink" (init-env "color" "pink"))
	  ---
	  ("Other" ... (interactive '("Foreground color:") 'init-color)))
      (-> "Background"
	  ("Default" (init-default "bg-color"))
	  ---
	  ("Black" (init-env "bg-color" "black"))
	  ("White" (init-env "bg-color" "white"))
	  ("Grey" (init-env "bg-color" "grey"))
	  ("Red" (init-env "bg-color" "red"))
	  ("Blue" (init-env "bg-color" "blue"))
	  ("Yellow" (init-env "bg-color" "yellow"))
	  ("Green" (init-env "bg-color" "green"))
	  ("Orange" (init-env "bg-color" "orange"))
	  ("Magenta" (init-env "bg-color" "magenta"))
	  ("Brown" (init-env "bg-color" "brown"))
	  ("Pink" (init-env "bg-color" "pink"))
	  ("Light grey" (init-env "bg-color" "light grey"))
	  ("Dark grey" (init-env "bg-color" "dark grey"))
	  ("Broken white" (init-env "bg-color" "broken white"))
	  ---
	  ("Other" ... (interactive '("Background color:") 'init-bg-color))))
  (-> "Language"
      ("Default" (init-default "language"))
      ---
      ("British" (init-language "british"))
      ("Czech" (init-language "czech"))
      ("Danish" (init-language "danish"))
      ("Dutch" (init-language "dutch"))
      ("English" (init-language "english"))
      ("Finnish" (init-language "finnish"))
      ("French" (init-language "french"))
      ("German" (init-language "german"))
      ("Hungarian" (init-language "hungarian"))
      ("Italian" (init-language "italian"))
      ("Polish" (init-language "polish"))
      ("Portuguese" (init-language "portuguese"))
      ("Romanian" (init-language "romanian"))
      ("Russian" (init-language "russian"))
      ("Slovene" (init-language "slovene"))
      ("Spanish" (init-language "spanish"))
      ("Swedish" (init-language "swedish"))
      ("Ukrainian" (init-language "ukrainian")))
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
	  ("First indentation" ...
	   (interactive '("First indentation:") 'init-first-indent)))
      (-> "Spacing"
	  ("Default" (init-default "par-sep" "par-line-sep"
				   "interpargraph space"))
	  ---
	  ("Interline separation" ...
	   (interactive '("Separation between lines:") 'init-interline))
	  ("Interline space" ...
	   (interactive '("Space between lines:") 'init-interline-spc))
	  ("Interparagraph space" ...
	   (interactive '("Space between paragraphs:") 'init-interpar-spc)))
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
	  ("Default"
	   (check "*" (test-default? "page-type" "page-width" "page-height"))
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
	  ("Other" ...
	   (interactive '("Page width:" "Page height:") 'init-page-size)))
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
		("Odd page left margin" ...
		 (interactive '("Odd page left margin:")
			      '(lambda (s) (init-env "page-odd" s))))
		("Odd page right margin" ...
		 (interactive '("Odd page right margin:")
			      '(lambda (s) (init-env "page-right" s))))
		("Even page left margin" ...
		 (interactive '("Even page left margin:")
			      '(lambda (s) (init-env "page-even" s)))))
	  (when (test-env? "page-height-margin" "false")
		("Top margin" ...
		 (interactive '("Top margin:")
			      '(lambda (s) (init-env "page-top" s))))
		("Bottom margin" ...
		 (interactive '("Bottom margin:")
			      '(lambda (s) (init-env "page-bot" s)))))
	  ---
	  ("Alternative specification" (toggle-page-width-margin))
	  (when (test-env? "page-width-margin" "true")
		("Paragraph width" ...
		 (interactive '("Paragraph width:")
			      '(lambda (s) (init-env "par-width" s))))
		("Odd page shift" ...
		 (interactive '("Odd page shift:")
			      '(lambda (s) (init-env "page-odd-shift" s))))
		("Even page shift" ...
		 (interactive '("Even page shift:")
			      '(lambda (s) (init-env "page-even-shift" s))))))
      ---
      (group "Breaking")
      (-> "Algorithm"
	  ("Default" (init-default "page-breaking"))
	  ---
	  ("Sloppy" (init-env "page-breaking" "sloppy"))
	  ("Medium" (init-env "page-breaking" "medium"))
	  ("Professional" (init-env "page-breaking" "optimal")))
      (-> "Limits"
	  ("Allowed reduction" ...
	   (interactive '("How much shorter may pages become?")
			'init-page-shrink))
	  ("Allowed extension" ...
	   (interactive '("How much longer may pages become?")
			'init-page-extend)))
      (-> "Flexibility"
	  ("Default" (init-default "page-flexibility"))
	  ---
	  ("0" (init-env "page-flexibility" "0.0"))
	  ("1/4" (init-env "page-flexibility" "0.25"))
	  ("1/2" (init-env "page-flexibility" "0.5"))
	  ("3/4" (init-env "page-flexibility" "0.75"))
	  ("1" (init-env "page-flexibility" "1.0"))
	  ---
	  ("Other" ...
	   (interactive '("Flexibility factor for vertical spacing: ")
			'init-page-flexibility))))
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
