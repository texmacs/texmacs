
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
  (-> "Use package"
      (link use-package-menu)
      ---
      ("Other" ... (interactive '("Use package:") 'init-extra-style)))
  (-> "Master"
      ("Attach" (interactive '("Master file:") 'project-attach))
      ("Detach" (project-detach)))
  (-> "View"
      ("Preamble mode" (toggle-preamble))
      (-> "Informative flags"
	  ("Default" (init-default "info flag"))
	  ---
	  ("None" (init-env "info flag" "none"))
	  ("Short" (init-env "info flag" "short"))
	  ("Detailed" (init-env "info flag" "detailed"))))
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
	  (if (use-tt-fonts?)
	      (if (font-exists-in-tt? "luxirr")
		  ("Luxi" (init-env "font" "luxi"))))
	  ("Lucida" (init-env "font" "x-lucida"))
	  ("Utopia" (init-env "font" "x-utopia")))
      (-> "Math font"
	  ("Default" (init-default "math font"))
	  ---
	  ("Roman" (init-env "math font" "roman"))
	  ("Concrete" (init-env "math font" "concrete"))
	  ("Euler new roman" (init-env "math font" "ENR"))
	  ("Adobe" (init-env "math font" "adobe")))
      (-> "Prog font"
	  ("Default" (init-default "prog font"))
	  ---
	  ("Roman" (init-env "prog font" "roman"))
	  ("Pandora" (init-env "prog font" "pandora"))
	  ("Concrete" (init-env "prog font" "concrete"))
	  ("Times" (init-env "prog font" "times")))
      ---
      (-> "Size"
	  ("Default" (init-default "font base size"))
	  ---
	  ("8" (init-env "font base size" "8"))
	  ("9" (init-env "font base size" "9"))
	  ("10" (init-env "font base size" "10"))
	  ("11" (init-env "font base size" "11"))
	  ("12" (init-env "font base size" "12"))
	  ("14" (init-env "font base size" "14"))
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
	  ("Pink" (init-env "color" "pink")))
      (-> "Background"
	  ("Default" (init-default "background color"))
	  ---
	  ("Black" (init-env "background color" "black"))
	  ("White" (init-env "background color" "white"))
	  ("Grey" (init-env "background color" "grey"))
	  ("Red" (init-env "background color" "red"))
	  ("Blue" (init-env "background color" "blue"))
	  ("Yellow" (init-env "background color" "yellow"))
	  ("Green" (init-env "background color" "green"))
	  ("Orange" (init-env "background color" "orange"))
	  ("Magenta" (init-env "background color" "magenta"))
	  ("Brown" (init-env "background color" "brown"))
	  ("Pink" (init-env "background color" "pink"))
	  ("Light grey" (init-env "background color" "light grey"))
	  ("Dark grey" (init-env "background color" "dark grey"))
	  ("Broken white" (init-env "background color" "broken white"))))
  (-> "Language"
      ("Default" (init-default "language"))
      ---
      ("British" (init-language "british"))
      ("Czech" (init-language "czech"))
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
      ("Spanish" (init-language "spanish"))
      ("Swedish" (init-language "swedish"))
      ("Ukrainian" (init-language "ukrainian")))
  (-> "Paragraph"
      (-> "Style"
	  ("Default" (init-default "paragraph mode"))
	  ---
	  ("Justified" (init-env "paragraph mode" "justify"))
	  ("Left ragged" (init-env "paragraph mode" "left"))
	  ("Centered" (init-env "paragraph mode" "center"))
	  ("Right ragged" (init-env "paragraph mode" "right")))
      (-> "Hyphenation"
	  ("Default" (init-default "paragraph hyphenation"))
	  ---
	  ("Normal" (init-env "paragraph hyphenation" "normal"))
	  ("Professional" (init-env "paragraph hyphenation" "professional")))
      (-> "Margins"
	  ("Default" (init-default "first indentation"))
	  ---
	  ("First indentation" ...
	   (interactive '("First indentation:") 'init-first-indent)))
      (-> "Spacing"
	  ("Default" (init-default "interline space" "line stretch"
				   "interpargraph space"))
	  ---
	  ("Interline separation" ...
	   (interactive '("Separation between lines:") 'init-interline))
	  ("Interline space" ...
	   (interactive '("Space between lines:") 'init-interline-spc))
	  ("Interparagraph space" ...
	   (interactive '("Space between paragraphs:") 'init-interpar-spc)))
      (-> "Number of columns"
	  ("Default" (init-default "nr columns"))
	  ---
	  ("1" (init-env "nr columns" "1"))
	  ("2" (init-env "nr columns" "2"))
	  ("3" (init-env "nr columns" "3"))))
  (-> "Page"
      (-> "Type"
	  ("Default" (check "*" (test-default? "page medium"))
	   (init-default "page medium")
	   (set-page-parameters))
	  ---
	  ("Paper" (set-page-medium "paper"))
	  ("Papyrus" (set-page-medium "papyrus"))
	  ("Automatic" (set-page-medium "automatic")))
      (-> "Size"
	  ("Default" (check "*" (test-default? "page type"))
	   (init-default "page type")
	   (set-page-parameters))
	  ---
	  ("A3" (set-page-type "a3"))
	  ("A4" (set-page-type "a4"))
	  ("A5" (set-page-type "a5"))
	  ("B4" (set-page-type "b4"))
	  ("B5" (set-page-type "b5"))
	  ("B6" (set-page-type "b6"))
	  ("Letter" (set-page-type "letter"))
	  ("Legal" (set-page-type "legal"))
	  ("Executive" (set-page-type "executive"))
	  ("Lecture note" (set-page-type "lecture note"))
	  ---
	  ("Other" ...
	   (interactive '("Page width:" "Page height:") 'init-page-size)))
      (-> "Orientation"
	  ("Default" (check "*" (test-default? "page orientation"))
	   (init-default "page orientation")
	   (set-page-parameters))
	  ---
	  ("Portrait" (set-page-orientation "portrait"))
	  ("Landscape" (set-page-orientation "landscape")))
      (-> "Layout"
	  ("Default" (init-default "odd page margin" "even page margin"
				   "page right margin" "page top margin"
				   "page bottom margin" "paragraph width"))
	  ---
	  ("Set margins" ...
	   (interactive
	    '("Left margin:" "Right margin:"
	      "Top margin:" "Bottom margin:") 'init-page-margins))
	  ("Set text width" ...
	   (interactive '("Text width:") 'init-text-width)))
      (-> "Screen layout"
	  ("Default" (init-default "reduction page left margin"
				   "reduction page right margin"
				   "reduction page top margin"
				   "reduction page bottom margin"
				   "show header and footer"))
	  ---
	  ("Margins as on paper" (init-as-on-paper))
	  ("Reduce margins" ...
	   (interactive
	    '("Reduce left margin by:"
	      "Reduce right margin by:"
	      "Reduce top margin by:"
	      "Reduce bottom margin by:") 'init-screen-reduction))
	  ("Show header and footer" (toggle-visible-header-and-footer)))
      ---
      (group "Breaking")
      (-> "Algorithm"
	  ("Default" (init-default "page breaking"))
	  ---
	  ("Sloppy" (init-env "page breaking" "sloppy"))
	  ("Medium" (init-env "page breaking" "medium"))
	  ("Professional" (init-env "page breaking" "optimal")))
      (-> "Limits"
	  ("Allowed reduction" ...
	   (interactive '("How much shorter may pages become?")
			'init-page-shrink))
	  ("Allowed extension" ...
	   (interactive '("How much longer may pages become?")
			'init-page-extend)))
      (-> "Flexibility"
	  ("Default" (init-default "page flexibility"))
	  ---
	  ("0" (init-env "page flexibility" "0.0"))
	  ("1/4" (init-env "page flexibility" "0.25"))
	  ("1/2" (init-env "page flexibility" "0.5"))
	  ("3/4" (init-env "page flexibility" "0.75"))
	  ("1" (init-env "page flexibility" "1.0"))
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
