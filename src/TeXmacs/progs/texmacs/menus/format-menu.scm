
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-menu.scm
;; DESCRIPTION : menus for setting local formatting properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus format-menu)
  (:use (texmacs edit edit-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Font submenu in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-font-menu
  (-> "Name"
      ("Roman" (make-with "font" "roman"))
      (if (url-exists-in-tex? "pnr10.mf")
	  ("Pandora" (make-with "font" "pandora")))
      (if (url-exists-in-tex? "ccr10.mf")
	  ("Concrete" (make-with "font" "concrete")))
      ---
      (-> "Adobe"
	  (if (url-exists-in-tex? "rpagk.tfm")
	      ("Avant Garde" (make-with "font" "avant-garde")))
	  (if (url-exists-in-tex? "rpbkl.tfm")
	      ("Bookman" (make-with "font" "bookman")))
	  (if (url-exists-in-tex? "rpzcmi.tfm")
	      ("Chancery" (make-with "font" "chancery")))
	  (if (url-exists-in-tex? "rpcrr.tfm")
	      ("Courier" (make-with "font" "courier")))
	  (if (url-exists-in-tex? "rpzdr.tfm")
	      ("Dingbat" (make-with "font" "dingbat")))
	  (if (url-exists-in-tex? "rphvr.tfm")
	      ("Helvetica" (make-with "font" "helvetica")))
	  (if (url-exists-in-tex? "rpncr.tfm")
	      ("New Century Schoolbook"
	       (make-with "font" "new-century-schoolbook")))
	  (if (url-exists-in-tex? "rpplr.tfm")
	      ("Palatino" (make-with "font" "palatino")))
	  (if (url-exists-in-tex? "rptmr.tfm")
	      ("Times" (make-with "font" "times"))))
      (if (support-tt-fonts?)
	  (if (font-exists-in-tt? "luxirr")
	      (-> "True type"
		  (if (font-exists-in-tt? "luxirr")
		      ("Luxi" (make-with "font" "luxi")))))
	  (if (font-exists-in-tt? "times")
	      (-> "Microsoft"
		  (if (font-exists-in-tt? "andalemo")
		      ("Andalemo" (make-with "font" "ms-andalemo")))
		  (if (font-exists-in-tt? "arial")
		      ("Arial" (make-with "font" "ms-arial")))
		  (if (font-exists-in-tt? "comic")
		      ("Comic" (make-with "font" "ms-comic")))
		  (if (font-exists-in-tt? "cour")
		      ("Courier" (make-with "font" "ms-courier")))
		  (if (font-exists-in-tt? "georgia")
		      ("Georgia" (make-with "font" "ms-georgia")))
		  (if (font-exists-in-tt? "impact")
		      ("Impact" (make-with "font" "ms-impact")))
		  (if (font-exists-in-tt? "lucon")
		      ("Lucida" (make-with "font" "ms-lucida")))
		  (if (font-exists-in-tt? "tahoma")
		      ("Tahoma" (make-with "font" "ms-tahoma")))
		  (if (font-exists-in-tt? "times")
		      ("Times" (make-with "font" "ms-times")))
		  (if (font-exists-in-tt? "trebuc")
		      ("Trebuchet" (make-with "font" "ms-trebuchet")))
		  (if (font-exists-in-tt? "verdana")
		      ("Verdana" (make-with "font" "ms-verdana"))))))
      (-> "Latin"
	  (if (url-exists-in-tex? "callig15.mf")
	      ("Calligraphic" (make-with "font" "calligraphic")))
	  (if (url-exists-in-tex? "capbas.mf")
	      ("Capbas" (make-with "font" "capbas")))
	  (if (url-exists-in-tex? "cdr10.mf")
	      ("Duerer" (make-with "font" "duerer")))
	  (if (url-exists-in-tex? "hscs10.mf")
	      ("Hershey" (make-with "font" "hershey")))
	  (if (url-exists-in-tex? "la14.mf")
	      ("La" (make-with "font" "la")))
	  (if (url-exists-in-tex? "cmfi10.mf")
	      ("Messy" (make-with "font" "messy")))
	  (if (url-exists-in-tex? "ocr10.mf")
	      ("Optical" (make-with "font" "optical")))
	  (if (url-exists-in-tex? "cpcr10.mf")
	      ("Pacioli" (make-with "font" "pacioli")))
	  (if (url-exists-in-tex? "punk20.mf")
	      ("Punk" (make-with "font" "punk")))
	  (if (url-exists-in-tex? "twcal14.mf")
	      ("Tw#Calligraphic" (make-with "font" "twcal")))
	  (if (url-exists-in-tex? "va14.mf")
	      ("Va" (make-with "font" "va"))))
      (-> "Gothic"
	  (if (url-exists-in-tex? "blackletter.mf")
	      ("Blackletter" (make-with "font" "blackletter")))
	  (if (url-exists-in-tex? "eufm10.mf")
	      ("Euler" (make-with "font" "Euler")))
	  (if (url-exists-in-tex? "ygoth.mf")
	      ("Gothic" (make-with "font" "gothic")))
	  (if (url-exists-in-tex? "hge.mf")
	      ("Old English" (make-with "font" "old-english")))
	  (if (url-exists-in-tex? "schwell.mf")
	      ("Schwell" (make-with "font" "schwell")))
	  (if (url-exists-in-tex? "suet14.mf")
	      ("Suet" (make-with "font" "suet")))
	  (if (url-exists-in-tex? "yswab.mf")
	      ("Swab" (make-with "font" "swab"))))
      (-> "Foreign"
	  (if (url-exists-in-tex? "nash14.mf")
	      ("Arab" (make-with "font" "arab")))
	  (if (url-exists-in-tex? "artmr10.mf")
	      ("Armenian" (make-with "font" "armenian")))
	  ("Cyrillic" (make-with "font" "cyrillic"))
	  (if (url-exists-in-tex? "dvng10.mf")
	      ("Devangari" (make-with "font" "devangari")))
	  (if (url-exists-in-tex? "mxed.mf")
	      (-> "Georgian"
		  ("Mxedruli" (make-with "font" "mxedruli"))
		  ("Xucuri" (make-with "font" "xucuri"))))
	  (if (url-exists-in-tex? "grmn.mf")
	      ("Greek" (make-with "font" "greek")))
	  (if (url-exists-in-tex? "redis.mf")
	      ("Hebrew" (make-with "font" "hebrew")))
	  (if (url-exists-in-tex? "imr10.mf")
	      ("Icelandic" (make-with "font" "icelandic")))
	  (if (url-exists-in-tex? "eiadr10.mf")
	      ("Irish" (make-with "font" "irish")))
	  (if (url-exists-in-tex? "osmanian.mf")
	      ("Osmanian" (make-with "font" "osmanian")))
	  (if (url-exists-in-tex? "wtkr10.mf")
	      ("Turkish" (make-with "font" "turkish")))
	  (if (url-exists-in-tex? "wntml10.mf")
	      ("Tamil" (make-with "font" "tamil")))
	  (if (url-exists-in-tex? "thairz10.mf")
	      ("Thai" (make-with "font" "thai")))
	  (if (url-exists-in-tex? "vmr10.mf")
	      ("Vietnamese" (make-with "font" "vietnamese"))))
      (if (url-exists-in-tex? "givbc10.mf")
	  (-> "Archaic"
	      (if (url-exists-in-tex? "bard.mf")
		  ("Bard" (make-with "font" "bard")))
	      (if (url-exists-in-tex? "cypr10.mf")
		  ("Cypriot" (make-with "font" "cypriot")))
	      (if (url-exists-in-tex? "etr10.mf")
		  ("Etruscan" (make-with "font" "etruscan")))
	      (if (url-exists-in-tex? "givbc10.mf")
		  (-> "Greek"
		      ("4tH Century BC" (make-with "font" "greek4cbc"))
		      ("6th Century BC" (make-with "font" "greek6cbc"))))
	      (if (url-exists-in-tex? "linb10.mf")
		  ("Linear Beta" (make-with "font" "linearb")))
	      (if (url-exists-in-tex? "ogham.mf")
		  ("Ogham" (make-with "font" "ogham")))
	      (if (url-exists-in-tex? "phnc10.mf")
		  ("Phoenician" (make-with "font" "phoenician")))
	      (if (url-exists-in-tex? "fut10.mf")
		  (-> "Runic"
		      ("Default" (make-with "font" "runic"))
		      ("Futhark" (make-with "font" "runic*"))
		      ("Futhork" (make-with "font" "runic**"))))
	      ;; ("south arabian" (make-with "font" "southarabian"))
	      ;; ("syriac" (make-with "font" "syriac"))
	      (if (url-exists-in-tex? "izhitsa.mf")
	      ("Old Slavonic" (make-with "font" "old-slavonic")))
	      (if (url-exists-in-tex? "ugaritic.mf")
		  ("Ugaritic" (make-with "font" "ugaritic")))))
      (if (url-exists-in-tex? "cherokee.mf")
	  (-> "Phantasy"
	      (if (url-exists-in-tex? "cherokee.mf")
		  ("Cherokee" (make-with "font" "cherokee")))
	      (if (url-exists-in-tex? "shavian.mf")
		  ("Shavian" (make-with "font" "shavian")))
	      (if (url-exists-in-tex? "tengwar.mf")
		  ("Tengwar" (make-with "font" "tengwar")))))
      (if (url-exists-in-tex? "bbding10.mf")
	  (-> "Miscellaneous"
	      (if (url-exists-in-tex? "dancers.mf")
		  ("Dancers" (make-with "font" "dancers")))
	      (if (url-exists-in-tex? "bbding10.mf")
		  ("Dingbat" (make-with "font" "bbding")))
	      (if (url-exists-in-tex? "go10.mf")
		  ("Go" (make-with "font" "go")))
	      (if (url-exists-in-tex? "iching.mf")
		  ("Iching" (make-with "font" "iching")))
	      (if (url-exists-in-tex? "karta15.mf")
		  ("Karta" (make-with "font" "karta")))
	      (if (url-exists-in-tex? "klinz.mf")
		  ("Klinz" (make-with "font" "klinz")))
	      (if (url-exists-in-tex? "magic.mf")
		  ("Magic" (make-with "font" "magic")))
	      (if (url-exists-in-tex? "cmph10.mf")
		  ("Phonetic" (make-with "font" "phonetic")))
	      (if (url-exists-in-tex? "tsipa10.mf")
		  ("Tsipa" (make-with "font" "tsipa")))
	      (if (url-exists-in-tex? "wsuipa10.mf")
		  ("Wsuipa" (make-with "font" "wsuipa")))))
      (-> "X-windows"
	  ("Times" (make-with "font" "x-times"))
	  ("Courier" (make-with "font" "x-courier"))
	  ("Helvetica" (make-with "font" "x-helvetica"))
	  ("Utopia" (make-with "font" "x-utopia"))
	  ("Lucida" (make-with "font" "x-lucida"))))
  (-> "Variant"
      ("Roman" (make-with "font-family" "rm"))
      ("Typewriter" (make-with "font-family" "tt"))
      ("Sans serif" (make-with "font-family" "ss")))
  (-> "Series"
      ("Light" (make-with "font-series" "light"))
      ("Medium" (make-with "font-series" "medium"))
      ("Bold" (make-with "font-series" "bold")))
  (-> "Shape"
      ("Upright" (make-with "font-shape" "right"))
      ("Slanted" (make-with "font-shape" "slanted"))
      ("Italic" (make-with "font-shape" "italic"))
      ("Left slanted" (make-with "font-shape" "left-slanted"))
      ---
      ("Small caps" (make-with "font-shape" "small-caps"))
      ("Proportional" (make-with "font-shape" "proportional"))
      ("Condensed" (make-with "font-shape" "condensed"))
      ("Flat" (make-with "font-shape" "flat"))
      ("Long" (make-with "font-shape" "long")))
  (-> "Size" (link font-size-menu)))

(menu-bind font-size-menu
  ("5" (make-with "font-base-size" "5"))
  ("6" (make-with "font-base-size" "6"))
  ("7" (make-with "font-base-size" "7"))
  ("8" (make-with "font-base-size" "8"))
  ("9" (make-with "font-base-size" "9"))
  ("10" (make-with "font-base-size" "10"))
  ("11" (make-with "font-base-size" "11"))
  ("12" (make-with "font-base-size" "12"))
  ("14" (make-with "font-base-size" "14"))
  ("17" (make-with "font-base-size" "17"))
  ("20" (make-with "font-base-size" "20"))
  ("24" (make-with "font-base-size" "24"))
  ---
  ("Other" ... (interactive '("Font size:") 'make-with-font-base-size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Font submenu in math mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-font-menu
  (-> "Name"
      ("Roman" (make-with "math-font" "roman"))
      (if (url-exists-in-tex? "ccr10.mf")
	  ("Concrete" (make-with "math-font" "concrete")))
      (if (url-exists-in-tex? "eurm10.mf")
	  ("New Roman" "E-C-E" (make-with "math-font" "ENR")))
      ---
      (if (url-exists-in-tex? "rpsyr.tfm")
	  ("Adobe" (make-with "math-font" "adobe")))
      (if (url-exists-in-tex? "cdr10.mf")
	  ("Duerer" (make-with "math-font" "Duerer")))
      (if (url-exists-in-tex? "eufm10.mf")
	  ("Euler" (make-with "math-font" "Euler")))
      (-> "Calligraphic"
	  ("Default" (make-with "math-font" "cal"))
	  (if (url-exists-in-tex? "euxm10.mf")
	      ("Euler" (make-with "math-font" "cal**")))
	  (if (url-exists-in-tex? "rsfs10.mf")
	      ("Ralph Smith's" (make-with "math-font" "cal*"))))
      (-> "Blackboard bold"
	  ("Default" (make-with "math-font" "Bbb*"))
	  (if (url-exists-in-tex? "msbm10.mf")
	      ("A.M.S." (make-with "math-font" "Bbb")))
	  (if (url-exists-in-tex? "bbold10.mf")
	      ("Blackboard bold" (make-with "math-font" "Bbb**")))
	  (if (url-exists-in-tex? "ocmr10.mf")
	      ("Outlined roman" (make-with "math-font" "Bbb***")))
	  (if (url-exists-in-tex? "dsrom10.mf")
	      ("Double stroke" (make-with "math-font" "Bbb****")))))
  (if (real-math-font? (get-env "math-font"))
      (-> "Variant"
	  ("Roman" (make-with "math-font-family" "mr"))
	  ("Typewriter" (make-with "math-font-family" "mt"))
	  ("Sans serif" (make-with "math-font-family" "ms"))
	  ---
	  (-> "Text font"
	      ("Roman" (make-with "math-font-family" "rm"))
	      ("Typewriter" (make-with "math-font-family" "tt"))
	      ("Sans serif" (make-with "math-font-family" "ss"))
	      ("Bold" (make-with "math-font-family" "bf"))
	      ("Right" (make-with "math-font-family" "up"))
	      ("Slanted" (make-with "math-font-family" "sl"))
	      ("Italic" (make-with "math-font-family" "it"))))
      (if (real-math-family? (get-env "math-font-family"))
	  (-> "Series"
	      ("Light" (make-with "math-font-series" "light"))
	      ("Medium" (make-with "math-font-series" "medium"))
	      ("Bold" (make-with "math-font-series" "bold")))))
  (if (not (real-math-font? (get-env "math-font")))
      (-> "Variant"
	  ("Roman" (make-with "math-font-family" "mr"))
	  ("Typewriter" (make-with "math-font-family" "mt"))
	  ("Sans serif" (make-with "math-font-family" "ms")))
      (-> "Series"
	  ("Medium" (make-with "math-font-series" "medium"))
	  ("Bold" (make-with "math-font-series" "bold")))
      (-> "Shape"
	  ("Default" (make-with "math-font-shape" "normal"))
	  ("Right" (make-with "math-font-shape" "right"))
	  ("Slanted" (make-with "math-font-shape" "slanted"))
	  ("Italic" (make-with "math-font-shape" "italic"))))
  (-> "Size" (link font-size-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Font submenu in prog mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind prog-font-menu
  (-> "Name"
      ("roman" (make-with "prog-font" "roman"))
      (if (url-exists-in-tex? "ccr10.mf")
	  ("concrete" (make-with "prog-font" "concrete")))
      (if (url-exists-in-tex? "pnr10.mf")
	  ("pandora" (make-with "prog-font" "pandora"))))
  (-> "Variant"
      ("Roman" (make-with "prog-font-family" "rm"))
      ("Typewriter" (make-with "prog-font-family" "tt"))
      ("Sans serif" (make-with "prog-font-family" "ss")))
  (-> "Series"
      ("Medium" (make-with "prog-font-series" "medium"))
      ("Bold" (make-with "prog-font-series" "bold")))
  (-> "Shape"
      ("Default" (make-with "prog-font-shape" "normal"))
      ("Right" (make-with "prog-font-shape" "right"))
      ("Slanted" (make-with "prog-font-shape" "slanted"))
      ("Italic" (make-with "prog-font-shape" "italic")))
  (-> "Size" (link font-size-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Color and Language submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind color-menu
  ("Black" (make-with "color" "black"))
  ("White" (make-with "color" "white"))
  ("Grey" (make-with "color" "grey"))
  ("Red" (make-with "color" "red"))
  ("Blue" (make-with "color" "blue"))
  ("Yellow" (make-with "color" "yellow"))
  ("Green" (make-with "color" "green"))
  ("Orange" (make-with "color" "orange"))
  ("Magenta" (make-with "color" "magenta"))
  ("Brown" (make-with "color" "brown"))
  ("Pink" (make-with "color" "pink"))
  ---
  ("Other" ... (interactive '("Color:") 'make-with-color)))

(menu-bind language-menu
  ("British" (make-with "language" "british"))
  ("Czech" (make-with "language" "czech"))
  ("Danish" (make-with "language" "danish"))
  ("Dutch" (make-with "language" "dutch"))
  ("English" (make-with "language" "english"))
  ("Finnish" (make-with "language" "finnish"))
  ("French" (make-with "language" "french"))
  ("German" (make-with "language" "german"))
  ("Hungarian" (make-with "language" "hungarian"))
  ("Italian" (make-with "language" "italian"))
  ("Polish" (make-with "language" "polish"))
  ("Portuguese" (make-with "language" "portuguese"))
  ("Romanian" (make-with "language" "romanian"))
  ("Russian"
   (begin
     (make-with "language" "russian")
     (make-with "font" "cyrillic")))
  ("Slovene" (make-with "language" "slovene"))
  ("Spanish" (make-with "language" "spanish"))
  ("Swedish" (make-with "language" "swedish"))
  ("Ukrainian"
   (begin
     (make-with "language" "ukrainian")
     (make-with "font" "cyrillic"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra formatting submenus for math mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind math-format-menu
  (-> "Index level"
      ("Normal" (make-with "math-level" "0"))
      ("Script size" (make-with "math-level" "1"))
      ("Script script size" (make-with "math-level" "2")))
  (-> "Display style"
      ("On" (make-with "math-display" "true"))
      ("Off" (make-with "math-display" "false")))
  (-> "Condensed"
      ("On" (make-with "math-condensed" "true"))
      ("Off" (make-with "math-condensed" "false"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting of paragraphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind paragraph-menu
  (-> "Alignment"
      ("Justified" (make-line-with "par-mode" "justify"))
      ("Left ragged" (make-line-with "par-mode" "left"))
      ("Centered" (make-line-with "par-mode" "center"))
      ("Right ragged" (make-line-with "par-mode" "right")))
  (-> "Margins"
      ("Left margin" ... (interactive '("Left margin:") 'set-left-margin))
      ("Right margin" ... (interactive '("Right margin:") 'set-right-margin))
      ("First indentation" ...
       (interactive '("First indentation:") 'set-first-indent)))
  (-> "Spacing"
      ("Interline separation" ...
       (interactive '("Separation between lines:") 'set-interline))
      ("Interline space" ...
       (interactive '("Space between lines:") 'set-interline-spc))
      ("Interparagraph space" ...
       (interactive '("Space between paragraphs:") 'set-interpar-spc)))
  (-> "Hyphenation"
      ("Normal" (make-line-with "par-hyphen" "normal"))
      ("Professional"
       (make-line-with "par-hyphen" "professional")))
  (-> "Number of columns"
      ("1" (make-line-with "par-columns" "1"))
      ("2" (make-line-with "par-columns" "2"))
      ("3" (make-line-with "par-columns" "3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Format menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind format-menu
  (group "Font")
  (if (or (in-text?) (in-source?)) (link text-font-menu))
  (if (in-math?) (link math-font-menu))
  (if (in-prog?) (link prog-font-menu))
  ---
  (group "Content")
  (-> "Color" (link color-menu))
  (-> "Language" (link language-menu))
  (if (in-math?)
      ---
      (link math-format-menu))
  ---
  (group "Paragraph")
  (link paragraph-menu))
