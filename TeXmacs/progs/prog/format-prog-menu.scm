
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-prog-menu.scm
;; DESCRIPTION : local text formatting properties in prog mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog format-prog-menu)
  (:use (generic format-edit)))

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
;; The main Format menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind full-prog-format-menu
  (group "Font")
  (if (new-fonts?)
      ;;(link new-prog-font-menu))
      (link prog-font-menu))
  (if (not (new-fonts?))
      (link prog-font-menu))
  (if (simple-menus?)
      (-> "Color" (link color-menu)))
  (if (detailed-menus?)
      ---
      (group "Text")
      (link textual-properties-menu))
  ---
  (group "Paragraph")
  (link paragraph-menu)
  ---
  (group "Page")
  (link page-menu))

(menu-bind compressed-prog-format-menu
  (if (new-fonts?)
      ("Font" (interactive open-font-selector)))
  (if (not (new-fonts?))
      (-> "Font" (link text-font-menu)))
  (-> "Text" (link new-textual-properties-menu))
  ("Paragraph" (open-paragraph-format))
  (-> "Page" (link new-page-menu))
  ---
  (-> "Whitespace" (link space-menu))
  (-> "Break" (link break-menu))
  (-> "Transform" (link transform-menu))
  (-> "Specific" (link specific-menu)))

(menu-bind prog-format-menu
  (if (use-menus?)
      (link full-prog-format-menu))
  (if (use-popups?)
      (link compressed-prog-format-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind prog-format-icons
  ((balloon (icon "tm_italic.xpm") "Write italic text")
   (make-with "prog-font-shape" "italic"))
  ((balloon (icon "tm_bold.xpm") "Write bold text")
   (make-with "prog-font-series" "bold"))
  ((balloon (icon "tm_sansserif.xpm") "Use a sans serif font")
   (make-with "prog-font-family" "ss"))
  (if (not (in-graphics?))
      (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
	  (link color-menu))))
