
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-text-menu.scm
;; DESCRIPTION : menus for setting local formatting properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text format-text-menu)
  (:use (text format-text-edit)
	(fonts font-old-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Format menu in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind full-text-format-menu
  (group "Font")
  (if (new-fonts?)
      ;;(link new-text-font-menu))
      (link text-font-menu))
  (if (not (new-fonts?))
      (link text-font-menu))
  (if (simple-menus?)
      (-> "Color" (link color-menu)))
  (if (detailed-menus?)
      ---
      (group "Text")
      (link text-properties-menu))
  ---
  (group "Paragraph")
  (link paragraph-menu)
  ---
  (group "Page")
  (link page-menu))

(menu-bind compressed-text-format-menu
  (if (new-fonts?)
      ("Font" (interactive open-font-selector)))
  (if (not (new-fonts?))
      (-> "Font" (link text-font-menu)))
  ("Paragraph" (open-paragraph-format))
  ("Page" (open-page-format))
  ---
  (-> "Whitespace" (link space-menu))
  (-> "Indentation" (link indentation-menu))
  (-> "Break" (link break-menu))
  ---
  (-> "Color"
      (if (== (get-preference "experimental alpha") "on")
	  (-> "Opacity" (link opacity-menu))
	  ---)
      (link color-menu))
  (-> "Adjust" (link adjust-menu))
  (-> "Specific" (link specific-menu))
  (-> "Special" (link format-special-menu)))

(menu-bind text-format-menu
  (if (use-menus?)
      (link full-text-format-menu))
  (if (use-popups?)
      (link compressed-text-format-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-format-icons
  (if (not (style-has? "std-markup-dtd"))
      (=> (balloon (icon "tm_parstyle.xpm") "Set paragraph mode")
          ((balloon (icon "tm_left.xpm") "Align text to the left")
           (make-line-with "par-mode" "left"))
          ((balloon (icon "tm_center.xpm") "Center text")
           (make-line-with "par-mode" "center"))
          ((balloon (icon "tm_right.xpm") "Align text to the right")
           (make-line-with "par-mode" "right"))
          ((balloon (icon "tm_justify.xpm") "Justify text")
           (make-line-with "par-mode" "justify")))
      (=> (balloon (icon "tm_parindent.xpm") "Set paragraph margins")
          ("Left margin" (make-interactive-line-with "par-left"))
          ("Right margin" (make-interactive-line-with "par-right"))
          ("First indentation" (make-interactive-line-with "par-first")))
      /)
  (if (and (style-has? "std-markup-dtd") (not (in-source?)))
      ;;((balloon
      ;;(text (roman rm bold right 12 600) "S")
      ;;"Write bold text")
      ;;(make-with "font-series" "bold"))
      ((balloon (icon "tm_emphasize.xpm") "Emphasize text")
       (make 'em))
      ((balloon (icon "tm_strong.xpm") "Write strong text")
       (make 'strong))
      ((balloon (icon "tm_verbatim.xpm") "Write verbatim text")
       (make 'verbatim))
      ((balloon (icon "tm_sansserif.xpm") "Write sample text")
       (make 'samp))
      ((balloon (icon "tm_name.xpm") "Write a name")
       (make 'name)))
  (if (or (not (style-has? "std-markup-dtd")) (in-source?))
      ((balloon (icon "tm_italic.xpm") "Write italic text")
       (make-with "font-shape" "italic"))
      ((balloon (icon "tm_bold.xpm") "Write bold text")
       (make-with "font-series" "bold"))
      ((balloon (icon "tm_typewriter.xpm") "Use a typewriter font")
       (make-with "font-family" "tt"))
      ((balloon (icon "tm_sansserif.xpm") "Use a sans serif font")
       (make-with "font-family" "ss"))
      ((balloon (icon "tm_smallcaps.xpm") "Use small capitals")
       (make-with "font-shape" "small-caps")))
  (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
      (link color-menu)))
