
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-graphics.scm
;; DESCRIPTION : menus for graphics mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-graphics)
  (:use (texmacs edit edit-graphics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: provide automatic checkmarks for these actions

(menu-bind graphics-geometry-menu
  (-> "Frame"
      ("Default" (graphics-remove-property "gr-frame"))
      ---
      (group "Cartesian")
      (-> "Unit"
	  ("cm" (graphics-set-unit "1cm"))
	  ("inch" (graphics-set-unit "1in"))
	  ---
	  ("Other" ... (graphics-set-unit-ia)))
      (-> "Origin"
	  ("Base line" (graphics-set-origin "0cm" "0cm"))
	  ("Fraction height" (graphics-set-origin "0cm" "1yfrac"))
	  ---
	  ("Other"... (graphics-set-origin-ia))))
  (-> "Extents"
      ("Default" (graphics-remove-property "gr-clip"))
      ---
      ;; FIXME: insert methods for setting width, height and centering
      ("Other" ... (graphics-set-extents-ia)))
  (-> "Visual grid"
      ("Default" (graphics-set-visual-grid 'default))
      ---
      (-> "Type"
	  ("No grid"     (graphics-set-visual-grid 'empty))
	  ---
	  ("Cartesian"   (graphics-set-visual-grid 'cartesian))
	      ("Polar"       (graphics-set-visual-grid 'polar))
	  ("Logarithmic" (graphics-set-visual-grid 'logarithmic)))
      (when (!= (graphics-get-grid-type #t) 'empty)
        (-> "Center"
	    ("Default"      (graphics-set-grid-center "0" "0" #t))
	    ---
	    ("Other" ...    (graphics-set-grid-center-ia #t)))
	(-> "Unit length"
	    ("Default"      (graphics-set-grid-step "1" #t))
	    ---
	    ("0.1"          (graphics-set-grid-step "0.1" #t))
	    ("0.2"          (graphics-set-grid-step "0.2" #t))
	    ("0.5"          (graphics-set-grid-step "0.5" #t))
	    ("1"            (graphics-set-grid-step "1" #t))
	    ("2"            (graphics-set-grid-step "2" #t))
	    ("5"            (graphics-set-grid-step "5" #t))
	    ("10"           (graphics-set-grid-step "10" #t))
	    ---
	    ("Other" ...    (graphics-set-grid-step-ia #t))))
      (when (== (graphics-get-grid-type #t) 'polar)
	(-> "Number of polar steps"
	    ("Default"      (graphics-set-grid-astep "24" #t))
	    ---
	    ("4"            (graphics-set-grid-astep "4" #t))
	    ("6"            (graphics-set-grid-astep "6" #t))
	    ("8"            (graphics-set-grid-astep "8" #t))
	    ("12"           (graphics-set-grid-astep "12" #t))
	    ("16"           (graphics-set-grid-astep "16" #t))
	    ("24"           (graphics-set-grid-astep "24" #t))
	    ("30"           (graphics-set-grid-astep "30" #t))
	    ("36"           (graphics-set-grid-astep "36" #t))
	    ---
	    ("Other" ...    (graphics-set-grid-astep-ia #t))))
      (when (== (graphics-get-grid-type #t) 'logarithmic)
	(-> "Logarithmic base"
	    ("Default"      (graphics-set-grid-base "10" #t))
	    ---
	    ("6"            (graphics-set-grid-base "6" #t))
	    ("8"            (graphics-set-grid-base "8" #t))
	    ("10"           (graphics-set-grid-base "10" #t))
	    ("16"           (graphics-set-grid-base "16" #t))
	    ---
	    ("Other" ...    (graphics-set-grid-base-ia #t))))
      ---
      (group "Aspect")
      (when (!= (graphics-get-grid-type #t) 'empty)
	(-> "Color of the axes" (link grid-color-axes-menu))
	(-> "Color of the units" (link grid-color-units-menu))
	("Show subunits" (toggle-preference "show subunits"))
	(when (== (get-preference "show subunits") "on")
	  (-> "Color of the subunits" (link grid-color-subunits-menu))
	  (when (or (== (graphics-get-grid-type #t) 'cartesian)
		    (== (graphics-get-grid-type #t) 'polar))
	    (-> "Number of subunit steps"
		("Default" (graphics-set-grid-aspect 'detailed #f))
		---
		("2" (graphics-set-grid-aspect 'detailed 2))
		("3" (graphics-set-grid-aspect 'detailed 3))
		("4" (graphics-set-grid-aspect 'detailed 4))
		("5" (graphics-set-grid-aspect 'detailed 5))
		("6" (graphics-set-grid-aspect 'detailed 6))
		("8" (graphics-set-grid-aspect 'detailed 8))
		("10" (graphics-set-grid-aspect 'detailed 10))
		---
		("Other" ... (graphics-set-grid-nsubds-ia)))))))
  (-> "Edit grid"
      ("As visual grid"  (toggle-preference "as visual grid"))
      ---
      (-> "Type"
	  ("No grid"     (graphics-set-edit-grid 'empty))
	  ---
	  ("Cartesian"   (graphics-set-edit-grid 'cartesian))
	  ("Polar"       (graphics-set-edit-grid 'polar))
	  ("Logarithmic" (graphics-set-edit-grid 'logarithmic)))
      (when (!= (graphics-get-grid-type #f) 'empty)
	(-> "Center"
	    ("Default"      (graphics-set-grid-center "0" "0" #f))
	    ---
	    ("Other" ...    (graphics-set-grid-center-ia #f)))
	(-> "Unit length"
	    ("Default"      (graphics-set-grid-step "0.1" #f))
	    ---
	    ("0.05"         (graphics-set-grid-step "0.05" #f))
	    ("0.1"          (graphics-set-grid-step "0.1" #f))
	    ("0.2"          (graphics-set-grid-step "0.2" #f))
	    ("0.5"          (graphics-set-grid-step "0.5" #f))
	    ("1"            (graphics-set-grid-step "1" #f))
	    ("2"            (graphics-set-grid-step "2" #f))
	    ("5"            (graphics-set-grid-step "5" #f))
	    ("10"           (graphics-set-grid-step "10" #f))
	    ---
	    ("Other" ...    (graphics-set-grid-step-ia #f))))
      (when (== (graphics-get-grid-type #f) 'polar)
	(-> "Number of polar steps"
	    ("Default"      (graphics-set-grid-astep "24" #f))
	    ---
	    ("4"            (graphics-set-grid-astep "4" #f))
	    ("6"            (graphics-set-grid-astep "6" #f))
	    ("8"            (graphics-set-grid-astep "8" #f))
	    ("12"           (graphics-set-grid-astep "12" #f))
	    ("16"           (graphics-set-grid-astep "16" #f))
	    ("24"           (graphics-set-grid-astep "24" #f))
	    ("30"           (graphics-set-grid-astep "30" #f))
	    ("36"           (graphics-set-grid-astep "36" #f))
	    ("60"           (graphics-set-grid-astep "60" #f))
	    ---
	    ("Other" ...    (graphics-set-grid-astep-ia #f))))
      (when (== (graphics-get-grid-type #f) 'logarithmic)
	(-> "Logarithmic base"
	    ("Default"      (graphics-set-grid-base "10" #f))
	    ---
	    ("6"            (graphics-set-grid-base "6" #f))
	    ("8"            (graphics-set-grid-base "8" #f))
	    ("10"           (graphics-set-grid-base "10" #f))
	    ("16"           (graphics-set-grid-base "16" #f))
	    ---
	    ("Other" ...    (graphics-set-grid-base-ia #f))))))

(menu-bind graphics-mode-menu
  ("Point" (graphics-set-mode "point"))
  ("Line" (graphics-set-mode "line"))
  ("Polygon" (graphics-set-mode "cline"))
  ("Spline" (graphics-set-mode "spline"))
  ("C-Spline" (graphics-set-mode "cspline"))
  ("Text box" (graphics-set-mode "text-at"))
  ---
  ("Properties" (graphics-set-mode '(edit-prop))))

(menu-bind graphics-color-menu
  ("Default" (graphics-set-color "default"))
  ---
  ("Black" (graphics-set-color "black"))
  ("White" (graphics-set-color "white"))
  ("Grey" (graphics-set-color "grey"))
  ("Red" (graphics-set-color "red"))
  ("Blue" (graphics-set-color "blue"))
  ("Yellow" (graphics-set-color "yellow"))
  ("Green" (graphics-set-color "green"))
  ("Orange" (graphics-set-color "orange"))
  ("Magenta" (graphics-set-color "magenta"))
  ("Brown" (graphics-set-color "brown"))
  ("Pink" (graphics-set-color "pink"))
  ---
  ("Other" ... (interactive '("Color:") 'graphics-set-color)))

(menu-bind grid-color-axes-menu
  ("Default" (graphics-set-grid-color 'axes "default"))
  ---
  ("Black" (graphics-set-grid-color 'axes "black"))
  ("White" (graphics-set-grid-color 'axes "white"))
  ("Grey" (graphics-set-grid-color 'axes "grey"))
  ("Red" (graphics-set-grid-color 'axes "red"))
  ("Blue" (graphics-set-grid-color 'axes "blue"))
  ("Yellow" (graphics-set-grid-color 'axes "yellow"))
  ("Green" (graphics-set-grid-color 'axes "green"))
  ("Orange" (graphics-set-grid-color 'axes "orange"))
  ("Magenta" (graphics-set-grid-color 'axes "magenta"))
  ("Brown" (graphics-set-grid-color 'axes "brown"))
  ("Pink" (graphics-set-grid-color 'axes "pink"))
  ---
  ("Other" ... (interactive '("Color:") 'graphics-set-grid-color 'axes)))

(menu-bind grid-color-units-menu
  ("Default" (graphics-set-grid-color 'units "default"))
  ---
  ("Black" (graphics-set-grid-color 'units "black"))
  ("White" (graphics-set-grid-color 'units "white"))
  ("Grey" (graphics-set-grid-color 'units "grey"))
  ("Red" (graphics-set-grid-color 'units "red"))
  ("Blue" (graphics-set-grid-color 'units "blue"))
  ("Yellow" (graphics-set-grid-color 'units "yellow"))
  ("Green" (graphics-set-grid-color 'units "green"))
  ("Orange" (graphics-set-grid-color 'units "orange"))
  ("Magenta" (graphics-set-grid-color 'units "magenta"))
  ("Brown" (graphics-set-grid-color 'units "brown"))
  ("Pink" (graphics-set-grid-color 'units "pink"))
  ---
  ("Other" ... (interactive '("Color:") 'graphics-set-grid-color 'units)))

(menu-bind grid-color-subunits-menu
  ("Default" (graphics-set-grid-color 'subunits "default"))
  ---
  ("Black" (graphics-set-grid-color 'subunits "black"))
  ("White" (graphics-set-grid-color 'subunits "white"))
  ("Grey" (graphics-set-grid-color 'subunits "grey"))
  ("Red" (graphics-set-grid-color 'subunits "red"))
  ("Blue" (graphics-set-grid-color 'subunits "blue"))
  ("Yellow" (graphics-set-grid-color 'subunits "yellow"))
  ("Green" (graphics-set-grid-color 'subunits "green"))
  ("Orange" (graphics-set-grid-color 'subunits "orange"))
  ("Magenta" (graphics-set-grid-color 'subunits "magenta"))
  ("Brown" (graphics-set-grid-color 'subunits "brown"))
  ("Pink" (graphics-set-grid-color 'subunits "pink"))
  ---
  ("Other" ... (interactive '("Color:") 'graphics-set-grid-color 'subunits)))

(menu-bind graphics-line-width-menu
  ("Default" (graphics-set-line-width "default"))
  ---
  ("0.5 ln" (graphics-set-line-width "0.5ln"))
  ("1 ln" (graphics-set-line-width "1ln"))
  ("2 ln" (graphics-set-line-width "2ln"))
  ---
  ("Other" ... (interactive '("Line width:") 'graphics-set-line-width)))

(menu-bind graphics-text-align-menu
  ("Default" (begin (graphics-set-property "gr-text-halign" "left")
		    (graphics-set-property "gr-text-valign" "bottom")))
  ---
  (-> "Horizontal"
      ("Left" (graphics-set-property "gr-text-halign" "left"))
      ("Center" (graphics-set-property "gr-text-halign" "center"))
      ("Right" (graphics-set-property "gr-text-halign" "right")))
  (-> "Vertical"
      ("Bottom" (graphics-set-property "gr-text-valign" "bottom"))
      ("Center" (graphics-set-property "gr-text-valign" "center"))
      ("Top" (graphics-set-property "gr-text-valign" "top"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind graphics-menu
  (-> "Geometry" (link graphics-geometry-menu))
  (-> "Mode" (link graphics-mode-menu))
  (-> "Color" (link graphics-color-menu))
  (-> "Line width" (link graphics-line-width-menu))
  (-> "Text box alignment" (link graphics-text-align-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-graphics-icons
  (=> (balloon (icon "tm_cell_size.xpm") "Graphics geometry")
      (link graphics-geometry-menu))
  ;(=> (balloon (icon "tm_cell_special.xpm") "Graphical mode")
  ;    (link graphics-mode-menu))
  (=> (balloon (icon "tm_color.xpm") "Color of new graphics")
      (link graphics-color-menu))
  (=> (balloon (icon "tm_line_width.xpm") "Line width for new graphics")
      (link graphics-line-width-menu))
  (=> (balloon (icon "tm_text_align.xpm") "Text box alignment")
      (link graphics-text-align-menu))
  |
  (   (balloon (icon "tm_point_mode.xpm") "Add points")
      (graphics-set-mode "point"))
  (   (balloon (icon "tm_line_mode.xpm") "Add lines")
      (graphics-set-mode "line"))
  (   (balloon (icon "tm_cline_mode.xpm") "Add polygons")
      (graphics-set-mode "cline"))
  (   (balloon (icon "tm_spline_mode.xpm") "Add splines")
      (graphics-set-mode "spline"))
  (   (balloon (icon "tm_cspline_mode.xpm") "Add closed splines")
      (graphics-set-mode "cspline"))
  (   (balloon (icon "tm_arc_mode.xpm") "Add arcs")
      (graphics-set-mode "arc"))
  (   (balloon (icon "tm_carc_mode.xpm") "Add closed arcs")
      (graphics-set-mode "carc"))
  (   (balloon (icon "tm_textat_mode.xpm") "Add text boxes")
      (graphics-set-mode "text-at"))
  |
  (   (balloon (icon "tm_edit_props.xpm") "Change objects properties")
      (graphics-set-mode '(edit-prop))))
