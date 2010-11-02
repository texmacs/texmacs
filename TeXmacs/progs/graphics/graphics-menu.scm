
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-menu.scm
;; DESCRIPTION : menus for graphics mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-menu)
  (:use (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: provide automatic checkmarks for these actions

(menu-bind graphics-geometry-menu
  (-> "Extents"
      ("Default" (graphics-remove-property "gr-geometry"))
      ---
      ("Width" (interactive graphics-set-width))
      ("Height" (interactive graphics-set-height))
      (-> "Alignment"
	  ("Top" (graphics-set-geo-valign "top"))
	  ("Center" (graphics-set-geo-valign "center"))
	  ("Bottom" (graphics-set-geo-valign "bottom"))))
  (-> "Frame"
      ("Default" (graphics-remove-property "gr-frame"))
      ---
      (group "Cartesian")
      (-> "Unit"
	  ("1 cm" (graphics-set-unit "1cm"))
	  ("1 inch" (graphics-set-unit "1in"))
	  ---
	  ("Other" (interactive graphics-set-unit)))
      (-> "Origin"
	  ("Centered" (graphics-set-origin "0.5gw" "0.5gh"))
	  ("Baseline" (graphics-set-origin "0cm" "0.5gh"))
	  ("Axis" (graphics-set-origin "0cm" (length-add "0.5gh" "1yfrac")))
	  ---
	  ("Other" (interactive graphics-set-origin)))))

(menu-bind graphics-grids-menu
  ("Default" (graphics-reset-grids))
  ---
  (-> "Visual grid"
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
	    ("Other"        (graphics-interactive-set-grid-center #t)))
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
	    ("Other"        (graphics-interactive-set-grid-step #t))))
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
	    ("Other"        (graphics-interactive-set-grid-astep #t))))
      (when (== (graphics-get-grid-type #t) 'logarithmic)
	(-> "Logarithmic base"
	    ("Default"      (graphics-set-grid-base "10" #t))
	    ---
	    ("6"            (graphics-set-grid-base "6" #t))
	    ("8"            (graphics-set-grid-base "8" #t))
	    ("10"           (graphics-set-grid-base "10" #t))
	    ("16"           (graphics-set-grid-base "16" #t))
	    ---
	    ("Other"        (graphics-interactive-set-grid-base #t))))
      ---
      (group "Aspect")
      (when (!= (graphics-get-grid-type #t) 'empty)
	(-> "Color of the axes" (link grid-color-axes-menu))
	(-> "Color of the units" (link grid-color-units-menu))
	("Show subunits" (grid-toggle-show-subunits))
	(when (grid-show-subunits?)
	  (-> "Color of the subunits" (link grid-color-subunits-menu))
	  (when (or (== (graphics-get-grid-type #t) 'cartesian)
		    (== (graphics-get-grid-type #t) 'polar))
	    (-> "Number of subunit steps"
		("Default" (graphics-set-grid-aspect 'detailed #f #t))
		---
		("2" (graphics-set-grid-aspect 'detailed 2 #t))
		("3" (graphics-set-grid-aspect 'detailed 3 #t))
		("4" (graphics-set-grid-aspect 'detailed 4 #t))
		("5" (graphics-set-grid-aspect 'detailed 5 #t))
		("6" (graphics-set-grid-aspect 'detailed 6 #t))
		("8" (graphics-set-grid-aspect 'detailed 8 #t))
		("10" (graphics-set-grid-aspect 'detailed 10 #t))
		---
		("Other" (graphics-interactive-set-grid-nsubds #t)))))))
  (-> "Edit grid"
      ("As visual grid"  (grid-toggle-as-visual-grid))
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
	    ("Other"        (graphics-interactive-set-grid-center #f)))
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
	    ("Other"        (graphics-interactive-set-grid-step #f))))
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
	    ("Other"        (graphics-interactive-set-grid-astep #f))))
      (when (== (graphics-get-grid-type #f) 'logarithmic)
	(-> "Logarithmic base"
	    ("Default"      (graphics-set-grid-base "10" #f))
	    ---
	    ("6"            (graphics-set-grid-base "6" #f))
	    ("8"            (graphics-set-grid-base "8" #f))
	    ("10"           (graphics-set-grid-base "10" #f))
	    ("16"           (graphics-set-grid-base "16" #f))
	    ---
	    ("Other"        (graphics-interactive-set-grid-base #f))))
      (when (or (== (graphics-get-grid-type #f) 'cartesian)
		(== (graphics-get-grid-type #f) 'polar)
	)
	(-> "Number of subunit steps"
	    ("Default" (graphics-set-grid-aspect 'detailed #f #f))
	    ---
	    ("2" (graphics-set-grid-aspect 'detailed 2 #f))
	    ("3" (graphics-set-grid-aspect 'detailed 3 #f))
	    ("4" (graphics-set-grid-aspect 'detailed 4 #f))
	    ("5" (graphics-set-grid-aspect 'detailed 5 #f))
	    ("6" (graphics-set-grid-aspect 'detailed 6 #f))
	    ("8" (graphics-set-grid-aspect 'detailed 8 #f))
	    ("10" (graphics-set-grid-aspect 'detailed 10 #f))
	    ---
	    ("Other" (graphics-interactive-set-grid-nsubds #f))))))

(menu-bind graphics-mode-menu
  ("Point" (graphics-set-mode "point"))
  ("Line" (graphics-set-mode "line"))
  ("Polygon" (graphics-set-mode "cline"))
  ("Spline" (graphics-set-mode "spline"))
  ("Closed spline" (graphics-set-mode "cspline"))
  ("Arc" (graphics-set-mode "arc"))
  ("Circle" (graphics-set-mode "carc"))
  ("Text box" (graphics-set-mode "text-at"))
  ---
  ("Move" (graphics-set-mode '(group-edit move)))
  ("Zoom/unzoom" (graphics-set-mode '(group-edit zoom)))
  ("Rotate" (graphics-set-mode '(group-edit rotate)))
  ("Group/ungroup" (graphics-set-mode '(group-edit group-ungroup)))
  ---
  ("Properties" (graphics-set-mode '(group-edit props))))

(menu-bind graphics-color-menu
  ("Default" (graphics-set-color "default"))
  ---
  ("None" (graphics-set-color "none"))
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
  ("Other" (interactive graphics-set-color)))

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
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'axes x)) "Color")))

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
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'units x)) "Color")))

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
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'subunits x)) "Color")))

(menu-bind graphics-point-style-menu
  ("Default" (graphics-set-point-style "default"))
  ---
  ("Disk" (graphics-set-point-style "disk"))
  ("Round" (graphics-set-point-style "round"))
  ("Square" (graphics-set-point-style "square")))

(menu-bind graphics-line-width-menu
  ("Default" (graphics-set-line-width "default"))
  ---
  ("0.5 ln" (graphics-set-line-width "0.5ln"))
  ("1 ln" (graphics-set-line-width "1ln"))
  ("2 ln" (graphics-set-line-width "2ln"))
  ---
  ("Other" (interactive graphics-set-line-width)))

(menu-bind graphics-dash-menu
  (-> "Style"
      ("None" (graphics-set-dash-style "default"))
      ---
      ("- - - - - - - - -"    (graphics-set-dash-style "10"))
      ("----  ----  ----  --" (graphics-set-dash-style "11100"))
      ("---- - ---- - ---- -" (graphics-set-dash-style "1111010"))
      ---
      ("Other" (interactive graphics-set-dash-style)))
  (-> "Unit"
      ("Default" (graphics-set-dash-style-unit "default"))
      ---
      ("2 ln" (graphics-set-dash-style-unit "2ln"))
      ("5 ln" (graphics-set-dash-style-unit "5ln"))
      ("10 ln" (graphics-set-dash-style-unit "10ln"))
      ---
      ("Other" (interactive graphics-set-dash-style-unit))))

(menu-bind graphics-line-arrows-menu
  ("None" (graphics-set-line-arrows 0))
  ---
  (" ------------------>" (graphics-set-line-arrows 1))
  ("<---------------->" (graphics-set-line-arrows 2)))

(menu-bind graphics-fill-color-menu
  ("Default" (graphics-set-fill-color "default"))
  ---
  ("None" (graphics-set-fill-color "none"))
  ("Black" (graphics-set-fill-color "black"))
  ("White" (graphics-set-fill-color "white"))
  ("Grey" (graphics-set-fill-color "grey"))
  ("Red" (graphics-set-fill-color "red"))
  ("Blue" (graphics-set-fill-color "blue"))
  ("Yellow" (graphics-set-fill-color "yellow"))
  ("Green" (graphics-set-fill-color "green"))
  ("Orange" (graphics-set-fill-color "orange"))
  ("Magenta" (graphics-set-fill-color "magenta"))
  ("Brown" (graphics-set-fill-color "brown"))
  ("Pink" (graphics-set-fill-color "pink"))
  ---
  ("Other" (interactive graphics-set-fill-color)))

(menu-bind graphics-text-align-menu
  ("Default" (begin (graphics-set-textat-halign "default")
		    (graphics-set-textat-valign "default")))
  ---
  (-> "Horizontal"
      ("Default" (graphics-set-textat-halign "default"))
      ---
      ("Left" (graphics-set-textat-halign "left"))
      ("Center" (graphics-set-textat-halign "center"))
      ("Right" (graphics-set-textat-halign "right")))
  (-> "Vertical"
      ("Default" (graphics-set-textat-valign "default"))
      ---
      ("Bottom" (graphics-set-textat-valign "bottom"))
      ("Base" (graphics-set-textat-valign "base"))
      ("Center" (graphics-set-textat-valign "center"))
      ("Top" (graphics-set-textat-valign "top"))))

(menu-bind graphics-enable-change-properties-menu
  ("Color"  (graphics-toggle-color-enabled))
  ("Point style" (graphics-toggle-point-style-enabled))
  ("Line width" (graphics-toggle-line-width-enabled))
  ("Dash style" (graphics-toggle-dash-style-enabled))
  ("Dash unit" (graphics-toggle-dash-style-unit-enabled))
  ("Line arrows" (graphics-toggle-line-arrows-enabled))
  ("Fill color" (graphics-toggle-fill-color-enabled))
  ("Text box horizontal alignment" (graphics-toggle-textat-halign-enabled))
  ("Text box vertical alignment" (graphics-toggle-textat-valign-enabled)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind graphics-menu
  (-> "Geometry" (link graphics-geometry-menu))
  (-> "Grids" (link graphics-grids-menu))
  (-> "Mode" (link graphics-mode-menu))
  (-> "Color" (link graphics-color-menu))
  (-> "Point style" (link graphics-point-style-menu))
  (-> "Fill color" (link graphics-fill-color-menu))
  (-> "Line properties"
      (-> "Width" (link graphics-line-width-menu))
      (-> "Dashes" (link graphics-dash-menu))
      (-> "Arrows" (link graphics-line-arrows-menu)))
 ;(-> "Fill"
 ;    (-> "Fill mode" ...)
 ;    (-> "Fill color" ...))
  (-> "Text box alignment" (link graphics-text-align-menu))
  (-> "Enable change" (link graphics-enable-change-properties-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind graphics-icons
  (=> (balloon (icon "tm_graphics_geometry.xpm") "Graphics geometry")
      (link graphics-geometry-menu))
  (=> (balloon (icon "tm_graphics_grid.xpm") "Graphics grids")
      (link graphics-grids-menu))
  |
  ;(=> (balloon (icon "tm_cell_special.xpm") "Graphical mode")
  ;    (link graphics-mode-menu))
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
  (   (balloon (icon "tm_carc_mode.xpm") "Add circles")
      (graphics-set-mode "carc"))
  (   (balloon (icon "tm_textat_mode.xpm") "Add text boxes")
      (graphics-set-mode "text-at"))
  |
  (=> (balloon (icon "tm_point_style.xpm") "Point style")
      (link graphics-point-style-menu))
  (=> (balloon (icon "tm_line_width.xpm") "Line width")
      (link graphics-line-width-menu))
  (=> (balloon (icon "tm_line_style.xpm") "Dashes")
      (link graphics-dash-menu))
  (=> (balloon (icon "tm_line_arrows.xpm") "Line arrows")
      (link graphics-line-arrows-menu))
  (=> (balloon (icon "tm_gr_color.xpm") "Line color")
      (link graphics-color-menu))
  (=> (balloon (icon "tm_fill.xpm") "Fill color")
      (link graphics-fill-color-menu))
  (=> (balloon (icon "tm_text_align.xpm") "Text box alignment")
      (link graphics-text-align-menu))
  ;;(=> (balloon (icon "tm_toggle_change_props.xpm") "Enable change property")
  ;;    (link graphics-enable-change-properties-menu))
  |
  (   (balloon (icon "tm_edit_props.xpm") "Change objects properties")
      (graphics-set-mode '(group-edit props)))
  (   (balloon (icon "tm_group_move.xpm") "Move objects")
      (graphics-set-mode '(group-edit move)))
  (   (balloon (icon "tm_group_zoom.xpm") "Zoom/unzoom objects")
      (graphics-set-mode '(group-edit zoom)))
  (   (balloon (icon "tm_group_rotate.xpm") "Rotate objects")
      (graphics-set-mode '(group-edit rotate)))
  (   (balloon (icon "tm_group_group.xpm") "Group/ungroup objects")
      (graphics-set-mode '(group-edit group-ungroup)))
 ;(   (balloon (icon "tm_group_group.xpm") "Group objects")
 ;    (group-selected-objects))
 ;(   (balloon (icon "tm_group_ungroup.xpm") "Ungroup objects")
 ;    (ungroup-current-object))
)
