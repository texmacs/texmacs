
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
  (-> "Grid"
      ("Default" (begin (graphics-remove-property "gr-grid")
			(graphics-remove-property "gr-edit-grid")
			(graphics-remove-property "gr-grid-aspect")
			(graphics-remove-property "gr-grid-aspect-props")))
      ---
      (-> "Visual grid"
	  ("No grid"     (graphics-set-visual-grid 'empty))
          ---
	  (-> "Type"
	      ("Cartesian"   (graphics-set-visual-grid 'cartesian))
    	      ("Polar"       (graphics-set-visual-grid 'polar))
	      ("Logarithmic" (graphics-set-visual-grid 'logarithmic)))
	  (-> "Center"
	      ("Default"      (graphics-set-grid-center "0" "0" #t))
              ---
	      ("Other" ...    (graphics-set-grid-center-ia #t)))
	  (-> "Unit length"
	      ("Default"      (graphics-set-grid-step "1" #t))
              ---
	      ("0.05"         (graphics-set-grid-step "0.05" #t))
	      ("0.1"          (graphics-set-grid-step "0.1" #t))
	      ("0.2"          (graphics-set-grid-step "0.2" #t))
	      ("0.4"          (graphics-set-grid-step "0.4" #t))
	      ("0.5"          (graphics-set-grid-step "0.5" #t))
	      ("1"            (graphics-set-grid-step "1" #t))
	      ("2"            (graphics-set-grid-step "2" #t))
	      ("3"            (graphics-set-grid-step "3" #t))
	      ("4"            (graphics-set-grid-step "4" #t))
	      ("5"            (graphics-set-grid-step "5" #t))
              ---
	      ("Other" ...    (graphics-set-grid-step-ia #t)))
	  (-> "Nb polar steps"
	      ("Default"      (graphics-set-grid-astep "8" #t))
              ---
	      ("4"            (graphics-set-grid-astep "4" #t))
	      ("8"            (graphics-set-grid-astep "8" #t))
	      ("16"           (graphics-set-grid-astep "16" #t))
	      ("32"           (graphics-set-grid-astep "32" #t))
	      ("64"           (graphics-set-grid-astep "64" #t))
              ---
	      ("Other" ...    (graphics-set-grid-astep-ia #t)))
	  (-> "Logarithmic base"
	      ("6"            (graphics-set-grid-base "6" #t))
	      ("8"            (graphics-set-grid-base "8" #t))
	      ("10"           (graphics-set-grid-base "10" #t))
	      ("16"           (graphics-set-grid-base "16" #t))
              ---
	      ("Other" ...    (graphics-set-grid-base-ia #t)))
	  ---
	  (-> "Aspect"
	      ("Properties" ... (graphics-set-grid-aspect-properties-ia))
	      ---
	      ("Units only" (graphics-set-grid-aspect 'units-only #f))
	      (-> "Detailed"
		  ("2 subds" (graphics-set-grid-aspect 'detailed 2))
		  ("3 subds" (graphics-set-grid-aspect 'detailed 3))
		  ("4 subds" (graphics-set-grid-aspect 'detailed 4))
		  ("5 subds" (graphics-set-grid-aspect 'detailed 5))
                  ---
		  ("Default" (graphics-set-grid-aspect 'detailed #f)))))
      (-> "Edit grid"
	  ("No grid"     (graphics-set-edit-grid 'empty))
          ---
	  (-> "Type"
	      ("Cartesian"   (graphics-set-edit-grid 'cartesian))
	      ("Polar"       (graphics-set-edit-grid 'polar))
	      ("Logarithmic" (graphics-set-edit-grid 'logarithmic)))
	  (-> "Center"
	      ("Default"      (graphics-set-grid-center "0" "0" #f))
              ---
	      ("Other" ...    (graphics-set-grid-center-ia #f)))
	  (-> "Unit length"
	      ("Default"      (graphics-set-grid-step "0.1" #f))
              ---
	      ("0.025"        (graphics-set-grid-step "0.025" #f))
	      ("0.05"         (graphics-set-grid-step "0.05" #f))
	      ("0.1"          (graphics-set-grid-step "0.1" #f))
	      ("0.2"          (graphics-set-grid-step "0.2" #f))
	      ("0.4"          (graphics-set-grid-step "0.4" #f))
	      ("0.5"          (graphics-set-grid-step "0.5" #f))
	      ("1"            (graphics-set-grid-step "1" #f))
	      ("2"            (graphics-set-grid-step "2" #f))
	      ("3"            (graphics-set-grid-step "3" #f))
	      ("4"            (graphics-set-grid-step "4" #f))
	      ("5"            (graphics-set-grid-step "5" #f))
              ---
	      ("Other" ...    (graphics-set-grid-step-ia #f)))
	  (-> "Nb polar steps"
	      ("Default"      (graphics-set-grid-astep "80" #f))
              ---
	      ("8"            (graphics-set-grid-astep "8" #f))
	      ("16"           (graphics-set-grid-astep "16" #f))
	      ("32"           (graphics-set-grid-astep "32" #f))
	      ("40"           (graphics-set-grid-astep "40" #f))
	      ("80"           (graphics-set-grid-astep "80" #f))
	      ("160"          (graphics-set-grid-astep "160" #f))
	      ("320"          (graphics-set-grid-astep "320" #f))
	      ("640"          (graphics-set-grid-astep "640" #f))
              ---
	      ("Other" ...    (graphics-set-grid-astep-ia #f)))
	  (-> "Logarithmic base"
	      ("6"            (graphics-set-grid-base "6" #f))
	      ("8"            (graphics-set-grid-base "8" #f))
	      ("10"           (graphics-set-grid-base "10" #f))
	      ("16"           (graphics-set-grid-base "16" #f))
              ---
	      ("Other" ...    (graphics-set-grid-base-ia #f)))))
  (-> "Extents"
      ("Default" (graphics-remove-property "gr-clip"))
      ---
      ;; FIXME: insert methods for setting width, height and centering
      ("Other" ... (graphics-set-extents-ia))))

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
