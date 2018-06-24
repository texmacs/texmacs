
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
  (:use (graphics graphics-env)
	(graphics graphics-main)
        (graphics graphics-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: provide automatic checkmarks for these actions

(menu-bind graphics-extents-menu
  ("Width" (interactive graphics-set-width))
  ("Height" (interactive graphics-set-height)))

(menu-bind graphics-auto-crop-menu
  ("Crop" (graphics-toggle-auto-crop))
  ---
  (when (graphics-auto-crop?)
    (group "Padding")
    ("none" (graphics-set-crop-padding "0spc"))
    ("1 spc" (graphics-set-crop-padding "1spc"))
    ("1 em" (graphics-set-crop-padding "1em"))
    ---
    ("Other" (interactive graphics-set-crop-padding))))

(menu-bind graphics-alignment-menu
  ("Top" (graphics-set-geo-valign "top"))
  ("Axis" (graphics-set-geo-valign "axis"))
  ("Center" (graphics-set-geo-valign "center"))
  ("Bottom" (graphics-set-geo-valign "bottom")))

(menu-bind graphics-overlap-menu
  ("None" (graphics-set-overlap "0cm"))
  ("1 cm" (graphics-set-overlap "1cm"))
  ("2 cm" (graphics-set-overlap "2cm"))
  ("Full" (graphics-set-overlap "1pag"))
  ---
  ("Other" (interactive graphics-set-overlap)))

(menu-bind graphics-resize-menu
  (group "Width")
  ("Fast decrease" (graphics-decrease-hsize-fast))
  ("Slow decrease" (graphics-decrease-hsize))
  ("Slow increase" (graphics-increase-hsize))
  ("Fast increase" (graphics-increase-hsize-fast))
  ---
  (group "Height")
  ("Fast decrease" (graphics-decrease-vsize-fast))
  ("Slow decrease" (graphics-decrease-vsize))
  ("Slow increase" (graphics-increase-vsize))
  ("Fast increase" (graphics-increase-vsize-fast)))

(menu-bind graphics-frame-unit-menu
  ("1 cm" (graphics-set-unit "1cm"))
  ("1 inch" (graphics-set-unit "1in"))
  ("0.1 par" (graphics-set-unit "0.1par"))
  ;;("5 em" (graphics-set-unit "5em"))
  ---
  ("Other" (interactive graphics-set-unit)))

(menu-bind graphics-frame-origin-menu
  ("Center" (graphics-set-origin "0.5gw" "0.5gh"))
  ("Left top" (graphics-set-origin "0gw" "1gh"))
  ("Left axis" (graphics-set-origin "0gw" (length-add "0.5gh" "1yfrac")))
  ("Left center" (graphics-set-origin "0gw" "0.5gh"))
  ("Left bottom" (graphics-set-origin "0gw" "0gh"))
  ---
  ("Other" (interactive graphics-set-origin)))

(menu-bind graphics-move-menu
  (group "Slow")
  ("Left" (graphics-move-origin-left))
  ("Right" (graphics-move-origin-right))
  ("Down" (graphics-move-origin-down))
  ("Up" (graphics-move-origin-up))
  ---
  (group "Fast")
  ("Left" (graphics-move-origin-left-fast))
  ("Right" (graphics-move-origin-right-fast))
  ("Down" (graphics-move-origin-down-fast))
  ("Up" (graphics-move-origin-up-fast)))

(menu-bind graphics-zoom-menu
  ("Zoom in" (graphics-zoom-in))
  ("Zoom out" (graphics-zoom-out))
  ---
  ("10%" (graphics-set-zoom 0.1))
  ("25%" (graphics-set-zoom 0.25))
  ("50%" (graphics-set-zoom 0.5))
  ("75%" (graphics-set-zoom 0.75))
  ("100%" (graphics-set-zoom 1.0))
  ("150%" (graphics-set-zoom 1.5))
  ("200%" (graphics-set-zoom 2.0))
  ("300%" (graphics-set-zoom 3.0))
  ("400%" (graphics-set-zoom 4.0))
  ("500%" (graphics-set-zoom 5.0))
  ("1000%" (graphics-set-zoom 10.0)))

(menu-bind graphics-global-menu
  (group "Graphics")
  (if (not (inside-graphical-over-under?))
      (-> "Size" (link graphics-extents-menu))
      (-> "Resize" (link graphics-resize-menu))
      (-> "Crop" (link graphics-auto-crop-menu))
      (-> "Alignment" (link graphics-alignment-menu)))
  (if (inside-graphical-over-under?)
      ("Draw over" (graphics-toggle-over-under))
      (-> "Overlap" (link graphics-overlap-menu)))
  ---
  (-> "Unit" (link graphics-frame-unit-menu))
  (-> "Origin" (link graphics-frame-origin-menu))
  (-> "Move" (link graphics-move-menu))
  (-> "Zoom" (link graphics-zoom-menu)))

(menu-bind graphics-visual-grid-menu
  (-> "Type"
      ("No grid"     (graphics-set-visual-grid 'empty))
      ---
      ("Cartesian"   (graphics-set-visual-grid 'cartesian))
      ("Polar"       (graphics-set-visual-grid 'polar))
      ("Logarithmic" (graphics-set-visual-grid 'logarithmic))
      ("Notebook"    (graphics-set-notebook-grid)))
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

(menu-bind graphics-edit-grid-menu
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
	("Other" (graphics-interactive-set-grid-nsubds #f)))))

(menu-bind graphics-grids-menu
  ("Default" (graphics-reset-grids))
  ---
  (link graphics-visual-grid-menu))

(menu-bind graphics-mode-menu
  ("Point" (graphics-set-mode '(edit point)))
  ("Line" (graphics-set-mode '(edit line)))
  ("Polygon" (graphics-set-mode '(edit cline)))
  (-> "Curve"
      ("Spline" (graphics-set-mode '(edit spline)))
      ("Smooth" (graphics-set-mode '(edit smooth)))
      ("Bezier" (graphics-set-mode '(edit bezier))))
  (-> "Closed curve"      
      ("Closed spline" (graphics-set-mode '(edit cspline)))
      ("Closed smooth" (graphics-set-mode '(edit csmooth)))
      ("Closed bezier" (graphics-set-mode '(edit cbezier))))
  ("Arc" (graphics-set-mode '(edit arc)))
  ("Circle" (graphics-set-mode '(edit carc)))
  ---
  ("Text" (graphics-set-mode '(edit text-at)))
  ("Mathematics" (graphics-set-mode '(edit math-at)))
  ("Long text" (graphics-set-mode '(edit document-at)))
  ("Hand drawn" (graphics-set-mode '(hand-edit line))) 
  (assuming (style-has? "std-markup-dtd")
    (with u '(arrow-with-text arrow-with-text*)
      (with l (list-filter u (lambda (s) (style-has? (symbol->string s))))
        (for (tag (sort l symbol<=?))
          ((eval (upcase-first (symbol->string tag)))
           (import-from (graphics graphics-markup))
           (graphics-set-mode `(edit ,tag))))))
    (with u (list-difference gr-tags-user '(arrow-with-text arrow-with-text*))
      (with l (list-filter u (lambda (s) (style-has? (symbol->string s))))
        (assuming (nnull? l)
          ---
          (for (tag (sort l symbol<=?))
            ((eval (upcase-first (symbol->string tag)))
             (import-from (graphics graphics-markup))
             (graphics-set-mode `(edit ,tag))))))))
  ---
  ("Set properties" (graphics-set-mode '(group-edit edit-props)))
  ("Move objects" (graphics-set-mode '(group-edit move)))
  ("Resize objects" (graphics-set-mode '(group-edit zoom)))
  ("Rotate objects" (graphics-set-mode '(group-edit rotate)))
  ("Group/ungroup" (graphics-set-mode '(group-edit group-ungroup)))
  (assuming (not (tree-innermost user-anim-context?))
    ("Animate objects" (graphics-set-mode '(group-edit animate)))))

(menu-bind graphics-opacity-menu
  ("0%" (graphics-set-opacity "0%"))
  ("10%" (graphics-set-opacity "10%"))
  ("20%" (graphics-set-opacity "20%"))
  ("30%" (graphics-set-opacity "30%"))
  ("40%" (graphics-set-opacity "40%"))
  ("50%" (graphics-set-opacity "50%"))
  ("60%" (graphics-set-opacity "60%"))
  ("70%" (graphics-set-opacity "70%"))
  ("80%" (graphics-set-opacity "80%"))
  ("90%" (graphics-set-opacity "90%"))
  ("100%" (graphics-set-opacity "100%"))
  ---
  ("Other" (interactive graphics-set-opacity)))

(menu-bind graphics-color-menu
  ;;("Default" (graphics-set-color "default"))
  ("None" (graphics-set-color "none"))
  ---
  (if (allow-pattern-colors?)
      (pick-background "1gu" (graphics-set-color answer)))
  (if (not (allow-pattern-colors?))
      (pick-color (graphics-set-color answer)))
  ---
  ("Palette" (interactive-color (lambda (c) (graphics-set-color c)) '()))
  ("Pattern" (open-pattern-selector graphics-set-color "1gu"))
  ("Other" (interactive graphics-set-color)))

(menu-bind grid-color-axes-menu
  ("Default" (graphics-set-grid-color 'axes "default"))
  ---
  (pick-color (graphics-set-grid-color 'axes answer))
  ---
  ("Palette" (interactive-color
              (lambda (c) (graphics-set-grid-color 'axes c)) '()))
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'axes x)) "Color")))

(menu-bind grid-color-units-menu
  ("Default" (graphics-set-grid-color 'units "default"))
  ---
  (pick-color (graphics-set-grid-color 'units answer))
  ---
  ("Palette" (interactive-color
              (lambda (c) (graphics-set-color 'units c)) '()))
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'units x)) "Color")))

(menu-bind grid-color-subunits-menu
  ("Default" (graphics-set-grid-color 'subunits "default"))
  ---
  (pick-color (graphics-set-grid-color 'subunits answer))
  ---
  ("Palette" (interactive-color
              (lambda (c) (graphics-set-grid-color 'subunits c)) '()))
  ("Other" (interactive
	       (lambda (x) (graphics-set-grid-color 'subunits x)) "Color")))

(menu-bind graphics-point-style-menu
  ;;("Default" (graphics-set-point-style "default"))
  ;;---
  ;;("Disk" (graphics-set-point-style "disk"))
  ("Disk" (graphics-set-point-style "default"))
  ("Round" (graphics-set-point-style "round"))
  ("Square" (graphics-set-point-style "square"))
  ("Diamond" (graphics-set-point-style "diamond"))
  ("Triangle" (graphics-set-point-style "triangle"))
  ("Star" (graphics-set-point-style "star"))
  ("Plus" (graphics-set-point-style "plus"))
  ("Cross" (graphics-set-point-style "cross")))

(menu-bind graphics-point-size-menu
  ("1 ln" (graphics-set-point-size "1ln"))
  ("2 ln" (graphics-set-point-size "2ln"))
  ("2.5 ln" (graphics-set-point-size "default"))
  ("4 ln" (graphics-set-point-size "4ln"))
  ("5 ln" (graphics-set-point-size "5ln"))
  ("8 ln" (graphics-set-point-size "8ln"))
  ("10 ln" (graphics-set-point-size "10ln"))
  ---
  ("1 px" (graphics-set-point-size "1px"))
  ("2 px" (graphics-set-point-size "2px"))
  ("2.5 px" (graphics-set-point-size "2.5px"))
  ("4 px" (graphics-set-point-size "4px"))
  ("5 px" (graphics-set-point-size "5px"))
  ("8 px" (graphics-set-point-size "8px"))
  ("10 px" (graphics-set-point-size "10px"))
  ---
  ("Other" (interactive graphics-set-point-size)))

(menu-bind graphics-point-border-menu
  ("0.5 ln" (graphics-set-point-border "0.5ln"))
  ("1 ln" (graphics-set-point-border "default"))
  ("2 ln" (graphics-set-point-border "2ln"))
  ("5 ln" (graphics-set-point-border "5ln"))
  ---
  ("0.5 px" (graphics-set-point-border "0.5px"))
  ("1 px" (graphics-set-point-border "1px"))
  ("2 px" (graphics-set-point-border "2px"))
  ("5 px" (graphics-set-point-border "5px"))
  ---
  ("Other" (interactive graphics-set-point-border)))

(menu-bind graphics-line-width-menu
  ;;("Default" (graphics-set-line-width "default"))
  ;;---
  ("0.5 ln" (graphics-set-line-width "0.5ln"))
  ;;("1 ln" (graphics-set-line-width "1ln"))
  ("1 ln" (graphics-set-line-width "default"))
  ("2 ln" (graphics-set-line-width "2ln"))
  ("5 ln" (graphics-set-line-width "5ln"))
  ---
  ("Other" (interactive graphics-set-line-width)))

(menu-bind graphics-dash-menu
  (group "Style")
  ;;("Default" (graphics-set-dash-style "default"))
  ;;--
  ("-----" (graphics-set-dash-style "default"))
  (". . . . ." (graphics-set-dash-style "10"))
  ("- - - - -" (graphics-set-dash-style "11100"))
  ("- . - . -" (graphics-set-dash-style "1111010"))
  ---
  (group "Motif")
  ("Zigzag" (graphics-set-dash-style "zigzag"))
  ("Wave" (graphics-set-dash-style "wave"))
  ("Pulse" (graphics-set-dash-style "pulse"))
  ("Loops" (graphics-set-dash-style "loops"))
  ("Meander" (graphics-set-dash-style "meander"))
  ;;---
  ;;("Other" (interactive graphics-set-dash-style_))
  ---
  (group "Unit")
  ;;("Default" (graphics-set-dash-style-unit "default"))
  ;;---
  ("2 ln" (graphics-set-dash-style-unit "2ln"))
  ("5 ln" (graphics-set-dash-style-unit "5ln"))
  ("10 ln" (graphics-set-dash-style-unit "10ln"))
  ("20 ln" (graphics-set-dash-style-unit "20ln"))
  ---
  ("Other" (interactive graphics-set-dash-style-unit*)))

(menu-bind graphics-line-arrows-menu
  (group "Right arrow")
  ("None" (graphics-set-arrow-end "default"))
  ("--->" (graphics-set-arrow-end "<gtr>"))
  ("---|>" (graphics-set-arrow-end "|<gtr>"))
  ("--->>" (graphics-set-arrow-end "<gtr><gtr>"))
  ("---<" (graphics-set-arrow-end "<less>"))
  ("---<|" (graphics-set-arrow-end "<less>|"))
  ("---<<" (graphics-set-arrow-end "<less><less>"))
  ("---|" (graphics-set-arrow-end "|"))
  ("---o" (graphics-set-arrow-end "o"))
  ---
  (group "Left arrow")
  ("None" (graphics-set-arrow-begin "default"))
  ("<---" (graphics-set-arrow-begin "<less>"))
  ("<|---" (graphics-set-arrow-begin "<less>|"))
  ("<<---" (graphics-set-arrow-begin "<less><less>"))
  (">---" (graphics-set-arrow-begin "<gtr>"))
  ("|>---" (graphics-set-arrow-begin "|<gtr>"))
  (">>---" (graphics-set-arrow-begin "<gtr><gtr>"))
  ("|---" (graphics-set-arrow-begin "|"))
  ("o---" (graphics-set-arrow-begin "o")))

(menu-bind graphics-line-portion-menu
  ("0" (graphics-set-line-portion "0"))
  ("0.25" (graphics-set-line-portion "0.25"))
  ("0.5" (graphics-set-line-portion "0.5"))
  ("0.75" (graphics-set-line-portion "0.75"))
  ("1" (graphics-set-line-portion "1")))

(menu-bind graphics-fill-color-menu
  ;;("Default" (graphics-set-fill-color "default"))
  ;;("None" (graphics-set-fill-color "none"))
  ("None" (graphics-set-fill-color "default"))
  ---
  (if (allow-pattern-colors?)
      (pick-background "1gu" (graphics-set-fill-color answer)))
  (if (not (allow-pattern-colors?))
      (pick-color (graphics-set-fill-color answer)))
  ---
  ("Palette" (interactive-color (lambda (c) (graphics-set-fill-color c)) '()))
  ("Pattern" (open-pattern-selector graphics-set-fill-color "1gu"))
  ("Other" (interactive graphics-set-fill-color)))

(menu-bind graphics-text-halign-menu
  ;;("Default" (graphics-set-text-at-halign "default"))
  ;;---
  ;;("Left" (graphics-set-text-at-halign "left"))
  ("Left" (graphics-set-text-at-halign "default"))
  ("Center" (graphics-set-text-at-halign "center"))
  ("Right" (graphics-set-text-at-halign "right")))

(menu-bind graphics-text-valign-menu
  ;;("Default" (graphics-set-text-at-valign "default"))
  ;;---
  ("Bottom" (graphics-set-text-at-valign "bottom"))
  ;;("Base" (graphics-set-text-at-valign "base"))
  ("Base" (graphics-set-text-at-valign "default"))
  ("Axis" (graphics-set-text-at-valign "axis"))
  ("Center" (graphics-set-text-at-valign "center"))
  ("Top" (graphics-set-text-at-valign "top")))

(menu-bind graphics-doc-valign-menu
  ;;("Default" (graphics-set-doc-at-valign "default"))
  ;;---
  ("Bottom" (graphics-set-doc-at-valign "bottom"))
  ("Base" (graphics-set-doc-at-valign "base"))
  ("Axis" (graphics-set-doc-at-valign "axis"))
  ("Center" (graphics-set-doc-at-valign "center"))
  ;;("Top" (graphics-set-text-at-valign "top"))
  ("Top" (graphics-set-doc-at-valign "default")))

(define (doc-at-mode w m)
  (if (nstring? w) (set! w "1par"))
  (if (nstring? m) (set! m "min"))
  (cond ((or (== m "min") (== m "default")) "compact")
        ((and (== w "1par") (== m "exact")) "wide")
        ((and (== w "1hpar") (== m "exact")) "half wide")
        (else w)))

(menu-bind graphics-doc-mode-menu
  ("Compact" (graphics-set-doc-at-compact))
  ("Wide" (graphics-set-doc-at-width "1par"))
  ("Half wide" (graphics-set-doc-at-width "1hpar"))
  ("Other" (interactive graphics-set-doc-at-width))
  ---
  ("Border" (graphics-toggle-doc-at-border))
  ("Padded" (graphics-toggle-doc-at-padded)))

(menu-bind graphics-anim-type-menu
  ("Inanimated" (graphics-set-anim-type "inanimated"))
  ("Animated" (graphics-set-anim-type "animated"))
  ---
  ("Ink in" (graphics-set-anim-type "ink in"))
  ("Ink out" (graphics-set-anim-type "ink out"))
  ("Fade in" (graphics-set-anim-type "fade in"))
  ("Fade out" (graphics-set-anim-type "fade out")))

(menu-bind graphics-snap-menu
  ("None" (graphics-set-snap "none"))
  ("All" (graphics-set-snap "all"))
  ---
  (if (!= (graphics-get-grid-type #t) 'empty)
      ("Grid points" (graphics-toggle-snap "grid point"))
      ("Grid curves" (graphics-toggle-snap "grid curve point"))
      ("Grid-curve intersections"
       (graphics-toggle-snap "curve-grid intersection")))
  ("Curve points" (graphics-toggle-snap "curve point"))
  ("Curve intersections" (graphics-toggle-snap "curve-curve intersection"))
  ("Text corners" (graphics-toggle-snap "text border point"))
  ("Text borders" (graphics-toggle-snap "text border"))
  ---
  (-> "Snap distance"
      ("2 px" (graphics-set-snap-distance "2px"))
      ("5 px" (graphics-set-snap-distance "5px"))
      ("10 px" (graphics-set-snap-distance "10px"))
      ("20 px" (graphics-set-snap-distance "20px"))
      ---
      ("Other" (interactive graphics-set-snap-distance)))
  (assuming (graphics-mode-attribute? (graphics-mode) "text-at-margin")
    (-> "Text padding"
        ("0.5 spc" (graphics-set-snap-text-padding "0.5spc"))
        ("1 spc" (graphics-set-snap-text-padding "1spc"))
        ("1.5 spc" (graphics-set-snap-text-padding "1.5spc"))
        ("2 spc" (graphics-set-snap-text-padding "2spc"))
        ---
        ("3 ln" (graphics-set-snap-text-padding "3ln"))
        ("5 ln" (graphics-set-snap-text-padding "5ln"))
        ("7 ln" (graphics-set-snap-text-padding "7ln"))
        ("10 ln" (graphics-set-snap-text-padding "10ln"))
        ---
        ("Other" (interactive graphics-set-snap-text-padding)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind graphics-insert-menu
  (-> "Geometry" (link graphics-global-menu))
  (-> "Grid" (link graphics-grids-menu))
  (assuming (inside? 'screens)
    (-> "Slides" (link graphics-screens-menu)))
  (-> "Overlays" (link graphics-overlays-menu))
  ---
  (link graphics-mode-menu))

(menu-bind graphics-focus-menu
  (-> (eval (upcase-first (gr-mode->string (graphics-mode))))
      (link graphics-mode-menu))
  (if (inside-graphical-over-under?)
      ("Exit graphics" (graphics-exit-right)))
  (assuming (nnot (tree-innermost overlays-context?))
    (link graphics-focus-overlays-menu))
  (assuming (nnull? (graphics-mode-attributes (graphics-mode)))
    ---
    (assuming (graphics-mode-attribute? (graphics-mode) "color")
      (-> "Color" (link graphics-color-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "fill-color")
      (-> "Fill color" (link graphics-fill-color-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "opacity")
      (assuming (== (get-preference "experimental alpha") "on")
        (-> "Opacity" (link graphics-opacity-menu))))
    (assuming (graphics-mode-attribute? (graphics-mode) "point-style")
      (-> "Point style" (link graphics-point-style-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "point-size")
      (-> "Point size" (link graphics-point-size-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "point-border")
      (-> "Point border" (link graphics-point-border-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "line-width")
      (-> "Line width" (link graphics-line-width-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "dash-style")
      (-> "Line dashes" (link graphics-dash-menu)))
    (assuming
        (or (graphics-mode-attribute? (graphics-mode) "arrow-begin")
            (graphics-mode-attribute? (graphics-mode) "arrow-end"))
      (-> "Line arrows" (link graphics-line-arrows-menu)))
    (assuming #f
      ;;(graphics-mode-attribute? (graphics-mode) "line-portion")
      (-> "Line portion" (link graphics-line-portion-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "text-at-halign")
      (-> "Horizontal alignment" (link graphics-text-halign-menu)))
    (assuming (graphics-mode-attribute? (graphics-mode) "text-at-valign")
      (-> "Vertical alignment" (link graphics-text-valign-menu)))
    (assuming (not (graphics-mode-attribute? (graphics-mode) "text-at-valign"))
      (assuming (graphics-mode-attribute? (graphics-mode) "doc-at-valign")
        (-> "Vertical alignment" (link graphics-doc-valign-menu))))
    (assuming (graphics-mode-attribute? (graphics-mode) "doc-at-width")
      (-> "Text box style" (link graphics-doc-mode-menu))))
  (assuming (graphics-get-anim-type)
    (-> "Status" (link graphics-anim-type-menu)))
  ---
  (-> "Snap" (link graphics-snap-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (graphics-global-icons)
  (=> (balloon (icon "tm_graphics_geometry.xpm") "Graphics geometry")
      (link graphics-global-menu))
  (=> (balloon (icon "tm_graphics_grid.xpm") "Graphics grids")
      (link graphics-grids-menu))
  (assuming (inside? 'screens)
    (=> (balloon (icon "tm_overlays.xpm") "Graphical slides and overlays")
        (group "Slides")
        (link graphics-screens-menu)
        ---
        (group "Overlays")
        (link graphics-overlays-menu)))
  (assuming (not (inside? 'screens))
    (=> (balloon (icon "tm_overlays.xpm") "Graphical overlays")
        (link graphics-overlays-menu)))
  ((balloon (icon "tm_exit_image.xpm") "Exit graphics mode")
   (graphics-exit-right)))

(tm-menu (graphics-insert-icons)
  ;;(=> (balloon (icon "tm_cell_special.xpm") "Graphical mode")
  ;;    (link graphics-mode-menu))
  ((check (balloon (icon "tm_point_mode.xpm") "Insert points")
          "v" (== (graphics-mode) '(edit point)))
   (graphics-set-mode '(edit point)))
  ((check (balloon (icon "tm_line_mode.xpm") "Insert lines")
          "v" (== (graphics-mode) '(edit line)))
   (graphics-set-mode '(edit line)))
  ((check (balloon (icon "tm_cline_mode.xpm") "Insert polygons")
          "v" (== (graphics-mode) '(edit cline)))
   (graphics-set-mode '(edit cline)))
  ((check (balloon (icon "tm_spline_mode.xpm") "Insert splines")
          "v" (== (graphics-mode) '(edit spline)))
   (graphics-set-mode '(edit spline)))
  ((check (balloon (icon "tm_cspline_mode.xpm") "Insert closed splines")
          "v" (== (graphics-mode) '(edit cspline)))
   (graphics-set-mode '(edit cspline)))
  ((check (balloon (icon "tm_arc_mode.xpm") "Insert arcs")
          "v" (== (graphics-mode) '(edit arc)))
   (graphics-set-mode '(edit arc)))
  ((check (balloon (icon "tm_carc_mode.xpm") "Insert circles")
          "v" (== (graphics-mode) '(edit carc)))
   (graphics-set-mode '(edit carc)))
  /
  ((check (balloon (icon "tm_textat_mode.xpm") "Insert text")
          "v" (== (graphics-mode) '(edit text-at)))
   (graphics-set-mode '(edit text-at)))
  ((check (balloon (icon "tm_math.xpm") "Insert mathematics")
          "v" (== (graphics-mode) '(edit math-at)))
   (graphics-set-mode '(edit math-at)))
  ((check (balloon (icon "tm_document_at.xpm") "Insert multiple paragraphs")
          "v" (== (graphics-mode) '(edit document-at)))
   (graphics-set-mode '(edit document-at)))
  ((check (balloon (icon "tm_ink_mode.xpm") "Insert hand drawn curves")
          "v" (== (graphics-mode) '(hand-edit line)))
   (graphics-set-mode '(hand-edit line))))

(tm-menu (graphics-group-icons)
  ((check (balloon (icon "tm_edit_props.xpm") "Edit object properties")
          "v" (== (graphics-mode) '(group-edit edit-props)))
   (graphics-set-mode '(group-edit edit-props)))
  ((check (balloon (icon "tm_group_move.xpm") "Move objects")
          "v" (== (graphics-mode) '(group-edit move)))
   (graphics-set-mode '(group-edit move)))
  ((check (balloon (icon "tm_group_zoom.xpm") "Zoom/unzoom objects")
          "v" (== (graphics-mode) '(group-edit zoom)))
   (graphics-set-mode '(group-edit zoom)))
  ((check (balloon (icon "tm_group_rotate.xpm") "Rotate objects")
          "v" (== (graphics-mode) '(group-edit rotate)))
   (graphics-set-mode '(group-edit rotate)))
  ((check (balloon (icon "tm_group_group.xpm") "Group/ungroup objects")
          "v" (== (graphics-mode) '(group-edit group-ungroup)))
   (graphics-set-mode '(group-edit group-ungroup)))
  (assuming (not (tree-innermost user-anim-context?))
    ((check (balloon (icon "tm_animate.xpm") "Animate object")
            "v" (== (graphics-mode) '(group-edit animate)))
     (graphics-set-mode '(group-edit animate)))))

(tm-menu (graphics-property-icons)
  (assuming (graphics-mode-attribute? (graphics-mode) "color")
    /
    (mini #t
      (group "Color:")
      (with col (graphics-get-property "gr-color")
        (assuming (== col "default")
          (=> (color "black" #f #f 25 17)
              (link graphics-color-menu)))
        (assuming (in? col (list "none" "mixed"))
          (=> (eval col)
              (link graphics-color-menu)))
        (assuming (nin? col (list "default" "none" "mixed"))
          (=> (color col #f #f 25 17)
              (link graphics-color-menu))))))
  (assuming (graphics-mode-attribute? (graphics-mode) "fill-color")
    /
    (mini #t
      (group "Fill color:")
      (with col (graphics-get-property "gr-fill-color")
        (assuming (== col "default")
          (=> "none"
              (link graphics-fill-color-menu)))
        (assuming (in? col (list "none" "mixed"))
          (=> (eval col)
              (link graphics-fill-color-menu)))
        (assuming (nin? col (list "default" "none" "mixed"))
          (=> (color col #f #f 25 17)
              (link graphics-fill-color-menu))))))
  (assuming (== (get-preference "experimental alpha") "on")
    (assuming (graphics-mode-attribute? (graphics-mode) "opacity")
      /
      (mini #t
        (group "Opacity:")
        (let* ((o (graphics-get-property "gr-opacity"))
               (s (if (== o "default") "100%" o)))
          (=> (eval s)
              (link graphics-opacity-menu))))))
  (assuming (graphics-mode-attribute? (graphics-mode) "point-style")
    /
    (mini #t
      (group "Point style:")
      (let* ((ps (graphics-get-property "gr-point-style"))
             (s (if (== ps "default") "disk" ps)))
	(=> (eval s)
	    (link graphics-point-style-menu)))))
  (assuming (graphics-mode-attribute? (graphics-mode) "point-size")
    /
    (mini #t
      (group "Size:")
      (let* ((ps (graphics-get-property "gr-point-size"))
             (s (if (== ps "default") "2.5ln" ps)))
	(=> (eval s)
	    (link graphics-point-size-menu)))))
  (assuming (graphics-mode-attribute? (graphics-mode) "point-border")
    /
    (mini #t
      (group "Border:")
      (let* ((ps (graphics-get-property "gr-point-border"))
             (s (if (== ps "default") "1ln" ps)))
	(=> (eval s)
	    (link graphics-point-border-menu)))))
  (assuming
      (or (graphics-mode-attribute? (graphics-mode) "line-width")
          (graphics-mode-attribute? (graphics-mode) "dash-style"))
    /
    (mini #t
      (group "Line style:")
      (let* ((lw (graphics-get-property "gr-line-width"))
             (s (if (== lw "default") "1ln" lw)))
	(=> (eval s)
	    (link graphics-line-width-menu)))
      (let* ((dash (graphics-get-property "gr-dash-style"))
             (s (decode-dash dash)))
        (=> (eval s)
            (link graphics-dash-menu)))))
  (assuming
      (or (graphics-mode-attribute? (graphics-mode) "arrow-begin")
          (graphics-mode-attribute? (graphics-mode) "arrow-end"))
    /
    (mini #t
      (group "Arrows:")
      (let* ((arrow-begin (graphics-get-property "gr-arrow-begin"))
             (arrow-end (graphics-get-property "gr-arrow-end"))
             (s (string-append (decode-arrow arrow-begin)
                               "---"
                               (decode-arrow arrow-end))))
        (=> (eval s)
            (link graphics-line-arrows-menu)))))
  (assuming #f
    ;;(graphics-mode-attribute? (graphics-mode) "line-portion")
    /
    (mini #t
      (group "Portion:")
      (let* ((portion (graphics-get-property "gr-line-portion"))
             (s (if (== portion "default") "1" portion)))
        (=> (eval s)
            (link graphics-line-portion-menu)))))
  (assuming (or (graphics-mode-attribute? (graphics-mode) "text-at-halign")
                (graphics-mode-attribute? (graphics-mode) "text-at-valign")
                (graphics-mode-attribute? (graphics-mode) "doc-at-valign"))
    /
    (mini #t
      (group "Alignment:")
      (let* ((al (graphics-get-property "gr-text-at-halign"))
             (s (if (== al "default") "left" al)))
	(=> (eval s)
	    (link graphics-text-halign-menu)))
      (assuming (graphics-mode-attribute? (graphics-mode) "text-at-valign")
        (let* ((al (graphics-get-property "gr-text-at-valign"))
               (s (if (== al "default") "base" al)))
          (=> (eval s)
              (link graphics-text-valign-menu))))
      (assuming (not (graphics-mode-attribute? (graphics-mode)
                                               "text-at-valign"))
        (assuming (graphics-mode-attribute? (graphics-mode) "doc-at-valign")
          (let* ((al (graphics-get-property "gr-doc-at-valign"))
                 (s (if (== al "default") "top" al)))
            (=> (eval s)
                (link graphics-doc-valign-menu)))))))
  (assuming (graphics-mode-attribute? (graphics-mode) "doc-at-width")
    /
    (mini #t
      (group "Style:")
      (let* ((w (graphics-get-property "gr-doc-at-width"))
             (m (graphics-get-property "gr-doc-at-hmode")))
        (=> (eval (doc-at-mode w m))
            (link graphics-doc-mode-menu))))))

(tm-menu (graphics-snap-icons)
  (mini #t
    (=> "Snap:"
        (link graphics-snap-menu))
    (if (!= (graphics-get-grid-type #t) 'empty)
        (if (graphics-get-snap "grid point")
            ((balloon (icon "tm_snap_grid.xpm")
                      "Snap to grid")
             (graphics-reset-snap "grid point")))
        (if (graphics-get-snap "grid curve point")
            ((balloon (icon "tm_snap_grid_curve.xpm")
                      "Snap to grid curve")
             (graphics-reset-snap "grid curve point")))
        (if (graphics-get-snap "curve-grid intersection")
            ((balloon (icon "tm_snap_grid_intersection.xpm")
                      "Snap to intersections of curves")
             (graphics-reset-snap "curve-grid intersection"))))
    (if (graphics-get-snap "curve point")
        ((balloon (icon "tm_snap_curve.xpm")
                  "Snap to curves")
         (graphics-reset-snap "curve point")))
    (if (graphics-get-snap "curve-curve intersection")
        ((balloon (icon "tm_snap_curve_intersection.xpm")
                  "Snap to intersections of curves")
         (graphics-reset-snap "curve-curve intersection")))
    (if (graphics-get-snap "text border point")
        ((balloon (icon "tm_snap_text_border.xpm")
                  "Snap to text corners")
         (graphics-reset-snap "text border point")))
    (if (graphics-get-snap "text border")
        ((balloon (icon "tm_snap_text_deco.xpm")
                  "Snap to text borders")
         (graphics-reset-snap "text border")))))

(define (gr-mode->string s)
  (cond ((== s '(edit point)) "point")
        ((== s '(edit line)) "line")
        ((== s '(edit cline)) "polygon")
        ((== s '(edit spline)) "spline")
        ((== s '(edit cspline)) "closed spline")
        ((== s '(edit bezier)) "bezier")
        ((== s '(edit cbezier)) "closed bezier")
        ((== s '(edit smooth)) "smooth")
        ((== s '(edit csmooth)) "closed smooth")
        ((== s '(edit arc)) "arc")
        ((== s '(edit carc)) "circle")
        ((== s '(edit text-at)) "text")
        ((== s '(edit math-at)) "mathematics")
        ((== s '(edit document-at)) "long text")
        ((== s '(group-edit props)) "properties")
        ((== s '(group-edit edit-props)) "properties")
        ((== s '(group-edit animate)) "animate")
        ((== s '(group-edit move)) "move")
        ((== s '(group-edit zoom)) "resize")
        ((== s '(group-edit rotate)) "rotate")
        ((== s '(hand-edit line)) "hand drawn")
        ((== s '(group-edit group-ungroup)) "group/ungroup")
        ((and (list-2? s) (== (car s) 'edit) (in? (cadr s) gr-tags-user))
         (symbol->string (cadr s)))
        (else "unknown")))

(tm-menu (graphics-icons)
  (link graphics-global-icons)
  /
  (link graphics-insert-icons)
  /
  (link graphics-group-icons))

(tm-menu (graphics-focus-icons)
  (mini #t
    (=> (balloon (eval (upcase-first (gr-mode->string (graphics-mode))))
                 "Current graphical mode")
        (link graphics-mode-menu)))
  (assuming (nnot (tree-innermost overlays-context?))
    (link graphics-focus-overlays-icons))
  (assuming (nnull? (graphics-mode-attributes (graphics-mode)))
    (link graphics-property-icons))
  (assuming (graphics-get-anim-type)
    /
    (mini #t
      (group "Status:")
      (=> (eval (graphics-get-anim-type))
          (link graphics-anim-type-menu))))
  /
  (link graphics-snap-icons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special menus for draw-over / draw-under
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-hidden-menu t)
  (:require (tree-in? t '(draw-over draw-under)))
  ---
  ("Enter graphics" (graphics-enter))
  (assuming (hidden-child? t 2)
    (dynamic (string-input-menu t 2))))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(draw-over draw-under)))
  (glue #f #f 10 0)
  ((balloon (icon "tm_enter_image.xpm") "Enter graphics mode")
   (graphics-enter))
  (assuming (hidden-child? t 2)
    (dynamic (string-input-icon t 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special menus for text-at and its variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-horizontal? t)
  (:require (graphical-text-at-context? t))
  #f)

(tm-menu (text-at-halign-menu)
  ("Left" (object-set-text-at-halign "default"))
  ("Center" (object-set-text-at-halign "center"))
  ("Right" (object-set-text-at-halign "right")))

(tm-menu (text-at-valign-menu)
  ("Bottom" (object-set-text-at-valign "bottom"))
  ("Base" (object-set-text-at-valign "default"))
  ("Axis" (object-set-text-at-valign "axis"))
  ("Center" (object-set-text-at-valign "center"))
  ("Top" (object-set-text-at-valign "top")))

(tm-menu (doc-at-valign-menu)
  ("Bottom" (object-set-doc-at-valign "bottom"))
  ("Base" (object-set-doc-at-valign "base"))
  ("Axis" (object-set-doc-at-valign "axis"))
  ("Center" (object-set-doc-at-valign "center"))
  ("Top" (object-set-doc-at-valign "default")))

(menu-bind doc-at-fill-color-menu
  ("None" (object-set-fill-color "default"))
  ---
  (if (allow-pattern-colors?)
      (pick-background "1gu" (object-set-fill-color answer)))
  (if (not (allow-pattern-colors?))
      (pick-color (object-set-fill-color answer)))
  ---
  ("Palette" (interactive-color (lambda (c) (object-set-fill-color c)) '()))
  ("Pattern" (open-pattern-selector object-set-fill-color "1gu"))
  ("Other" (interactive object-set-fill-color)))

(tm-menu (doc-at-mode-menu)
  ("Compact" (object-set-doc-at-compact))
  ("Wide" (object-set-doc-at-width "1par"))
  ("Half wide" (object-set-doc-at-width "1hpar"))
  ---
  ("Border" (object-toggle-doc-at-border))
  ("Padded" (object-toggle-doc-at-padded)))

(tm-menu (focus-hidden-menu t)
  (:require (graphical-text-at-context? t))
  ---
  (-> "Horizontal alignment" (link text-at-halign-menu))
  (-> "Vertical alignment" (link text-at-valign-menu)))

(tm-menu (focus-hidden-menu t)
  (:require (graphical-long-text-at-context? t))
  ---
  (-> "Fill color" (link doc-at-fill-color-menu))
  (-> "Horizontal alignment" (link text-at-halign-menu))
  (-> "Vertical alignment" (link doc-at-valign-menu))
  (-> "Text box style" (link doc-at-mode-menu)))

(tm-menu (focus-hidden-icons t)
  (:require (graphical-text-at-context? t))
  /
  (mini #t
    (group "Alignment:")
    (with s (object-get-property "text-at-halign")
      (=> (eval s) (link text-at-halign-menu)))
    (with s (object-get-property "text-at-valign")
      (=> (eval s) (link text-at-valign-menu)))))

(tm-menu (focus-hidden-icons t)
  (:require (graphical-long-text-at-context? t))
  /
  (mini #t
    (group "Fill color:")
    (with col (object-get-property "fill-color")
      (assuming (in? col (list "" "none" "default" "mixed"))
        (=> (eval (if (== "mixed") col "none"))
            (link doc-at-fill-color-menu)))
      (assuming (nin? col (list "" "none" "default" "mixed"))
        (=> (color col #f #f 25 17)
            (link doc-at-fill-color-menu)))))
  /
  (mini #t
    (group "Alignment:")
    (with s (object-get-property "text-at-halign")
      (=> (eval s) (link text-at-halign-menu)))
    (with s (object-get-property "doc-at-valign")
      (=> (eval s) (link doc-at-valign-menu))))
  /
  (mini #t
    (group "Style:")
    (let* ((w (object-get-property "doc-at-width"))
           (m (object-get-property "doc-at-hmode")))
      (=> (eval (doc-at-mode w m)) (link doc-at-mode-menu)))))
