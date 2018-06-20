
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-menu.scm
;; DESCRIPTION : menus for setting local formatting properties
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-menu)
  (:use (generic generic-menu)
	(generic format-edit)
	(generic format-geometry-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  ("Other" (make-interactive-with "font-base-size")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for text properties and formatting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind opacity-menu
  ("0%" (make-with-like '(with-opacity "0%" "")))
  ("10%" (make-with-like '(with-opacity "10%" "")))
  ("20%" (make-with-like '(with-opacity "20%" "")))
  ("30%" (make-with-like '(with-opacity "30%" "")))
  ("40%" (make-with-like '(with-opacity "40%" "")))
  ("50%" (make-with-like '(with-opacity "50%" "")))
  ("60%" (make-with-like '(with-opacity "60%" "")))
  ("70%" (make-with-like '(with-opacity "70%" "")))
  ("80%" (make-with-like '(with-opacity "80%" "")))
  ("90%" (make-with-like '(with-opacity "90%" "")))
  ("100%" (make-with-like '(with-opacity "100%" "")))
  ---
  ("Other" (make-interactive-with-opacity)))
  
(menu-bind exact-opacity-menu
  ("0%" (make-with "opacity" "0%"))
  ("10%" (make-with "opacity" "10%"))
  ("20%" (make-with "opacity" "20%"))
  ("30%" (make-with "opacity" "30%"))
  ("40%" (make-with "opacity" "40%"))
  ("50%" (make-with "opacity" "50%"))
  ("60%" (make-with "opacity" "60%"))
  ("70%" (make-with "opacity" "70%"))
  ("80%" (make-with "opacity" "80%"))
  ("90%" (make-with "opacity" "90%"))
  ("100%" (make-with "opacity" "100%"))
  ---
  ("Other" (make-interactive-with "opacity")))

(menu-bind color-menu
  (with setter (lambda (col) (make-with "color" col))
    (if (allow-pattern-colors?)
        (pick-background "" (setter answer)))
    (if (not (allow-pattern-colors?))
        (pick-color (setter answer)))
    ---
    ("Palette" (interactive-color setter) '())
    (if (allow-pattern-colors?)
        ("Pattern" (open-pattern-selector setter "1cm")))
    ("Other" (make-interactive-with "color"))))

(menu-bind horizontal-space-menu
  ("Stretchable" (interactive make-hspace))
  ("Rigid" (interactive make-space))
  ("Rigid box" (interactive make-var-space))
  ("Tab" (make-htab "5mm"))
  ("Custom tab" (interactive make-htab)))

(menu-bind adjust-menu
  ("Move" (make-move "" ""))
  ("Shift" (make-shift "" ""))
  ---
  ("Resize" (make-resize "" "" "" ""))
  ("Extend" (make-extend "" "" "" ""))
  ("Clip" (make-clipped "" "" "" ""))
  ---
  ("Smash" (make 'smash))
  ("Reduce" (make-reduce-by "0.5ex"))
  ("Swell" (make 'swell)))

(menu-bind linear-transform-menu
  ("Rotate" (make-with-like `(rotate "45" "")))
  ("Dilate" (make-with-like `(dilate "1.2" "0.9" "")))
  ("Skew" (make-with-like `(skew "0.333" "")))
  ("Linear 2D" (make-with-like `(linear-2d "1.2" "0.2" "0.2" "1.2" ""))))

(menu-bind format-special-menu
  ("Group" (make-rigid))
  ("Indivisible" (make 'indivisible))
  ("Phantom" (make 'phantom))
  ("Superpose" (make 'superpose))
  ("Repeat object" (make 'repeat))
  ("Decorate atoms" (make 'datoms 2))
  ;;("Decorate lines" (make 'dlines 2))
  ;;("Decorate pages" (make 'dpages 2))
  )

(menu-bind transform-menu
  (link adjust-menu)
  ---
  (link format-special-menu))

(menu-bind text-font-effects-menu
  ("Embold" (make 'embold))
  ("Blackboard embold" (make 'embbb))
  ("Slanted" (make 'slanted))
  ---
  ("Magnify horizontally" (make 'hmagnified))
  ("Magnify vertically" (make 'vmagnified))
  ("Condensed" (make 'condensed))
  ("Extended" (make 'extended))
  ---
  ("Degraded" (make 'degraded))
  ("Distorted" (make 'distorted))
  ("Gnawed" (make 'gnawed)))

(menu-bind text-effects-menu
  ("Blur" (make 'blur))
  ("Outline" (make 'outline))
  ("Thicken" (make 'thicken))
  ("Erode" (make 'erode))
  ---
  ("Shadow" (make 'shadow))
  ("Engrave" (make 'engrave))
  ("Emboss" (make 'emboss))
  ("Shadowed raise" (make 'shadowed-raise))
  ("Outlined engrave" (make 'outlined-engrave))
  ("Outlined emboss" (make 'outlined-emboss))
  ;;("Burning" (make-effect 'burning))
  ;;("Bubble" (make-effect 'bubble))
  ---
  ("Degrade" (make 'degrade))
  ("Distort" (make 'distort))
  ("Gnaw" (make 'gnaw)))

(menu-bind specific-menu
  ("TeXmacs" (make-specific "texmacs"))
  ("LaTeX" (make-specific "latex"))
  ("HTML" (make-specific "html"))
  ("Screen" (make-specific "screen"))
  ("Printer" (make-specific "printer"))
  ("Image" (make-specific "image"))
  ("Even pages" (make-specific "even"))
  ("Odd pages" (make-specific "odd")))

(menu-bind text-properties-menu
  (-> "Color" (link color-menu))
  (if (== (get-preference "experimental alpha") "on")
      (-> "Opacity" (link opacity-menu)))
  (-> "Space" (link horizontal-space-menu))
  (-> "Transform" (link transform-menu))
  (-> "Specific" (link specific-menu))
  (-> "Font effects" (link text-font-effects-menu))
  (assuming (== (get-preference "bitmap effects") "on")
    (-> "Graphical effects" (link text-effects-menu))))

(menu-bind textual-properties-menu
  (-> "Color" (link color-menu))
  (if (== (get-preference "experimental alpha") "on")
      (-> "Opacity" (link opacity-menu)))
  (-> "Space" (link horizontal-space-menu))
  (-> "Transform" (link transform-menu))
  (-> "Specific" (link specific-menu))
  (-> "Font effects" (link text-font-effects-menu))
  (assuming (== (get-preference "bitmap effects") "on")
    (-> "Graphical effects" (link text-effects-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pen selection for graphical effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (effect-pen-icon t)
  (with p (get-effect-pen t)
    (if (string? p)
        (string-append "tm_pen_" p ".xpm")
        "tm_customized.xpm")))

(tm-menu (select-effect-pen-menu t)
  ("Gaussian" (set-effect-pen t "gaussian"))
  ("Oval" (set-effect-pen t "oval"))
  ("Rectangular" (set-effect-pen t "rectangular"))
  ("Motion" (set-effect-pen t "motion")))

(tm-menu (focus-misc-menu t)
  (:require (pen-effect-context? t))
  (-> "Effect pen" (dynamic (select-effect-pen-menu t))))

(tm-menu (focus-misc-icons t)
  (:require (pen-effect-context? t))
  (=> (balloon (icon (eval (effect-pen-icon t))) "Select pen")
      (dynamic (select-effect-pen-menu t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Paragraph menu and submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind vertical-space-menu
  (group "Vertical space before")
  ("Small skip" (make-vspace-before "0.5fn"))
  ("Medium skip" (make-vspace-before "1fn"))
  ("Big skip" (make-vspace-before "2fn"))
  ("Other" (interactive make-vspace-before))
  ---
  (group "Vertical space after")
  ("Small skip" (make-vspace-after "0.5fn"))
  ("Medium skip" (make-vspace-after "1fn"))
  ("Big skip" (make-vspace-after "2fn"))
  ("Other" (interactive make-vspace-after)))

(menu-bind indentation-menu
  ("Disable indentation before" (make 'no-indent))
  ("Enable indentation before" (make 'yes-indent))
  ("Disable indentation after" (make 'no-indent*))
  ("Enable indentation after" (make 'yes-indent*)))

(menu-bind line-break-menu
  ("New line" (make 'next-line))
  ("Line break" (make 'line-break))
  ("No line break" (make 'no-break))
  ("New paragraph" (make 'new-line)))

(menu-bind paragraph-menu
  (-> "Alignment"
      ("Left aligned" (make-line-with "par-mode" "left"))
      ("Centered" (make-line-with "par-mode" "center"))
      ("Right aligned" (make-line-with "par-mode" "right"))
      ---
      ("Justified" (make-line-with "par-mode" "justify"))
      ("Flexibility" (make-interactive-line-with "par-flexibility")))
  (-> "Margins"
      ("Left margin" (make-interactive-line-with "par-left"))
      ("Right margin" (make-interactive-line-with "par-right"))
      ("First indentation" (make-interactive-line-with "par-first"))
      ---
      (link indentation-menu))
  (-> "Spacing"
      ("Interline separation" (make-interactive-line-with "par-sep"))
      ("Interline space" (make-interactive-line-with "par-line-sep"))
      ("Interparagraph space" (make-interactive-line-with "par-par-sep"))
      ---
      (link vertical-space-menu))
  (-> "Line breaking"
      ("Normal" (make-line-with "par-hyphen" "normal"))
      ("Professional"
       (make-line-with "par-hyphen" "professional"))
      ---
      (link line-break-menu))
  (-> "Number of columns"
      ("1" (make-line-with "par-columns" "1"))
      ("2" (make-line-with "par-columns" "2"))
      ("3" (make-line-with "par-columns" "3"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Page menu and submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (page-numbering-context? t)
  (tree-in? t '(set-this-page-header set-this-page-footer
                set-header set-footer
                set-odd-page-header set-even-page-header
                set-odd-page-footer set-even-page-footer
                set-page-number set-page-number-macro)))

(tm-define (notify-activated t)
  (:require (page-numbering-context? t))
  (refresh-window))

(tm-define (notify-disactivated t)
  (:require (page-numbering-context? t))
  (refresh-window))

(menu-bind page-header-menu
  ("This page header" (make 'set-this-page-header))
  ("Permanent header" (make 'set-header))
  ("Odd page header" (make 'set-odd-page-header))
  ("Even page header" (make 'set-even-page-header)))

(menu-bind page-footer-menu
  ("This page footer" (make 'set-this-page-footer))
  ("Permanent footer" (make 'set-footer))
  ("Odd page footer" (make 'set-odd-page-footer))
  ("Even page footer" (make 'set-even-page-footer)))

(menu-bind page-numbering-menu
  ("Renumber this page" (make 'set-page-number))
  ("Page number text" (make 'set-page-number-macro)))

(menu-bind page-break-menu
  (group "Page break before")
  ("New page" (make 'new-page*))
  ("New double page" (make 'new-dpage*))
  ("Page break" (make 'page-break*))
  ("No page break" (make 'no-break-here*))
  ---
  (group "Page break after")
  ("New page" (make-new-page))
  ("New double page" (make-new-dpage))
  ("Page break" (make-page-break))
  ("No page break" (make 'no-break-here)))

(menu-bind page-menu
  (-> "Header" (link page-header-menu))
  (-> "Footer" (link page-footer-menu))
  (-> "Numbering" (link page-numbering-menu))
  (-> "Break" (link page-break-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Horizontal/vertical space and line/page break submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind space-menu
  (group "Horizontal space")
  (link horizontal-space-menu)
  ---
  (link vertical-space-menu))

(menu-bind break-menu
  (group "Line break")
  (link line-break-menu)
  ---
  (link page-break-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Format menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind format-menu
  (if (or (in-text?) (in-source?)) (link text-format-menu))
  (if (in-math?) (link math-format-menu))
  (if (in-prog?) (link prog-format-menu)))
