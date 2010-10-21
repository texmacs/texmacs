
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : insert-menu.scm
;; DESCRIPTION : menus for inserting new structure
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic insert-menu)
  (:use (generic generic-edit)
	(generic format-edit)
	(generic format-geometry-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-link-menu
  ("Label" (make-label))
  ("Reference" (make 'reference))
  ("Page reference" (make 'pageref))
  ---
  (if (detailed-menus?)
      ("Include" (choose-file make-include "Include file" "")))
  ("Hyperlink" (make 'hlink))
  (if (detailed-menus?)
      ("Action" (make 'action)))
  (if (simple-menus?)
      ("Footnote" (make 'footnote)))
  (if (style-has? "std-dtd")
      ---
      (-> "Citation"
	  (if (not (style-has? "cite-author-year-dtd"))
	      ("Visible" (make 'cite))
	      ("Invisible" (make 'nocite))
	      ("Detailed" (make 'cite-detail)))
	  (if (style-has? "cite-author-year-dtd")
	      (group "Abbreviated authors")
	      ("Raw" (make 'cite-raw))
	      ("Textual" (make 'cite-textual))
	      ("Parenthesized" (make 'cite-parenthesized))
	      ---
	      (group "Full author list")
	      ("Raw" (make 'cite-raw*))
	      ("Textual" (make 'cite-textual*))
	      ("Parenthesized" (make 'cite-parenthesized*))
	      ---
	      (group "Decomposed")
	      ("Parenthesis" (make 'render-cite))
	      ("Abreviated authors" (make 'cite-author-link))
	      ("Full author list" (make 'cite-author*-link))
	      ("Year" (make 'cite-year-link))
	      ("Invisible" (make 'nocite))))
      (-> "Index entry"
	  ("Main" (make 'index))
	  ("Sub" (make 'subindex))
	  ("Subsub" (make 'subsubindex))
	  ("Complex" (make 'index-complex))
	  ---
	  ("Interjection" (make 'index-line)))
      (-> "Glossary entry"
	  ("Regular" (make 'glossary))
	  ("Explained" (make 'glossary-explain))
	  ("Duplicate" (make 'glossary-dup))
	  ---
	  ("Interjection" (make 'glossary-line)))))

(menu-bind insert-image-menu
  (if (style-has? "env-float-dtd")
      (when (in-text?)
	    ("Small figure" (make 'small-figure))
	    ("Big figure" (make 'big-figure))
	    ---))
  ("Draw image" (make-graphics))
  (when (selection-active-small?)
    ("Draw over selection" (make-graphics-over-selection)))
  ("Link image" (choose-file make-link-image "Load image" "image"))
  ("Insert image" (choose-file make-inline-image "Load image" "image"))
  (if (detailed-menus?)
      ("Thumbnails" (make-thumbnails)))
  (if (or (lazy-plugin-force)
	  (and (style-has? "scripts-dtd") (supports-scripts? "gnuplot")))
      ---
      (-> "Plot" (link scripts-plot-menu))))

(menu-bind insert-animation-menu
  ("Fixed" (interactive make-anim-constant))
  ("Compose" (make 'anim-compose))
  ("Repeat" (make 'anim-repeat))
  ---
  (-> "Translate"
      ("Rightwards" (interactive make-anim-translate-right))
      ("Leftwards" (interactive make-anim-translate-left))
      ("Upwards" (interactive make-anim-translate-up))
      ("Downwards" (interactive make-anim-translate-down)))
  (-> "Progressive"
      ("Rightwards" (interactive make-anim-progressive-right))
      ("Leftwards" (interactive make-anim-progressive-left))
      ("Upwards" (interactive make-anim-progressive-up))
      ("Downwards" (interactive make-anim-progressive-down))
      ("From center" (interactive make-anim-progressive-center)))
  ---
  ("Animation" (choose-file make-animation "Load file" "animation"))
  ("Sound" (choose-file make-sound "Load file" "sound")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert floating content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-page-insertion-menu
  ("Footnote" (make 'footnote))
  ---
  ("Floating object" (make-insertion "float"))
  ("Floating figure" (begin (make-insertion "float") (make 'big-figure)))
  ("Floating table" (begin (make-insertion "float") (make 'big-table))))

(menu-bind position-float-menu
  ("Top" (toggle-insertion-positioning "t"))
  ("Here" (toggle-insertion-positioning "h"))
  ("Bottom" (toggle-insertion-positioning "b"))
  ("Other pages" (toggle-insertion-positioning-not "f")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Insert menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-menu
  (-> "Table" (link insert-table-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Link" (link insert-link-menu))
  (if (detailed-menus?)
      (if (style-has? "std-fold-dtd")
	  (-> "Fold" (link insert-fold-menu)))
      (-> "Animation" (link insert-animation-menu)))
  (-> "Mathematics" (link insert-math-menu))
  (if (and (style-has? "program-dtd") (detailed-menus?))
      (-> "Session" (link insert-session-menu)))
  ---
  (-> "Space"
      ("Rigid" (interactive make-var-space))
      ---
      (group "Horizontal")
      ("Stretchable" (interactive make-hspace))
      ("Rigid" (interactive make-space))
      ("Tab" (make-htab "5mm"))
      ("Custom tab" (interactive make-htab))
      ---
      (group "Vertical before")
      ("Small skip" (make-vspace-before "0.5fn"))
      ("Medium skip" (make-vspace-before "1fn"))
      ("Big skip" (make-vspace-before "2fn"))
      ("Other" (interactive make-vspace-before))
      ---
      (group "Vertical after")
      ("Small skip" (make-vspace-after "0.5fn"))
      ("Medium skip" (make-vspace-after "1fn"))
      ("Big skip" (make-vspace-after "2fn"))
      ("Other" (interactive make-vspace-after)))
  (-> "Break"
      ("New line" (make 'next-line))
      ("Line break" (make 'line-break))
      ("No line break" (make 'no-break))
      ("New paragraph" (make 'new-line))
      ---
      ("New page" (make-new-page))
      ("New page before" (make 'new-page*))
      ("New double page" (make-new-dpage))
      ("New double page before" (make 'new-dpage*))
      ("Page break" (make-page-break))
      ("Page break before" (make 'page-break*))
      ("No page break before" (make 'no-page-break*))
      ("No page break after" (make 'no-page-break)))
  (-> "Indentation flag"
      ("Disable indentation before" (make 'no-indent))
      ("Enable indentation before" (make 'yes-indent))
      ---
      ("Disable indentation after" (make 'no-indent*))
      ("Enable indentation after" (make 'yes-indent*)))
  (if (and (style-has? "env-float-dtd") (detailed-menus?))
      (-> "Page insertion"
	  (when (not (inside? 'float))
		(link insert-page-insertion-menu))
	  ---
	  (when (inside? 'float)
		(group "Position float")
		(link position-float-menu))))
  (-> "Header and footer"
      (group "This page")
      ("Header" (make 'set-this-page-header))
      ("Footer" (make 'set-this-page-footer))
      ---
      (group "Permanent")
      ("Header" (make 'set-header))
      ("Footer" (make 'set-footer))
      ("Odd page header" (make 'set-odd-page-header))
      ("Odd page footer" (make 'set-odd-page-footer))
      ("Even page header" (make 'set-even-page-header))
      ("Even page footer" (make 'set-even-page-footer)))
  (-> "Page numbering"
      ("Renumber this page" (make 'set-page-number))
      ("Page number text" (make 'set-page-number-macro)))
  (if (detailed-menus?)
      ---
      (-> "Specific"
	  ("TeXmacs" (make-specific "texmacs"))
	  ("LaTeX" (make-specific "latex"))
	  ("HTML" (make-specific "html"))
	  ("Screen" (make-specific "screen"))
	  ("Printer" (make-specific "printer"))
	  ("Image" (make-specific "image")))
      (if (not (in-source?))
	  (-> "Macro" (link source-transformational-menu))
	  (-> "Executable" (link source-executable-menu)))
      (-> "Special"
	  ("Group" (make-rigid))
	  ("Superpose" (make 'superpose))
	  ---
	  ("Move object" (interactive make-move))
	  ("Shift object" (interactive make-shift))
	  ("Resize object" (interactive make-resize))
	  ("Clip object" (interactive make-clipped))
	  ---
	  ("Repeat object" (make 'repeat))
;;        ---
	  ("Decorate atoms" (make-arity 'datoms 2))
;;        ("decorate lines" (make-arity 'dlines 2))
;;        ("decorate pages" (make-arity 'dpages 2))
;;        ---
;;        ("page insertion" (make 'float))
	  )))
