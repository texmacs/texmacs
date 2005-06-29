
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : insert-menu.scm
;; DESCRIPTION : menus for inserting new structure
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic insert-menu)
  (:use (generic generic-edit) (generic format-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-link-menu
  ("Label" (make 'label))
  ("Reference" (make 'reference))
  ("Page reference" (make 'pageref))
  ---
  ("Include" (choose-file make-include "Include file" ""))
  ("Hyperlink" (make 'hlink))
  ("Action" (make 'action))
  (if (style-has? "std-dtd")
      ---
      (-> "Citation"
	  ("Visible" (make 'cite))
	  ("Invisible" (make 'nocite))
	  ("Detailed" (make 'cite-detail)))
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
  ;("Draw image" (make-graphics))
  ("Link image" (choose-file make-link-image "Load image" "image"))
  ("Insert image" (choose-file make-inline-image "Load image" "image"))
  ("Thumbnails" (make-thumbnails))
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
  (-> "Fold" (link insert-fold-menu))
  (-> "Animation" (link insert-animation-menu))
  (-> "Mathematics" (link insert-math-menu))
  (if (style-has? "program-dtd")
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
  (if (style-has? "env-float-dtd")
      (-> "Page insertion"
	  (when (not (inside? 'float))
		(link insert-page-insertion-menu))
	  ---
	  (when (inside? 'float)
		(group "Position float")
		(link position-float-menu))))
  (-> "Header and footer"
      (group "This page")
      ("Header" (make-assign-arg "page-this-header"))
      ("Footer" (make-assign-arg "page-this-footer"))
      ---
      (group "Permanent")
      ("Header" (make 'set-header))
      ("Footer" (make 'set-footer))
      ("Odd page header" (make-assign-arg "page-odd-header"))
      ("Odd page footer" (make-assign-arg "page-odd-footer"))
      ("Even page header" (make-assign-arg "page-even-header"))
      ("Even page footer" (make-assign-arg "page-even-footer")))
  (-> "Page numbering"
      ("Renumber this page" (make-assign-arg "page-nr"))
      ("Page number text" (make-assign-macro "page-the-page")))
  ---
  (-> "Specific"
      ("TeXmacs" (make-specific "texmacs"))
      ("LaTeX" (make-specific "latex"))
      ("HTML" (make-specific "html"))
      ("Screen" (make-specific "screen"))
      ("Printer" (make-specific "printer")))
  (if (not (in-source?))
      (-> "Macro" (link source-transformational-menu))
      (-> "Executable" (link source-executable-menu)))
  (-> "Special"
      ("Group" (make-group))
      ("Move object" (interactive make-move))
      ("Resize object" (interactive make-resize))
      ("Repeat object" (make 'repeat))
;;    ---
      ("Decorate atoms" (make-arity 'datoms 2))
;;    ("decorate lines" (make-arity 'dlines 2))
;;    ("decorate pages" (make-arity 'dpages 2))
;;    ---
;;    ("page insertion" (make 'float))
      ))
