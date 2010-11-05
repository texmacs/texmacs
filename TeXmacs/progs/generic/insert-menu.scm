
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
      (-> "Session" (link insert-session-menu))))
