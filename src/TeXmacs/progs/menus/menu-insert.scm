
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-insert.scm
;; DESCRIPTION : menus for inserting new structure
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-insert)
  (:use
     (texmacs edit edit-graphics) (texmacs edit edit-fold)
     (texmacs edit edit-format) (texmacs edit edit-misc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-table-menu
  (if (style-has? "std-dtd")
      (when (and (in-text?) (style-has? "env-float-dtd"))
	    ("Small table" (make 'small-table))
	    ("Big table" (make 'big-table))
	    ---))
  ("Plain tabular" (make 'tabular))
  ("Centered tabular" (make 'tabular*))
  ("Plain block" (make 'block))
  ("Centered block" (make 'block*))
  (if (style-has? "std-dtd")
      (when (in-math?)
	    ---
	    ("Matrix" (make 'matrix))
	    ("Determinant" (make 'det))
	    ("Choice" (make 'choice))
	    ("Stack" (make 'stack)))))

(menu-bind insert-link-menu
  ("Label" (make 'label))
  ("Reference" (make 'reference))
  ("Page reference" (make 'pageref))
  ---
  ("Include" ... (choose-file "Include file" "" 'make-include))
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

(menu-bind insert-switch-menu
  ("Superpose" (make 'superpose))
  (if (style-has? "std-dtd")
      ---
      ("Folded" (make-fold))
      (if (or (inside? "unfold")
	      (and (not (inside? "unfold")) (not (inside? "fold"))))
	  (when (inside? "unfold")
		("Fold" (fold))))
      (if (inside? "fold")
	  ("Unfold" (unfold)))
      ---
      ("Switch" (make-switch))
      (when (inside? "switch")
	    ("Add switch before" (switch-insert "before"))
	    ("Add switch after" (switch-insert "after"))
	    ("Remove this switch" (switch-remove "here"))
	    ---
	    (when (< 0 (switch-get-position))
		  ("Switch to previous" (switch-to "previous")))
	    (when (< (switch-get-position) (switch-get-last))
		  ("Switch to next" (switch-to "next")))
	    (when (< 0 (switch-get-position))
		  ("Switch to first" (switch-to "first")))
	    (when (< (switch-get-position) (switch-get-last))
		  ("Switch to last" (switch-to "last"))))))

(menu-bind insert-image-menu
  (if (style-has? "env-float-dtd")
      (when (in-text?)
	    ("Small figure" (make 'small-figure))
	    ("Big figure" (make 'big-figure))
	    ---))
  ;("Draw image" (make-graphics))
  ("Link image" ... (choose-file "Load image" "image" 'make-link-image))
  ("Insert image" ...
   (choose-file "Load image" "image" 'make-inline-image)))

(menu-bind insert-mathematics-menu
  (when (not (in-math?))
	("Formula" "$" (begin (noop) (make-with "mode" "math"))))
  (when (in-math?)
	("Text" "A-$" (begin (noop) (make-with "mode" "text"))))
  (if (style-has? "env-math-dtd")
      (when (in-text?)
	    ---
	    ("Equation" (begin (make 'equation*) (temp-proof-fix)))
	    ("Equations" (begin (make 'eqnarray*) (temp-proof-fix))))))

(menu-bind insert-session-menu
  (when (and (style-has? "std-dtd") (in-text?))
	(link session-menu)
	---
	("Other" ...
	 (interactive '("Session type:" "Session name:") 'make-session))))

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
  ("Top" (toggle-insertion-position "t"))
  ("Here" (toggle-insertion-position "h"))
  ("Bottom" (toggle-insertion-position "b"))
  ("Other pages" (toggle-insertion-position-not "f")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Insert menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-menu
  (-> "Link" (link insert-link-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Table" (link insert-table-menu))
  (-> "Switch" (link insert-switch-menu))
  (-> "Mathematics" (link insert-mathematics-menu))
  (if (style-has? "program-dtd")
      (-> "Session" (link insert-session-menu)))
  ---
  (-> "Space"
      ("Rigid" ...
       (interactive
	'("Horizontal space:" "Base level:" "Top level:") 'make-var-space))
      ---
      (group "Horizontal")
      ("Stretchable" ... (interactive '("Horizontal space:") 'make-hspace))
      ("Rigid" ... (interactive '("Horizontal space:") 'make-space))
      ("Tab" (make-htab "5mm"))
      ("Custom tab" ... (interactive '("Minimal space:") 'make-htab))
      ---
      (group "Vertical before")
      ("Small skip" (make-vspace-before "0.5fn"))
      ("Medium skip" (make-vspace-before "1fn"))
      ("Big skip" (make-vspace-before "2fn"))
      ("Other" ... (interactive '("Vertical space:") 'make-vspace-before))
      ---
      (group "Vertical after")
      ("Small skip" (make-vspace-after "0.5fn"))
      ("Medium skip" (make-vspace-after "1fn"))
      ("Big skip" (make-vspace-after "2fn"))
      ("Other" ... (interactive '("Vertical space:") 'make-vspace-after)))
  (-> "Break"
      ("New line" (make 'next-line))
      ("Line break" (make 'line-break))
      ("No line break" (make 'no-break))
      ("New paragraph" (make 'new-line))
      ---
      ("New page" (make-new-page))
      ("New page before" (make 'new-page*))
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
	  (when (not (inside? "float"))
		(link insert-page-insertion-menu))
	  ---
	  (when (inside? "float")
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
      ("Even page footer" (make-assign-arg "page-even-footer"))
      ---
      (group "Structural")
      ("Odd page text" (make-assign-macro-arg "odd-page-text"))
      ("Even page text" (make-assign-macro-arg "even-page-text")))
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
      ("Move object" (interactive '("Horizontal:" "Vertical:") 'make-move))
      ("Resize object" (interactive
			'("Left:" "Bottom:" "Right:" "Top:") 'make-resize))
      ("Repeat object" (make 'repeat))
;;    ---
      ("Decorate atoms" (make-arity 'datoms 2))
;;    ("decorate lines" (make-arity 'dlines 2))
;;    ("decorate pages" (make-arity 'dpages 2))
;;    ---
;;    ("page insertion" (make 'float))
      ))
