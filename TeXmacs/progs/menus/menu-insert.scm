
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
    (texmacs edit edit-fold) (texmacs edit edit-preamble)
    (texmacs edit edit-format) (texmacs edit edit-misc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Insert menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-table-menu
  (if (and (in-text?) (style-has? "env-float-dtd"))
      ("Small table" (make-compound-arity "small-table" 2))
      ("Big table" (make-compound-arity "big-table" 2))
      ---)
  ("Plain tabular" (make-compound-arg "tabular"))
  ("Centered tabular" (make-compound-arg "tabular*"))
  ("Plain block" (make-compound-arg "block"))
  ("Centered block" (make-compound-arg "block*"))
  (if (in-math?)
      ---
      ("Matrix" (make-compound-arg "matrix"))
      ("Determinant" (make-compound-arg "det"))
      ("Choice" (make-compound-arg "choice"))
      ("Stack" (make-compound-arg "stack"))))

(menu-bind insert-link-menu
  ("Label" (make-inactive-label))
  ("Reference" (make-inactive-reference))
  ("Page reference" (make-inactive-pageref))
  ---
  ("Include" ... (choose-file "Include file" "" 'make-include))
  ("Hyperlink" (make-inactive-hyperlink))
  ("Action" (make-inactive-action))
  ---
  (-> "Citation"
      ("Visible" (make-inactive-apply-arg "cite"))
      ("Invisible" (make-inactive-apply-arg "nocite")))
  (-> "Index entry"
      ("Main" (make-inactive-apply-arg "index"))
      ("Sub" (make-inactive-apply-args "subindex" 2))
      ("Subsub" (make-inactive-apply-args "subsubindex" 3))
      ("Complex" (make-inactive-apply-args "index-complex" 4))
      ---
      ("Interjection" (make-inactive-apply-args "index-line" 2)))
  (-> "Glossary entry"
      ("Regular" (make-inactive-apply-arg "glossary"))
      ("Explained" (make-inactive-apply-args "glossary-explain" 2))
      ("Duplicate" (make-inactive-apply-arg "glossary-dup"))
      ---
      ("Interjection" (make-inactive-apply-arg "glossary-line"))))

(menu-bind insert-presentation-tag-menu
  ("Underline" (make-compound-arg "underline"))
  ("Overline" (make-compound-arg "overline"))
  ("Subscript" (make-script #t #f))
  ("Superscript" (make-script #t #t)))

(menu-bind insert-switch-menu
  ("Superpose" (make-inactive-superpose))
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
	      ("Switch to last" (switch-to "last")))))

(menu-bind insert-image-menu
  (if (and (in-text?) (style-has? "env-float-dtd"))
      ("Small figure" (make-compound-arity "small-figure" 2))
      ("Big figure" (make-compound-arity "big-figure" 2))
      ---)
  ("Link image" ... (choose-file "Load image" "image" 'make-link-image))
  ("Insert image" ...
   (choose-file "Load image" "image" 'make-inline-image)))

(menu-bind insert-page-insertion-menu
  ("Footnote" (make-big-compound "footnote"))
  ---
  ("Floating object" (make-insertion "float"))
  ("Floating figure" (begin (make-insertion "float")
			    (make-compound-arity "big-figure" 2)))
  ("Floating table" (begin (make-insertion "float")
			   (make-compound-arity "big-table" 2))))

(menu-bind position-float-menu
  ("Top" (toggle-insertion-position "t"))
  ("Here" (toggle-insertion-position "h"))
  ("Bottom" (toggle-insertion-position "b"))
  ("Other pages" (toggle-insertion-position-not "f")))

(menu-bind insert-transformational-menu
  ("Assign" (make-inactive-assign))
  ("With" (make-inactive-with))
  ("Value" (make-inactive-value))
  ---
  ("Macro" (make-inactive-macro))
  ("Argument" (make-inactive-argument))
  ("Compound" (make-inactive-compound))
  ---
  ("Function" (make-inactive-function))
  ("Apply" (make-inactive-apply)))

(menu-bind insert-executable-menu
  (-> "Arithmetic"
      ("Plus" (make-inactive-plus))
      ("Minus" (make-inactive-minus))
      ("Times" (make-inactive-times))
      ("Over" (make-inactive-over))
      ("Div" (make-inactive-div))
      ("Mod" (make-inactive-mod)))
  (-> "Text"
      ("Merge" (make-inactive-merge))
      ("Length" (make-inactive-length))
      ("Range" (make-inactive-range))
      ("Number" (make-inactive-number))
      ("Date" (make-inactive-date))
      ("Formatted date" (make-inactive-date))
      ("Translate" (make-inactive-translate))
      ("Find file" (make-inactive-find-file)))
  (-> "Tuple"
      ("Tuple?" (make-inactive-is-tuple))
      ("Merge" (make-inactive-merge))
      ("Length" (make-inactive-length))
      ("Range" (make-inactive-range))
      ("Look up" (make-inactive-look-up)))
  (-> "Condition"
      ("Not" (make-inactive-not))
      ("And" (make-inactive-and))
      ("Or" (make-inactive-or))
      ("Exclusive or" (make-inactive-xor))
      ---
      ("Equal" (make-inactive-equal))
      ("Not equal" (make-inactive-unequal))
      ("Less" (make-inactive-less))
      ("Less or equal" (make-inactive-lesseq))
      ("Greater" (make-inactive-greater))
      ("Greater or equal" (make-inactive-greatereq)))
  (-> "Programming"
      ("If" (make-inactive-if))
      ("Case" (make-inactive-case))
      ;; ("for" (make-inactive-for))
      ("While" (make-inactive-while))
      ;; ("extern" (make-inactive-extern))
      ;; ("authorize" (make-inactive-authorize))
      ))

(menu-bind insert-menu
  (-> "Link" (link insert-link-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Table" (link insert-table-menu))
  (-> "Switch" (link insert-switch-menu))
  (-> "Presentation" (link insert-presentation-tag-menu))
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
      ("New line" (make-format "next_line"))
      ("Line break" (make-format "line_break"))
      ("No line break" (make-format "no_line_break"))
      ("New paragraph" (make-format "new_line"))
      ---
      ("New page" (make-new-page))
      ("New page before" (make-new-page-before))
      ("Page break" (make-page-break))
      ("Page break before" (make-page-break-before))
      ("No page break before" (make-format "no_page_break_before"))
      ("No page break after" (make-format "no_page_break_after")))
  (-> "Indentation flag"
      ("Disable indentation before" (make-format "no_first_indentation"))
      ("Enable indentation before" (make-format "enable_first_indentation"))
      ---
      ("Disable indentation after" (make-format "no_indentation_after"))
      ("Enable indentation after" (make-format "enable_indentation_after")))
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
      ("Header" (make-inactive-assign-arg "this page header"))
      ("Footer" (make-inactive-assign-arg "this page footer"))
      ---
      (group "Permanent")
      ("Header" (make-inactive-apply-arg "set-header"))
      ("Footer" (make-inactive-apply-arg "set-footer"))
      ("Odd page header" (make-inactive-assign-arg "odd page header"))
      ("Odd page footer" (make-inactive-assign-arg "odd page footer"))
      ("Even page header" (make-inactive-assign-arg "even page header"))
      ("Even page footer" (make-inactive-assign-arg "even page footer"))
      ---
      (group "Structural")
      ("Odd page text" (make-inactive-assign-function-arg
			"odd-page-text"))
      ("Even page text" (make-inactive-assign-function-arg
			 "even-page-text")))
  (-> "Page numbering"
      ("Renumber this page" (make-inactive-assign-arg "page number"))
      ("Page number text" (make-inactive-assign-function "thepage")))
  ---
  (-> "Specific"
      ("TeXmacs" (make-inactive-specific "texmacs"))
      ("LaTeX" (make-inactive-specific "latex"))
      ("HTML" (make-inactive-specific "html"))
      ("Screen" (make-inactive-specific "screen"))
      ("Printer" (make-inactive-specific "printer")))
  (-> "Macro" (link insert-transformational-menu))
  (-> "Executable" (link insert-executable-menu))
  (-> "Special"
      ("Group" (make-group))
      ("Move object" (interactive '("Horizontal:" "Vertical:") 'make-move))
      ("Resize object" (interactive
			'("Left:" "Bottom:" "Right:" "Top:") 'make-resize))
      ("Repeat object" (make-inactive-repeat))
;;    ---
      ("Decorate atoms" (make-inactive-decorate-atoms))
;;    ("decorate lines" (make-inactive-decorate-lines))
;;    ("decorate pages" (make-inactive-decorate-pages))
;;    ---
;;    ("page insertion" (make-inactive-float))
      ))
