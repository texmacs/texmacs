
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
    (texmacs edit edit-fold) (texmacs edit edit-format)
    (texmacs edit edit-misc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Insert menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-table-menu
  (if (and (in-text?) (style-has? "env-float-dtd"))
      ("Small table" (make 'small-table))
      ("Big table" (make 'big-table))
      ---)
  ("Plain tabular" (make 'tabular))
  ("Centered tabular" (make 'tabular*))
  ("Plain block" (make 'block))
  ("Centered block" (make 'block*))
  (if (in-math?)
      ---
      ("Matrix" (make 'matrix))
      ("Determinant" (make 'det))
      ("Choice" (make 'choice))
      ("Stack" (make 'stack))))

(menu-bind insert-link-menu
  ("Label" (make 'label))
  ("Reference" (make 'reference))
  ("Page reference" (make 'pageref))
  ---
  ("Include" ... (choose-file "Include file" "" 'make-include))
  ("Hyperlink" (make 'hlink))
  ("Action" (make 'action))
  ---
  (-> "Citation"
      ("Visible" (make 'cite))
      ("Invisible" (make 'nocite)))
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
      ("Interjection" (make 'glossary-line))))

(menu-bind insert-presentation-tag-menu
  ("Underline" (make 'underline))
  ("Overline" (make 'overline))
  ("Subscript" (make-script #t #f))
  ("Superscript" (make-script #t #t)))

(menu-bind insert-switch-menu
  ("Superpose" (make 'superpose))
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
      ("Small figure" (make 'small-figure))
      ("Big figure" (make 'big-figure))
      ---)
  ("Link image" ... (choose-file "Load image" "image" 'make-link-image))
  ("Insert image" ...
   (choose-file "Load image" "image" 'make-inline-image)))

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

(menu-bind insert-transformational-menu
  ("Assign" (make 'assign))
  ("With" (make-arity 'with 3))
  ("Value" (make 'value))
  ---
  ("Macro" (make 'macro))
  ("Argument" (make 'arg))
  ("Compound" (make 'compound))
  ---
  ("Long macro" (make 'xmacro))
  ("Get label" (make 'get_label))
  ("Get arity" (make 'get_arity))
  ("Map arguments" (make 'map_args)))

(menu-bind insert-executable-menu
  (-> "Arithmetic"
      ("Plus" (make 'plus))
      ("Minus" (make 'minus))
      ("Times" (make 'times))
      ("Over" (make 'over))
      ("Div" (make 'div))
      ("Mod" (make 'mod)))
  (-> "Text"
      ("Merge" (make 'merge))
      ("Length" (make 'length))
      ("Range" (make 'range))
      ("Number" (make 'number))
      ("Date" (make 'date))
      ("Formatted date" (make 'date))
      ("Translate" (make 'translate))
      ("Find file" (make 'find_file)))
  (-> "Tuple"
      ("Tuple?" (make 'is_tuple))
      ("Merge" (make 'merge))
      ("Length" (make 'length))
      ("Range" (make 'range))
      ("Look up" (make 'look_up)))
  (-> "Condition"
      ("Not" (make 'not))
      ("And" (make 'and))
      ("Or" (make 'or))
      ("Exclusive or" (make 'xor))
      ---
      ("Equal" (make 'equal))
      ("Not equal" (make 'unequal))
      ("Less" (make 'less))
      ("Less or equal" (make 'lesseq))
      ("Greater" (make 'greater))
      ("Greater or equal" (make 'greatereq)))
  (-> "Programming"
      ("If" (make 'if))
      ("Case" (make 'case))
      ;; ("for" (make 'for))
      ("While" (make 'while))
      ;; ("extern" (make 'extern))
      ;; ("authorize" (make 'authorize))
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
      ("Header" (make-assign-arg "this page header"))
      ("Footer" (make-assign-arg "this page footer"))
      ---
      (group "Permanent")
      ("Header" (make 'set-header))
      ("Footer" (make 'set-footer))
      ("Odd page header" (make-assign-arg "odd page header"))
      ("Odd page footer" (make-assign-arg "odd page footer"))
      ("Even page header" (make-assign-arg "even page header"))
      ("Even page footer" (make-assign-arg "even page footer"))
      ---
      (group "Structural")
      ("Odd page text" (make-assign-macro-arg "odd-page-text"))
      ("Even page text" (make-assign-macro-arg "even-page-text")))
  (-> "Page numbering"
      ("Renumber this page" (make-assign-arg "page number"))
      ("Page number text" (make-assign-macro "thepage")))
  ---
  (-> "Specific"
      ("TeXmacs" (make-specific "texmacs"))
      ("LaTeX" (make-specific "latex"))
      ("HTML" (make-specific "html"))
      ("Screen" (make-specific "screen"))
      ("Printer" (make-specific "printer")))
  (-> "Macro" (link insert-transformational-menu))
  (-> "Executable" (link insert-executable-menu))
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
