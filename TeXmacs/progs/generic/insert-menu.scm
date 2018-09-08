
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
  (:use (utils edit selections)
	(generic generic-edit)
	(generic format-edit)
	(generic format-geometry-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert links
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-link-menu
  (when (not (selection-active-non-small?))
    ("Label" (make-label))
    ("Reference" (make 'reference))
    ("Page reference" (make 'pageref)))
  ---
  (when (not (selection-active?))
    (if (detailed-menus?)
        ("Include" (choose-file make-include "Include file" ""))))
  (when (not (selection-active-non-small?))
    ("Hyperlink" (make 'hlink))
    (if (detailed-menus?)
        ("Action" (make 'action))))
  (if (simple-menus?)
      ("Footnote" (make-wrapped 'footnote)))
  (if (and (style-has? "std-dtd") (in-text?))
      ---
      (when (not (selection-active-non-small?))
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
            ("Interjection" (make 'glossary-line))))
      (-> "Alternate"
          ("Bibliography"
           (make-alternate "Name of bibliography" "bib" 'with-bib))
          ("Table of contents"
           (make-alternate "Name of table of contents" "toc" 'with-toc))
          ("Index"
           (make-alternate "Name of index" "idx" 'with-index))
          ("Glossary"
           (make-alternate "Name of glossary" "gly" 'with-glossary))
          ("List of figures"
           (make-alternate "Name of list of figures" "figure" 'with-figure-list))
          ("List of tables"
           (make-alternate "Name of list of tables" "table" 'with-table-list))))
  (if (and (style-has? "calc-dtd")
           (not (style-has? "icourse-dtd"))
           (calc-ready?))
      ---
      (link calc-insert-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-image-menu
  (if (and (style-has? "env-float-dtd") (in-text?))
      (when (not (selection-active-non-small?))
        ("Small figure"
         (wrap-selection-small
           (make 'small-figure)))
        ("Big figure"
         (wrap-selection-small
           (insert-go-to '(big-figure "" (document "")) '(0 0))))
        ---))
  ("Link image" (choose-file make-link-image "Load image" "image"))
  ("Insert image" (choose-file make-inline-image "Load image" "image"))
  (if (detailed-menus?)
      ("Thumbnails" (make-thumbnails)))
  (if (or (lazy-plugin-force)
	  (and (style-has? "scripts-dtd") (scripts-defined? "gnuplot")))
      (-> "Plot" (link scripts-plot-menu)))
  ---
  ("Draw image" (make-graphics))
  (when (selection-active-small?)
    ("Draw over selection" (make-graphics-over-selection)))
  ("Ink here" (make-graphics-over))
  (if (in-math?)
      ("Commutative diagram" (make-cd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Insert menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-insert-menu
  (if (not (in-text?))
      ("Text" (make 'text)))
  (if (not (in-math?))
      (-> "Mathematics" (link insert-math-menu)))
  (-> "Table" (link insert-table-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Link" (link insert-link-menu))
  (if (detailed-menus?)
      (if (style-has? "std-fold-dtd")
          (-> "Fold" (link insert-fold-menu)))
      (-> "Animation" (link insert-animation-menu)))
  (if (and (style-has? "session-dtd") (detailed-menus?) (not (in-math?)))
      (-> "Session" (link insert-session-menu))))

(menu-bind insert-menu
  (if (in-text?) (link text-menu))
  (if (in-math?) (link math-menu))
  (if (not (or (in-text?) (in-math?))) (link texmacs-insert-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main Insert icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-insert-icons
  /
  (if (not (in-text?))
      ((balloon (icon "tm_textual.xpm") "Insert plain text")
       (make 'text)))
  (if (not (in-math?))
      (=> (balloon (icon "tm_math.xpm") "Insert mathematics")
	  (link insert-math-menu)))
  (=> (balloon (icon "tm_table.xpm") "Insert a table")
      (link insert-table-menu))
  (=> (balloon (icon "tm_image.xpm") "Insert a picture")
      (link insert-image-menu))
  (=> (balloon (icon "tm_link.xpm") "Insert a link")
      (link insert-link-menu))
  (if (detailed-menus?)
      (if (style-has? "std-fold-dtd")
	  (=> (balloon (icon "tm_switch.xpm") "Switching and folding")
	      (link insert-fold-menu)))
      (=> (balloon (icon "tm_animate.xpm") "Animation")
	  (link insert-animation-menu)))
  (if (and (style-has? "session-dtd") (detailed-menus?) (in-text?))
      (=> (balloon (icon "tm_shell.xpm")
		   "Start an interactive session")
	  (link insert-session-menu))))
