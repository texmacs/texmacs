
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-menu.scm
;; DESCRIPTION : menus for inserting structure in text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-menu)
  (:use (text format-text-edit) (text std-text-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind author-menu
  ("Insert author" (make-doc-data-element 'doc-author-data))
  ---
  (when (inside? 'doc-author-data)
	("Address" (make-author-data-element 'author-address))
	("Email" (make-author-data-element 'author-email))
	("Homepage" (make-author-data-element 'author-homepage))
	("Note" (make-author-data-element 'author-note))))

(menu-bind title-menu
  (when (not (inside? 'doc-data))
	("Insert title" (make-doc-data)))
  ---
  (when (inside? 'doc-data)
	("Subtitle" (make-doc-data-element 'doc-subtitle))
	(-> "Author" (link author-menu))
	(-> "Date"
	    ("Default" (make-doc-data-element 'doc-date))
	    ("Today"
	     (begin (make-doc-data-element 'doc-date) (make-arity 'date 0))))
	(-> "Note"
	    ("General note" (make-doc-data-element 'doc-note))
	    ("Written with TeXmacs" (begin (make-doc-data-element 'doc-note)
					   (make 'with-TeXmacs-text))))
	(-> "Hidden"
	    (if (doc-data-disactivated?)
		("Activate hidden" (doc-data-activate-all)))
	    (if (not (doc-data-disactivated?))
		("Show hidden" (doc-data-disactivate-all)))
	    ---
	    ("Running title" (make-doc-data-element 'doc-running-title))
	    ("Running author" (make-doc-data-element 'doc-running-author))
	    ("Keywords" (make-doc-data-element 'doc-keywords))
	    ("A.M.S. subject classification"
	     (make-doc-data-element 'doc-AMS-class))))
  ---
  (when (and (not (inside? 'doc-data)) (not (inside? 'abstract)))
	("Abstract" (make 'abstract))))

(menu-bind letter-header-menu
  (when (not (inside? 'letter-header))
	("Header" (make 'letter-header)))
  (when (inside? 'letter-header)
	("Address" (make-header 'address))
	("Date" (make-header 'letter-date))
	("Today" (begin (make-header 'letter-date) (make-arity 'date 0)))
	("Destination" (make-header 'destination)))
  ---
  (when (not (inside? 'letter-header))
	("Opening" (make 'opening))
	("Closing" (make 'closing))
	("Signature" (make 'signature)))
  ---
  ("C.C." (make 'cc))
  ("Encl." (make 'encl)))

(menu-bind exam-header-menu
  ("Class" (make-header 'class))
  ("Date" (begin (go-end-of-header-element) (make 'title-date)))
  ("Title" (make-header 'title)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind chapter-menu
  ("Part" (make-section 'part))
  ("Chapter" (make-section 'chapter))
  ("Appendix" (make-section 'appendix))
  ("Prologue" (make-unnamed-section 'prologue))
  ("Epilogue" (make-unnamed-section 'epilogue)))

(menu-bind section-menu
  ("Section" (make-section 'section))
  ("Subsection" (make-section 'subsection))
  ("Subsubsection" (make-section 'subsubsection))
  ---
  ("Paragraph" (make-section 'paragraph))
  ("Subparagraph" (make-section 'subparagraph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theorem like environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind environment-menu
  (if (style-has? "header-exam-dtd")
      ("Exercise" (make 'exercise))
      ("Problem" (make 'problem)))
  (if (not (style-has? "header-exam-dtd"))
      (if (style-has? "env-theorem-dtd")
	  ("Theorem" (make 'theorem))
	  ("Proposition" (make 'proposition))
	  ("Lemma" (make 'lemma))
	  ("Corollary" (make 'corollary))
	  ("Proof" (make 'proof))
	  ("Axiom" (make 'axiom))
	  ("Definition" (make 'definition))
	  ("Notation" (make 'notation))
	  ---
	  ("Remark" (make 'remark))
	  ("Note" (make 'note))
	  ("Example" (make 'example))
	  ("Warning" (make 'warning))
	  ("Exercise" (make 'exercise))
	  ("Problem" (make 'problem))
	  ---)
      ("Verbatim" (make 'verbatim))
      ("Code" (make 'code))
      ("Quote" (make 'quote-env))
      ("Quotation" (make 'quotation))
      ("Verse" (make 'verse))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind content-tag-menu
  ("Strong" (make 'strong))
  ("Emphasize" (make 'em))
  ("Definition" (make 'dfn))
  ("Sample" (make 'samp))
  ---
  ("Name" (make 'name))
  ("Person" (make 'person))
  ("Cite" (make 'cite*))
  ("Abbreviation" (make 'abbr))
  ("Acronym" (make 'acronym))
  ---
  ("Verbatim" (make 'verbatim))
  ("Keyboard" (make 'kbd))
  ("Code" (make 'code*))
  ("Variable" (make 'var)))

(menu-bind presentation-tag-menu
  (if (style-has? "std-markup-dtd")
      ("Underline" (make 'underline))
      ("Overline" (make 'overline)))
  ("Subscript" (make-script #f #t))
  ("Superscript" (make-script #t #t)))

(menu-bind size-tag-menu
  ("Really tiny" (make 'really-tiny))
  ("Tiny" (make 'tiny))
  ("Very small" (make 'very-small))
  ("Small" (make 'small))
  ("Normal" (make 'normal-size))
  ("Large" (make 'large))
  ("Very large" (make 'very-large))
  ("Huge" (make 'huge))
  ("Really huge" (make 'really-huge)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind itemize-menu
  ("Default" (make-tmlist 'itemize))
  ---
  ("Bullets" (make-tmlist 'itemize-dot))
  ("Dashes" (make-tmlist 'itemize-minus))
  ("Arrows" (make-tmlist 'itemize-arrow)))

(menu-bind enumerate-menu
  ("Default" (make-tmlist 'enumerate))
  ---
  ("1, 2, 3, ..." (make-tmlist 'enumerate-numeric))
  ("i, ii, iii, ..." (make-tmlist 'enumerate-roman))
  ("I, II, III, ..." (make-tmlist 'enumerate-Roman))
  ("a, b, c, ..." (make-tmlist 'enumerate-alpha))
  ("A, B, C, ..." (make-tmlist 'enumerate-Alpha)))

(menu-bind description-menu
  ("Default" (make-tmlist 'description))
  ---
  ("Compact" (make-tmlist 'description-compact))
  ("Aligned" (make-tmlist 'description-aligned))
  ("Dashes" (make-tmlist 'description-dash))
  ("Long" (make-tmlist 'description-long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generated content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind automatic-menu
  ("Table of contents" (make-aux "table-of-contents" "toc"))
  ("Bibliography" (interactive make-bib))
  ("Index" (make-aux "the-index" "idx"))
  ("Glossary" (make-aux "the-glossary" "gly"))
  ;;("List of figures" (make-aux* "the-glossary*" "figure" "List of figures"))
  ;;("List of tables" (make-aux* "the-glossary*" "table" "List of tables"))
  ("List of figures" (make-aux "list-of-figures" "figure"))
  ("List of tables" (make-aux "list-of-tables" "table")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style dependent menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-menu
  (if (and (style-has? "header-title-dtd")
	   (not (style-has? "header-letter-dtd"))
	   (not (style-has? "header-exam-dtd")))
      (-> "Title" (link title-menu)))
  (if (style-has? "header-letter-dtd")
      (-> "Header" (link letter-header-menu)))
  (if (style-has? "header-exam-dtd")
      (-> "Header" (link exam-header-menu)))
  (if (style-has? "book-style")
      (-> "Chapter" (link chapter-menu)))
  (if (and (style-has? "section-base-dtd")
	   (not (style-has? "header-exam-dtd")))
      (-> "Section" (link section-menu)))
  (if (style-has? "std-markup-dtd")
      (-> "Environment" (link environment-menu)))
  (if (style-has? "section-base-dtd")
      (-> "Automatic" (link automatic-menu)))
  ---
  (if (style-has? "std-markup-dtd")
      (-> "Content tag" (link content-tag-menu))
      (-> "Size tag" (link size-tag-menu)))
  (-> "Presentation tag" (link presentation-tag-menu))
  (if (style-has? "std-list-dtd")
      ---
      (-> "Itemize" (link itemize-menu))
      (-> "Enumerate" (link enumerate-menu))
      (-> "Description" (link description-menu))
      (when (inside-list-tag?) ("New item" (make-item))))
  ---
  (-> "Mathematics" (link insert-math-menu))
  (-> "Table" (link insert-table-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Link" (link insert-link-menu))
  (if (detailed-menus?)
      (if (style-has? "std-fold-dtd")
	  (-> "Fold" (link insert-fold-menu)))
      (-> "Animation" (link insert-animation-menu)))
  (if (and (style-has? "program-dtd") (detailed-menus?))
      (-> "Session" (link insert-session-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-icons
  ;;("Goedenmiddag" (display* "Hi there\n"))
  ;;(input (display* answer "\n") "string" '("Hello" "Bonjour") "0.5w")
  (if (and (style-has? "header-title-dtd")
	   (not (style-has? "header-letter-dtd"))
	   (not (style-has? "header-exam-dtd")))
      (=> (balloon (icon "tm_title.xpm") "Enter a title")
	  (link title-menu)))
  (if (style-has? "header-letter-dtd")
      (=> (balloon (icon "tm_title.xpm") "Make a letter environment")
	  (link letter-header-menu)))
  (if (style-has? "header-exam-dtd")
      (=> (balloon (icon "tm_title.xpm") "Enter title information")
	  (link exam-header-menu)))
  (if (style-has? "book-style")
      (=> (balloon (icon "tm_chapter.xpm") "Start a new chapter")
	  (link chapter-menu)))
  (if (and (style-has? "section-base-dtd")
	   (not (style-has? "header-exam-dtd")))
      (=> (balloon (icon "tm_section.xpm") "Start a new section")
	  (link section-menu)))
  (if (style-has? "std-markup-dtd")
      (=> (balloon (icon "tm_theorem.xpm") "Insert an environment")
	  (link environment-menu)))
  (=> (balloon (icon "tm_parstyle.xpm") "Set paragraph mode")
      ((balloon (icon "tm_left.xpm") "Align text to the left")
       (make-line-with "par-mode" "left"))
      ((balloon (icon "tm_center.xpm") "Center text")
       (make-line-with "par-mode" "center"))
      ((balloon (icon "tm_right.xpm") "Align text to the right")
       (make-line-with "par-mode" "right"))
      ((balloon (icon "tm_justify.xpm") "Justify text")
       (make-line-with "par-mode" "justify")))
  (=> (balloon (icon "tm_parindent.xpm") "Set paragraph margins")
      ("Left margin" (make-interactive-line-with "par-left"))
      ("Right margin" (make-interactive-line-with "par-right"))
      ("First indentation" (make-interactive-line-with "par-first")))
  (if (and (style-has? "env-float-dtd") (detailed-menus?))
      (if (not (inside? 'float))
	  (=> (balloon (icon "tm_pageins.xpm") "Make a page insertion")
	      (link insert-page-insertion-menu)))
      (if (inside? 'float)
	  (=> (balloon (icon "tm_floatpos.xpm") "Position floating object")
	      (link position-float-menu))))
;;((balloon (icon "tm_footnote.xpm") "Insert a footnote") ())
;;((balloon (icon "tm_margin.xpm") "Insert a marginal note") ())
;;((balloon (icon "tm_floating.xpm") "Insert a floating object") ())
;;((balloon (icon "tm_multicol.xpm") "Start multicolumn context") ())
  (if (style-has? "section-base-dtd")
      (=> (balloon (icon "tm_index.xpm")
		   "Insert automatically generated content")
	  (link automatic-menu)))
  (if (style-has? "std-list-dtd")
      /
      (=> (balloon (icon "tm_itemize.xpm") "Itemize")
	  (link itemize-menu))
      (=> (balloon (icon "tm_enumerate.xpm") "Enumerate")
	  (link enumerate-menu))
      (=> (balloon (icon "tm_description.xpm") "Description")
	  (link description-menu))
      (if (inside-list-tag?)
	  ((balloon (icon "tm_item.xpm") "Insert a new item")
	   (make-item))))
  (link text-format-icons)
  (link texmacs-insert-icons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode focus icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (itemize-enumerate-context? t))
  ((check "Numbered" "v" (enumerate-context? t))
   (numbered-toggle t)))

(tm-menu (focus-toggle-icons t)
  (:require (itemize-enumerate-context? t))
  ((check (balloon (icon "tm_numbered.xpm") "Toggle numbering") "v"
          (enumerate-context? t))
   (numbered-toggle t)))
