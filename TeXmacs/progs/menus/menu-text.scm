
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-text.scm
;; DESCRIPTION : menus for inserting structure in text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-text)
  (:use
    (texmacs edit edit-text) (texmacs edit edit-format)
    (texmacs edit edit-preamble)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind letter-header-menu
  (when (not (inside? "letter-header"))
	("Header" (make-big-compound "letter-header")))
  (when (inside? "letter-header")
	("Address" (make-header-compound "address"))
	("Date" (make-header-compound "letter-date"))
	("Today" (begin (make-header-compound "letter-date") (make-date)))
	("Destination" (make-header-compound "destination")))
  ---
  (when (not (inside? "letter-header"))
	("Opening" (make-big-compound "opening"))
	("Closing" (make-big-compound "closing"))
	("Signature" (make-big-compound "signature")))
  ---
  ("C.C." (make-big-compound "cc"))
  ("Encl." (make-big-compound "encl")))

(menu-bind exam-header-menu
  ("Class" (make-header-compound "class"))
  ("Date" (begin (go-end-of-header-element)
		 (make-compound-arg "title-date")))
  ("Title" (make-header-compound "title")))

(menu-bind title-menu
  (when (and (not (inside? "make-title")) (not (inside? "abstract")))
	("Make title" (begin (make-big-compound "make-title")
			     (make-compound-arg "title"))))
  (when (inside? "make-title")
	("Title" (make-header-compound "title"))
	("Author" (make-header-compound "author"))
	("Address" (make-header-compound "address"))
	("Email" (make-header-compound "title-email"))
	("Date" (make-header-compound "title-date"))
	---
	("TeXmacs notice" (make-compound "made-by-TeXmacs"))
	("Running title" (make-header-apply "header-title"))
	("Running author" (make-header-apply "header-author"))
	("Address block" (make-compound-arg "address-block"))
	("Today" (begin (make-header-compound "title-date")
			(make-date))))
  ---
  (when (and (not (inside? "make-title")) (not (inside? "abstract")))
	("Abstract" (make-big-compound "abstract")))
  (when (and (not (inside? "make-title")) (inside? "abstract"))
	("Keywords" (make-section-arg "keywords"))
	("A.M.S. subject classification" (make-section-arg "AMS-class"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind chapter-menu
  ("Chapter" (make-section-arg "chapter"))
  ("Appendix" (make-section-arg "appendix"))
  ("Prologue" (make-section "prologue"))
  ("Epilogue" (make-section "epilogue")))

(menu-bind section-menu
  ("Section" (make-section-arg "section"))
  ("Subsection" (make-section-arg "subsection"))
  ("Subsubsection" (make-section-arg "subsubsection"))
  ---
  ("Paragraph" (make-section-arg "paragraph"))
  ("Subparagraph" (make-section-arg "subparagraph")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theorem like environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind environment-menu
  (if (style-has? "header-exam-dtd")
      ("Exercise" (make-big-compound "exercise"))
      ("Problem" (make-big-compound "problem")))
  (if (not (style-has? "header-exam-dtd"))
      (if (style-has? "env-default-dtd")
	  ("Theorem" (make-big-compound "theorem"))
	  ("Proposition" (make-big-compound "proposition"))
	  ("Lemma" (make-big-compound "lemma"))
	  ("Corollary" (make-big-compound "corollary"))
	  ("Proof" (make-big-compound "proof"))
	  ("Axiom" (make-big-compound "axiom"))
	  ("Definition" (make-big-compound "definition"))
	  ---
	  ("Remark" (make-big-compound "remark"))
	  ("Note" (make-big-compound "note"))
	  ("Example" (make-big-compound "example"))
	  ("Warning" (make-big-compound "warning"))
	  ("Exercise" (make-big-compound "exercise"))
	  ("Problem" (make-big-compound "problem"))
	  ---)
      ("Verbatim" (make-big-compound "verbatim"))
      ("Code" (make-big-compound "code"))
      ("Quote" (make-big-compound "quote-env"))
      ("Quotation" (make-big-compound "quotation"))
      ("Verse" (make-big-compound "verse"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind content-tag-menu
  ("Strong" (make-compound-arg "strong"))
  ("Emphasize" (make-compound-arg "em"))
  ("Definition" (make-compound-arg "dfn"))
  ("Sample" (make-compound-arg "samp"))
  ---
  ("Name" (make-compound-arg "name"))
  ("Person" (make-compound-arg "person"))
  ("Cite" (make-compound-arg "cite*"))
  ("Abbreviation" (make-compound-arg "abbr"))
  ("Acronym" (make-compound-arg "acronym"))
  ---
  ("Verbatim" (make-compound-arg "verbatim"))
  ("Keyboard" (make-compound-arg "kbd"))
  ("Code" (make-compound-arg "code*"))
  ("Variable" (make-compound-arg "var")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enumerations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind itemize-menu
  ("Default" (make-tmlist "itemize"))
  ---
  ("Bullets" (make-tmlist "itemize-dot"))
  ("Dashes" (make-tmlist "itemize-minus"))
  ("Arrows" (make-tmlist "itemize-arrow")))

(menu-bind enumerate-menu
  ("Default" (make-tmlist "enumerate"))
  ---
  ("1, 2, 3, ..." (make-tmlist "enumerate-numeric"))
  ("i, ii, iii, ..." (make-tmlist "enumerate-roman"))
  ("I, II, III, ..." (make-tmlist "enumerate-Roman"))
  ("a, b, c, ..." (make-tmlist "enumerate-alpha"))
  ("A, B, C, ..." (make-tmlist "enumerate-Alpha")))

(menu-bind description-menu
  ("Default" (make-tmlist "description"))
  ---
  ("Compact" (make-tmlist "description-compact"))
  ("Aligned" (make-tmlist "description-aligned"))
  ("Dashes" (make-tmlist "description-dash"))
  ("Long" (make-tmlist "description-long")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generated content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind automatic-menu
  ("Table of contents" (make-aux "table-of-contents" "toc"))
  ("Bibliography" ...
   (interactive
    '("Bibliography style:" "Bibliography file:") 'make-bib))
  ("Index" (make-aux "the-index" "idx"))
  ("Glossary" (make-aux "the-glossary" "gly"))
  ("List of figures" (make-aux* "the-glossary*" "figure" "List of figures"))
  ("List of tables" (make-aux* "the-glossary*" "table" "List of tables")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting mathematical markup into the text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-mathematics-menu
  ("Formula" "$" (make-with "mode" "math"))
  (if (style-has? "env-math-dtd")
      ---
      ("Equation" (begin (make-big-compound "equation*") (temp-proof-fix)))
      ("Equations" (begin (make-compound-arg "eqnarray*") (temp-proof-fix)))))

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
  (if (and (style-has? "section-latex-dtd")
	   (not (style-has? "header-exam-dtd")))
      (-> "Section" (link section-menu)))
  (if (style-has? "std-markup-dtd")
      (-> "Environment" (link environment-menu))
      (-> "Content tag" (link content-tag-menu)))
  (if (style-has? "common-base-dtd")
      ---)
  (if (style-has? "std-list-dtd")
      (-> "Itemize" (link itemize-menu))
      (-> "Enumerate" (link enumerate-menu))
	(-> "Description" (link description-menu))
	(when (or (inside-list?) (inside-description?))
	      ("New item" (make-item)))
	---)
  (-> "Mathematics" (link insert-mathematics-menu))
  (if (style-has? "program-dtd")
      (-> "Session"
	  (link session-menu)
	  ---
	  ("Other" ... (interactive
			'("Session type:" "Session name:") 'make-session))))
  (if (style-has? "section-automatic-dtd")
      (-> "Automatic" (link automatic-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-text-icons
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
  (if (and (style-has? "section-latex-dtd")
	   (not (style-has? "header-exam-dtd")))
      (=> (balloon (icon "tm_section.xpm") "Start a new section")
	  (link section-menu)))
  (if (style-has? "std-markup-dtd")
      (=> (balloon (icon "tm_theorem.xpm") "Insert an environment")
	  (link environment-menu)))
  (=> (balloon (icon "tm_parstyle.xpm") "Set paragraph mode")
      ((balloon (icon "tm_left.xpm") "Align text to the left")
       (make-line-with "paragraph mode" "left"))
      ((balloon (icon "tm_center.xpm") "Center text")
       (make-line-with "paragraph mode" "center"))
      ((balloon (icon "tm_right.xpm") "Align text to the right")
       (make-line-with "paragraph mode" "right"))
      ((balloon (icon "tm_justify.xpm") "Justify text")
       (make-line-with "paragraph mode" "justify")))
  (=> (balloon (icon "tm_parindent.xpm") "Set paragraph margins")
      ("Left margin" (interactive '("Left margin:") 'set-left-margin))
      ("Right margin" (interactive '("Right margin:") 'set-right-margin))
      ("First indentation"
       (interactive '("First indentation:") 'set-first-indent)))
  (if (style-has? "env-float-dtd")
      (if (not (inside? "float"))
	  (=> (balloon (icon "tm_pageins.xpm") "Make a page insertion")
	      (link insert-page-insertion-menu)))
      (if (inside? "float")
	  (=> (balloon (icon "tm_floatpos.xpm") "Position floating object")
	      (link position-float-menu))))
;;((balloon (icon "tm_footnote.xpm") "Insert a footnote") ())
;;((balloon (icon "tm_margin.xpm") "Insert a marginal note") ())
;;((balloon (icon "tm_floating.xpm") "Insert a floating object") ())
;;((balloon (icon "tm_multicol.xpm") "Start multicolumn context") ())
  (if (style-has? "section-automatic-dtd")
      (=> (balloon (icon "tm_index.xpm")
		   "Insert automatically generated content")
	  (link automatic-menu)))
  (if (style-has? "std-list-dtd")
      |
      (=> (balloon (icon "tm_itemize.xpm") "Itemize")
	  (link itemize-menu))
      (=> (balloon (icon "tm_enumerate.xpm") "Enumerate")
	  (link enumerate-menu))
      (=> (balloon (icon "tm_description.xpm") "Description")
	  (link description-menu))
      (if (or (inside-list?) (inside-description?))
	  ((balloon (icon "tm_item.xpm") "Insert a new item#(A-;)")
	   (make-item))))
  |
  (=> (balloon (icon "tm_math.xpm") "Insert mathematics")
      (link insert-mathematics-menu))
  (if (style-has? "program-dtd")
      (=> (balloon (icon "tm_shell.xpm")
		   "Start an interactive session")
	  (link session-menu)
	  ---
	  ("Other" ...
	   (interactive
	    '("Session type:" "Session name:") 'make-session)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-modifier-icons
  (if (style-has? "std-markup-dtd")
      ;;((balloon
      ;;(text (roman rm bold right 12 600) "S")
      ;;"Write bold text#(A-C-b)")
      ;;(make-with "font series" "bold"))
      ((balloon (icon "tm_emphasize.xpm") "Emphasize text#(F5)")
       (make-compound-arg "em"))
      ((balloon (icon "tm_strong.xpm") "Write strong text#(F6)")
       (make-compound-arg "strong"))
      ((balloon (icon "tm_verbatim.xpm") "Write verbatim text#(F7)")
       (make-compound-arg "verbatim"))
      ((balloon (icon "tm_sansserif.xpm") "Write sample text#(F8)")
       (make-compound-arg "samp"))
      ((balloon (icon "tm_name.xpm") "Write a name#(S-F6)")
       (make-compound-arg "name")))
  (if (not (style-has? "std-markup-dtd"))
      ((balloon (icon "tm_italic.xpm") "Write italic text#(A-C-i)")
       (make-with "font shape" "italic"))
      ((balloon (icon "tm_bold.xpm") "Write bold text#(A-C-b)")
       (make-with "font series" "bold"))
      ((balloon (icon "tm_typewriter.xpm") "Use a typewriter font#(A-C-t)")
       (make-with "font family" "tt"))
      ((balloon (icon "tm_sansserif.xpm") "Use a sans serif font#(A-C-s)")
       (make-with "font family" "ss"))
      ((balloon (icon "tm_smallcaps.xpm") "Use small capitals#(A-C-p)")
       (make-with "font shape" "small-caps"))))
