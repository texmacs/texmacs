
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
  (:use (texmacs edit edit-text) (texmacs edit edit-format)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind letter-header-menu
  (when (not (inside? "letter-header"))
	("Header" (make 'letter-header)))
  (when (inside? "letter-header")
	("Address" (make-header 'address))
	("Date" (make-header 'letter-date))
	("Today" (begin (make-header 'letter-date) (make-arity 'date 0)))
	("Destination" (make-header 'destination)))
  ---
  (when (not (inside? "letter-header"))
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

(menu-bind title-menu
  (when (and (not (inside? "make-title")) (not (inside? "abstract")))
	("Make title" (begin (make 'make-title) (make 'title))))
  (when (inside? "make-title")
	("Title" (make-header 'title))
	("Author" (make-header 'author))
	("Address" (make-header 'address))
	("Email" (make-header 'title-email))
	("Date" (make-header 'title-date))
	---
	("TeXmacs notice" (make 'made-by-TeXmacs))
	("Running title" (make-header 'header-title))
	("Running author" (make-header 'header-author))
	("Address block" (make 'address-block))
	("Today" (begin (make-header 'title-date)
			(make-arity 'date 0))))
  ---
  (when (and (not (inside? "make-title")) (not (inside? "abstract")))
	("Abstract" (make 'abstract)))
  (when (and (not (inside? "make-title")) (inside? "abstract"))
	("Keywords" (make-section 'keywords))
	("A.M.S. subject classification" (make-section 'AMS-class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind chapter-menu
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
      (if (style-has? "env-default-dtd")
	  ("Theorem" (make 'theorem))
	  ("Proposition" (make 'proposition))
	  ("Lemma" (make 'lemma))
	  ("Corollary" (make 'corollary))
	  ("Proof" (make 'proof))
	  ("Axiom" (make 'axiom))
	  ("Definition" (make 'definition))
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
      ("Equation" (begin (make 'equation*) (temp-proof-fix)))
      ("Equations" (begin (make 'eqnarray*) (temp-proof-fix)))))

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
       (make-line-with "par-mode" "left"))
      ((balloon (icon "tm_center.xpm") "Center text")
       (make-line-with "par-mode" "center"))
      ((balloon (icon "tm_right.xpm") "Align text to the right")
       (make-line-with "par-mode" "right"))
      ((balloon (icon "tm_justify.xpm") "Justify text")
       (make-line-with "par-mode" "justify")))
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
  ;((balloon (icon "tm_insert_graphics.xpm") "Insert graphics")
  ;(make-graphics))
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
      ;;(make-with "font-series" "bold"))
      ((balloon (icon "tm_emphasize.xpm") "Emphasize text#(F5)")
       (make 'em))
      ((balloon (icon "tm_strong.xpm") "Write strong text#(F6)")
       (make 'strong))
      ((balloon (icon "tm_verbatim.xpm") "Write verbatim text#(F7)")
       (make 'verbatim))
      ((balloon (icon "tm_sansserif.xpm") "Write sample text#(F8)")
       (make 'samp))
      ((balloon (icon "tm_name.xpm") "Write a name#(S-F6)")
       (make 'name)))
  (if (not (style-has? "std-markup-dtd"))
      ((balloon (icon "tm_italic.xpm") "Write italic text#(A-C-i)")
       (make-with "font-shape" "italic"))
      ((balloon (icon "tm_bold.xpm") "Write bold text#(A-C-b)")
       (make-with "font-series" "bold"))
      ((balloon (icon "tm_typewriter.xpm") "Use a typewriter font#(A-C-t)")
       (make-with "font-family" "tt"))
      ((balloon (icon "tm_sansserif.xpm") "Use a sans serif font#(A-C-s)")
       (make-with "font-family" "ss"))
      ((balloon (icon "tm_smallcaps.xpm") "Use small capitals#(A-C-p)")
       (make-with "font-shape" "small-caps"))))
