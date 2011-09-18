
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
  (:use (text format-text-edit)
        (text std-text-edit)
        (text tm-structure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind title-menu
  (when (not (inside? 'doc-data))
	("Insert title" (make-doc-data)))
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
;; Enunciations, quotations and programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind enunciation-menu
  (if (style-has? "env-theorem-dtd")
      ("Theorem" (make 'theorem))
      ("Proposition" (make 'proposition))
      ("Lemma" (make 'lemma))
      ("Corollary" (make 'corollary))
      ("Proof" (make 'proof))
      ---
      ("Axiom" (make 'axiom))
      ("Definition" (make 'definition))
      ("Notation" (make 'notation))
      ---
      ("Remark" (make 'remark))
      ("Note" (make 'note))
      ("Example" (make 'example))
      ("Warning" (make 'warning))
      ---)
  ("Exercise" (make 'exercise))
  ("Problem" (make 'problem))
  ("Solution" (make 'solution))
  ("Question" (make 'question))
  ("Answer" (make 'answer)))

(menu-bind prominent-menu
  ("Quote" (make 'quote-env))
  ("Quotation" (make 'quotation))
  ("Verse" (make 'verse))
  ---
  ("Padded" (make 'padded))
  ("Underlined" (make 'underlined))
  ("Lines around" (make 'bothlined))
  ("Framed" (make 'framed))
  ---
  ("Indent" (make 'indent))
  ("Jump in" (make 'jump-in))
  ("Compact" (make 'compact))
  ---
  ("Centered" (make 'center))
  ("Left aligned" (make 'left-aligned))
  ("Right aligned" (make 'right-aligned))
  (if (style-has? "ornaments-dtd")
      ---
      ("Manila paper" (make 'manila-paper))
      ("Rough paper" (make 'rough-paper))
      ("Ridged paper" (make 'ridged-paper))
      ("Pine" (make 'pine))
      ("Granite" (make 'granite))
      ("Metal" (make 'metal))))

(menu-bind code-menu
  ("Algorithm" (make 'algorithm))
  ("Pseudo code" (make 'pseudo-code))
  ---
  ("Indent" (make 'indent))
  ("Tabbed" (make 'tabbed))
  ---
  (-> "Inline code"
      ("C++" (make 'cpp))
      ("Mathemagix" (make 'mmx))
      ("Scheme" (make 'scm))
      ("Shell" (make 'shell))
      ("Verbatim" (make 'verbatim)))
  (-> "Block of code"
      ("C++" (make 'cpp-code))
      ("Mathemagix" (make 'mmx-code))
      ("Scheme" (make 'scm-code))
      ("Shell" (make 'shell-code))
      ("Verbatim" (make 'verbatim-code))))

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
  ("Superscript" (make-script #t #t))
  (if (and (style-has? "std-markup-dtd")
           (== (get-preference "experimental alpha") "on"))
      ---
      ("Pastel" (make 'pastel))
      ("Greyed" (make 'greyed))
      ("Light" (make 'light))))

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

(menu-bind list-menu
  ("Itemize" (make-tmlist 'itemize))
  ---
  ("Bullets" (make-tmlist 'itemize-dot))
  ("Dashes" (make-tmlist 'itemize-minus))
  ("Arrows" (make-tmlist 'itemize-arrow))
  ---
  ("Enumerate" (make-tmlist 'enumerate))
  ---
  ("1, 2, 3, ..." (make-tmlist 'enumerate-numeric))
  ("i, ii, iii, ..." (make-tmlist 'enumerate-roman))
  ("I, II, III, ..." (make-tmlist 'enumerate-Roman))
  ("a, b, c, ..." (make-tmlist 'enumerate-alpha))
  ("A, B, C, ..." (make-tmlist 'enumerate-Alpha))
  ---
  ("Description" (make-tmlist 'description))
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
  (if (or (style-has? "env-theorem-dtd")
          (style-has? "header-exam-dtd"))
      (-> "Enunciation" (link enunciation-menu)))
  (if (style-has? "std-markup-dtd")
      (-> "Prominent" (link prominent-menu)))
  (if (style-has? "std-markup-dtd")
      (-> "Program" (link code-menu)))
  (if (style-has? "section-base-dtd")
      (-> "Automatic" (link automatic-menu)))
  (if (style-has? "std-list-dtd")
      ---
      (-> "Itemize" (link itemize-menu))
      (-> "Enumerate" (link enumerate-menu))
      (-> "Description" (link description-menu)))
  ---
  (if (style-has? "std-markup-dtd")
      (-> "Content tag" (link content-tag-menu))
      (-> "Size tag" (link size-tag-menu)))
  (-> "Presentation tag" (link presentation-tag-menu))
  ---
  (-> "Mathematics" (link insert-math-menu))
  (-> "Table" (link insert-table-menu))
  (-> "Image" (link insert-image-menu))
  (-> "Link" (link insert-link-menu))
  (if (detailed-menus?)
      (if (style-has? "std-fold-dtd")
	  (-> "Fold" (link insert-fold-menu)))
      (-> "Animation" (link insert-animation-menu)))
  (if (and (style-has? "session-dtd") (detailed-menus?))
      (-> "Session" (link insert-session-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-icons
  ;;("Goedenmiddag" (display* "Hi there\n"))
  ;;(input (display* answer "\n") "string" '("Hello" "Bonjour") "0.5w")
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
  (if (or (style-has? "env-theorem-dtd")
          (style-has? "header-exam-dtd"))
      (=> (balloon (icon "tm_theorem.xpm") "Insert an enunciation")
	  (link enunciation-menu)))
  (if (style-has? "std-markup-dtd")
      (=> (balloon (icon "tm_prominent.xpm") "Insert a prominent piece of text")
	  (link prominent-menu)))
  (if (style-has? "std-markup-dtd")
      (=> (balloon (icon "tm_program.xpm") "Insert a computer program")
	  (link code-menu)))
  (if (style-has? "std-list-dtd")
      (=> (balloon (icon "tm_list.xpm") "Insert a list")
	  (link list-menu)))
  (if (and (style-has? "env-float-dtd") (detailed-menus?))
      ;;((balloon (icon "tm_footnote.xpm") "Insert a footnote") ())
      ;;((balloon (icon "tm_margin.xpm") "Insert a marginal note") ())
      ;;((balloon (icon "tm_floating.xpm") "Insert a floating object") ())
      ;;((balloon (icon "tm_multicol.xpm") "Start multicolumn context") ())
      (if (not (inside? 'float))
	  (=> (balloon (icon "tm_pageins.xpm") "Make a page insertion")
	      (link insert-page-insertion-menu)))
      (if (inside? 'float)
	  (=> (balloon (icon "tm_floatpos.xpm") "Position floating object")
	      (link position-float-menu))))
  (if (style-has? "section-base-dtd")
      (=> (balloon (icon "tm_index.xpm")
		   "Insert automatically generated content")
	  (link automatic-menu)))
  (if (style-has? "std-markup-dtd")
      /)
  (link text-format-icons)
  (link texmacs-insert-icons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for entering title information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-document-extra-menu t)
  (:require (document-propose-title?))
  ("Title" (make-doc-data)))

(tm-menu (focus-document-extra-icons t)
  (:require (document-propose-title?))
  (minibar
    ((balloon "Title" "Insert title") (make-doc-data))))

(tm-menu (focus-document-extra-menu t)
  (:require (document-propose-abstract?))
  ("Abstract" (make 'abstract)))

(tm-menu (focus-document-extra-icons t)
  (:require (document-propose-abstract?))
  (minibar
    ((balloon "Abstract" "Insert abstract") (make 'abstract))))

(tm-define (focus-can-move? t)
  (:require (doc-title-context? t))
  #f)

(tm-menu (focus-title-menu)
  ("Subtitle" (make-doc-data-element 'doc-subtitle))
  ("Author" (make-doc-data-element 'doc-author-data))
  ("Date" (make-doc-data-element 'doc-date))
  ("Today"
   (begin (make-doc-data-element 'doc-date) (make-arity 'date 0)))
  ("Note" (make-doc-data-element 'doc-note))
  ("TeXmacs notice" (begin (make-doc-data-element 'doc-note)
                           (make 'with-TeXmacs-text))))

(tm-menu (focus-title-hidden-menu)
  ("Running title" (make-doc-data-element 'doc-running-title))
  ("Running author" (make-doc-data-element 'doc-running-author))
  ("Keywords" (make-doc-data-element 'doc-keywords))
  ("M.S.C."
   (make-doc-data-element 'doc-AMS-class)))

(tm-menu (focus-title-icons)
  (assuming (doc-data-has-hidden?)
    ((check (balloon (icon "tm_show_hidden.xpm") "Show hidden") "v"
	    (doc-data-disactivated?))
     (doc-data-activate-toggle)))
  (mini #t
    (inert ("Title" (noop))))
  (=> (balloon (icon "tm_add.xpm") "Add title information")
      (link focus-title-menu)
      (-> "Hidden" (link focus-title-hidden-menu))))

(tm-menu (focus-ancestor-menu t)
  (:require (doc-title-context? t))
  (group "Title")
  (link focus-title-menu)
  ---
  (group "Hidden")
  (link focus-title-hidden-menu)
  ---)

(tm-menu (focus-ancestor-icons t)
  (:require (doc-title-context? t))
  (minibar (dynamic (focus-title-icons)))
  (glue #f #f 5 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for entering authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  (:require (doc-author-context? t))
  #f)

(tm-menu (focus-author-menu)
  ("Address" (make-author-data-element 'author-address))
  ("Email" (make-author-data-element 'author-email))
  ("Homepage" (make-author-data-element 'author-homepage))
  ("Note" (make-author-data-element 'author-note)))

(tm-menu (focus-author-icons)
  (mini #t
    (inert ("Author" (noop))))
  (=> (balloon (icon "tm_add.xpm") "Add author information")
      (link focus-author-menu)))

(tm-menu (focus-ancestor-menu t)
  (:require (doc-author-context? t))
  (group "Title")
  (link focus-title-menu)
  ---
  (group "Hidden")
  (link focus-title-hidden-menu)
  ---
  (group "Author")
  (link focus-author-menu)
  ---)

(tm-menu (focus-ancestor-icons t)
  (:require (doc-author-context? t))
  (minibar (dynamic (focus-title-icons)))
  (glue #f #f 5 0)
  (minibar (dynamic (focus-author-icons)))
  (glue #f #f 5 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-section-menu)
  (for (s (tree-search-sections (buffer-tree)))
    ((eval (tm/section-get-title-string s))
     (when (and (tree->path s) (section-context? s))
       (tree-go-to s 0 :end)))))

(tm-menu (focus-document-extra-menu t)
  (:require (previous-section))
  (-> "Sections" (link focus-section-menu)))

(tm-menu (focus-document-extra-icons t)
  (:require (previous-section))
  (mini #t
    (=> (eval (tm/section-get-title-string (previous-section)))
	(link focus-section-menu))))

(tm-menu (focus-extra-menu t)
  (:require (section-context? t))
  ---
  (-> "Go to section"
      (link focus-section-menu)))

(tm-menu (focus-extra-icons t)
  (:require (section-context? t))
  (glue #f #f 5 0)
  (mini #t
    (=> (eval (tm/section-get-title-string t))
	(link focus-section-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-tag-name l)
  (:require (in? l (algorithm-tag-list)))
  (with r (algorithm-root l)
    (with s (upcase-first (tree-name (tree r)))
      (string-replace s "-" " "))))

(tm-menu (focus-toggle-menu t)
  (:require (algorithm-context? t))
  (when (not (algorithm-named? (focus-tree)))
    ((check "Numbered" "v" (algorithm-numbered? (focus-tree)))
     (algorithm-toggle-number (focus-tree))))
  ((check "Named" "v" (algorithm-named? (focus-tree)))
   (algorithm-toggle-name t))
  ((check "Specified" "v" (algorithm-specified? (focus-tree)))
   (algorithm-toggle-specification t)))

(tm-menu (focus-toggle-icons t)
  (:require (algorithm-context? t))
  (when (not (algorithm-named? (focus-tree)))
    ((check (balloon (icon "tm_numbered.xpm") "Toggle numbering") "v"
	    (algorithm-numbered? (focus-tree)))
     (algorithm-toggle-number (focus-tree))))
  ((check (balloon (icon "tm_small_textual.xpm") "Toggle name") "v"
	  (algorithm-named? (focus-tree)))
   (algorithm-toggle-name t))
  ((check (balloon (icon "tm_specified.xpm") "Toggle specification") "v"
	  (algorithm-specified? (focus-tree)))
   (algorithm-toggle-specification t)))
