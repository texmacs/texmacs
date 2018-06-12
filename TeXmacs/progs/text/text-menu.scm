
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
  (:use (text text-edit)
        (text text-structure)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Format menu in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind full-text-format-menu
  (group "Font")
  (if (new-fonts?)
      ;;(link new-text-font-menu))
      (link text-font-menu))
  (if (not (new-fonts?))
      (link text-font-menu))
  (if (simple-menus?)
      (-> "Color" (link color-menu)))
  (if (detailed-menus?)
      ---
      (group "Text")
      (link text-properties-menu))
  ---
  (group "Paragraph")
  (link paragraph-menu)
  ---
  (group "Page")
  (link page-menu))

(menu-bind compressed-text-format-menu
  (if (new-fonts?)
      ("Font" (interactive open-font-selector)))
  (if (not (new-fonts?))
      (-> "Font" (link text-font-menu)))
  ("Paragraph" (open-paragraph-format))
  ("Page" (open-page-format))
  (when (inside? 'table)
    ("Cell" (open-cell-properties))
    ("Table" (open-table-properties)))
  ---
  (-> "Whitespace" (link space-menu))
  (-> "Indentation" (link indentation-menu))
  (-> "Break" (link break-menu))
  (when (and (selection-active-small?) (tm-atomic? (selection-tree)))
    ("Hyphenate as" (interactive hyphenate-selection-as)))
  ---
  (-> "Color"
      (if (== (get-preference "experimental alpha") "on")
          (-> "Opacity" (link opacity-menu))
          ---)
      (link color-menu))
  (-> "Adjust" (link adjust-menu))
  (-> "Transform" (link linear-transform-menu))
  (-> "Specific" (link specific-menu))
  (-> "Special" (link format-special-menu))
  (-> "Font effects" (link text-font-effects-menu))
  (assuming (== (get-preference "bitmap effects") "on")
    (-> "Graphical effects" (link text-effects-menu))))

(menu-bind text-format-menu
  (if (use-menus?)
      (link full-text-format-menu))
  (if (use-popups?)
      (link compressed-text-format-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document headers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind title-menu
  (when (not (inside? 'doc-data))
        ("Insert title" (make-doc-data)))
  (when (and (not (inside? 'doc-data)) (not (inside? 'abstract-data)))
        ("Abstract" (make-abstract-data))))

(menu-bind letter-header-menu
  (when (not (inside? 'letter-header))
        ("Header" (make 'letter-header)))
  (when (inside? 'letter-header)
        ("Address" (make-header 'address))
        ("Date" (make-header 'letter-date))
        ("Today" (begin (make-header 'letter-date) (make 'date 0)))
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
      ("Convention" (make 'convention))
      ---
      ("Remark" (make 'remark))
      ("Note" (make 'note))
      ("Example" (make 'example))
      ("Warning" (make 'warning))
      ("Acknowledgments" (make 'acknowledgments*))
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
  ("Indent" (make 'indent))
  ("Jump in" (make 'jump-in))
  ("Compact" (make 'compact))
  ---
  ("Centered" (make 'center))
  ("Left aligned" (make 'left-aligned))
  ("Right aligned" (make 'right-aligned))
  ---
  ("Padded" (make 'padded))
  ("Overlined" (make 'overlined))
  ("Underlined" (make 'underlined))
  ("Lines around" (make 'bothlined))
  ("Framed" (make 'framed))
  ("Ornamented" (make 'ornamented))
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
  ("Pseudo code" (make 'render-code))
  ---
  ("Indent" (make 'indent))
  ("Tabbed" (make 'tabbed))
  ---
  (-> "Inline code"
      ("C++" (make 'cpp))
      ("Mathemagix" (make 'mmx))
      ("Python" (make 'python))
      ("R" (make 'r))
      ("Scala" (make 'scala))
      ("Scheme" (make 'scm))
      ("Shell" (make 'shell))
      ("Scilab" (make 'scilab))
      ("Verbatim" (make 'verbatim)))
  (-> "Block of code"
      ("C++" (make 'cpp-code))
      ("Mathemagix" (make 'mmx-code))
      ("Python" (make 'python-code))
      ("R" (make 'r-code))
      ("Scala" (make 'scala-code))
      ("Scheme" (make 'scm-code))
      ("Shell" (make 'shell-code))
      ("Scilab" (make 'scilab-code))
      ("Verbatim" (make 'verbatim-code))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes and floating objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind note-menu
  (when (not (or (inside? 'float) (inside? 'footnote)))
    ("Footnote" (make 'footnote))
    ("Marginal note" (make-marginal-note))
    ;;("Balloon" (make-balloon))
    ---
    ("Floating object" (make-insertion "float"))
    ("Floating phantom" (insert '(phantom-float "float" "hf")))
    ("Floating figure" (begin (make-insertion "float") (make 'big-figure)))
    ("Floating table" (begin (make-insertion "float")
                             (insert-go-to '(big-table "" "") '(0 0))
                             (make 'tabular)))
    ("Floating algorithm" (begin (make-insertion "float") (make 'algorithm)))))

(menu-bind position-marginal-note-menu
  (group "Horizontal position")
  ("Automatic" (set-marginal-note-hpos "normal"))
  ("Left" (set-marginal-note-hpos "left"))
  ("Right" (set-marginal-note-hpos "right"))
  ("Left on even pages" (set-marginal-note-hpos "even-left"))
  ("Right on even pages" (set-marginal-note-hpos "even-right"))
  ---
  (group "Vertical alignment")
  ("Top" (set-marginal-note-valign "t"))
  ("Center" (set-marginal-note-valign "c"))
  ("Bottom" (set-marginal-note-valign "b")))

(tm-define (marginal-note-context? t)
  (tree-is? t 'marginal-note))

(tm-menu (focus-float-menu t)
  (:require (marginal-note-context? t))
  ---
  (link position-marginal-note-menu)
  ---)

(menu-bind float-menu
  ("Top" (toggle-insertion-positioning "t"))
  ("Here" (toggle-insertion-positioning "h"))
  ("Bottom" (toggle-insertion-positioning "b"))
  ("Other pages" (toggle-insertion-positioning-not "f")))

(tm-menu (focus-float-menu t)
  (:require (rich-float-context? t))
  (if (in-multicol-style?)
      ("Wide float" (float-toggle-wide t)))
  (-> "Allowed positions" (link float-menu))
  (if (cursor-at-anchor?)
      ("Go to float" (go-to-float)))
  (if (not (cursor-at-anchor?))
      ("Go to anchor" (go-to-anchor))))

(tm-menu (focus-float-menu t)
  (:require (phantom-float-context? t))
  (-> "Allowed positions" (link float-menu)))

(tm-menu (focus-float-menu t)
  (:require (floatable-context? t))
  ("Make floating" (turn-floating t)))

(tm-menu (focus-float-menu t)
  (:require (tree-is? t 'footnote))
  (if (in-multicol-style?)
      ("Wide footnote" (float-toggle-wide t)))
  (if (cursor-at-anchor?)
      ("Go to footnote" (go-to-float)))
  (if (not (cursor-at-anchor?))
      ("Go to anchor" (go-to-anchor))))

(menu-bind position-balloon-menu
  (group "Horizontal alignment")
  ("Outer left" (set-balloon-halign "Left"))
  ("Inner left" (set-balloon-halign "left"))
  ("Center" (set-balloon-halign "center"))
  ("Inner right" (set-balloon-halign "right"))
  ("Outer right" (set-balloon-halign "Right"))
  ---
  (group "Vertical alignment")
  ("Outer bottom" (set-balloon-valign "Bottom"))
  ("Inner bottom" (set-balloon-valign "bottom"))
  ("Center" (set-balloon-valign "center"))
  ("Inner top" (set-balloon-valign "top"))
  ("Outer top" (set-balloon-valign "Top")))

(tm-menu (focus-float-menu t)
  (:require (balloon-context? t))
  ---
  (link position-balloon-menu)
  ---)

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
  ("Variable" (make 'var))
  ---
  ("Deleted" (make 'deleted))
  ("Fill out" (make 'fill-out))
  ("Marked" (make 'marked)))

(menu-bind presentation-tag-menu
  (if (style-has? "std-markup-dtd")
      ("Underline" (make 'underline))
      ("Overline" (make 'overline))
      ("Strike through" (make 'strike-through))
      ---)
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

(menu-bind text-language-menu
  (for (lan supported-languages)
    (when (supported-language? lan)
      ((check (eval (upcase-first lan)) "v" (test-env? "language" lan))
       (make (string->symbol lan))))))

(tm-menu (local-supported-scripts-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (scripts-list)))
    (for (name l)
      ((eval (scripts-name name))
       (make-with "prog-scripts" name)))))

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
  ("Long" (make-tmlist 'description-long))
  ("Paragraphs" (make-tmlist 'description-paragraphs)))

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
  ("Long" (make-tmlist 'description-long))
  ("Paragraphs" (make-tmlist 'description-paragraphs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically generated content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind automatic-menu
  ("Table of contents" (make-aux "table-of-contents" "toc-prefix" "toc"))
  (assuming (get-boolean-preference "gui:new bibliography dialogue")
    ("Bibliography" (interactive open-bibliography-inserter)))
  (assuming (not (get-boolean-preference "gui:new bibliography dialogue"))
    (if (with-database-tool?)
        ("Bibliography" (make-database-bib)))
    (if (not (with-database-tool?))
        ("Bibliography" (choose-file make-bib "Bibliography file" "BibTeX"))))
  ("Index" (make-aux "the-index" "index-prefix" "idx"))
  ("Glossary" (make-aux "the-glossary" "glossary-prefix" "gly"))
  ;;("List of figures" (make-aux* "the-glossary*" "figure-list-prefix" "figure" "List of figures"))
  ;;("List of tables" (make-aux* "the-glossary*" "table-list-prefix" "table" "List of tables"))
  ("List of figures" (make-aux "list-of-figures" "figure-list-prefix" "figure"))
  ("List of tables" (make-aux "list-of-tables" "table-list-prefix" "table")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text menus for inserting block content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-block-menu
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
  (if (and (style-has? "env-float-dtd") (detailed-menus?))
      (-> "Note" (link note-menu)))
  (if (style-has? "section-base-dtd")
      (-> "Automatic" (link automatic-menu)))
  (if (style-has? "std-list-dtd")
      ---
      (-> "Itemize" (link itemize-menu))
      (-> "Enumerate" (link enumerate-menu))
      (-> "Description" (link description-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text menus for inserting inline content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-inline-menu
  (if (style-has? "std-markup-dtd")
      (-> "Content tag" (link content-tag-menu))
      (-> "Size tag" (link size-tag-menu)))
  (-> "Presentation tag" (link presentation-tag-menu))
  (if (style-has? "std-markup-dtd")
      (-> "Language" (link text-language-menu)))
  (-> "Scripts" (link local-supported-scripts-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style dependent menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-menu
  (link text-block-menu)
  ---
  (link text-inline-menu)
  ---
  (link texmacs-insert-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for inserting block markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-block-icons
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
      (=> (balloon (icon "tm_pageins.xpm") "Insert a note or a floating object")
          (link note-menu)))
  (if (style-has? "section-base-dtd")
      (=> (balloon (icon "tm_index.xpm")
                   "Insert automatically generated content")
          (link automatic-menu))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for modifying text properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-format-icons
  (if (not (style-has? "std-markup-dtd"))
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
      /)
  (if (and (style-has? "std-markup-dtd") (not (in-source?)))
      ;;((balloon
      ;;(text (roman rm bold right 12 600) "S")
      ;;"Write bold text")
      ;;(make-with "font-series" "bold"))
      ((balloon (icon "tm_emphasize.xpm") "Emphasize text")
       (make 'em))
      ((balloon (icon "tm_strong.xpm") "Write strong text")
       (make 'strong))
      ((balloon (icon "tm_verbatim.xpm") "Write verbatim text")
       (make 'verbatim))
      ((balloon (icon "tm_sansserif.xpm") "Write sample text")
       (make 'samp))
      ((balloon (icon "tm_name.xpm") "Write a name")
       (make 'name)))
  (if (or (not (style-has? "std-markup-dtd")) (in-source?))
      ((balloon (icon "tm_italic.xpm") "Write italic text")
       (make-with "font-shape" "italic"))
      ((balloon (icon "tm_bold.xpm") "Write bold text")
       (make-with "font-series" "bold"))
      ((balloon (icon "tm_typewriter.xpm") "Use a typewriter font")
       (make-with "font-family" "tt"))
      ((balloon (icon "tm_sansserif.xpm") "Use a sans serif font")
       (make-with "font-family" "ss"))
      ((balloon (icon "tm_smallcaps.xpm") "Use small capitals")
       (make-with "font-shape" "small-caps")))
  (=> (balloon (icon "tm_color.xpm") "Select a foreground color")
      (link color-menu)))

(menu-bind text-inline-icons
  (link text-format-icons))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind text-icons
  ;;("Goedenmiddag" (display* "Hi there\n"))
  ;;(mini #t (input (display* answer "\n") "string" '("Hello" "Bonjour") "0.5w"))
  (link text-block-icons)
  (if (style-has? "std-markup-dtd")
      /)
  (link text-inline-icons)
  (if (in-manual?) (link tmdoc-icons))
  (link texmacs-insert-icons)
  (if (and (in-presentation?) (not (visible-icon-bar? 0)))
    /
    (link dynamic-icons)))

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
  ("Abstract" (make-abstract-data)))

(tm-menu (focus-document-extra-icons t)
  (:require (document-propose-abstract?))
  (minibar
    ((balloon "Abstract" "Insert abstract") (make-abstract-data))))

(tm-define (focus-can-move? t)
  (:require (doc-title-context? t))
  #f)

(tm-menu (focus-title-menu)
  ("Subtitle" (make-doc-data-element 'doc-subtitle))
  ("Author" (make-doc-data-element 'doc-author))
  ("Date" (make-doc-data-element 'doc-date))
  ("Today"
   (begin (make-doc-data-element 'doc-date) (make 'date 0)))
  ("Miscellanous" (make-doc-data-element 'doc-misc))
  ("Note" (make-doc-data-element 'doc-note))
  ("TeXmacs notice" (begin (make-doc-data-element 'doc-note)
                           (make 'with-TeXmacs-text))))

(tm-menu (focus-title-hidden-menu)
  ("Running title" (make-doc-data-element 'doc-running-title))
  ("Running author" (make-doc-data-element 'doc-running-author)))

(tm-menu (focus-title-option-menu)
  ("No clustering"
   (set-doc-title-clustering #f))
  ("Cluster by affiliation"
   (set-doc-title-clustering "cluster-by-affiliation"))
  ("Maximal clustering"
   (set-doc-title-clustering "cluster-all")))

(tm-menu (focus-title-icons)
  (assuming (doc-data-has-hidden?)
    ((check (balloon (icon "tm_show_hidden.xpm") "Show hidden") "v"
            (doc-data-deactivated?))
     (doc-data-activate-toggle)))
  (mini #t
    (inert ("Title" (noop))))
  (=> (balloon (icon "tm_add.xpm") "Add title information")
      (link focus-title-menu)
      (-> "Hidden" (link focus-title-hidden-menu)))
  (=> (balloon (icon "tm_focus_prefs.xpm") "Title presentation options")
      (link focus-title-option-menu)))

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
  //)

(tm-define (focus-has-preferences? t)
  (:require (tree-in? t '(doc-note author-note)))
  #f)

(tm-menu (focus-tag-edit-menu l)
  (:require (and (in? l '(author-email author-homepage author-misc))
                 (or (test-doc-title-clustering? "cluster-all")
                     (test-doc-title-clustering? "cluster-by-affiliation"))))
  (with l* (symbol-append l '-note)
    (dynamic (focus-tag-edit-menu l*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for entering authors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  (:require (doc-author-context? t))
  #f)

(tm-menu (focus-author-menu)
  ("Affiliation" (make-author-data-element 'author-affiliation))
  ("Email" (make-author-data-element 'author-email))
  ("Homepage" (make-author-data-element 'author-homepage))
  ("Miscellanous" (make-author-data-element 'author-misc))
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
  //
  (minibar (dynamic (focus-author-icons)))
  //)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-can-move? t)
  (:require (abstract-data-context? t))
  #f)

(tm-menu (focus-abstract-menu)
  ("Arxiv category" (make-abstract-data-element 'abstract-arxiv))
  ("A.C.M. computing class" (make-abstract-data-element 'abstract-acm))
  ("A.M.S. subject class" (make-abstract-data-element 'abstract-msc))
  ("Physics and astronomy class" (make-abstract-data-element 'abstract-pacs))
  ("Keywords" (make-abstract-data-element 'abstract-keywords)))

(tm-define (focus-tag-name l)
  (:require (== l 'abstract))
  "Abstract text")

(tm-menu (focus-abstract-icons)
  (mini #t
    (inert ("Abstract" (noop))))
  (=> (balloon (icon "tm_add.xpm") "Add abstract information")
      (link focus-abstract-menu)))

(tm-menu (focus-ancestor-menu t)
  (:require (abstract-data-context? t))
  (group "Abstract")
  (link focus-abstract-menu)
  ---)

(tm-menu (focus-ancestor-icons t)
  (:require (abstract-data-context? t))
  (minibar (dynamic (focus-abstract-icons)))
  //)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-show-in-menu? l)
  (:require (and (string-ends? l "-numbered")
                 (in? (string->symbol (string-drop-right l 9))
                      (section-tag-list))))
  #f)

(tm-define (parameter-show-in-menu? l)
  (:require (== l "appendix-prefix"))
  #f)

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
  (mini #t
    (=> (eval (tm/section-get-title-string t))
        (link focus-section-menu))))

(tm-define (child-proposals t i)
  (:require (and (tree-in? t '(bibliography bibliography*)) (== i 1)))
  (bib-standard-styles))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menu for lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-options l)
  (:require (in? l (list-tag-list)))
  (list "compact-list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menu for theorems and proofs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (standard-options l)
  (:require (or (in? l (enunciation-tag-list))
                (in? l (render-enunciation-tag-list))
                (in? l '(proof render-proof))))
  (list "number-europe" ;; "number-us"
        "number-long-article"
        "framed-theorems" "hanging-theorems"))

(tm-menu (focus-extra-menu t)
  (:require (dueto-supporting-context? t))
  ---
  (when (not (dueto-added? t))
    ("Due to" (dueto-add t))))

(tm-menu (focus-extra-icons t)
  (:require (dueto-supporting-context? t))
  //
  (when (not (dueto-added? t))
    ("Due to" (dueto-add t))))

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

(tm-define (standard-options l)
  (:require (in? l (algorithm-tag-list)))
  (list "centered-program" "framed-program"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus menus for floating objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-float-icons t)
  (:require (marginal-note-context? t))
  (=> (balloon (icon "tm_position_float.xpm")
               "Position of marginal note")
      (link position-marginal-note-menu)))

(tm-menu (focus-float-icons t)
  (:require (rich-float-context? t))
  (if (in-multicol-style?)
      ((check (balloon (icon "tm_wide_float.xpm") "Make float wide") "v"
              (float-wide? (focus-tree)))
       (float-toggle-wide t)))
  (=> (balloon (icon "tm_position_float.xpm")
               "Allowed positions of floating object")
      (link float-menu))
  ((balloon (icon "tm_anchor.xpm")
            "Go to anchor or float")
   (cursor-toggle-anchor)))

(tm-menu (focus-float-icons t)
  (:require (phantom-float-context? t))
  (=> (balloon (icon "tm_position_float.xpm")
               "Allowed positions of floating object")
      (link float-menu)))

(tm-menu (focus-float-icons t)
  (:require (floatable-context? t))
  ((balloon (icon "tm_position_float.xpm")
            "Let the environment float")
   (turn-floating (tree-innermost floatable-context?))))

(tm-menu (focus-float-icons t)
  (:require (footnote-context? t))
  (if (in-multicol-style?)
      ((check (balloon (icon "tm_wide_float.xpm") "Make footnote wide") "v"
              (float-wide? (focus-tree)))
       (float-toggle-wide t)))
  ((balloon (icon "tm_anchor.xpm")
            "Go to anchor or footnote")
   (cursor-toggle-anchor)))

(tm-menu (focus-float-icons t)
  (:require (balloon-context? t))
  (=> (balloon (icon "tm_position_float.xpm")
               "Alignment of balloon")
      (link position-balloon-menu)))

(tm-define (standard-options l)
  (:require (in? l (numbered-unnumbered-append
                     (append (small-figure-tag-list) (big-figure-tag-list)))))
  (list "captions-above"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Possibility to rename titled environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (titled-context? t))
  ((check "Named" "v" (titled-named? (focus-tree)))
   (titled-toggle-name t))
  (dynamic (former t)))

(tm-menu (focus-toggle-icons t)
  (:require (titled-context? t))
  ((check (balloon (icon "tm_small_textual.xpm") "Toggle name") "v"
          (titled-named? (focus-tree)))
   (titled-toggle-name t))
  (dynamic (former t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Framed environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (or (frame-context? t) (frame-titled-context? t)))
  ((check "Named" "v" (frame-titled? (focus-tree)))
   (frame-toggle-title t))
  (dynamic (former t)))

(tm-menu (focus-toggle-icons t)
  (:require (or (frame-context? t) (frame-titled-context? t)))
  ((check (balloon (icon "tm_small_textual.xpm") "Toggle name") "v"
          (frame-titled? (focus-tree)))
   (frame-toggle-title t))
  (dynamic (former t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming automatically generated sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (automatic-section-context? t))
  (dynamic (former t))
  ("Rename" (interactive automatic-section-rename)))

(tm-menu (focus-toggle-icons t)
  (:require (automatic-section-context? t))
  (dynamic (former t))
  ((balloon (icon "tm_small_textual.xpm") "Rename section")
   (interactive automatic-section-rename)))
