
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-doc.scm
;; DESCRIPTION : documentation for top-level documents
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-doc)
  (:use (generic generic-doc)
        (generic document-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level document documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-doc-top-generalities t)
  (let* ((st (get-style-list))
         (style (if (null? st) "none" (car st))))
    ($block
      ($para
        "The " ($tmdoc-link "main/text/man-structure" "current focus")
        " is on the entire document. "
        "In this particular situation, the focus toolbar "
        "displays some important information about your document, "
        "such as its style (" ($tmstyle style)
        "), paper size (" (get-init "page-type")
        ") and font size (" (get-init "font-base-size") "pt). "
        "The toolbar also indicates the current section "
        "at the position of your cursor.")
    
      ($para
        "The above properties and the current section can also "
        "be changed using the items on the focus toolbar or "
        "in the " ($menu "Focus") " menu. "
        "For instance, by clicking on the current style, "
        "paper size or font size, a pulldown menu will open "
        "from which you can modify the current setting. "
        "By clicking on the " ($tmdoc-icon "tm_add.xpm") " icon "
        "after the document style, you may select additional style packages.")

      ($para
        "Similarly, when clicking on the current section, "
        "a pulldown menu with all sections in the document will open, "
        "which allows you to quickly jump to a particular section. "
        "In the case when your cursor is at the start of your document, "
        "and no document title has been entered yet, "
        "then a " ($menu "Title") " button will appear "
        "for inserting a title. Similarly, if your cursor is "
        "just after the title, then an " ($menu "Abstract")
        " button will appear for entering the abstract."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level generalities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-doc t)
  (:require (tree-is-buffer? t))
  (let* ((style (get-style-list))
	 (stdoc (and (nnull? style) (tmdoc-search-style (car style)))))
    ($tmdoc
      ($tmdoc-title "Top level contextual help")
      ($when #t
	($unfolded-documentation "Top level editing"
	  (focus-doc-top-generalities t)))
      ($when #t
	($unfolded-documentation "Current document style"
	  ($when stdoc stdoc)
	  ($when (not stdoc)
	    ($explain `(tmstyle ,(car style))
	      "No documentation available."))))
      ($when (nnull? (cdr style))
	($unfolded-documentation "Other active style customizations"
	  ($for (pack (cdr style))
	    ($with packdoc (tmdoc-search-style pack)
	      ($when packdoc packdoc)
	      ($when (not packdoc)
		($explain `(tmpackage ,pack)
		  "No documentation available.")))))))))
