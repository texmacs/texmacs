
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-doc.scm
;; DESCRIPTION : documentation of table tags
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-doc)
  (:use (generic generic-doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert and remove children
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-insert t)
  (:require (table-markup-context? t))
  ($with lab (tree-label t)
    ($para
      "The " ($markup lab) " tag is a tabular environment. "
      "New rows and columns can be inserted using the following "
      "keyboard shortcuts, menu entries, or icons on the focus toolbar: ")
    ($description-long
      ($when (structured-horizontal? t)
        ($describe-item
            ($inline ($shortcut (structured-insert-left)) ", "
                     ($menu "Focus" "Insert left") ", "
                     ($tmdoc-icon "tm_insert_left.xpm"))
          "Insert a new column at the left-hand side of the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-insert-right)) ", "
                     ($menu "Focus" "Insert right") ", "
                     ($tmdoc-icon "tm_insert_right.xpm"))
          "Insert a new column at the right-hand side of the cursor."))
      ($when (structured-vertical? t)
        ($describe-item
            ($inline ($shortcut (structured-insert-up)) ", "
                     ($menu "Focus" "Insert above") ", "
                     ($tmdoc-icon "tm_insert_up.xpm"))
          "Insert a new row above the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-insert-down)) ", "
                     ($menu "Focus" "Insert down") ", "
                     ($tmdoc-icon "tm_insert_down.xpm"))
          "Insert a new row below the cursor.")))
    ($para
      "Existing rows and columns can be removed as follows:")
    ($description-long
      ($when (structured-horizontal? t)
        ($describe-item
            ($inline ($shortcut (structured-remove-left)) ", "
                     ($menu "Focus" "Remove left") ", "
                     ($tmdoc-icon "tm_delete_left.xpm"))
          "Remove the column at the left-hand side of the cursor.")
        ($describe-item
            ($inline ($shortcut (structured-remove-right)) ", "
                     ($menu "Focus" "Remove right") ", "
                     ($tmdoc-icon "tm_delete_right.xpm"))
          "Remove the current column and move to the next one."))
      ($when (structured-horizontal? t)
        ($describe-item
            ($inline ($menu "Focus" "Remove above") ", "
                     ($tmdoc-icon "tm_delete_up.xpm"))
          "Remove the row above the cursor.")
        ($describe-item
            ($inline ($menu "Focus" "Remove below") ", "
                     ($tmdoc-icon "tm_delete_down.xpm"))
          "Remove the current row and move to the one below.")))))
