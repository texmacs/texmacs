
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : session-doc.scm
;; DESCRIPTION : documentation of session tags
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic session-doc)
  (:use (generic generic-doc)
        (dynamic session-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert and remove fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-generate (focus-doc-insert t)
  (:require (field-context? t))
  ($with lab (tree-label t)
    ($para
      "The " ($markup lab) " tag is used inside interactive sessions. "
      "New input fields can be inserted using the the following "
      "keyboard shortcuts, menu entries, or icons on the focus toolbar: ")
    ($description-long
      ($describe-item
          ($inline ($shortcut (structured-insert-up)) ", "
                   ($menu "Focus" "Insert above") ", "
                   ($tmdoc-icon "tm_insert_up.xpm"))
        "Insert a new input field above the cursor.")
      ($describe-item
          ($inline ($shortcut (structured-insert-down)) ", "
                   ($menu "Focus" "Insert down") ", "
                   ($tmdoc-icon "tm_insert_down.xpm"))
        "Insert a new input field below the cursor."))
    ($para
      "Existing input or input/output fields can be removed as follows:")
    ($description-long
      ($describe-item
          ($inline ($menu "Focus" "Remove above") ", "
                   ($tmdoc-icon "tm_delete_up.xpm"))
        "Remove the field above the cursor.")
      ($describe-item
          ($inline ($menu "Focus" "Remove below") ", "
                   ($tmdoc-icon "tm_delete_down.xpm"))
        "Remove the current field and move to the one below.")
      ($describe-item
          ($inline ($menu "Focus" "Remove banner"))
        "Remove the start-up banner of the session.")
      ($describe-item
          ($inline ($menu "Focus" "Remove last field"))
        "Remove the last field of the session."))))
