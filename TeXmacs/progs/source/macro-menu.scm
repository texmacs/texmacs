
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : macro-menu.scm
;; DESCRIPTION : menus for inserting user-defined macros.
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source macro-menu)
  (:use (source macro-edit)
	(source macro-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exposed menu for editing user-defined macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-macro-menu
  (assuming (nnull? (get-macro-list :preamble))
    (for (m (get-macro-list :preamble :sort 20))
      ((eval `(verbatim ,m)) (make (string->symbol m))))
    ---)
  (assuming (nnull? (get-macro-list :packages))
    (for (pack (get-public-style-list))
      (assuming (nnull? (get-macro-list pack))
        (-> (eval pack)
            (for (m (get-macro-list pack :sort 30))
              ((eval `(verbatim ,m)) (make (string->symbol m)))))))
    ---)
  ("New macro" (open-macro-editor "" :global))
  (when (can-create-context-macro?)
    ("New context macro" (create-context-macro "" :global)))
  (when (can-create-table-macro?)
    ("New table macro" (create-table-macro "" :global))))
