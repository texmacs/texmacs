
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : bib-menu.scm
;; DESCRIPTION : menus for managing bibliographic databases
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database bib-menu)
  (:use (database bib-widgets)
        (database db-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu for maintaining bibliographic databases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind bib-menu
  ("Import" (choose-file bib-import-bibtex "Import from BibTeX file" "bibtex"))
  (when (bib-exportable?)
    ("Export" (choose-file bib-export-bibtex "Export to BibTeX file" "bibtex")))
  ---
  ("Active bibliography" (load-db-buffer "tmfs://db/bib/global"))
  ("Collected entries" (noop))
  ("Conflicting entries" (noop))
  ---
  ("Show search tool" (noop))
  ("Show sorting tool" (noop))
  ---
  ("Preferences" (open-bib-preferences)))
