
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : version-menu.scm
;; DESCRIPTION : menus for versioning portions of text
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version version-menu)
  (:use (version version-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main version menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind version-menu
  (-> "File"
      ;;("Merge" (noop))
      ("Compare" (choose-file compare-file "Compare with file" ""))
      ---
      ("Show both versions" (version-show-all 'version-both))
      ("Show old version" (version-show-all 'version-old))
      ("Show new version" (version-show-all 'version-new))
      ---
      ("Retain current version" (version-retain-all 'current))
      ("Retain old version" (version-retain-all 0))
      ("Retain new version" (version-retain-all 1)))
  ;;(-> "Subversion"
  ;;    ("Update" (noop))
  ;;    ("Changes" (noop))
  ;;    ("Commit" (noop)))
  ---
  (-> "Move"
      ("First difference" (version-first-difference))
      ("Previous difference" (version-previous-difference))
      ("Next difference" (version-next-difference))
      ("Last difference" (version-last-difference)))
  (when (inside-version?)
    (-> "Show"
	("Both versions" (version-show-both))
	("Old version" (version-show-old))
	("New version" (version-show-new)))
    (-> "Retain"
	("Current version" (version-retain-current))
	("Old version" (version-retain-old))
	("New version" (version-retain-new)))))
