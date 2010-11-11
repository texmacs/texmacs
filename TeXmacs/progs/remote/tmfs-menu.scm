
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-menu.scm
;; DESCRIPTION : menus for user accounts on the TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-menu)
  (:use (remote tmfs-remote)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu for setting properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (std-property-types)
  '(owner type date read write classify-type classify-value project))

(tm-menu (remote-set-property-menu)
  (let* ((l1 (or (remote-get-property-types) '()))
	 (l2 (list-difference l1 (std-property-types)))
	 (l3 (list-sort (map symbol->string l2) string<=?)))
    (for (type l3)
      ((eval (upcase-first type))
       (interactive-remote-set-property type)))
    ---
    ("Other" (interactive-remote-set-property-and-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu for setting the current project
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (remote-set-project-menu)
  (let* ((l1 (or (remote-get-projects) '()))
	 (l2 (list-sort l1 (lambda (x y) (string<=? (car x) (car y)))))
	 (prj (remote-get-property "project")))
    (for (x l2)
      (let* ((val (car x))
             (new-file (cdr x))
             (old-file prj))
        ((check (eval val) "v" (== new-file old-file))
         (remote-set-property "project" new-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main remote file menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind remote-file-menu
  ("New file" (interactive remote-new-file))
  ---
  (when (remote-buffer?)
    (-> "Permissions"
	("Owner" (check "o" (remote-permission? (get-name-buffer) "owner"))
	 (interactive-remote-set-property "owner"))
	("Read" (check "o" (remote-permission? (get-name-buffer) "read"))
	 (interactive-remote-set-property "read"))
	("Write" (check "o" (remote-permission? (get-name-buffer) "write"))
	 (interactive-remote-set-property "write")))
    (-> "Properties" (link remote-set-property-menu)))
  (-> "Project"
      (when (remote-buffer?)
	("None" (remote-set-property "project" ""))
	---
	(link remote-set-project-menu)
	---)
      ("Create" (interactive remote-new-project)))
  (-> "Transfer"
      ("Export to remote server"
       (choose-file interactive-remote-export "Export to remote server" ""))
      (when (not (remote-buffer?))
	("Export current file"
	 (interactive-remote-export (get-name-buffer))))
      (when (remote-buffer?)
	("Import current file"
	 (choose-file (lambda (u) (remote-import (get-name-buffer) u))
		      "Import from remote server" ""))))
  (-> "Browse"
      ("Home directory" (remote-home-directory))
      (when (remote-buffer?)
	("File information" (remote-file-information)))))
