
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmfs-menu.scm
;; DESCRIPTION : menus for user accounts on the TeXmacs server
;; COPYRIGHT   : (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (remote tmfs-menu)
  (:use (remote tmfs-remote)))

(define (remote-set-property-menu-entry type)
  (list type (lambda () (interactive-remote-set-property type))))

(tm-define (remote-set-property-menu)
  (let* ((l1 (or (remote-get-property-types) '()))
	 (l2 (list-difference l1 '(owner read write date type)))
	 (l3 (list-sort (map symbol->string l2) string<=?)))
    (menu-dynamic
      ,@(map remote-set-property-menu-entry l3)
      ---
      ("Other" (interactive-remote-set-property-and-value)))))

(menu-bind remote-file-menu
  ("New file" (interactive remote-new-file))
  ("New classifier" (interactive remote-new-classifier))
  ---
  (when (remote-buffer?)
    (-> "Permissions"
	("Owner" (check "v" (remote-permission? (get-name-buffer) "owner"))
	 (interactive-remote-set-property "owner"))
	("Read" (check "v" (remote-permission? (get-name-buffer) "read"))
	 (interactive-remote-set-property "read"))
	("Write" (check "v" (remote-permission? (get-name-buffer) "write"))
	 (interactive-remote-set-property "write")))
    (-> "Properties" (link remote-set-property-menu)))
  (-> "Browse"
      ("Home directory" (remote-home-directory))
      (when (remote-buffer?)
	("File information" (remote-file-information)))))
