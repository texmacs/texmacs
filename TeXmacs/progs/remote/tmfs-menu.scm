
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

(texmacs-module (remote tmfs-menu))

(menu-bind remote-file-menu
  ("New file" (interactive remote-new-file))
  ---
  (when (remote-buffer?)
    (-> "Permissions"
	("Owner" (check "v" (remote-permission? (get-name-buffer) "owner"))
	 (interactive-remote-set-property "owner"))
	("Read" (check "v" (remote-permission? (get-name-buffer) "read"))
	 (interactive-remote-set-property "read"))
	("Write" (check "v" (remote-permission? (get-name-buffer) "write"))
	 (interactive-remote-set-property "write")))
    (-> "Properties"
	("Set property" (interactive-remote-set-property-and-value))
	("Get property" (interactive remote-get-property)))))
