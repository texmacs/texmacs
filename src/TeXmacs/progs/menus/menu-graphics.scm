
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-graphics.scm
;; DESCRIPTION : menus for graphics mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-graphics)
  (:use (texmacs edit edit-graphics)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: provide automatic checkmarks for these actions

(menu-bind graphics-geometry-menu
  (-> "Frame"
      ("Default" (graphics-remove-property "graphical frame"))
      ---
      (group "Cartesian")
      (-> "Unit"
	  ("cm" (graphics-set-unit "1cm"))
	  ("inch" (graphics-set-unit "1in"))
	  ---
	  ("Other" ... (graphics-set-unit-ia)))
      (-> "Origin"
	  ("Base line" (graphics-set-origin "0cm" "0cm"))
	  ("Fraction height" (graphics-set-origin "0cm" "1yfrac"))
	  ---
	  ("Other"... (graphics-set-origin-ia))))
  (-> "Extents"
      ("Default" (graphics-remove-property "graphical clip"))
      ---
      ;; FIXME: insert methods for setting width, height and centering
      ("Other" ... (graphics-set-extents-ia))))

(menu-bind graphics-mode-menu
  ("Point" (graphics-set-mode "point"))
  ("Line" (graphics-set-mode "line"))
  ("Polygon" (graphics-set-mode "cline")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menus for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind graphics-menu
  (-> "Size and position" (link graphics-geometry-menu))
  (-> "Mode" (link graphics-mode-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for graphics mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-graphics-icons
  (=> (balloon (icon "tm_cell_size.xpm") "Graphics geometry")
      (link graphics-geometry-menu)))
