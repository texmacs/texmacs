
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-menu.scm
;; DESCRIPTION : the Table menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-menu)
  (:use (table table-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Table menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-insert-row-menu
  ("Above" (table-insert-row #f))
  ("Below" (table-insert-row #t)))

(menu-bind table-insert-column-menu
  ("To the left" (table-insert-column #f))
  ("To the right" (table-insert-column #t)))

(menu-bind table-halign-menu
  ("Left" (table-set-halign "l"))
  ("Middle" (table-set-halign "c"))
  ("Right" (table-set-halign "r")))

(menu-bind table-valign-menu
  ("Bottom" (table-set-valign "b"))
  ("Middle" (table-set-valign "c"))
  ("Fraction bar height" (table-set-valign "f"))
  ("Top" (table-set-valign "t")))

(menu-bind table-border-menu
  ("Border" ... (table-set-border-ia))
  ("Left border" ... (table-set-lborder-ia))
  ("Right border" ... (table-set-rborder-ia))
  ("Bottom border" ... (table-set-bborder-ia))
  ("Top border" ... (table-set-tborder-ia))
  ---
  ("Padding" ... (table-set-padding-ia))
  ("Left padding" ... (table-set-lpadding-ia))
  ("Right padding" ... (table-set-rpadding-ia))
  ("Bottom padding" ... (table-set-bpadding-ia))
  ("Top padding" ... (table-set-tpadding-ia)))

(menu-bind table-limits-menu
  ("Minimal number of rows" ... (table-set-min-rows-ia))
  ("Minimal number of columns" ... (table-set-min-columns-ia))
  ("Maximal number of rows" ... (table-set-max-rows-ia))
  ("Maximal number of columns" ... (table-set-max-columns-ia)))

(menu-bind table-special-menu
  ("Deactivate" (table-disactivate))
  ("Set extension center" (table-format-center))
  ("Extract format" (table-extract-format))
  ---
  ("Use paragraph width" (table-use-paragraph-width))
  ("Width" ... (table-set-width-ia))
  ("Height" ... (table-set-height-ia))
  ---
  ("Hyphenation" (toggle-table-hyphen))
  (-> "Border" (link table-border-menu))
;;(-> "Origin"
;;    ("Row" (table-set-row-origin-ia))
;;    ("Column" (table-set-column-origin-ia)))
  (-> "Size limits" (link table-limits-menu)))

(menu-bind cell-mode-menu
  ("Cells" (set-cell-mode "cell"))
  ("Rows" (set-cell-mode "row"))
  ("Columns" (set-cell-mode "column"))
  ("Entire table" (set-cell-mode "table")))

(menu-bind cell-halign-menu
  ("Left" (cell-set-halign "l"))
  ("Center" (cell-set-halign "c"))
  ("Baseline" (cell-set-halign "R"))
  ("Right" (cell-set-halign "r"))
  ("Decimal dot" (cell-set-halign "."))
  ("Decimal comma" (cell-set-halign ",")))

(menu-bind cell-valign-menu
  ("Bottom" (cell-set-valign "b"))
  ("Baseline" (cell-set-valign "B"))
  ("Center" (cell-set-valign "c"))
  ("Top" (cell-set-valign "t")))

(menu-bind cell-width-menu
  ("Set width" ... (cell-set-width-ia))
  ---
  ("Minimum mode" (cell-set-hmode "min"))
  ("Exact mode" (cell-set-hmode "exact"))
  ("Maximum mode" (cell-set-hmode "max")))

(menu-bind cell-height-menu
  ("Set height" ... (cell-set-height-ia))
  ---
  ("Minimum mode" (cell-set-vmode "min"))
  ("Exact mode" (cell-set-vmode "exact"))
  ("Maximum mode" (cell-set-vmode "max")))

(menu-bind cell-border-menu
  ("Border" ... (cell-set-border-ia))
  ("Left border" ... (cell-set-lborder-ia))
  ("Right border" ... (cell-set-rborder-ia))
  ("Bottom border" ... (cell-set-bborder-ia))
  ("Top border" ... (cell-set-tborder-ia))
  ---
  ("Padding" ... (cell-set-padding-ia))
  ("Left padding" ... (cell-set-lpadding-ia))
  ("Right padding" ... (cell-set-rpadding-ia))
  ("Bottom padding" ... (cell-set-bpadding-ia))
  ("Top padding" ... (cell-set-tpadding-ia)))

(menu-bind cell-color-menu
  ("None" (cell-set-background ""))
  ("Foreground" (cell-set-background "foreground"))
  ---
  ("Black" (cell-set-background "black"))
  ("White" (cell-set-background "white"))
  ("Grey" (cell-set-background "grey"))
  ("Red" (cell-set-background "red"))
  ("Blue" (cell-set-background "blue"))
  ("Yellow" (cell-set-background "yellow"))
  ("Green" (cell-set-background "green"))
  ("Orange" (cell-set-background "orange"))
  ("Magenta" (cell-set-background "magenta"))
  ("Brown" (cell-set-background "brown"))
  ("Pink" (cell-set-background "pink"))
  ---
  ("Other"  ... (interactive '("Cell color:") 'cell-set-background)))

(menu-bind cell-special-menu
  ("Set span" ... (cell-set-span-ia))
  ("Subtable" (make-subtable))
  ---
  (-> "Text height correction"
      ("Off" (cell-set-vcorrect "n"))
      ---
      ("Bottom" (cell-set-vcorrect "b"))
      ("Top" (cell-set-vcorrect "t"))
      ("Both" (cell-set-vcorrect "a")))
  (-> "Hyphenation"
      ("Off" (cell-set-hyphen "n"))
      ---
      ("Top" (cell-set-hyphen "t"))
      ("Center" (cell-set-hyphen "c"))
      ("Bottom" (cell-set-hyphen "b"))
      ---
      ("Multi-paragraph" (cell-toggle-multi-paragraph)))
  (-> "Distribute unused space"
      ("Horizontal part" ... (cell-set-hpart-ia))
      ("Vertical part" ... (cell-set-vpart-ia)))
  (-> "Glue decorations" (tile 2 (link cell-decoration-icons))))

(menu-bind table-menu
  (-> "Insert row" (link table-insert-row-menu))
  (-> "Insert column" (link table-insert-column-menu))
  (-> "Horizontal table alignment" (link table-halign-menu))
  (-> "Vertical table alignment" (link table-valign-menu))
  (-> "Special table properties" (link table-special-menu))
  ---
  (-> "Cell operation mode" (link cell-mode-menu))
  (-> "Horizontal cell alignment" (link cell-halign-menu))
  (-> "Vertical cell alignment" (link cell-valign-menu))
  (-> "Cell width" (link cell-width-menu))
  (-> "Cell height" (link cell-height-menu))
  (-> "Cell border" (link cell-border-menu))
  (-> "Cell background color" (link cell-color-menu))
  (-> "Special cell properties" (link cell-special-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for manipulation of tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-insert-icons
  ((balloon (icon "tm_table_arow.xpm")
	    "Insert row above (A-up)")
   (table-insert-row #f))
  ((balloon (icon "tm_table_brow.xpm")
	    "Insert row below (A-down)")
   (table-insert-row #t))
  ((balloon (icon "tm_table_lcol.xpm")
	    "Insert column to the left (A-left)")
   (table-insert-column #f))
  ((balloon (icon "tm_table_rcol.xpm")
	    "Insert column to the right (A-right)")
   (table-insert-column #t))
  ---
  ((balloon (icon "tm_table_drow.xpm")
	    "Delete row (M-t h delete)")
   (table-delete-row #t))
  ((balloon (icon "tm_table_dcol.xpm")
	    "Delete column (M-t v delete)")
   (table-delete-column #t)))

(menu-bind table-hpos-icons
  ((balloon (icon "tm_table_left.xpm")
	    "Align table at the left (M-t H l)")
   (table-set-halign "l"))
  ((balloon (icon "tm_table_hcenter.xpm")
	    "Align table at the horizontal center (M-t H c)")
   (table-set-halign "c"))
  ((balloon (icon "tm_table_right.xpm")
	    "Align table to the right (M-t H r)")
   (table-set-halign "r"))
  ((icon "tm_empty.xpm") (noop))
  ((balloon (icon "tm_table_bleft.xpm")
	    "Align table at base of left column (M-t H L)")
   (table-set-halign "L"))
  ((balloon (icon "tm_table_bhcenter.xpm")
	    "Align table at base of middle column (M-t H C)")
   (table-set-halign "C"))
  ((balloon (icon "tm_table_bright.xpm")
	    "Align table at base of right column (M-t H R)")
   (table-set-halign "R"))
  ((balloon (icon "tm_table_hbase.xpm")
	    "Align table at base of origin column (M-t H O)")
   (table-set-halign "O")))

(menu-bind table-vpos-icons
  ((balloon (icon "tm_table_bottom.xpm")
	    "Align table at the bottom (M-t V b)")
   (table-set-valign "b"))
  ((balloon (icon "tm_table_vcenter.xpm")
	    "Align table at the center (M-t V c)")
   (table-set-valign "c"))
  ((balloon (icon "tm_table_top.xpm")
	    "Align table at the top (M-t V t)")
   (table-set-valign "t"))
  ((balloon (icon "tm_table_vfrac.xpm")
	    "Align table at fraction bar height (M-t V f)")
   (table-set-valign "f"))
  ((balloon (icon "tm_table_bbottom.xpm")
	    "Align table at the base of bottom row (M-t V B)")
   (table-set-valign "B"))
  ((balloon (icon "tm_table_bvcenter.xpm")
	    "Align table at the base of middle row (M-t V C)")
   (table-set-valign "C"))
  ((balloon (icon "tm_table_btop.xpm")
	    "Align table at the base of top row (M-t V T)")
   (table-set-valign "T"))
  ((balloon (icon "tm_table_vbase.xpm")
	    "Align table at the base of origin row (M-t V O)")
   (table-set-valign "O")))

(menu-bind cell-mode-icons
  ((balloon (icon "tm_cell_by_cell.xpm")
	    "Perform operations on cells (M-t m c)")
   (set-cell-mode "cell"))
  ((balloon (icon "tm_cell_by_row.xpm")
	    "Perform operations on rows (M-t m h)")
   (set-cell-mode "row"))
  ((balloon (icon "tm_cell_by_column.xpm")
	    "Perform operations on columns (M-t m v)")
   (set-cell-mode "column"))
  ((balloon (icon "tm_cell_by_table.xpm")
	    "Perform operations on entire table (M-t m t)")
   (set-cell-mode "table")))

(menu-bind cell-pos-icons
  ((balloon (icon "tm_cell_left.xpm")
	    "Align cell to the left (M-t h l)")
   (cell-set-halign "l"))
  ((balloon (icon "tm_cell_hcenter.xpm")
	    "Horizontally center the cell (M-t h c)")
   (cell-set-halign "c"))
  ((balloon (icon "tm_cell_hbase.xpm")
	    "Horizontally align cell to the base (M-t h R)")
   (cell-set-halign "R"))
  ((balloon (icon "tm_cell_right.xpm")
	    "Align cell to the right (M-t h r)")
   (cell-set-halign "r"))
  ((balloon (icon "tm_cell_hdot.xpm")
	    "Align cell to the decimal dot (M-t h .)")
   (cell-set-halign "."))
  ((balloon (icon "tm_cell_hcomma.xpm")
	    "Align cell to the decimal comma (M-t h ,)")
   (cell-set-halign ","))
  ((balloon (icon "tm_cell_bottom.xpm")
	    "Align cell to the bottom (M-t v b)")
   (cell-set-valign "b"))
  ((balloon (icon "tm_cell_vbase.xpm")
	    "Vertically align cell to the base (M-t v B)")
   (cell-set-valign "B"))
  ((balloon (icon "tm_cell_vcenter.xpm")
	    "Vertically center the cell (M-t v c)")
   (cell-set-valign "c"))
  ((balloon (icon "tm_cell_top.xpm")
	    "Align cell to the top (M-t v t)")
   (cell-set-valign "t")))

(menu-bind cell-size-icons
  ((balloon (icon "tm_cell_width.xpm")
	    "Set width of cell (M-t h s)")
   (cell-set-width-ia))
  ((balloon (icon "tm_cell_hmin.xpm")
	    "Width is minimum of specified width and box width (M-t h m)")
   (cell-set-hmode "min"))
  ((balloon (icon "tm_cell_hexact.xpm")
	    "Width is exactly the specified width (M-t h e)")
   (cell-set-hmode "exact"))
  ((balloon (icon "tm_cell_hmax.xpm")
	    "Width is maximum of specified width and box width (M-t h M)")
   (cell-set-hmode "max"))
  ((balloon (icon "tm_cell_height.xpm")
	    "Set height of cell (M-t v s)")
   (cell-set-height-ia))
  ((balloon (icon "tm_cell_vmin.xpm")
	    "Height is minimum of specified height and box height (M-t v m)")
   (cell-set-vmode "min"))
  ((balloon (icon "tm_cell_vexact.xpm")
	    "Height is exactly the specified height (M-t v e)")
   (cell-set-vmode "exact"))
  ((balloon (icon "tm_cell_vmax.xpm")
	    "Height is maximum of specified height and box height (M-t v M)")
   (cell-set-vmode "max")))

(menu-bind cell-decoration-icons
  ((balloon (icon "tm_decorate_left.xpm")
	    "Use left hand column as border (M-t b left)")
   (table-column-decoration #f))
  ((balloon (icon "tm_decorate_right.xpm")
	    "Use right hand column as border (M-t b right)")
   (table-column-decoration #t))
  ((balloon (icon "tm_decorate_up.xpm")
	    "Use row above as border (M-t b up)")
   (table-row-decoration #f))
  ((balloon (icon "tm_decorate_down.xpm")
	    "Use row below as border (M-t b down)")
   (table-row-decoration #t)))

(menu-bind table-icons
  |
  (=> (balloon (icon "tm_table_insert.xpm")
	       "Insert or delete rows or columns")
      (link table-insert-icons))
  (=> (balloon (icon "tm_table_pos.xpm") "Position the table")
      (tile 4 (link table-hpos-icons))
      ---
      (tile 4 (link table-vpos-icons)))
  (=> (balloon (icon "tm_table_special.xpm") "Set special table properties")
      (link table-special-menu))
  |
  (if (== (get-cell-mode) "cell")
      (=> (balloon (icon "tm_cell_cell.xpm")
		   "Change cell operation mode")
	  (link cell-mode-icons)))
  (if (== (get-cell-mode) "row")
      (=> (balloon (icon "tm_cell_row.xpm")
		   "Change cell operation mode")
	  (link cell-mode-icons)))
  (if (== (get-cell-mode) "column")
      (=> (balloon (icon "tm_cell_column.xpm")
		   "Change cell operation mode")
	  (link cell-mode-icons)))
  (if (== (get-cell-mode) "table")
      (=> (balloon (icon "tm_cell_table.xpm")
		   "Change cell operation mode")
	  (link cell-mode-icons)))
  (=> (balloon (icon "tm_cell_pos.xpm") "Modify cell alignment")
      (tile 6 (link cell-pos-icons)))
  (=> (balloon (icon "tm_cell_size.xpm") "Modify cell size")
      (tile 4 (link cell-size-icons)))
  (=> (balloon (icon "tm_cell_border.xpm") "Change border of cell")
      (link cell-border-menu))
  (=> (balloon (icon "tm_cell_background.xpm")
	       "Set background color of cell")
      (link cell-color-menu))
  (=> (balloon (icon "tm_cell_special.xpm") "Set special cell properties")
      (link  cell-special-menu)))
