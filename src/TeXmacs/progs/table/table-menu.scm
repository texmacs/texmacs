
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
;; Inserting tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-table-menu
  (if (style-has? "std-dtd")
      (when (and (in-text?) (style-has? "env-float-dtd"))
	    ("Small table" (make 'small-table))
	    ("Big table" (make 'big-table))
	    ---))
  ("Plain tabular" (make 'tabular))
  ("Centered tabular" (make 'tabular*))
  ("Plain block" (make 'block))
  ("Centered block" (make 'block*))
  (if (style-has? "std-dtd")
      (when (in-math?)
	    ---
	    ("Matrix" (make 'matrix))
	    ("Determinant" (make 'det))
	    ("Choice" (make 'choice))
	    ("Stack" (make 'stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the Table menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-width-menu
  ("Automatic" (table-set-automatic-width))
  ("Paragraph" (table-set-exact-width "1par"))
  ("Exact" (check "o" (table-test-exact-width?))
   (interactive table-set-exact-width))
  ("Minimal" (check "o" (table-test-minimal-width?))
   (interactive table-set-minimal-width))
  ("Maximal" (check "o" (table-test-maximal-width?))
   (interactive table-set-maximal-width)))

(menu-bind table-height-menu
  ("Automatic" (table-set-automatic-height))
  ("Exact" (check "o" (table-test-exact-height?))
   (interactive table-set-exact-height))
  ("Minimal" (check "o" (table-test-minimal-height?))
   (interactive table-set-minimal-height))
  ("Maximal" (check "o" (table-test-maximal-height?))
   (interactive table-set-maximal-height)))

(menu-bind table-halign-menu
  ("Left" (table-set-halign "l"))
  ("Middle" (table-set-halign "c"))
  ("Right" (table-set-halign "r")))

(menu-bind table-valign-menu
  ("Bottom" (table-set-valign "b"))
  ("Baseline" (table-set-valign "B"))
  ("Middle" (table-set-valign "c"))
  ("Axis" (table-set-valign "f"))
  ("Top" (table-set-valign "t")))

(menu-bind table-border-menu
  ("All" (interactive table-set-border))
  ("Left" (table-interactive-set "table-lborder"))
  ("Right" (table-interactive-set "table-rborder"))
  ("Bottom" (table-interactive-set "table-bborder"))
  ("Top" (table-interactive-set "table-tborder")))

(menu-bind table-padding-menu
  ("All" (interactive table-set-padding))
  ("Left" (table-interactive-set "table-lsep"))
  ("Right" (table-interactive-set "table-rsep"))
  ("Bottom" (table-interactive-set "table-bsep"))
  ("Top" (table-interactive-set "table-tsep")))

(menu-bind table-limits-menu
  ("Minimal number of rows" (table-interactive-set "table-min-rows"))
  ("Minimal number of columns" (table-interactive-set "table-min-cols"))
  ("Maximal number of rows" (table-interactive-set "table-max-rows"))
  ("Maximal number of columns" (table-interactive-set "table-max-cols")))

(menu-bind table-special-menu
  ("Deactivate" (table-disactivate))
  ("Extract format" (table-extract-format))
  ---
  ("Hyphenation" (toggle-table-hyphen))
  (-> "Origin"
      ("Row" (table-interactive-set "table-row-origin"))
      ("Column" (table-interactive-set "table-col-origin")))
  (-> "Size limits" (link table-limits-menu)))

(menu-bind cell-mode-menu
  ("Cells" (set-cell-mode "cell"))
  ("Rows" (set-cell-mode "row"))
  ("Columns" (set-cell-mode "column"))
  ("Entire table" (set-cell-mode "table")))

(menu-bind cell-halign-menu
  ("Left" (cell-set-halign "l"))
  ("Center" (cell-set-halign "c"))
  ("Right" (cell-set-halign "r"))
  (-> "Baseline"
      ("Left" (cell-set-halign "L"))
      ("Center" (cell-set-halign "C"))
      ("Right" (cell-set-halign "R")))
  (-> "Decimal dot"
      ("Left" (cell-set-halign "L."))
      ("Center" (cell-set-halign "C."))
      ("Right" (cell-set-halign "R.")))
  (-> "Decimal comma"
      ("Left" (cell-set-halign "L,"))
      ("Center" (cell-set-halign "C,"))
      ("Right" (cell-set-halign "R,"))))

(menu-bind cell-valign-menu
  ("Bottom" (cell-set-valign "b"))
  ("Center" (cell-set-valign "c"))
  ("Top" (cell-set-valign "t"))
  (-> "Baseline"
      ("Bottom" (cell-set-valign "B"))
      ("Center" (cell-set-valign "C"))
      ("Top" (cell-set-valign "T"))))

(menu-bind cell-width-menu
  ("Automatic" (cell-set-automatic-width))
  ("Exact" (check "o" (cell-test-exact-width?))
   (interactive cell-set-exact-width))
  ("Minimal" (check "o" (cell-test-minimal-width?))
   (interactive cell-set-minimal-width))
  ("Maximal" (check "o" (cell-test-maximal-width?))
   (interactive cell-set-maximal-width)))

(menu-bind cell-height-menu
  ("Automatic" (cell-set-automatic-height))
  ("Exact" (check "o" (cell-test-exact-height?))
   (interactive cell-set-exact-height))
  ("Minimal" (check "o" (cell-test-minimal-height?))
   (interactive cell-set-minimal-height))
  ("Maximal" (check "o" (cell-test-maximal-height?))
   (interactive cell-set-maximal-height)))

(menu-bind cell-border-menu
  ("All" (interactive cell-set-border))
  ("Left" (cell-interactive-set "cell-lborder"))
  ("Right" (cell-interactive-set "cell-rborder"))
  ("Bottom" (cell-interactive-set "cell-bborder"))
  ("Top" (cell-interactive-set "cell-tborder")))

(menu-bind cell-padding-menu
  ("All" (interactive cell-set-padding))
  ("Left" (cell-interactive-set "cell-lsep"))
  ("Right" (cell-interactive-set "cell-rsep"))
  ("Bottom" (cell-interactive-set "cell-bsep"))
  ("Top" (cell-interactive-set "cell-tsep")))

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
  ("Other"  (interactive cell-set-background)))

(menu-bind cell-special-menu
  ("Set span" (interactive cell-set-span))
  ("Subtable" (make-subtable))
  ;;("Multi-paragraph" (cell-toggle-multi-paragraph))
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
      ("Bottom" (cell-set-hyphen "b")))
  (-> "Distribute unused space"
      ("Horizontal part" (cell-interactive-set "cell-hpart"))
      ("Vertical part" (cell-interactive-set "cell-vpart")))
  ;;(-> "Glue decorations" (tile 2 (link cell-decoration-icons)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Table menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-menu
  (-> "Insert"
      ("Row above" (table-insert-row #f))
      ("Row below" (table-insert-row #t))
      ("Column to the left" (table-insert-column #f))
      ("Column to the right" (table-insert-column #t)))
  (-> "Remove"
      ("This row" (table-remove-row #f))
      ("This column" (table-remove-column #f)))
  ---
  (-> "Width" (link table-width-menu))
  (-> "Height" (link table-height-menu))
  (-> "Border" (link table-border-menu))
  (-> "Padding" (link table-padding-menu))
  (-> "Horizontal alignment" (link table-halign-menu))
  (-> "Vertical alignment" (link table-valign-menu))
  ---
  (-> "Special properties" (link table-special-menu)))

(menu-bind cell-menu
  (-> "Operation mode" (link cell-mode-menu))
  ---
  (-> "Width" (link cell-width-menu))
  (-> "Height" (link cell-height-menu))
  (-> "Border" (link cell-border-menu))
  (-> "Padding" (link cell-padding-menu))
  (-> "Horizontal alignment" (link cell-halign-menu))
  (-> "Vertical alignment" (link cell-valign-menu))
  (-> "Background color" (link cell-color-menu))
  ---
  (-> "Special properties" (link cell-special-menu)))

(menu-bind vertical-table-cell-menu
  (=> "Table" (link table-menu))
  (if (== (get-cell-mode) "cell") (=> "Cell" (link cell-menu)))
  (if (== (get-cell-mode) "row") (=> "Row" (link cell-menu)))
  (if (== (get-cell-mode) "column") (=> "Column" (link cell-menu)))
  (if (== (get-cell-mode) "table") (=> "Cells" (link cell-menu))))

(menu-bind horizontal-table-cell-menu
  (-> "Table" (link table-menu))
  (if (== (get-cell-mode) "cell") (-> "Cell" (link cell-menu)))
  (if (== (get-cell-mode) "row") (-> "Row" (link cell-menu)))
  (if (== (get-cell-mode) "column") (-> "Column" (link cell-menu)))
  (if (== (get-cell-mode) "table") (-> "Cells" (link cell-menu))))

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
   (table-remove-row #t))
  ((balloon (icon "tm_table_dcol.xpm")
	    "Delete column (M-t v delete)")
   (table-remove-column #t)))

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
	    "Align table at axis (M-t V f)")
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
  ((balloon (icon "tm_cell_right.xpm")
	    "Align cell to the right (M-t h r)")
   (cell-set-halign "r"))
  ((balloon (icon "tm_cell_hbase.xpm")
	    "Horizontally align cell to the base (M-t h R)")
   (cell-set-halign "R"))
  ((balloon (icon "tm_cell_hdot.xpm")
	    "Align cell to the decimal dot (M-t h .)")
   (cell-set-halign "L."))
  ((balloon (icon "tm_cell_hcomma.xpm")
	    "Align cell to the decimal comma (M-t h ,)")
   (cell-set-halign "L,"))
  ((balloon (icon "tm_cell_bottom.xpm")
	    "Align cell to the bottom (M-t v b)")
   (cell-set-valign "b"))
  ((balloon (icon "tm_cell_vcenter.xpm")
	    "Vertically center the cell (M-t v c)")
   (cell-set-valign "c"))
  ((balloon (icon "tm_cell_top.xpm")
	    "Align cell to the top (M-t v t)")
   (cell-set-valign "t"))
  ((balloon (icon "tm_cell_vbase.xpm")
	    "Vertically align cell to the base (M-t v B)")
   (cell-set-valign "B")))

(menu-bind cell-size-icons
  ((balloon (icon "tm_cell_width.xpm")
	    "Set width of cell (M-t h s)")
   (cell-interactive-set "cell-width"))
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
   (cell-interactive-set "cell-height"))
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
  (=> (balloon (icon "tm_table_size.xpm") "Specify the size of the table")
      (group "Width")
      (link table-width-menu)
      ---
      (group "Height")
      (link table-height-menu))
  (=> (balloon (icon "tm_table_border.xpm") "Change the border of the table")
      (group "Border")
      (link table-width-menu)
      ---
      (group "Padding")
      (link table-height-menu))
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
  (=> (balloon (icon "tm_cell_size.xpm") "Modify cell size")
      (group "Width")
      (link cell-width-menu)
      ---
      (group "Height")
      (link cell-height-menu))
  (=> (balloon (icon "tm_cell_border.xpm") "Change border of cell")
      (group "Border")
      (link cell-border-menu)
      ---
      (group "Padding")
      (link cell-padding-menu))
  (=> (balloon (icon "tm_cell_pos.xpm") "Modify cell alignment")
      (group "Horizontal alignment")
      (link cell-halign-menu)
      ---
      (group "Vertical alignment")
      (link cell-valign-menu))
  (=> (balloon (icon "tm_cell_background.xpm")
	       "Set background color of cell")
      (link cell-color-menu))
  (=> (balloon (icon "tm_cell_special.xpm") "Set special cell properties")
      (link  cell-special-menu)))
