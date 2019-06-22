
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-menu.scm
;; DESCRIPTION : the Table menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-menu)
  (:use (table table-edit)
        (generic generic-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-table-menu
  (if (and (style-has? "std-dtd") (in-text?) (style-has? "env-float-dtd"))
      (when (not (selection-active-non-small?))
        ("Small table"
         (wrap-selection-small
           (insert-go-to '(small-table "" "") '(0 0))
           (make 'tabular)))
        ("Big table"
         (wrap-selection-small
           (insert-go-to '(big-table "" (document "")) '(0 0))
           (make 'tabular)))
        ---))
  (if (and (style-has? "calc-dtd") (calc-ready?))
      (link calc-table-menu)
      ---)
  (if (not (in-math?))
      ("Wide tabular" (make-wrapped 'wide-tabular)))
  (when (not (selection-active-non-small?))
    ("Plain tabular" (make 'tabular))
    ("Centered tabular" (make 'tabular*)))
  (if (not (in-math?))
      ("Wide block" (make-wrapped 'wide-block)))
  (when (not (selection-active-non-small?))
    ("Plain block" (make 'block))
    ("Centered block" (make 'block*)))
  (when (not (selection-active-non-small?))
    (if (and (style-has? "std-dtd") (in-math?))
        ---
        ("Matrix" (make 'matrix))
        ("Determinant" (make 'det))
        ("Choice" (make 'choice))
        ("Stack" (make 'stack)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the Table menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-size-menu
  ("Set number of rows" (interactive table-set-rows))
  ("Minimal number of rows" (table-interactive-set "table-min-rows"))
  ("Maximal number of rows" (table-interactive-set "table-max-rows"))
  ---
  ("Set number of columns" (interactive table-set-columns))
  ("Minimal number of columns" (table-interactive-set "table-min-cols"))
  ("Maximal number of columns" (table-interactive-set "table-max-cols")))

(menu-bind table-width-menu
  ("Automatic" (table-set-automatic-width))
  ("Paragraph" (table-set-exact-width "1par"))
  ("Exact" (table-ia-exact-width))
  ("Minimal" (table-ia-minimal-width))
  ("Maximal" (table-ia-maximal-width)))

(menu-bind table-height-menu
  ("Automatic" (table-set-automatic-height))
  ("Exact" (table-ia-exact-height))
  ("Minimal" (table-ia-minimal-height))
  ("Maximal" (table-ia-maximal-height)))

(menu-bind table-halign-menu
  ("Left" (table-set-halign "l"))
  ("Middle" (table-set-halign "c"))
  ("Right" (table-set-halign "r"))
  ---
  ("Left baseline" (table-set-halign "L"))
  ("Middle baseline" (table-set-halign "C"))
  ("Right baseline" (table-set-halign "R"))
  ("Specific baseline" (interactive table-specific-halign)))

(menu-bind table-valign-menu
  ("Axis" (table-set-valign "f"))
  ("Bottom" (table-set-valign "b"))
  ("Middle" (table-set-valign "c"))
  ("Top" (table-set-valign "t"))
  ---
  ("Bottom baseline" (table-set-valign "B"))
  ("Middle baseline" (table-set-valign "C"))
  ("Top baseline" (table-set-valign "T"))
  ("Specific baseline" (interactive table-specific-valign)))

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

(menu-bind table-special-menu
  ("Table breaking" (toggle-table-hyphen))
  ;;("Deactivate" (table-deactivate))
  ("Create table macro" (create-table-macro "")))

(menu-bind cell-mode-menu
  ("Cells" (set-cell-mode "cell"))
  ("Rows" (set-cell-mode "row"))
  ("Columns" (set-cell-mode "column"))
  ("Entire table" (set-cell-mode "table")))

(menu-bind cell-halign-menu
  ("Left" (cell-set-halign "l"))
  ("Center" (cell-set-halign "c"))
  ("Right" (cell-set-halign "r"))
  ;;(-> "Baseline"
  ;;    ("Left" (cell-set-halign "L"))
  ;;    ("Center" (cell-set-halign "C"))
  ;;    ("Right" (cell-set-halign "R")))
  ;;(-> "Decimal dot"
  ;;    ("Left" (cell-set-halign "L."))
  ;;    ("Center" (cell-set-halign "C."))
  ;;    ("Right" (cell-set-halign "R.")))
  ;;(-> "Decimal comma"
  ;;    ("Left" (cell-set-halign "L,"))
  ;;    ("Center" (cell-set-halign "C,"))
  ;;    ("Right" (cell-set-halign "R,")))
  ("Decimal dot" (cell-set-halign "L."))
  ("Decimal comma" (cell-set-halign "L,")))

(menu-bind cell-valign-menu
  ("Bottom" (cell-set-valign "b"))
  ("Center" (cell-set-valign "c"))
  ("Top" (cell-set-valign "t"))
  ;;(-> "Baseline"
  ;;    ("Bottom" (cell-set-valign "B"))
  ;;    ("Center" (cell-set-valign "C"))
  ;;    ("Top" (cell-set-valign "T")))
  ("Baseline" (cell-set-valign "B")))

(menu-bind cell-width-menu
  ("Automatic" (cell-set-automatic-width))
  ("Exact" (cell-ia-exact-width))
  ("Minimal" (cell-ia-minimal-width))
  ("Maximal" (cell-ia-maximal-width))
  ("Stretch factor" (cell-interactive-set "cell-hpart")))

(menu-bind cell-height-menu
  ("Automatic" (cell-set-automatic-height))
  ("Exact" (cell-ia-exact-height))
  ("Minimal" (cell-ia-minimal-height))
  ("Maximal" (cell-ia-maximal-height))
  ("Stretch factor" (cell-interactive-set "cell-vpart"))
  (-> "Adjust limits"
      ("None" (cell-set-vcorrect "n"))
      ("Bottom" (cell-set-vcorrect "b"))
      ("Top" (cell-set-vcorrect "t"))
      ("Both" (cell-set-vcorrect "a"))))

(menu-bind cell-span-menu
  ("Horizontal" (interactive cell-set-column-span))
  ("Vertical" (interactive cell-set-row-span)))

(menu-bind cell-border-icons-menu
  (with width (cell-get-pen-width)
    ((icon "tm_border_none.xpm")
     (cell-set-borders "0ln" "0ln" "0ln" "0ln" #f #f #f #f))
    ((icon "tm_border_l.xpm")
     (cell-set-borders "0ln" "0ln" width "0ln" #f #f #f #f))
    ((icon "tm_border_r.xpm")
     (cell-set-borders "0ln" "0ln" "0ln" width #f #f #f #f))
    ((icon "tm_border_lr.xpm")
     (cell-set-borders "0ln" "0ln" width width #f #f #f #f))
    ((icon "tm_border_t.xpm")
     (cell-set-borders width "0ln" "0ln" "0ln" #f #f #f #f))
    ((icon "tm_border_lt.xpm")
     (cell-set-borders width "0ln" width "0ln" #f #f #f #f))
    ((icon "tm_border_rt.xpm")
     (cell-set-borders width "0ln" "0ln" width #f #f #f #f))
    ((icon "tm_border_lrt.xpm")
     (cell-set-borders width "0ln" width width #f #f #f #f))
    ((icon "tm_border_b.xpm")
     (cell-set-borders "0ln" width "0ln" "0ln" #f #f #f #f))
    ((icon "tm_border_lb.xpm")
     (cell-set-borders "0ln" width width "0ln" #f #f #f #f))
    ((icon "tm_border_rb.xpm")
     (cell-set-borders "0ln" width "0ln" width #f #f #f #f))
    ((icon "tm_border_lrb.xpm")
     (cell-set-borders "0ln" width width width #f #f #f #f))
    ((icon "tm_border_tb.xpm")
     (cell-set-borders width width "0ln" "0ln" #f #f #f #f))
    ((icon "tm_border_ltb.xpm")
     (cell-set-borders width width width "0ln" #f #f #f #f))
    ((icon "tm_border_rtb.xpm")
     (cell-set-borders width width "0ln" width #f #f #f #f))
    ((icon "tm_border_lrtb.xpm")
     (cell-set-borders width width width width #f #f #f #f))))

(menu-bind cell-borders-icons-menu
  (with width (cell-get-pen-width)
    ((icon "tm_both_none.xpm")
     (cell-set-borders "0ln" "0ln" "0ln" "0ln" "0ln" "0ln" "0ln" "0ln"))
    ((icon "tm_both_h.xpm")
     (cell-set-borders "0ln" "0ln" width width "0ln" "0ln" width width))
    ((icon "tm_both_v.xpm")
     (cell-set-borders width width "0ln" "0ln" width width "0ln" "0ln"))
    ((icon "tm_both_hv.xpm")
     (cell-set-borders width width width width width width width width))))

(menu-bind cell-pen-width-menu
  ("0.5ln" (cell-set-pen-width "0.5ln"))
  ("1ln" (cell-set-pen-width "1ln"))
  ("2ln" (cell-set-pen-width "2ln"))
  ---
  ("Other" (interactive cell-set-pen-width)))

(menu-bind cell-compact-pen-width-menu
  ("0.5ln" (cell-set-pen-width "0.5ln"))
  ("1ln" (cell-set-pen-width "1ln"))
  ("2ln" (cell-set-pen-width "2ln"))
  ("Other" (interactive cell-set-pen-width)))

(menu-bind cell-border-menu
  ("All" (interactive cell-set-border))
  ("Horizontal" (interactive cell-set-hborder))
  ("Vertical" (interactive cell-set-vborder))
  ("Left" (interactive cell-set-lborder))
  ("Right" (interactive cell-set-rborder))
  ("Bottom" (interactive cell-set-bborder))
  ("Top" (interactive cell-set-tborder)))

(menu-bind cell-alt-border-menu
  (tile 4 (link cell-border-icons-menu))
  (if (selection-active-table?)
      ---
      (tile 4 (link cell-borders-icons-menu))))

(menu-bind cell-padding-menu
  ("All" (interactive cell-set-padding))
  ("Horizontal" (interactive cell-set-hpadding))
  ("Vertical" (interactive cell-set-vpadding))
  ("Left" (cell-interactive-set "cell-lsep"))
  ("Right" (cell-interactive-set "cell-rsep"))
  ("Bottom" (cell-interactive-set "cell-bsep"))
  ("Top" (cell-interactive-set "cell-tsep")))

(menu-bind cell-color-menu
  ("None" (cell-set-background ""))
  ("Foreground" (cell-set-background "foreground"))
  ---
  (pick-background "" (cell-set-background answer))
  ---
  ("Palette" (interactive-background
              (lambda (col) (cell-set-background col)) '()))
  ("Pattern" (open-pattern-selector cell-set-background "1cm"))
  ("Picture" (open-background-picture-selector cell-set-background))
  ("Other" (interactive cell-set-background)))

(menu-bind cell-wrapping-menu
  ("Off" (cell-set-hyphen "n"))
  ---
  ("Top" (cell-set-hyphen "t"))
  ("Center" (cell-set-hyphen "c"))
  ("Bottom" (cell-set-hyphen "b")))

(menu-bind cell-block-menu
  ("Never" (cell-set-block "no"))
  ("When line wrapping" (cell-set-block "auto"))
  ("Always" (cell-set-block "yes")))

(menu-bind cell-split-join-menu
  (when (and (== (get-cell-mode) "cell")
	     (not (selection-active-table?)))
    ("Insert subtable" (make-subtable)))
  (when (selection-active-table?)
    ("Join selected cells" (cell-set-span-selection)))
  (when (and (== (get-cell-mode) "cell")
	     (cell-spans-more?))
    ("Dissociate joined cells" (cell-reset-span))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Table menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-menu
  (assuming #f
    (-> "Insert"
        ("Row above" (table-insert-row #f))
        ("Row below" (table-insert-row #t))
        ("Column to the left" (table-insert-column #f))
        ("Column to the right" (table-insert-column #t))
        ("Blank row" (interactive table-insert-blank-row))
        ("Blank column" (interactive table-insert-blank-column)))
    (-> "Remove"
        ("This row" (table-remove-row #f))
        ("This column" (table-remove-column #f)))
    ---)
  (-> "Size" (link table-size-menu))
  (-> "Width" (link table-width-menu))
  (-> "Height" (link table-height-menu))
  (-> "Border" (link table-border-menu))
  (-> "Padding" (link table-padding-menu))
  (-> "Horizontal alignment" (link table-halign-menu))
  (-> "Vertical alignment" (link table-valign-menu))
  ;;---
  (-> "Special" (link table-special-menu)))

(menu-bind cell-menu
  (-> "Operation mode" (link cell-mode-menu))
  ;;---
  (-> "Width" (link cell-width-menu))
  (-> "Height" (link cell-height-menu))
  (when (and (== (get-cell-mode) "cell") (not (selection-active-any?)))
    (-> "Span" (link cell-span-menu)))
  (-> "Border" (link cell-alt-border-menu))
  (-> "Pen width" (link cell-pen-width-menu))
  (-> "Padding" (link cell-padding-menu))
  (-> "Horizontal alignment" (link cell-halign-menu))
  (-> "Vertical alignment" (link cell-valign-menu))
  (-> "Background color" (link cell-color-menu))
  ;;---
  (-> "Line wrapping" (link cell-wrapping-menu))
  (-> "Block content" (link cell-block-menu))
  ---
  (link cell-split-join-menu))

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

(tm-menu (standard-focus-menu t)
  (:require (table-markup-context? t))
  (dynamic (focus-tag-menu t))
  (-> "Move" (dynamic (focus-move-menu t)))
  (-> "Resize" (dynamic (focus-insert-menu t)))
  ---
  (group "Table")
  (link table-menu)
  ---
  (if (== (get-cell-mode) "cell") (group "Cell"))
  (if (== (get-cell-mode) "row") (group "Row"))
  (if (== (get-cell-mode) "column") (group "Column"))
  (if (== (get-cell-mode) "table") (group "Cells"))
  (link cell-menu)
  (dynamic (focus-extra-menu t))
  (dynamic (focus-hidden-menu t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icons for manipulation of tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind table-insert-icons
  ((balloon (icon "tm_table_arow.xpm")
	    "Insert row above")
   (table-insert-row #f))
  ((balloon (icon "tm_table_brow.xpm")
	    "Insert row below")
   (table-insert-row #t))
  ((balloon (icon "tm_table_lcol.xpm")
	    "Insert column to the left")
   (table-insert-column #f))
  ((balloon (icon "tm_table_rcol.xpm")
	    "Insert column to the right")
   (table-insert-column #t))
  ---
  ((balloon (icon "tm_table_drow.xpm")
	    "Delete row")
   (table-remove-row #t))
  ((balloon (icon "tm_table_dcol.xpm")
	    "Delete column")
   (table-remove-column #t)))

(menu-bind table-hpos-icons
  ((balloon (icon "tm_table_left.xpm")
	    "Align table at the left")
   (table-set-halign "l"))
  ((balloon (icon "tm_table_hcenter.xpm")
	    "Align table at the horizontal center")
   (table-set-halign "c"))
  ((balloon (icon "tm_table_right.xpm")
	    "Align table to the right")
   (table-set-halign "r"))
  ((icon "tm_empty.xpm") (noop))
  ((balloon (icon "tm_table_bleft.xpm")
	    "Align table at base of left column")
   (table-set-halign "L"))
  ((balloon (icon "tm_table_bhcenter.xpm")
	    "Align table at base of middle column")
   (table-set-halign "C"))
  ((balloon (icon "tm_table_bright.xpm")
	    "Align table at base of right column")
   (table-set-halign "R"))
  ((balloon (icon "tm_table_hbase.xpm")
	    "Align table at base of origin column")
   (table-set-halign "O")))

(menu-bind table-vpos-icons
  ((balloon (icon "tm_table_bottom.xpm")
	    "Align table at the bottom")
   (table-set-valign "b"))
  ((balloon (icon "tm_table_vcenter.xpm")
	    "Align table at the center")
   (table-set-valign "c"))
  ((balloon (icon "tm_table_top.xpm")
	    "Align table at the top")
   (table-set-valign "t"))
  ((balloon (icon "tm_table_vfrac.xpm")
	    "Align table at axis")
   (table-set-valign "f"))
  ((balloon (icon "tm_table_bbottom.xpm")
	    "Align table at the base of bottom row")
   (table-set-valign "B"))
  ((balloon (icon "tm_table_bvcenter.xpm")
	    "Align table at the base of middle row")
   (table-set-valign "C"))
  ((balloon (icon "tm_table_btop.xpm")
	    "Align table at the base of top row")
   (table-set-valign "T"))
  ((balloon (icon "tm_table_vbase.xpm")
	    "Align table at the base of origin row")
   (table-set-valign "O")))

(menu-bind cell-mode-icons
  ((balloon (icon "tm_cell_by_cell.xpm")
	    "Perform operations on cells")
   (set-cell-mode "cell"))
  ((balloon (icon "tm_cell_by_row.xpm")
	    "Perform operations on rows")
   (set-cell-mode "row"))
  ((balloon (icon "tm_cell_by_column.xpm")
	    "Perform operations on columns")
   (set-cell-mode "column"))
  ((balloon (icon "tm_cell_by_table.xpm")
	    "Perform operations on entire table")
   (set-cell-mode "table")))

(menu-bind cell-pos-icons
  ((balloon (icon "tm_cell_left.xpm")
	    "Align cell to the left")
   (cell-set-halign "l"))
  ((balloon (icon "tm_cell_hcenter.xpm")
	    "Horizontally center the cell")
   (cell-set-halign "c"))
  ((balloon (icon "tm_cell_right.xpm")
	    "Align cell to the right")
   (cell-set-halign "r"))
  ((balloon (icon "tm_cell_hbase.xpm")
	    "Horizontally align cell to the base")
   (cell-set-halign "R"))
  ((balloon (icon "tm_cell_hdot.xpm")
	    "Align cell to the decimal dot")
   (cell-set-halign "L."))
  ((balloon (icon "tm_cell_hcomma.xpm")
	    "Align cell to the decimal comma")
   (cell-set-halign "L,"))
  ((balloon (icon "tm_cell_bottom.xpm")
	    "Align cell to the bottom")
   (cell-set-valign "b"))
  ((balloon (icon "tm_cell_vcenter.xpm")
	    "Vertically center the cell")
   (cell-set-valign "c"))
  ((balloon (icon "tm_cell_top.xpm")
	    "Align cell to the top")
   (cell-set-valign "t"))
  ((balloon (icon "tm_cell_vbase.xpm")
	    "Vertically align cell to the base")
   (cell-set-valign "B")))

(menu-bind cell-size-icons
  ((balloon (icon "tm_cell_width.xpm")
	    "Set width of cell")
   (cell-interactive-set "cell-width"))
  ((balloon (icon "tm_cell_hmin.xpm")
	    "Width is minimum of specified width and box width")
   (cell-set-hmode "min"))
  ((balloon (icon "tm_cell_hexact.xpm")
	    "Width is exactly the specified width")
   (cell-set-hmode "exact"))
  ((balloon (icon "tm_cell_hmax.xpm")
	    "Width is maximum of specified width and box width")
   (cell-set-hmode "max"))
  ((balloon (icon "tm_cell_height.xpm")
	    "Set height of cell")
   (cell-interactive-set "cell-height"))
  ((balloon (icon "tm_cell_vmin.xpm")
	    "Height is minimum of specified height and box height")
   (cell-set-vmode "min"))
  ((balloon (icon "tm_cell_vexact.xpm")
	    "Height is exactly the specified height")
   (cell-set-vmode "exact"))
  ((balloon (icon "tm_cell_vmax.xpm")
	    "Height is maximum of specified height and box height")
   (cell-set-vmode "max")))

(menu-bind cell-decoration-icons
  ((balloon (icon "tm_decorate_left.xpm")
	    "Use left hand column as border")
   (table-column-decoration #f))
  ((balloon (icon "tm_decorate_right.xpm")
	    "Use right hand column as border")
   (table-column-decoration #t))
  ((balloon (icon "tm_decorate_up.xpm")
	    "Use row above as border")
   (table-row-decoration #f))
  ((balloon (icon "tm_decorate_down.xpm")
	    "Use row below as border")
   (table-row-decoration #t)))

(define (document-like? t)
  (or (tree-is? t 'document)
      (and (tree-is? t 'with)
           (document-like? (tree-up t)))))

(tm-menu (focus-tag-extra-icons t)
  (:require (table-markup-context? t))
  (if (document-like? (tree-up t))
      ((check (balloon (icon "tm_table_parwidth.xpm")
                       "Extend table to full paragraph width")
              "v" (table-test-parwidth?))
       (table-toggle-parwidth)))
  (=> (balloon (icon "tm_set_properties.xpm") "Table properties")
      (mini #f
	(link table-menu)
	---
	("Table properties" (open-table-properties)))))

(define (cell-halign-icon)
  (with h (cell-get-format "cell-halign")
    (cond ((== h "l") "tm_cell_left.xpm")
	  ((== h "c") "tm_cell_center.xpm")
	  ((== h "r") "tm_cell_right.xpm")
	  ((== h "L.") "tm_cell_dot.xpm")
	  ((== h "L,") "tm_cell_dot.xpm")
	  (else "tm_cell_left.xpm"))))

(tm-menu (focus-extra-icons t)
  (:require (table-markup-context? t))
  (glue #f #f 10 0)
  (minibar
    (if (== (get-cell-mode) "cell")
        (=> (balloon "Cell" "Change cell operation mode")
            (mini #f
              (link cell-mode-icons)
	      ---
	      ("Cell properties" (open-cell-properties)))))
    (if (== (get-cell-mode) "row")
        (=> (balloon "Row" "Change cell operation mode")
            (mini #f
              (link cell-mode-icons)
	      ---
	      ("Cell properties" (open-cell-properties)))))
    (if (== (get-cell-mode) "column")
        (=> (balloon "Column" "Change cell operation mode")
            (mini #f
              (link cell-mode-icons)
	      ---
	      ("Cell properties" (open-cell-properties)))))
    (if (== (get-cell-mode) "table")
        (=> (balloon "All cells" "Change cell operation mode")
            (mini #f
              (link cell-mode-icons)
	      ---
	      ("Cell properties" (open-cell-properties)))))
    (=> (balloon (icon "tm_cell_size_var.xpm") "Modify cell size")
        (mini #f
          (group "Width")
          (link cell-width-menu)
          ---
          (group "Height")
          (link cell-height-menu)
	  (when (and (== (get-cell-mode) "cell") (not (selection-active-any?)))
	    ---
	    (group "Span")
	    (link cell-span-menu))))
    (=> (balloon (icon "tm_cell_border.xpm") "Change border of cell")
        (mini #f
          (group "Border")
          (link cell-alt-border-menu)
          ---
          (group "Pen width")
          (link cell-compact-pen-width-menu)
          ---
          (group "Padding")
          (link cell-padding-menu)))
    (=> (balloon (icon (eval (cell-halign-icon))) "Modify cell alignment")
        (mini #f
          (group "Horizontal alignment")
          (link cell-halign-menu)
          ---
          (group "Vertical alignment")
          (link cell-valign-menu)))
    (=> (balloon (icon "tm_cell_background.xpm")
                 "Set background color of cell")
        (mini #f
          (link cell-color-menu)))
    ((check (balloon (icon "tm_cell_wrap.xpm") "Line wrapping inside cell")
            "v" (cell-test-wrap?))
     (cell-toggle-wrap))
    (if (and (not (cell-spans-more?))
	     (not (selection-active-table?))
	     (> (* (table-nr-rows) (table-nr-columns)) 1))
	((balloon (icon "tm_cell_subtable.xpm")
		  "Transform cell into subtable")
	 (make-subtable)))
    (if (and (cell-spans-more?)
	     (not (selection-active-table?)))
	((balloon (icon "tm_cell_var_subtable.xpm")
		  "Transform cell into subtable")
	 (make-subtable))
	((balloon (icon "tm_cell_split.xpm")
		  "Dissociate joined cells")
	 (cell-reset-span)))
    (if (selection-active-table?)
	((balloon (icon "tm_cell_join.xpm")
		  "Join selected cells")
	 (cell-set-span-selection)))))
