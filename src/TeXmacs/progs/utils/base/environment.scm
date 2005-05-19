
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : environment.scm
;; DESCRIPTION : properties & routines for built-in environment variables
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils base environment))

(drd-table env-var-description%
  ("color" "Foreground Colour")
  ("bg-color" "Background colour")
  ("magnification" "Magnification")
  ("font-base-size" "Font size")

  ("par-width" "Paragraph width")
  ("par-mode" "Paragraph mode")
  ("par-hyphen" "Paragraph hyphenation algorithm")
  ("par-width" "Paragraph width")
  ("par-left" "Left margin")
  ("par-right" "Right margin")
  ("par-first" "First indentation")
  ("par-sep" "Separation between lines")
  ("par-line-sep" "Space between lines")
  ("par-par-sep" "Space between paragraphs")
  ("par-columns" "Number of columns")

  ("page-odd" "Odd page left margin")
  ("page-right" "Odd page right margin")
  ("page-even" "Even page left margin")
  ("page-top" "Top margin")
  ("page-bot" "Bottom margin")
  ("page-odd-shift" "Odd page shift")
  ("page-even-shift" "Even page shift")
  ("page-shrink" "How much shorter may pages become?")
  ("page-extend" "How much longer may pages become?")
  ("page-flexibility" "Flexibility factor for vertical spacing")
  ("page-screen-left" "Left margin")
  ("page-screen-right" "Right margin")
  ("page-screen-top" "Top margin")
  ("page-screen-bot" "Bottom margin")

  ("table-width" "Table width")
  ("table-height" "Table height")
  ("table-lsep" "Left table padding")
  ("table-rsep" "Right table padding")
  ("table-bsep" "Bottom table padding")
  ("table-tsep" "Top table padding")
  ("table-lborder" "Left table border")
  ("table-rborder" "Right table border")
  ("table-bborder" "Bottom table border")
  ("table-tborder" "Top table border")
  ("table-row-origin" "Origin row")
  ("table-col-origin" "Origin column")
  ("table-min-rows" "Minimal number of rows")
  ("table-max-rows" "Maximal number of rows")
  ("table-min-cols" "Minimal number of columns")
  ("table-max-cols" "Maximal number of columns")
  
  ("cell-width" "Cell width")
  ("cell-height" "Cell height")
  ("cell-hpart" "Part in unused horizontal space")
  ("cell-vpart" "Part in unused vertical space")
  ("cell-lsep" "Left cell padding")
  ("cell-rsep" "Right cell padding")
  ("cell-bsep" "Bottom cell padding")
  ("cell-tsep" "Top cell padding")
  ("cell-lborder" "Left cell border")
  ("cell-rborder" "Right cell border")
  ("cell-bborder" "Bottom cell border")
  ("cell-tborder" "Top cell border"))
