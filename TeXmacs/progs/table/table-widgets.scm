
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-widgets.scm
;; DESCRIPTION : Widgets for table and cell properties
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-widgets)
  (:use (table table-menu)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encode/decode properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (decode-mode m)
  (cond ((== m "auto") "Auto")
	((== m "exact") "Exact")
	((== m "min") "Maximal")
	((== m "max") "Minimal")
	(else "Auto")))

(define (encode-mode m)
  (cond ((== m "Auto") "auto")
	((== m "Exact") "exact")
	((== m "Minimal") "max")
	((== m "Maximal") "min")
	(else "auto")))

(define (decode-vcorrect m)
  (cond ((== m "n") "Off")
	((== m "b") "Bottom")
	((== m "t") "Top")
	((== m "a") "Both")
	(else "Both")))

(define (encode-vcorrect m)
  (cond ((== m "Off") "n")
	((== m "Bottom") "b")
	((== m "Top") "t")
	((== m "Both") "a")
	(else "a")))

(define (decode-halign m)
  (cond ((== m "l") "Left")
	((== m "c") "Center")
	((== m "r") "Right")
	((== m "L.") "Decimal dot")
	((== m "L,") "Decimal comma")
	(else "Left")))

(define (encode-halign m)
  (cond ((== m "Left") "l")
	((== m "Center") "c")
	((== m "Right") "r")
	((== m "Decimal dot") "L.")
	((== m "Decimal comma") "L,")
	(else "l")))

(define (decode-valign m)
  (cond ((== m "t") "Top")
	((== m "c") "Center")
	((== m "b") "Bottom")
	((== m "B") "Baseline")
	(else "Baseline")))

(define (encode-valign m)
  (cond ((== m "Top") "t")
	((== m "Center") "c")
	((== m "Bottom") "b")
	((== m "Baseline") "B")
	(else "B")))

(define (decode-valign* m)
  (cond ((== m "f") "Axis")
	((== m "t") "Top")
	((== m "c") "Center")
	((== m "b") "Bottom")
	((== m "T") "Top baseline")
	((== m "C") "Center baseline")
	((== m "B") "Bottom baseline")
	(else "Axis")))

(define (encode-valign* m)
  (cond ((== m "Axis") "f")
	((== m "Top") "t")
	((== m "Center") "c")
	((== m "Bottom") "b")
	((== m "Top baseline") "T")
	((== m "Center baseline") "C")
	((== m "Bottom baseline") "B")
	(else "f")))

(define (decode-hyphen m)
  (cond ((== m "t") "Top")
	((== m "c") "Center")
	((== m "b") "Bottom")
	((== m "n") "Off")
	(else "Off")))

(define (encode-hyphen m)
  (cond ((== m "Top") "t")
	((== m "Center") "c")
	((== m "Bottom") "b")
	((== m "Off") "n")
	(else "n")))

(define (decode-block m)
  (cond ((== m "no") "Never")
	((== m "auto") "When wrapping")
	((== m "yes") "Always")
	(else "When wrapping")))

(define (encode-block m)
  (cond ((== m "Never") "no")
	((== m "When wrapping") "auto")
	((== m "Always") "yes")
	(else "auto")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cell-get-background)
  (with bg (cell-get-format "cell-background")
    (if (and (string? bg) (!= bg "")) bg "white")))

(tm-widget (cell-size-color-widget)
  (refreshable "cell-properties"
    (aligned
      (item (text "Width:")
	(horizontal
	  (enum (cell-set-format* "cell-hmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (cell-get-format "cell-hmode"))
		"7em")
	  ///
	  (input (cell-set-format* "cell-width" answer) "string"
		 (list (cell-get-format "cell-width")) "6em")
	  /// //
	  (text "Stretch:")
	  //
	  (input (cell-set-format* "cell-hpart" answer) "string"
		 (list (cell-get-format "cell-hpart")) "6em")))
      (item (text "Height:")
	(horizontal
	  (enum (cell-set-format* "cell-vmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (cell-get-format "cell-vmode"))
		"7em")
	  ///
	  (input (cell-set-format* "cell-height" answer) "string"
		 (list (cell-get-format "cell-height")) "6em")
	  /// //
	  (text "Stretch:")
	  //
	  (input (cell-set-format* "cell-vpart" answer) "string"
		 (list (cell-get-format "cell-vpart")) "6em"))))
    === ===
    (horizontal
      (text "Text height correction:")
      ///
      (enum (cell-set-format* "cell-vcorrect" (encode-vcorrect answer))
	    '("Off" "Bottom" "Top" "Both")
	    (decode-vcorrect (cell-get-format "cell-vcorrect"))
	    "7em")
      >>> >>>
      ;;(text "Background color:")
      ;;//
      ;;(=> (color (cell-get-background) #f #f 25 17)
      ;;    (link cell-color-menu))
      )))

(tm-widget (cell-border-widget)
  (horizontal
    >>> (bold (text "Border")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Left:")
	(input (cell-set-format* "cell-lborder" answer) "string"
	       (list (cell-get-format "cell-lborder") "0ln" "1ln") "6em"))
      (item (text "Right:")
	(input (cell-set-format* "cell-rborder" answer) "string"
	       (list (cell-get-format "cell-rborder") "0ln" "1ln") "6em"))
      (item (text "Top:")
	(input (cell-set-format* "cell-tborder" answer) "string"
	       (list (cell-get-format "cell-tborder") "0ln" "1ln") "6em"))
      (item (text "Bottom:")
	(input (cell-set-format* "cell-bborder" answer) "string"
	       (list (cell-get-format "cell-bborder") "0ln" "1ln") "6em")))))

(tm-widget (cell-padding-widget)
  (horizontal
    >>> (bold (text "Padding")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Left:")
	(input (cell-set-format* "cell-lsep" answer) "string"
	       (list (cell-get-format "cell-lsep") "1spc") "6em"))
      (item (text "Right:")
	(input (cell-set-format* "cell-rsep" answer) "string"
	       (list (cell-get-format "cell-rsep") "1spc") "6em"))
      (item (text "Top:")
	(input (cell-set-format* "cell-tsep" answer) "string"
	       (list (cell-get-format "cell-tsep") "1sep") "6em"))
      (item (text "Bottom:")
	(input (cell-set-format* "cell-bsep" answer) "string"
	       (list (cell-get-format "cell-bsep") "1sep") "6em")))))

(tm-widget (cell-alignment-widget)
  (horizontal
    >>> (bold (text "Alignment")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Horizontal:")
        (enum (cell-set-format* "cell-halign" (encode-halign answer))
              '("Left" "Center" "Right" "Decimal dot" "Decimal comma")
              (decode-halign (cell-get-format "cell-halign"))
              "7em"))
      (item (text "Vertical:")
        (enum (cell-set-format* "cell-valign" (encode-valign answer))
              '("Top" "Center" "Bottom" "Baseline")
              (decode-valign (cell-get-format "cell-valign"))
              "7em")))))

(tm-widget (cell-large-widget)
  (horizontal
    >>> (bold (text "Large cells")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Line wrapping:")
        (enum (cell-set-format* "cell-hyphen" (encode-hyphen answer))
              '("Off" "Top" "Center" "Bottom")
              (decode-hyphen (cell-get-format "cell-hyphen"))
              "11em"))
      (item (text "Block content:")
        (enum (cell-set-format* "cell-block" (encode-block answer))
              '("Never" "When wrapping" "Always")
              (decode-block (cell-get-format "cell-block"))
              "11em")))))

(tm-widget (cell-properties-widget quit)
  (padded
    (horizontal
      (vertical
        (dynamic (cell-size-color-widget)))
      >>>)
    ====== ======
    (horizontal
      >>>
      (vertical
	(dynamic (cell-border-widget)))
      >>>
      (vertical
	(dynamic (cell-padding-widget)))
      >>>)
    ====== ======
    (horizontal
      (vertical
        (dynamic (cell-alignment-widget)))
      >>>
      (vertical
	(dynamic (cell-large-widget))))))

(tm-define (open-cell-properties)
  (:interactive #t)
  (dialogue-window cell-properties-widget noop "Cell properties"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (table-size-widget)
  (refreshable "table-properties"
    (aligned
      (item (text "Rows:")
	(horizontal
	  (enum (when answer
                  (table-set-extents (string->number answer)
                                     (table-nr-columns)))
                (list (number->string (table-nr-rows))
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
                (number->string (table-nr-rows)) "3em")
          /// ///
          (text "Minimum:") ///
	  (enum (table-set-format* "table-min-rows" answer)
                (list (table-get-format "table-min-rows")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get-format "table-min-rows") "3em")
          /// ///
          (text "Maximum:") ///
	  (enum (table-set-format* "table-max-rows" answer)
                (list (table-get-format "table-max-rows")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get-format "table-max-rows") "3em")))
      (item (text "Columns:")
	(horizontal
	  (enum (when answer
                  (table-set-extents (table-nr-rows)
                                     (string->number answer)))
                (list (number->string (table-nr-columns))
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
                (number->string (table-nr-columns)) "3em")
          /// ///
          (text "Minimum:") ///
	  (enum (table-set-format* "table-min-cols" answer)
                (list (table-get-format "table-min-cols")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get-format "table-min-cols") "3em")
          /// ///
          (text "Maximum:") ///
	  (enum (table-set-format* "table-max-cols" answer)
                (list (table-get-format "table-max-cols")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get-format "table-max-cols") "3em")))
      (item === ===)
      (item (text "Width:")
	(horizontal
	  (enum (table-set-format* "table-hmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get-format "table-hmode"))
		"7em")
	  ///
	  (input (table-set-format* "table-width" answer) "string"
		 (list (table-get-format "table-width") "1par") "6em")
          >>> >>>))
      (item (text "Height:")
	(horizontal
	  (enum (table-set-format* "table-vmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get-format "table-vmode"))
		"7em")
	  ///
	  (input (table-set-format* "table-height" answer) "string"
		 (list (table-get-format "table-height")) "6em")
          >>> >>>)))))

(tm-widget (table-border-widget)
  (horizontal
    >>> (bold (text "Border")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Left:")
	(input (table-set-format* "table-lborder" answer) "string"
	       (list (table-get-format "table-lborder") "0ln" "1ln") "6em"))
      (item (text "Right:")
	(input (table-set-format* "table-rborder" answer) "string"
	       (list (table-get-format "table-rborder") "0ln" "1ln") "6em"))
      (item (text "Top:")
	(input (table-set-format* "table-tborder" answer) "string"
	       (list (table-get-format "table-tborder") "0ln" "1ln") "6em"))
      (item (text "Bottom:")
	(input (table-set-format* "table-bborder" answer) "string"
	       (list (table-get-format "table-bborder") "0ln" "1ln") "6em")))))

(tm-widget (table-padding-widget)
  (horizontal
    >>> (bold (text "Padding")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Left:")
	(input (table-set-format* "table-lsep" answer) "string"
	       (list (table-get-format "table-lsep") "0fn") "6em"))
      (item (text "Right:")
	(input (table-set-format* "table-rsep" answer) "string"
	       (list (table-get-format "table-rsep") "0fn") "6em"))
      (item (text "Top:")
	(input (table-set-format* "table-tsep" answer) "string"
	       (list (table-get-format "table-tsep") "0fn") "6em"))
      (item (text "Bottom:")
	(input (table-set-format* "table-bsep" answer) "string"
	       (list (table-get-format "table-bsep") "0fn") "6em")))))

(tm-widget (table-alignment-widget)
  (horizontal
    >>> (bold (text "Alignment")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Horizontal:")
        (enum (table-set-format* "table-halign" (encode-halign answer))
              '("Left" "Center" "Right")
              (decode-halign (table-get-format "table-halign"))
              "10em"))
      (item (text "Vertical:")
        (enum (table-set-format* "table-valign" (encode-valign* answer))
              '("Axis" "Top" "Center" "Bottom"
                "Top baseline" "Center baseline" "Bottom baseline")
              (decode-valign* (table-get-format "table-halign"))
              "10em")))))

(tm-widget (table-large-widget)
  (horizontal
    >>> (bold (text "Large tables")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (meti (horizontal // (text "Enable page breaking"))
        (toggle
         (table-set-format* "table-hyphen" (if answer "y" "n"))
         (== (table-get-format "table-hyphen") "y"))))
    (glue #f #t 0 0)))

(tm-widget (table-properties-widget quit)
  (padded
    (horizontal
      (vertical
        (dynamic (table-size-widget)))
      >>>)
    ====== ======
    (horizontal
      >>>
      (vertical
	(dynamic (table-border-widget)))
      >>>
      (vertical
	(dynamic (table-padding-widget)))
      >>>)
    ====== ======
    (horizontal
      >>>
      (vertical
        (dynamic (table-alignment-widget)))
      >>>
      (vertical
	(dynamic (table-large-widget)))
      >>>)))

(tm-define (open-table-properties)
  (:interactive #t)
  (dialogue-window table-properties-widget noop "Table properties"))
