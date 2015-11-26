
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
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cell-set var val)
  (let* ((active? (selection-active-table?))
         (p1 (selection-get-start))
         (p2 (selection-get-end)))
    (when val
      (cell-set-format var val)
      (cond ((and (== var "cell-hmode") (== val "auto"))
             (cell-set-format "cell-width" ""))
            ((and (== var "cell-width") (!= val "")
                  (== (cell-get-format "cell-hmode") "auto"))
             (cell-set-format "cell-hmode" "exact"))
            ((and (== var "cell-vmode") (== val "auto"))
             (cell-set-format "cell-height" ""))
            ((and (== var "cell-height") (!= val "")
                  (== (cell-get-format "cell-vmode") "auto"))
             (cell-set-format "cell-vmode" "exact")))
      ;;(refresh-now "cell-properties")
      (if active? ;; FIXME: find a robust way to keep the selection
          (selection-set p1 p2)))))

(define (cell-get var)
  (cell-get-format var))

(define (cell-get-background)
  (with bg (cell-get-format "cell-background")
    (if (and (string? bg) (!= bg "")) bg "white")))

(define (table-set var val)
  (when val
    (table-set-format var val)
    (cond ((and (== var "table-hmode") (== val "auto"))
	   (table-set-format "table-width" ""))
	  ((and (== var "table-width") (!= val "")
		(== (table-get-format "table-hmode") "auto"))
	   (table-set-format "table-hmode" "exact"))
	  ((and (== var "table-vmode") (== val "auto"))
	   (table-set-format "table-height" ""))
	  ((and (== var "table-height") (!= val "")
		(== (table-get-format "table-vmode") "auto"))
	   (table-set-format "table-vmode" "exact")))
    ;;(refresh-now "table-properties")
    ))

(define (table-get var)
  (table-get-format var))

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

(tm-widget (cell-size-color-widget)
  (refreshable "cell-properties"
    (aligned
      (item (text "Width:")
	(horizontal
	  (enum (cell-set "cell-hmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (cell-get "cell-hmode"))
		"7em")
	  ///
	  (input (cell-set "cell-width" answer) "string"
		 (list (cell-get "cell-width")) "6em")
	  /// //
	  (text "Stretch:")
	  //
	  (input (cell-set "cell-hpart" answer) "string"
		 (list (cell-get "cell-hpart")) "6em")))
      (item (text "Height:")
	(horizontal
	  (enum (cell-set "cell-vmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (cell-get "cell-vmode"))
		"7em")
	  ///
	  (input (cell-set "cell-height" answer) "string"
		 (list (cell-get "cell-height")) "6em")
	  /// //
	  (text "Stretch:")
	  //
	  (input (cell-set "cell-vpart" answer) "string"
		 (list (cell-get "cell-vpart")) "6em"))))
    === ===
    (horizontal
      (text "Text height correction:")
      ///
      (enum (cell-set "cell-vcorrect" (encode-vcorrect answer))
	    '("Off" "Bottom" "Top" "Both")
	    (decode-vcorrect (cell-get "cell-vcorrect"))
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
	(input (cell-set "cell-lborder" answer) "string"
	       (list (cell-get "cell-lborder") "0ln" "1ln") "6em"))
      (item (text "Right:")
	(input (cell-set "cell-rborder" answer) "string"
	       (list (cell-get "cell-rborder") "0ln" "1ln") "6em"))
      (item (text "Top:")
	(input (cell-set "cell-tborder" answer) "string"
	       (list (cell-get "cell-tborder") "0ln" "1ln") "6em"))
      (item (text "Bottom:")
	(input (cell-set "cell-bborder" answer) "string"
	       (list (cell-get "cell-bborder") "0ln" "1ln") "6em")))))

(tm-widget (cell-padding-widget)
  (horizontal
    >>> (bold (text "Padding")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Left:")
	(input (cell-set "cell-lsep" answer) "string"
	       (list (cell-get "cell-lsep") "1spc") "6em"))
      (item (text "Right:")
	(input (cell-set "cell-rsep" answer) "string"
	       (list (cell-get "cell-rsep") "1spc") "6em"))
      (item (text "Top:")
	(input (cell-set "cell-tsep" answer) "string"
	       (list (cell-get "cell-tsep") "1sep") "6em"))
      (item (text "Bottom:")
	(input (cell-set "cell-bsep" answer) "string"
	       (list (cell-get "cell-bsep") "1sep") "6em")))))

(tm-widget (cell-alignment-widget)
  (horizontal
    >>> (bold (text "Alignment")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Horizontal:")
        (enum (cell-set "cell-halign" (encode-halign answer))
              '("Left" "Center" "Right" "Decimal dot" "Decimal comma")
              (decode-halign (cell-get "cell-halign"))
              "7em"))
      (item (text "Vertical:")
        (enum (cell-set "cell-valign" (encode-valign answer))
              '("Top" "Center" "Bottom" "Baseline")
              (decode-valign (cell-get "cell-halign"))
              "7em")))))

(tm-widget (cell-large-widget)
  (horizontal
    >>> (bold (text "Large cells")) >>>)
  ===
  (refreshable "cell-properties"
    (aligned
      (item (text "Line wrapping:")
        (enum (cell-set "cell-hyphen" (encode-hyphen answer))
              '("Off" "Top" "Center" "Bottom")
              (decode-hyphen (cell-get "cell-hyphen"))
              "11em"))
      (item (text "Block content:")
        (enum (cell-set "cell-block" (encode-block answer))
              '("Never" "When wrapping" "Always")
              (decode-block (cell-get "cell-block"))
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
	  (enum (table-set "table-min-rows" answer)
                (list (table-get "table-min-rows")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get "table-min-rows") "3em")
          /// ///
          (text "Maximum:") ///
	  (enum (table-set "table-max-rows" answer)
                (list (table-get "table-max-rows")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get "table-max-rows") "3em")))
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
	  (enum (table-set "table-min-cols" answer)
                (list (table-get "table-min-cols")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get "table-min-cols") "3em")
          /// ///
          (text "Maximum:") ///
	  (enum (table-set "table-max-cols" answer)
                (list (table-get "table-max-cols")
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
		(table-get "table-max-cols") "3em")))
      (item === ===)
      (item (text "Width:")
	(horizontal
	  (enum (table-set "table-hmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get "table-hmode"))
		"7em")
	  ///
	  (input (table-set "table-width" answer) "string"
		 (list (table-get "table-width") "1par") "6em")
          >>> >>>))
      (item (text "Height:")
	(horizontal
	  (enum (table-set "table-vmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get "table-vmode"))
		"7em")
	  ///
	  (input (table-set "table-height" answer) "string"
		 (list (table-get "table-height")) "6em")
          >>> >>>)))))

(tm-widget (table-border-widget)
  (horizontal
    >>> (bold (text "Border")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Left:")
	(input (table-set "table-lborder" answer) "string"
	       (list (table-get "table-lborder") "0ln" "1ln") "6em"))
      (item (text "Right:")
	(input (table-set "table-rborder" answer) "string"
	       (list (table-get "table-rborder") "0ln" "1ln") "6em"))
      (item (text "Top:")
	(input (table-set "table-tborder" answer) "string"
	       (list (table-get "table-tborder") "0ln" "1ln") "6em"))
      (item (text "Bottom:")
	(input (table-set "table-bborder" answer) "string"
	       (list (table-get "table-bborder") "0ln" "1ln") "6em")))))

(tm-widget (table-padding-widget)
  (horizontal
    >>> (bold (text "Padding")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Left:")
	(input (table-set "table-lsep" answer) "string"
	       (list (table-get "table-lsep") "0fn") "6em"))
      (item (text "Right:")
	(input (table-set "table-rsep" answer) "string"
	       (list (table-get "table-rsep") "0fn") "6em"))
      (item (text "Top:")
	(input (table-set "table-tsep" answer) "string"
	       (list (table-get "table-tsep") "0fn") "6em"))
      (item (text "Bottom:")
	(input (table-set "table-bsep" answer) "string"
	       (list (table-get "table-bsep") "0fn") "6em")))))

(tm-widget (table-alignment-widget)
  (horizontal
    >>> (bold (text "Alignment")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Horizontal:")
        (enum (table-set "table-halign" (encode-halign answer))
              '("Left" "Center" "Right")
              (decode-halign (table-get "table-halign"))
              "10em"))
      (item (text "Vertical:")
        (enum (table-set "table-valign" (encode-valign* answer))
              '("Axis" "Top" "Center" "Bottom"
                "Top baseline" "Center baseline" "Bottom baseline")
              (decode-valign* (table-get "table-halign"))
              "10em")))))

(tm-widget (table-large-widget)
  (horizontal
    >>> (bold (text "Large tables")) >>>)
  ===
  (refreshable "table-properties"
    (aligned
      (meti (horizontal // (text "Enable page breaking"))
        (toggle
         (table-set "table-hyphen" (if answer "y" "n"))
         (== (table-get "table-hyphen") "y"))))
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
