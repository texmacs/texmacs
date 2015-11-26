
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
    ))

(define (cell-get var)
  (cell-get-format var))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (cell-size-widget)
  (bold (text "Size"))
  ===
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
	  /// ///
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
	  /// ///
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
      >>>)))

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

(tm-widget (cell-properties-widget quit)
  (padded
    (dynamic (cell-size-widget))
    === === ===
    (horizontal
      >>>
      (vertical
	(dynamic (cell-border-widget)))
      >>>
      (vertical
	(dynamic (cell-padding-widget)))
      >>>)))

(tm-define (open-cell-properties)
  (:interactive #t)
  (dialogue-window cell-properties-widget noop "Cell properties"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (table-size-widget)
  (bold (text "Size"))
  ===
  (refreshable "table-properties"
    (aligned
      (item (text "Width:")
	(horizontal
	  (enum (table-set "table-hmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get "table-hmode"))
		"7em")
	  ///
	  (input (table-set "table-width" answer) "string"
		 (list (table-get "table-width") "1par") "6em")))
      (item (text "Height:")
	(horizontal
	  (enum (table-set "table-vmode" (encode-mode answer))
		'("Auto" "Exact" "Minimal" "Maximal")
		(decode-mode (table-get "table-vmode"))
		"7em")
	  ///
	  (input (table-set "table-height" answer) "string"
		 (list (table-get "table-height")) "6em"))))))

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

(tm-widget (table-properties-widget quit)
  (padded
    (dynamic (table-size-widget))
    === === ===
    (horizontal
      >>>
      (vertical
	(dynamic (table-border-widget)))
      >>>
      (vertical
	(dynamic (table-padding-widget)))
      >>>)))

(tm-define (open-table-properties)
  (:interactive #t)
  (dialogue-window table-properties-widget noop "Table properties"))
