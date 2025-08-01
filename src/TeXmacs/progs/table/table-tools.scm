
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : table-tools.scm
;; DESCRIPTION : Tools for table and cell properties
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (table table-tools)
  (:use (table table-widgets)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Encode/decode properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define encode-mode table-encode-mode)
(define decode-mode table-decode-mode)
(define encode-vcorrect table-encode-vcorrect)
(define decode-vcorrect table-decode-vcorrect)
(define encode-halign table-encode-halign)
(define decode-halign table-decode-halign)
(define encode-valign table-encode-valign)
(define decode-valign table-decode-valign)
(define encode-valign* table-encode-valign*)
(define decode-valign* table-decode-valign*)
(define encode-hyphen table-encode-hyphen)
(define decode-hyphen table-decode-hyphen)

(define (decode-block m)
  (cond ((== m "no") "Never")
	((== m "auto") "Auto")
	((== m "yes") "Always")
	(else "Auto")))

(define (encode-block m)
  (cond ((== m "Never") "no")
	((== m "Auto") "auto")
	((== m "Always") "yes")
	(else "auto")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (window-inside-table? win)
  (with-window win
    (inside? 'table)))

(tm-define (window-cell-get-format win var)
  (with-window win
    (cell-get-format var)))

(tm-define (window-cell-set-format* win var val)
  (with-window win
    (cell-set-format* var val)
    (update-menus)))

(tm-define (window-table-get-format win var)
  (with-window win
    (table-get-format var)))

(tm-define (window-table-set-format* win var val)
  (with-window win
    (table-set-format* var val)
    (update-menus)))

(tm-define (window-table-set-extents win r c) 
  (with-window win
    (table-set-extents r c)))

(tm-define (window-table-nr-rows win) 
  (with-window win
    (table-nr-rows)))

(tm-define (window-table-nr-columns win) 
  (with-window win
    (table-nr-columns)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cell properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (cell-basic-tool win)
  (aligned
    (item (text "Width:")
      (with mode (decode-mode (window-cell-get-format win "cell-hmode"))
        (horizontal
          (enum (window-cell-set-format* win "cell-hmode" (encode-mode answer))
                '("Auto" "Exact" "Minimal" "Maximal") mode "6em")
          ///
          (when (!= mode "Auto")
            (input (window-cell-set-format* win "cell-width" answer) "string"
                   (list (window-cell-get-format win "cell-width"))
                   "6em")))))
    (item (text "Height:")
      (with mode (decode-mode (window-cell-get-format win "cell-vmode"))
        (horizontal
          (enum (window-cell-set-format* win "cell-vmode" (encode-mode answer))
                '("Auto" "Exact" "Minimal" "Maximal") mode "6em")
          ///
          (when (!= mode "Auto")
            (input (window-cell-set-format* win "cell-height" answer) "string"
                   (list (window-cell-get-format win "cell-height"))
                   "6em")))))
    (item (text "Align:")
      (let* ((ha (decode-halign (window-cell-get-format win "cell-halign")))
             (va (decode-valign (window-cell-get-format win "cell-valign"))))
        (horizontal
          (enum (cell-set-format* "cell-halign" (encode-halign answer))
                '("Left" "Center" "Right" "Decimal dot" "Decimal comma")
                ha "6em")
          ///
          (enum (cell-set-format* "cell-valign" (encode-valign answer))
                '("Top" "Center" "Bottom" "Baseline") va "6em"))))))

(tm-widget (cell-border-tool win)
  (let* ((set (lambda (v a) (window-cell-set-format* win v a)))
         (get (lambda (v) (list (window-cell-get-format win v) "0ln" "1ln")))
         (get* (lambda (v) (list (window-cell-get-format win v) "1spc")))
         (get** (lambda (v) (list (window-cell-get-format win v) "1sep"))))
    (horizontal
      (glue #t #f 0 0)
      (tile 3
        (text "")
        (horizontal // (text "Border") >>>)
        (horizontal // (text "Padding") >>>)

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Left:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "cell-lborder" answer) "string"
                 (get "cell-lborder") "6em") // //)
        (input (set "cell-lsep" answer) "string"
               (get* "cell-lsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Right:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "cell-rborder" answer) "string"
                 (get "cell-rborder") "6em") // //)
        (input (set "cell-rsep" answer) "string"
               (get* "cell-rsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Top:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "cell-tborder" answer) "string"
                 (get "cell-tborder") "6em") // //)
        (input (set "cell-tsep" answer) "string"
               (get** "cell-tsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Bottom:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "cell-bborder" answer) "string"
                 (get "cell-bborder") "6em") // //)
        (input (set "cell-bsep" answer) "string"
               (get** "cell-bsep") "6em"))
      (glue #t #f 0 0))))

(tm-widget (cell-special-tool win)
  (aligned
    (item (text "Line wrapping:")
      (enum (window-cell-set-format* win "cell-hyphen" (encode-hyphen answer))
            '("Off" "Top" "Center" "Bottom")
            (decode-hyphen (window-cell-get-format win "cell-hyphen"))
            "5em"))
    (item (text "Block content:")
      (enum (window-cell-set-format* win "cell-block" (encode-block answer))
            '("Never" "Auto" "Always")
            (decode-block (window-cell-get-format win "cell-block"))
            "5em"))
    (item (text "Horizontal stretch:")
      (input (window-cell-set-format* win "cell-hpart" answer) "string"
             (list (window-cell-get-format win "cell-hpart")) "5em"))
    (item (text "Vertical stretch:")
      (input (window-cell-set-format* win "cell-vpart" answer) "string"
             (list (window-cell-get-format win "cell-vpart")) "5em"))
    (item (text "Height correction:")
      (enum (window-cell-set-format* win "cell-vcorrect"
                                     (encode-vcorrect answer))
	    '("Off" "Bottom" "Top" "Both")
	    (decode-vcorrect (window-cell-get-format win "cell-vcorrect"))
	    "5em"))))

(tm-tool* (cell-properties-tool win)
  (:name "Cell properties")
  (centered
    (when (window-inside-table? win)
      (dynamic (cell-basic-tool win))))
  === ===
  (centered
    (when (window-inside-table? win)
      (dynamic (cell-border-tool win))))
  === ===
  (centered
    (when (window-inside-table? win)
      (dynamic (cell-special-tool win)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (table-extents-tool win)
  (let* ((set (lambda (v a) (window-table-set-format* win v a)))
         (get (lambda (v) (list (window-table-get-format win v)
                                "1" "2" "3" "4" "5" "6" "7" "8" ""))))
    (horizontal
      (glue #t #f 0 0)
      (tile 4
        (text "")
        (text "")
        (horizontal // (text "Minimal") >>>)
        (horizontal // (text "Maximal") >>>)

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Rows:") // //)
                  (glue #f #f 0 6))
        (horizontal
	  (enum (when answer
                  (window-table-set-extents
                   win (string->number answer) (window-table-nr-columns win)))
                (list (number->string (window-table-nr-rows win))
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
                (number->string (window-table-nr-rows win)) "4em") // // //)
        (horizontal
	  (enum (set "table-min-rows" answer)
                (get "table-min-rows")
		(table-get-format "table-min-rows") "4em") //)
        (enum (set "table-max-rows" answer)
              (get "table-max-rows")
              (table-get-format "table-max-rows") "4em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Columns:") // //)
                  (glue #f #f 0 6))
        (horizontal
	  (enum (when answer
                  (window-table-set-extents
                   win (window-table-nr-rows win) (string->number answer)))
                (list (number->string (window-table-nr-columns win))
                      "1" "2" "3" "4" "5" "6" "7" "8" "")
                (number->string (window-table-nr-columns win)) "4em") // // //)
        (horizontal
	  (enum (set "table-min-cols" answer)
                (get "table-min-cols")
		(table-get-format "table-min-cols") "4em") //)
        (enum (set "table-max-cols" answer)
              (get "table-max-cols")
              (table-get-format "table-max-cols") "4em"))
      (glue #t #f 0 0))))

(tm-widget (table-basic-tool win)
  (aligned
    (item (text "Width:")
      (with mode (decode-mode (window-table-get-format win "table-hmode"))
        (horizontal
          (enum (window-table-set-format* win "table-hmode"
                                          (encode-mode answer))
                '("Auto" "Exact" "Minimal" "Maximal") mode "6em")
          ///
          (when (!= mode "Auto")
            (input (window-table-set-format* win "table-width" answer) "string"
                   (list (window-table-get-format win "table-width"))
                   "6em")))))
    (item (text "Height:")
      (with mode (decode-mode (window-table-get-format win "table-vmode"))
        (horizontal
          (enum (window-table-set-format* win "table-vmode"
                                          (encode-mode answer))
                '("Auto" "Exact" "Minimal" "Maximal") mode "6em")
          ///
          (when (!= mode "Auto")
            (input (window-table-set-format* win "table-height" answer) "string"
                   (list (window-table-get-format win "table-height"))
                   "6em")))))
    (item (text "Align:")
      (let* ((ha (decode-halign (window-table-get-format win "table-halign")))
             (va (decode-valign* (window-table-get-format win "table-valign"))))
        (horizontal
          (enum (table-set-format* "table-halign" (encode-halign answer))
                '("Left" "Center" "Right") ha "6em")
          ///
          (enum (table-set-format* "table-valign" (encode-valign* answer))
                '("Axis" "Top" "Center" "Bottom"
                  "Top baseline" "Center baseline" "Bottom baseline")
                va "6em"))))))

(tm-widget (table-border-tool win)
  (let* ((set (lambda (v a) (window-table-set-format* win v a)))
         (get (lambda (v) (list (window-table-get-format win v) "0ln" "1ln")))
         (get* (lambda (v) (list (window-table-get-format win v) "0fn"))))
    (horizontal
      (glue #t #f 0 0)
      (tile 3
        (text "")
        (horizontal // (text "Border") >>>)
        (horizontal // (text "Padding") >>>)

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Left:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "table-lborder" answer) "string"
                 (get "table-lborder") "6em") // //)
        (input (set "table-lsep" answer) "string"
               (get* "table-lsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Right:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "table-rborder" answer) "string"
                 (get "table-rborder") "6em") // //)
        (input (set "table-rsep" answer) "string"
               (get* "table-rsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Top:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "table-tborder" answer) "string"
                 (get "table-tborder") "6em") // //)
        (input (set "table-tsep" answer) "string"
               (get* "table-tsep") "6em")

        (vertical (glue #f #f 0 6)
                  (horizontal (glue #t #f 0 0) (text "Bottom:") // //)
                  (glue #f #f 0 6))
        (horizontal
          (input (set "table-bborder" answer) "string"
                 (get "table-bborder") "6em") // //)
        (input (set "table-bsep" answer) "string"
               (get* "table-bsep") "6em"))
      (glue #t #f 0 0))))

(tm-widget (table-special-tool win)
  (aligned
    (meti (horizontal // (text "Enable page breaking"))
      (toggle
       (window-table-set-format* win "table-hyphen" (if answer "y" "n"))
       (== (window-table-get-format win "table-hyphen") "y")))))

(tm-tool* (table-properties-tool win)
  (:name "Table properties")
  (centered
    (when (window-inside-table? win)
      (dynamic (table-extents-tool win))))
  === ===
  (centered
    (when (window-inside-table? win)
      (dynamic (table-basic-tool win))))
  === ===
  (centered
    (when (window-inside-table? win)
      (dynamic (table-border-tool win))))
  === ===
  (centered
    (when (window-inside-table? win)
      (dynamic (table-special-tool win)))))
