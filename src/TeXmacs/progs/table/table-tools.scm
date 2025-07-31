
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

(tm-widget (cell-advanced-tool win)
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
      (dynamic (cell-advanced-tool win)))))
