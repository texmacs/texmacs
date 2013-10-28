
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : std-text-kbd.scm
;; DESCRIPTION : enter semantic textual markup using the keyboard
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text std-text-kbd)
  (:use (text text-kbd)
	(text std-text-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing the text format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-text?)
  ("font ^" (make-script #t #t))
  ("font hat" (make-script #t #t))
  ("font _" (make-script #f #t))
  ("font s" (make-with "font-family" "ss"))
  ("font t" (make-with "font-family" "tt"))
  ("font b" (make-with "font-series" "bold"))
  ("font m" (make-with "font-series" "medium"))
  ("font r" (make-with "font-shape" "right"))
  ("font i" (make-with "font-shape" "italic"))
  ("font l" (make-with "font-shape" "slanted"))
  ("font o" (make 'overline))
  ("font p" (make-with "font-shape" "small-caps"))
  ("font u" (make 'underline)))

(kbd-map
  (:profile macos)
  (:mode in-text?)
  ("macos {" (make-line-with "par-mode" "left"))
  ("macos |" (make-line-with "par-mode" "center"))
  ("macos }" (make-line-with "par-mode" "right"))
  ("macos C-{" (make-line-with "par-mode" "justify")))

(kbd-map
  (:profile windows)
  (:mode in-text?)
  ("windows 1" (make-line-with "par-line-sep" "0fn"))
  ("windows 2" (make-line-with "par-line-sep" "1fn"))
  ("windows 5" (make-line-with "par-line-sep" "0.5fn"))
  ("windows l" (make-line-with "par-mode" "left"))
  ("windows e" (make-line-with "par-mode" "center"))
  ("windows r" (make-line-with "par-mode" "right"))
  ("windows j" (make-line-with "par-mode" "justify"))
  ("windows t" (make 'indent)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard markup in text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-std-text?)
  ("text $" (make-equation*))
  ("text &" (make-eqnarray*))

  ("text a" (make 'abbr))
  ("text d" (make-tmlist 'description))
  ("text e" (make-tmlist 'enumerate))
  ("text i" (make-tmlist 'itemize))
  ("text m" (make 'em))
  ("text n" (make 'name))
  ("text p" (make 'samp))
  ("text s" (make 'strong))
  ("text v" (make 'verbatim))
  ("text ;" (make-item))
  ("text 0" (make-section 'chapter))
  ("text 1" (make-section 'section))
  ("text 2" (make-section 'subsection))
  ("text 3" (make-section 'subsubsection))
  ("text 4" (make-section 'paragraph))
  ("text 5" (make-section 'subparagraph))

  ("F5" (make 'em))
  ("F6" (make 'strong))
  ("F7" (make 'verbatim))
  ("F8" (make 'samp))
  ("S-F6" (make 'name)))
