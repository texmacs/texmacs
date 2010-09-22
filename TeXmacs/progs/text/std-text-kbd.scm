
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
  (:use (text format-text-edit)
	(text std-text-edit)))

(kbd-map
  (:mode in-std-text?)
  ("$" (if (inside? 'hybrid) (insert "$") (make 'math)))
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
