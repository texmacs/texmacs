
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-x.scm
;; DESCRIPTION : setup X fonts for text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-x))

(set-font-rules
  '(((x-times rm medium right $s $d) (x adobe-times-medium-r-normal $s $d))
    ((x-times rm medium italic $s $d) (x adobe-times-medium-i-normal $s $d))
    ((x-times rm bold right $s $d) (x adobe-times-bold-r-normal $s $d))
    ((x-times rm bold italic $s $d) (x adobe-times-bold-i-normal $s $d))

    ((x-times tt medium right $s $d) (x adobe-courier-medium-r-normal $s $d))
    ((x-times tt medium slanted $s $d)
      (x adobe-courier-medium-o-normal $s $d))
    ((x-times tt medium italic $s $d) (x adobe-courier-medium-i-normal $s $d))
    ((x-times tt bold right $s $d) (x adobe-courier-bold-r-normal $s $d))
    ((x-times tt bold slanted $s $d) (x adobe-courier-bold-o-normal $s $d))
    ((x-times tt bold italic $s $d) (x adobe-courier-bold-i-normal $s $d))

    ((x-times ss medium right $s $d)
      (x adobe-helvetica-medium-r-normal $s $d))
    ((x-times ss medium slanted $s $d)
      (x adobe-helvetica-medium-o-normal $s $d))
    ((x-times ss bold right $s $d) (x adobe-helvetica-bold-r-normal $s $d))
    ((x-times ss bold slanted $s $d) (x adobe-helvetica-bold-o-normal $s $d))

    ((x-courier $a medium right $s $d)
      (x adobe-courier-medium-r-normal $s $d))
    ((x-courier $a medium slanted $s $d)
      (x adobe-courier-medium-o-normal $s $d))
    ((x-courier $a medium italic $s $d)
      (x adobe-courier-medium-i-normal $s $d))
    ((x-courier $a bold right $s $d) (x adobe-courier-bold-r-normal $s $d))
    ((x-courier $a bold slanted $s $d) (x adobe-courier-bold-o-normal $s $d))
    ((x-courier $a bold italic $s $d) (x adobe-courier-bold-i-normal $s $d))

    ((x-helvetica $a medium right $s $d)
      (x adobe-helvetica-medium-r-normal $s $d))
    ((x-helvetica $a medium slanted $s $d)
      (x adobe-helvetica-medium-o-normal $s $d))
    ((x-helvetica $a bold right $s $d)
      (x adobe-helvetica-bold-r-normal $s $d))
    ((x-helvetica $a bold slanted $s $d)
      (x adobe-helvetica-bold-o-normal $s $d))

    ((x-utopia $a medium right $s $d) (x adobe-utopia-medium-r-normal $s $d))
    ((x-utopia $a medium italic $s $d) (x adobe-utopia-medium-i-normal $s $d))
    ((x-utopia $a bold right $s $d) (x adobe-utopia-bold-r-normal $s $d))
    ((x-utopia $a bold italic $s $d) (x adobe-utopia-bold-i-normal $s $d))

    ((x-lucida $a medium right $s $d) (x b&h-lucida-medium-r-normal $s $d))
    ((x-lucida $a medium italic $s $d) (x b&h-lucida-medium-i-normal $s $d))
    ((x-lucida $a bold right $s $d) (x b&h-lucida-bold-r-normal $s $d))
    ((x-lucida $a bold italic $s $d) (x b&h-lucida-bold-i-normal $s $d))))
