
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : handwriting.scm
;; DESCRIPTION : tools for handwriting recognition
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils handwriting handwriting))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define last-glyph '())
(tm-define glyph-table (make-ahash-table))

(define glyphs-loaded? #f)

(define (load-glyphs)
  (when (not glyphs-loaded?)
    (set! glyphs-loaded? #t)
    (with l (if (url-exists? "~/.TeXmacs/system/glyphs.scm")
                (load-object "~/.TeXmacs/system/glyphs.scm")
                '())
      (set! glyph-table (list->ahash-table l))
      (for (x l)
        (let* ((key (car x))
               (im (cdr x)))
          (for (y im)
            (glyph-register key y)))))))

(define (save-glyphs)
  (save-object "~/.TeXmacs/system/glyphs.scm"
               (ahash-table->list glyph-table)))

(tm-define (learn-glyph name)
  (load-glyphs)
  (when (and (list? last-glyph) (nnull? last-glyph))
    (glyph-register name last-glyph)
    (with old (or (ahash-ref glyph-table name) '())
      (ahash-set! glyph-table name (cons last-glyph old))
      (save-glyphs))))

(tm-define (recognize-glyph)
  (load-glyphs)
  (when (and (list? last-glyph) (nnull? last-glyph))
    (display* "Recognized as " (glyph-recognize last-glyph) "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Widget interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (glyph-widget)
  (padded
    (ink (set! last-glyph answer)))
  ---
  (hlist
    >>>
    (=> "Learn"
        (tile 8
          ("a" (learn-glyph "a"))
          ("b" (learn-glyph "b"))
          ("c" (learn-glyph "c"))
          ("d" (learn-glyph "d"))
          ("e" (learn-glyph "e"))
          ("f" (learn-glyph "f"))
          ("g" (learn-glyph "g"))
          ("h" (learn-glyph "h"))
          ("i" (learn-glyph "i"))
          ("j" (learn-glyph "j"))
          ("k" (learn-glyph "k"))
          ("l" (learn-glyph "l"))
          ("m" (learn-glyph "m"))
          ("n" (learn-glyph "n"))
          ("o" (learn-glyph "o"))
          ("p" (learn-glyph "p"))
          ("q" (learn-glyph "q"))
          ("r" (learn-glyph "r"))
          ("s" (learn-glyph "s"))
          ("t" (learn-glyph "t"))
          ("u" (learn-glyph "u"))
          ("v" (learn-glyph "v"))
          ("w" (learn-glyph "w"))
          ("x" (learn-glyph "x"))
          ("y" (learn-glyph "y"))
          ("z" (learn-glyph "z")))
        ---
        (tile 8
          ("A" (learn-glyph "A"))
          ("B" (learn-glyph "B"))
          ("C" (learn-glyph "C"))
          ("D" (learn-glyph "D"))
          ("E" (learn-glyph "E"))
          ("F" (learn-glyph "F"))
          ("G" (learn-glyph "G"))
          ("H" (learn-glyph "H"))
          ("I" (learn-glyph "I"))
          ("J" (learn-glyph "J"))
          ("K" (learn-glyph "K"))
          ("L" (learn-glyph "L"))
          ("M" (learn-glyph "M"))
          ("N" (learn-glyph "N"))
          ("O" (learn-glyph "O"))
          ("P" (learn-glyph "P"))
          ("Q" (learn-glyph "Q"))
          ("R" (learn-glyph "R"))
          ("S" (learn-glyph "S"))
          ("T" (learn-glyph "T"))
          ("U" (learn-glyph "U"))
          ("V" (learn-glyph "V"))
          ("W" (learn-glyph "W"))
          ("X" (learn-glyph "X"))
          ("Y" (learn-glyph "Y"))
          ("Z" (learn-glyph "Z")))
        ---
        (tile 5
          ("0" (learn-glyph "0"))
          ("1" (learn-glyph "1"))
          ("2" (learn-glyph "2"))
          ("3" (learn-glyph "3"))
          ("4" (learn-glyph "4"))
          ("5" (learn-glyph "5"))
          ("6" (learn-glyph "6"))
          ("7" (learn-glyph "7"))
          ("8" (learn-glyph "8"))
          ("9" (learn-glyph "9")))
        ---
        (tile 8
          (symbol "<alpha>" (learn-glyph "<alpha>"))
          (symbol "<beta>" (learn-glyph "<beta>"))
          (symbol "<gamma>" (learn-glyph "<gamma>"))
          (symbol "<delta>" (learn-glyph "<delta>"))
          (symbol "<epsilon>" (learn-glyph "<epsilon>"))
          (symbol "<zeta>" (learn-glyph "<zeta>"))
          (symbol "<eta>" (learn-glyph "<eta>"))
          (symbol "<theta>" (learn-glyph "<theta>"))
          (symbol "<iota>" (learn-glyph "<iota>"))
          (symbol "<kappa>" (learn-glyph "<kappa>"))
          (symbol "<lambda>" (learn-glyph "<lambda>"))
          (symbol "<mu>" (learn-glyph "<mu>"))
          (symbol "<nu>" (learn-glyph "<nu>"))
          (symbol "<xi>" (learn-glyph "<xi>"))
          (symbol "<omicron>" (learn-glyph "<omicron>"))
          (symbol "<pi>" (learn-glyph "<pi>"))
          (symbol "<rho>" (learn-glyph "<rho>"))
          (symbol "<sigma>" (learn-glyph "<sigma>"))
          (symbol "<tau>" (learn-glyph "<tau>"))
          (symbol "<upsilon>" (learn-glyph "<upsilon>"))
          (symbol "<phi>" (learn-glyph "<phi>"))
          (symbol "<chi>" (learn-glyph "<chi>"))
          (symbol "<psi>" (learn-glyph "<psi>"))
          (symbol "<omega>" (learn-glyph "<omega>"))))
    ("Recognize" (recognize-glyph))
    >>>))

(tm-define (learn-glyphs)
  (show glyph-widget))
