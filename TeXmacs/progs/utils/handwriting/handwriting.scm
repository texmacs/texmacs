
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
          ("z" (learn-glyph "z"))))
    ("Recognize" (recognize-glyph))
    >>>))

(tm-define (learn-glyphs)
  (show glyph-widget))
