
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-preamble.scm
;; DESCRIPTION : shortcuts for dynamic markup
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (keyboard kbd-preamble)
  (:use
    (texmacs edit edit-preamble) (texmacs edit edit-format)
    (texmacs edit edit-misc)))

(kbd-map
  ("executable" "" "Insert executable markup")
  ("inactive" "" "Insert inactive markup")

  ("inactive a" (make-inactive-argument))
  ("inactive A" (make-inactive-map-args))
  ("inactive c" (make-inactive-compound))
  ("inactive d" (make-inactive-delay))
  ("inactive D" (make-inactive-drd-props))
  ("inactive e" (make-inactive-compound))
  ("inactive E" (make-inactive-eval-args))
  ("inactive h" (make-inactive-hold))
  ("inactive i" (make-inactive-include))
  ("inactive l" (make-inactive-latex))
  ("inactive m" (make-inactive-macro))
  ("inactive M" (make-inactive-meaning))
  ("inactive n" (make-inactive-get-arity))
  ("inactive N" (make-inactive-get-label))
  ("inactive p" (make-inactive-provides))
  ("inactive q" (make-inactive-quote))
  ("inactive r" (make-inactive-release))
  ("inactive s" (make-inactive-surround))
  ("inactive t" (make-inactive-tag))
  ("inactive v" (make-inactive-value))
  ("inactive w" (make-inactive-with))
  ("inactive W" (make-inactive-write))
  ("inactive x" (make-inactive-xmacro))
  ("inactive *" (make-inactive-action))
  ("inactive =" (make-inactive-assign))
  ("inactive (" (make-tuple))
  ("inactive <" (make-tuple))
  ("inactive @" (make-attr))
  ("inactive >" (make-inactive-hyperlink))
  ("inactive #" (make-inactive-argument))
  ("inactive $" (make-inactive-symbol))
  ("inactive G" (make 'graphics))
  ("inactive S" (make 'superpose))
  ("inactive P" (make 'point))
  ("inactive L" (make 'line))
  ("inactive C" (make 'cline))
  ("inactive F" (make 'fill))

  ("executable c" (make-inactive-case))
  ("executable d" (make-inactive-div))
  ("executable e" (make-inactive-extern))
  ("executable f" (make-inactive-find-file))
  ("executable F" (make-inactive-flag))
  ("executable i" (make-inactive-if))
  ("executable l" (make-inactive-length))
  ("executable m" (make-inactive-mod))
  ("executable q" (make-inactive-is-tuple))
  ("executable t" (make-inactive-translate))
  ("executable w" (make-inactive-while))
  ("executable x" (make-inactive-eval))
  ("executable |" (make-inactive-or))
  ("executable ^" (make-inactive-xor))
  ("executable &" (make-inactive-and))
  ("executable !" (make-inactive-not))
  ("executable +" (make-inactive-plus))
  ("executable -" (make-inactive-minus))
  ("executable *" (make-inactive-times))
  ("executable /" (make-inactive-over))
  ("executable ;" (make-inactive-merge))
  ("executable ," (make-inactive-range))
  ("executable #" (make-inactive-number))
  ("executable @" (make-date))
  ("executable [" (make-inactive-look-up))
  ("executable =" (make-inactive-equal))
  ("executable <" (make-inactive-less))
  ("executable >" (make-inactive-greater))
  ("executable ?" (make-inactive-if))
  ("executable C-i" (make-inactive-var-if))
  ("executable C-?" (make-inactive-var-if))
  ("executable C-@" (make-inactive-date))
  ("executable C-=" (make-inactive-unequal))
  ("executable C-<" (make-inactive-lesseq))
  ("executable C->" (make-inactive-greatereq)))
