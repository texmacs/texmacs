
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : misc-fonts.scm
;; DESCRIPTION : setup miscellaneous fonts for text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-misc))

(set-font-rules
  '(((cherokee $a medium $b $s $d) (tex cherokee $s $d 0))
    ((tengwar $a medium $b $s $d) (tex tengwar $s $d 0))
    ((shavian $a medium $b $s $d) (tex shavian $s $d 0))

    ((bbding $a medium $b $s $d) (tex bbding $s $d))
    ((dancers $a medium $b $s $d) (tex dancers $s $d 0))
    ((go $a $b $c $s $d) (tex go $s $d))
    ((iching $a $b $c $s $d) (tex iching $s $d 0))
    ((karta $a medium $b $s $d) (tex karta $s $d 15))
    ((klinz $a medium $b $s $d) (tex klinz $s $d 0))
    ((magic $a medium $b $s $d) (tex magic $s $d 0))
    ((phonetic $a medium right $s $d) (tex cmph $s $d))
    ((phonetic $a medium italic $s $d) (tex cmphi $s $d))
    ((phonetic $a bold $b $s $d) (tex cmphb $s $d))
    ((tsipa $a medium $b $s $d) (tex tsipa $s $d))
    ((wsuipa $a medium right $s $d) (tex wsuipa $s $d))
    ((wsuipa $a medium slanted $s $d) (tex wslipa $s $d))
    ((wsuipa $a bold $b $s $d) (tex wbxipa $s $d))))
