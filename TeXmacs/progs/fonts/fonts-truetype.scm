
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-truetype.scm
;; DESCRIPTION : True Type fonts
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-truetype))

(set-font-rules
  '(((luxi tt bold slanted $s $d) (truetype luximbi $s $d))
    ((luxi tt bold $c $s $d) (truetype luximb $s $d))
    ((luxi tt $b slanted $s $d) (truetype luximri $s $d))
    ((luxi tt $b $c $s $d) (truetype luximr $s $d))
    ((luxi ss bold slanted $s $d) (truetype luxisbi $s $d))
    ((luxi ss bold $c $s $d) (truetype luxisb $s $d))
    ((luxi ss $b slanted $s $d) (truetype luxisri $s $d))
    ((luxi ss $b $c $s $d) (truetype luxisr $s $d))
    ((luxi $a bold slanted $s $d) (truetype luxirbi $s $d))
    ((luxi $a bold $c $s $d) (truetype luxirb $s $d))
    ((luxi $a $b slanted $s $d) (truetype luxirri $s $d))
    ((luxi $a $b $c $s $d) (truetype luxirr $s $d))

    ((ms-andalemo rm $b $c $s $d) (truetype andalemo $s $d))
    ((ms-arial rm xbold $a $s $d) (truetype ariblk $s $d))
    ((ms-arial rm bold italic $s $d) (truetype arialbi $s $d))
    ((ms-arial rm bold $a $s $d) (truetype arialbd $s $d))
    ((ms-arial rm $a italic $s $d) (truetype ariali $s $d))
    ((ms-arial rm $a $b $s $d) (truetype arial $s $d))
    ((ms-comic rm bold $a $s $d) (truetype comicbd $s $d))
    ((ms-comic rm $a $b $s $d) (truetype comic $s $d))
    ((ms-courier rm bold italic $s $d) (truetype courbi $s $d))
    ((ms-courier rm bold $a $s $d) (truetype courbd $s $d))
    ((ms-courier rm $a italic $s $d) (truetype couri $s $d))
    ((ms-courier rm $a $b $s $d) (truetype cour $s $d))
    ((ms-georgia rm bold italic $s $d) (truetype georgiaz $s $d))
    ((ms-georgia rm bold $a $s $d) (truetype georgiab $s $d))
    ((ms-georgia rm $a italic $s $d) (truetype georgiai $s $d))
    ((ms-georgia rm $a $b $s $d) (truetype georgia $s $d))
    ((ms-impact rm $b $c $s $d) (truetype impact $s $d))
    ((ms-lucida rm $b $c $s $d) (truetype lucon $s $d))
    ((ms-tahoma rm bold $a $s $d) (truetype tahomabd $s $d))
    ((ms-tahoma rm $a $b $s $d) (truetype tahoma $s $d))
    ((ms-times rm bold italic $s $d) (truetype timesbi $s $d))
    ((ms-times rm bold $a $s $d) (truetype timesbd $s $d))
    ((ms-times rm $a italic $s $d) (truetype timesi $s $d))
    ((ms-times rm $a $b $s $d) (truetype times $s $d))
    ((ms-trebuchet rm bold italic $s $d) (truetype trebucbi $s $d))
    ((ms-trebuchet rm bold $a $s $d) (truetype trebucbd $s $d))
    ((ms-trebuchet rm $a italic $s $d) (truetype trebucit $s $d))
    ((ms-trebuchet rm $a $b $s $d) (truetype trebuc $s $d))
    ((ms-verdana rm bold italic $s $d) (truetype verdanaz $s $d))
    ((ms-verdana rm bold $a $s $d) (truetype verdanab $s $d))
    ((ms-verdana rm $a italic $s $d) (truetype verdanai $s $d))
    ((ms-verdana rm $a $b $s $d) (truetype verdana $s $d))

    ((fireflysung $v $a $b $s $d) (unicode fireflysung $s $d))
    ((ipa $v bold proportional $s $d) (unicode ipagp $s $d))
    ((ipa $v bold $b $s $d) (unicode ipag $s $d))
    ((ipa $v $a proportional $s $d) (unicode ipamp $s $d))
    ((ipa $v $a $b $s $d) (unicode ipam $s $d))
    ;;((ipa $v $a $b $s $d) (unicode ipagui $s $d))
    ((kochi $v bold $b $s $d) (unicode kochi-gothic $s $d))
    ((kochi $v $a $b $s $d) (unicode kochi-mincho $s $d))))
