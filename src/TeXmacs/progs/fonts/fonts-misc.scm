
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
  '(((tc rm medium right $s $d) (tex tcrm $s $d))
    ((tc rm medium slanted $s $d) (tex tcsl $s $d))
    ((tc rm medium italic $s $d) (tex tcti $s $d))
    ((tc rm bold right $s $d) (tex tcbx $s $d))
    ((tc rm bold slanted $s $d) (tex tcbl $s $d))
    ((tc rm bold italic $s $d) (tex tcbi $s $d))
    ((tc ss medium right $s $d) (tex tcss $s $d))
    ((tc ss medium slanted $s $d) (tex tcsi $s $d))
    ((tc ss medium italic $s $d) (tex tcsi $s $d))
    ((tc ss bold right $s $d) (tex tcsx $s $d))
    ((tc ss bold slanted $s $d) (tex tcso $s $d))
    ((tc ss bold italic $s $d) (tex tcso $s $d))
    ((tc tt $a right $s $d) (tex tctt $s $d))
    ((tc tt $a slanted $s $d) (tex tcst $s $d))
    ((tc tt $a italic $s $d) (tex tcit $s $d))
    ((tc $a $b small-caps $s $d) (tc $a $b right $s $d))

    ((tcx rm medium right $s $d) (tex rtcxr $s $d 0))
    ((tcx rm medium slanted $s $d) (tex rtcxsl $s $d 0))
    ((tcx rm medium italic $s $d) (tex rtcxi $s $d 0))
    ((tcx rm bold right $s $d) (tex rtcxb $s $d 0))
    ((tcx rm bold slanted $s $d) (tex rtcxbsl $s $d 0))
    ((tcx rm bold italic $s $d) (tex rtcxbi $s $d 0))
    ((tcx ss medium right $s $d) (tex rtcxss $s $d 0))
    ((tcx ss medium slanted $s $d) (tex rtcxsssl $s $d 0))
    ((tcx ss medium italic $s $d) (tex rtcxsssl $s $d 0))
    ((tcx ss bold right $s $d) (tex rtcxbss $s $d 0))
    ((tcx ss bold slanted $s $d) (tex rtcxbsso $s $d 0))
    ((tcx ss bold italic $s $d) (tex rtcxbsso $s $d 0))
    ((tcx tt medium right $s $d) (tex tcxtt $s $d 0))
    ((tcx tt medium slanted $s $d) (tex tcxttsl $s $d 0))
    ((tcx tt medium italic $s $d) (tex tcxttsl $s $d 0))
    ((tcx tt bold right $s $d) (tex tcxbtt $s $d 0))
    ((tcx tt bold slanted $s $d) (tex tcxbttsl $s $d 0))
    ((tcx tt bold italic $s $d) (tex tcxbttsl $s $d 0))
    ((tcx $a $b small-caps $s $d) (tcx $a $b right $s $d))

    ((frc rm medium right $s $d) (ec frcr $s $d))
    ((frc rm medium slanted $s $d) (tex frcsl $s $d))
    ((frc rm medium italic $s $d) (tex frcsl $s $d))
    ((frc rm medium long $s $d) (tex frca $s $d))
    ((frc rm bold right $s $d) (tex frcbx $s $d))
    ((frc rm bold slanted $s $d) (tex frcslbx $s $d))
    ((frc rm bold italic $s $d) (tex frcslbx $s $d))
    ((frc rm light $c $s $d) (tex frcf $s $d))
    ((frc ss $a right $s $d) (tex frcc $s $d))
    ((frc ss $a italic $s $d) (tex frcslc $s $d))
    ((frc tt $b $c $s $d) (tex frcw $s $d))

    ((cherokee $a medium $b $s $d) (tex cherokee $s $d 0))
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
