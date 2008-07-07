
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fonts-composite.scm
;; DESCRIPTION : agglomerated fonts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-composite))

(set-font-rules
  '(((modern $v $a $b $s $d)
     (compound (ec (roman $v $a $b $s $d))
	       (la (cyrillic $v $a $b $s $d))
	       (cmr (tex cmr $s $d))
	       (cmmi (tex cmmi $s $d))
	       (cmsy (tex cmsy $s $d))
	       (msam (tex msam $s $d))
	       (msbm (tex $sbm $s $d))
	       (stmary (tex stmary $s $d))
	       (wasy (tex wasy $s $d))
	       (line (tex line $s $d))
	       (cmex (tex cmex $s $d))
	       (math-cal (tex cmsy $s $d))
	       (math-frak (tex eufm $s $d))
	       (math-bbb (tex bbm $s $d))
	       (math-upgreek (tex grmn $s $d))
	       (math-bold-1 (tex cmbx $s $d))
	       (math-bold-2 (tex cmmib $s $d))
	       (math-bold-cal (tex cmbsy $s $d))
	       (long (virtual long $s $d))
	       (negate (virtual negate $s $d))
	       (misc (virtual misc $s $d))
	       (hangul (unbatang $v $a $b $s $d))
	       (oriental (ipa $v $a $b $s $d))
	       (any (roman $v $a $b $s $d))))))
