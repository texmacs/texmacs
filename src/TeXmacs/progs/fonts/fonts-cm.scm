
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cm-fonts.scm
;; DESCRIPTION : setup cm fonts for text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-cm))

(set-font-rules
  '(((roman* rm medium right $s $d) (cm cmr $s $d))
    ((roman rm medium right $s $d)
     (compound
       (cork-cmacc (cm cmr $s $d))
       (cork-cmsy (tex cmsy $s $d))
       (cork-cmmi (tex cmmi $s $d))))
    ((roman rm medium slanted $s $d) (cm cmsl $s $d))
    ((roman rm medium italic $s $d) (cm cmti $s $d))
    ((roman rm medium left-slanted $s $d) (cm cmff $s $d))
    ((roman rm medium small-caps $s $d) (cm cmcsc $s $d))
    ((roman rm medium slanted-small-caps $s $d) (cm cmcscsl $s $d))
    ((roman rm medium long $s $d) (cm cmdunh $s $d))
    ((roman rm medium italic-right $s $d) (cm cmu $s $d))
    ((roman rm bold right $s $d) (cm cmbx $s $d))
    ((roman rm bold slanted $s $d) (cm cmbxsl $s $d))
    ((roman rm bold italic $s $d) (cm cmbxti $s $d))
    ((roman rm bold condensed $s $d) (cm cmbom $s $d))
    ((roman rm bold small-caps $s $d) (cm cmbcsc $s $d))
    ((roman tt light right $s $d) (cm cmtl $s $d))
    ((roman tt light slanted $s $d) (cm cmsltl $s $d))
    ((roman tt medium right $s $d) (cm cmtt $s $d))
    ((roman tt medium slanted $s $d) (cm cmsltt $s $d))
    ((roman tt medium italic $s $d) (cm cmitt $s $d))
    ((roman tt medium small-caps $s $d) (cm cmtcsc $s $d))
    ((roman tt medium proportional $s $d) (cm cmvtt $s $d))
    ((roman tt bold right $s $d) (cm cmttb $s $d))
    ((roman ss light right $s $d) (cm cmbr $s $d))
    ((roman ss light slanted $s $d) (cm cmbrsl $s $d))
    ((roman ss medium right $s $d) (cm cmss $s $d))
    ((roman ss medium slanted $s $d) (cm cmssi $s $d))
    ((roman ss medium italic $s $d) (cm cmssi $s $d))
    ((roman ss medium flat $s $d) (cm cmssq $s $d))
    ((roman ss medium flat-italic $s $d) (cm cmssqi $s $d))
    ((roman ss bold right $s $d) (cm cmssbx $s $d))
    ((roman ss bold condensed $s $d) (cm cmssdc $s $d))

    ((concrete rm medium right $s $d) (cm ccr $s $d))
    ((concrete rm medium slanted $s $d) (cm ccsl $s $d))
    ((concrete rm medium italic $s $d) (cm ccti $s $d))
    ((concrete rm light slanted $s $d) (cm ccslc $s $d 9))
    ((concrete rm medium small-caps $s $d) (cm cccsc $s $d))
    ((concrete rm bold right $s $d) (cm cmbx $s $d))
    ((concrete rm bold slanted $s $d) (cm cmbxsl $s $d))
    ((concrete rm bold italic $s $d) (cm cmbxti $s $d))
    ((concrete rm bold condensed $s $d) (cm cmbom $s $d))
    ((concrete tt $a right $s $d) (cm cmtt $s $d))
    ((concrete tt $a slanted $s $d) (cm cmsltt $s $d))
    ((concrete tt $a italic $s $d) (cm cmitt $s $d))
    ((concrete tt medium small-caps $s $d) (cm cmtcsc $s $d))
    ((concrete tt $a proportional $s $d) (cm cmvtt $s $d))
    ((concrete ss medium right $s $d) (cm cmss $s $d))
    ((concrete ss medium slanted $s $d) (cm cmssi $s $d))
    ((concrete ss medium italic $s $d) (cm cmssi $s $d))
    ((concrete ss bold right $s $d) (cm cmbx $s $d))
    ((concrete ss bold condensed $s $d) (cm cmssdc $s $d))

    ((pandora rm medium right $s $d) (cm pnr $s $d))
    ((pandora rm medium slanted $s $d) (cm pnsl $s $d))
    ((pandora rm medium italic $s $d) (cm pnsl $s $d))
    ((pandora rm bold right $s $d) (cm pnb $s $d))
    ((pandora tt $a right $s $d) (cm pntt $s $d 9))
    ((pandora ss medium right $s $d) (cm pnss $s $d))
    ((pandora ss medium slanted $s $d) (cm pnssi $s $d))
    ((pandora ss medium italic $s $d) (cm pnssi $s $d))
    ((pandora ss bold right $s $d) (cm pnssb $s $d))

    ((duerer rm medium right $s $d) (tex cdr $s $d))
    ((duerer rm medium slanted $s $d) (tex cdsl $s $d))
    ((duerer rm medium italic $s $d) (tex cdi $s $d))
    ((duerer rm bold right $s $d) (tex cdb $s $d))
    ((duerer tt $a right $s $d) (tex cdtt $s $d))
    ((duerer ss medium right $s $d) (tex cdss $s $d))

    ((calligraphic $a medium $b $s $d) (tex callig $s $d 15))
    ((capbas $a medium $b $s $d) (tex capbas $s $d 0))
    ((hershey $a medium $b $s $d) (tex hscs $s $d))
    ((la $a medium $b $s $d) (tex la $s $d 14))
    ((messy $a medium $b $s $d) (tex cmfi $s $d))
    ((optical $a medium $b $s $d) (tex ocr $s $d))
    ((pacioli $a medium right $s $d) (tex cpcr $s $d))
    ((pacioli $a medium slanted $s $d) (tex cpcsl $s $d))
    ((punk $a medium right $s $d) (cm punk $s $d 20))
    ((punk $a medium slanted $s $d) (cm punksl $s $d 20))
    ((punk $a bold $b $s $d) (cm punkbx $s $d 20))
    ((twcal $a $b $c $s $d) (tex twcal $s $d 14))
    ((va $a medium $b $s $d) (tex va $s $d 14))

    ((Euler rm medium $b $s $d) (tex eufm $s $d))
    ((Euler rm bold $b $s $d) (tex eufb $s $d))
    ((ENR rm medium $b $s $d) (tex eurm $s $d))
    ((ENR rm bold $b $s $d) (tex eurb $s $d))
    ((gothic $a $b $c $s $d) (tex ygoth $s $d 0))
    ((schwell $a $b $c $s $d) (tex schwell $s $d 0))
    ((suet $a $b $c $s $d) (tex suet $s $d 14))
    ((swab $a $b $c $s $d) (tex yswab $s $d 0))
    ((blackletter $a $b $c $s $d) (tex blackletter $s $d 0))
    ((old-english $a $b $c $s $d) (tex hge $s $d 0))))
