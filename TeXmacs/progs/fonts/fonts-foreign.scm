
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : foreign-fonts.scm
;; DESCRIPTION : setup foreign fonts for text mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-foreign))

(set-font-rules
  '(;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; general foreign language fonts
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ((arab $a medium $c $s $d) (tex nash $s $d 14))
    ((arab $a medium $c $s $d) (tex nash14bf $s $d 14))

    ((armenian rm medium right $s $d) (tex artmr $s $d))
    ((armenian rm medium slanted $s $d) (tex artmsl $s $d))
    ((armenian rm medium italic $s $d) (tex artmi $s $d))
    ((armenian rm bold right $s $d) (tex artmb $s $d))
    ((armenian rm bold slanted $s $d) (tex artmbs $s $d))
    ((armenian rm bold italic $s $d) (tex artmbi $s $d))
    ((armenian ss medium right $s $d) (tex arssr $s $d))
    ((armenian ss medium slanted $s $d) (tex arsssl $s $d))
    ((armenian ss bold right $s $d) (tex arssb $s $d))
    ((armenian ss bold slanted $s $d) (tex arssbs $s $d))

    ((devangari rm medium $a $s $d) (tex dvng $s $d))
    ((devangari rm light $a $s $d) (tex dvpn $s $d))
    ((devangari ss medium $a $s $d) (tex dvpn $s $d))

    ((greek rm medium right $s $d) (tex grmn $s $d))
    ((greek rm medium slanted $s $d) (tex grmo $s $d))
    ((greek rm medium italic $s $d) (tex grmi $s $d))
    ((greek rm medium small-caps $s $d) (tex grmc $s $d))
    ((greek rm medium italic-right $s $d) (tex grmu $s $d))
    ((greek rm bold right $s $d) (tex grxn $s $d))
    ((greek rm bold slanted $s $d) (tex grxo $s $d))
    ((greek rm bold italic $s $d) (tex grxi $s $d))
    ((greek rm bold small-caps $s $d) (tex grxc $s $d))
    ((greek rm bold italic-right $s $d) (tex grxu $s $d))
    ((greek ss medium right $s $d) (tex gsmn $s $d))
    ((greek ss medium slanted $s $d) (tex gsmo $s $d))
    ((greek ss medium italic $s $d) (tex gsmi $s $d))
    ((greek ss medium small-caps $s $d) (tex gsmc $s $d))
    ((greek ss medium italic-right $s $d) (tex gsmu $s $d))
    ((greek ss bold right $s $d) (tex gsxn $s $d))
    ((greek ss bold slanted $s $d) (tex gsxo $s $d))
    ((greek ss bold italic $s $d) (tex gsxi $s $d))
    ((greek ss bold small-caps $s $d) (tex gsxc $s $d))
    ((greek ss bold italic-right $s $d) (tex gsxu $s $d))
    ((greek tt medium right $s $d) (tex gttn $s $d))
    ((greek tt medium slanted $s $d) (tex gtto $s $d))
    ((greek tt medium italic $s $d) (tex gtti $s $d))
    ((greek tt medium small-caps $s $d) (tex gttc $s $d))
    ((greek tt medium italic-right $s $d) (tex gttu $s $d))
    ((greek oo medium right $s $d) (tex gomn $s $d))
    ((greek oo medium slanted $s $d) (tex gomo $s $d))
    ((greek oo medium italic $s $d) (tex gomi $s $d))
    ((greek oo medium small-caps $s $d) (tex gomc $s $d))
    ((greek oo medium italic-right $s $d) (tex gomu $s $d))
    ((greek oo bold right $s $d) (tex goxn $s $d))
    ((greek oo bold slanted $s $d) (tex goxo $s $d))
    ((greek oo bold italic $s $d) (tex goxi $s $d))
    ((greek oo bold small-caps $s $d) (tex goxc $s $d))
    ((greek oo bold italic-right $s $d) (tex goxu $s $d))
    ((greek ll medium right $s $d) (tex glmn $s $d))
    ((greek ll medium slanted $s $d) (tex glmo $s $d))
    ((greek ll medium italic $s $d) (tex glmi $s $d))
    ((greek ll medium small-caps $s $d) (tex glmc $s $d))
    ((greek ll medium italic-right $s $d) (tex glmu $s $d))
    ((greek ll bold right $s $d) (tex glxn $s $d))
    ((greek ll bold slanted $s $d) (tex glxo $s $d))
    ((greek ll bold italic $s $d) (tex glxi $s $d))
    ((greek ll bold small-caps $s $d) (tex glxc $s $d))
    ((greek ll bold italic-right $s $d) (tex glxu $s $d))
    ((greek lt medium right $s $d) (tex gltn $s $d))
    ((greek lt medium slanted $s $d) (tex glto $s $d))
    ((greek lt medium small-caps $s $d) (tex gltc $s $d))

    ((hebrew $a medium right $s $d) (tex redis $s $d))
    ((hebrew $a medium slanted $s $d) (tex rediss $s $d))
    ((hebrew $a bold right $s $d) (tex redisbx $s $d))

    ((icelandic rm medium right $s $d) (tex imr $s $d))
    ((icelandic rm medium slanted $s $d) (tex imsl $s $d))
    ((icelandic rm medium italic $s $d) (tex imti $s $d))
    ((icelandic rm bold right $s $d) (tex imbx $s $d))
    ((icelandic rm bold slanted $s $d) (tex imbxsl $s $d))
    ((icelandic ss medium right $s $d) (tex imss $s $d))
    ((icelandic ss medium italic $s $d) (tex imssi $s $d))
    ((icelandic ss bold right $s $d) (tex imssbx $s $d))
    ((icelandic tt medium right $s $d) (tex imtt $s $d))

    ((irish rm medium right $s $d) (tex eiadr $s $d))
    ((irish rm medium slanted $s $d) (tex eiadsl $s $d))
    ((irish rm medium italic $s $d) (tex eiadci $s $d))
    ((irish rm bold right $s $d) (tex eiadbx $s $d))
    ((irish rm bold slanted $s $d) (tex eiadbxsl $s $d))
    ((irish rm bold italic $s $d) (tex eiadbxi $s $d))
    ((irish rm medium small-caps $s $d) (tex eiadcsc $s $d))
    ((irish ss medium right $s $d) (tex eiadss $s $d))
    ((irish ss medium italic $s $d) (tex eiadssi $s $d))
    ((irish tt medium right $s $d) (tex eiadtt $s $d))
    ((irish tt medium slanted $s $d) (tex eiadsltt $s $d))
    ((irish tt medium small-caps $s $d) (tex eiadtcsc $s $d))

    ((osmanian $a medium $b $s $d) (tex osmanian $s $d 0))

    ((tamil $a medium $b $s $d) (tex wntml $s $d))

    ((thai $a medium $b $s $d) (tex thairz $s $d))
    ((thai $a bold $b $s $d) (tex thaibz $s $d))

    ((turkish $a medium $b $s $d) (tex wtkr $s $d))

    ((vietnamese rm medium right $s $d) (tex vmr $s $d))
    ((vietnamese rm medium slanted $s $d) (tex vmsl $s $d))
    ((vietnamese rm medium italic $s $d) (tex vmti $s $d))
    ((vietnamese rm bold right $s $d) (tex vmbx $s $d))
    ((vietnamese rm bold italic $s $d) (tex vmbxti $s $d))
    ((vietnamese rm medium small-caps $s $d) (tex vmcsc $s $d))
    ((vietnamese ss medium right $s $d) (tex vmss $s $d))
    ((vietnamese ss medium italic $s $d) (tex vmssi $s $d))
    ((vietnamese tt medium right $s $d) (tex vmtt $s $d))
    ((vietnamese tt medium slanted $s $d) (tex vmsltt $s $d))
    ((vietnamese tt medium small-caps $s $d) (tex vmtcsc $s $d))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; cyrillic fonts
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ((cyrillic rm medium right $s $d) (la larm $s $d))
    ((cyrillic rm medium slanted $s $d) (la lasl $s $d))
    ((cyrillic rm medium italic $s $d) (la lati $s $d))
    ((cyrillic rm medium left-slanted $s $d) (la laff $s $d))
    ((cyrillic rm medium small-caps $s $d) (la lacc $s $d))
    ((cyrillic rm medium long $s $d) (la ladh $s $d))
    ((cyrillic rm medium italic-right $s $d) (la laui $s $d))
    ((cyrillic rm bold right $s $d) (la labx $s $d))
    ((cyrillic rm bold slanted $s $d) (la labl $s $d))
    ((cyrillic rm bold italic $s $d) (la labi $s $d))
    ((cyrillic rm bold condensed $s $d) (la larb $s $d))
    ((cyrillic rm bold small-caps $s $d) (la laxc $s $d))
    ((cyrillic rm bold slanted-small-caps $s $d) (la laoc $s $d))
    ((cyrillic tt medium right $s $d) (la latt $s $d))
    ((cyrillic tt medium slanted $s $d) (la last $s $d))
    ((cyrillic tt medium italic $s $d) (la last $s $d))
    ((cyrillic tt medium small-caps $s $d) (la latc $s $d))
    ((cyrillic tt medium proportional $s $d) (la lavt $s $d))
    ((cyrillic tt medium italic-proportional $s $d) (la lavi $s $d))
    ((cyrillic tt bold right $s $d) (la latt $s $d))
    ((cyrillic ss medium right $s $d) (la lass $s $d))
    ((cyrillic ss medium slanted $s $d) (la lasi $s $d))
    ((cyrillic ss medium italic $s $d) (la lasi $s $d))
    ((cyrillic ss bold right $s $d) (la lasx $s $d))
    ((cyrillic ss bold italic $s $d) (la laso $s $d))

    ((old-slavonic $a $b $c $s $d) (tex izhitsa $s $d 0))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; georgian fonts
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    ((mxedruli $a medium right $s $d) (tex mxed $s $d))
    ((mxedruli $a bold right $s $d) (tex mxedbf $s $d))
    ((mxedruli $a medium italic $s $d) (tex mxedi $s $d))
    ((mxedruli $a medium small-caps $s $d) (tex mxedc $s $d))

    ((xucuri $a $b $c $s $d) (tex xuc $s $d))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; archaic languages
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ((bard $a $b $c $s $d) (tex bard $s $d 0))
    ((cypriot $a $b $c $s $d) (tex cypr $s $d))
    ((etruscan $a $b $c $s $d) (tex etr $s $d))
    ((greek4cbc $a $b $c $s $d) (tex givbc $s $d))
    ((greek6cbc $a $b $c $s $d) (tex gvibc $s $d))
    ((linearb $a $b $c $s $d) (tex linb $s $d))
    ((ogham $a medium $b $s $d) (tex ogham $s $d 0))
    ((phoenician $a $b $c $s $d) (tex phnc $s $d))
    ((runic $a $b $c $s $d) (tex fut $s $d))
    ((runic* $a $b $c $s $d) (tex futhol $s $d))
    ((runic** $a $b $c $s $d) (tex futhor $s $d))
    ((southarabian $a $b $c $s $d) (tex SouthArabian $s $d 0))
    ((syriac $a $b $c $s $d) (tex estrangelo $s $d 0))
    ((ugaritic $a $b $c $s $d) (tex ugaritic $s $d 0))))
