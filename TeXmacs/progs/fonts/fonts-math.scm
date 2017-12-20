
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-fonts.scm
;; DESCRIPTION : setup cm fonts for math mode
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts fonts-math))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic mathematical fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font-rules
  `(;; Main math font
    ((math $ecrm $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
	   $cal $frak $bbb $upgreek $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (math (tex $cmr $s $d)
	    (tex $cmmi $s $d)
	    (tex $cmsy $s $d)
	    (tex $msam $s $d)
	    (tex $msbm $s $d)
	    (tex $stmary $s $d)
	    (tex $wasy $s $d)
	    (tex $line $s $d)
	    (tex $cmex $s $d)
	    (tex $cal $s $d)
	    (tex $frak $s $d)
	    (tex $bbb $s $d)
	    (tex $upgreek $s $d)
	    (tex $bold1 $s $d)
	    (tex $bold2 $s $d)
	    (tex $boldcal $s $d)
	    (virtual tradi-long $s $d)
	    (virtual tradi-negate $s $d)
	    (virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-std $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmsy msam msbm stmary wasy line
	   cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-std $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmbsy msam msbm stmary wasyb linew
	   cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))
    ((math-conc $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi xccsy xccam xccbm stmary wasy line
	   cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
	   cmbrsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

    ;; Upright math font
    ((var-math $ecrm $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
	       $cal $frak $bbb $upgreek $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (var-math (tex $cmmi $s $d)
		(tex $cmr $s $d)
		(tex $cmsy $s $d)
		(tex $msam $s $d)
		(tex $msbm $s $d)
		(tex $stmary $s $d)
		(tex $wasy $s $d)
		(tex $line $s $d)
		(tex $cmex $s $d)
		(tex $cal $s $d)
		(tex $frak $s $d)
		(tex $bbb $s $d)
		(tex $upgreek $s $d)
		(tex $bold1 $s $d)
		(tex $bold2 $s $d)
		(tex $boldcal $s $d)
		(virtual tradi-long $s $d)
		(virtual tradi-negate $s $d)
		(virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ;; Math font with modified italic math letters
    ((alt-math $ecrm $ecti $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
               $cal $frak $bbb $upgreek $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (alt-math (tex $cmr $s $d)
                (tex $cmmi $s $d)
                (tex $ecti $s $d)
                (tex $cmsy $s $d)
                (tex $msam $s $d)
                (tex $msbm $s $d)
                (tex $stmary $s $d)
                (tex $wasy $s $d)
                (tex $line $s $d)
                (tex $cmex $s $d)
                (tex $cal $s $d)
                (tex $frak $s $d)
                (tex $bbb $s $d)
                (tex $upgreek $s $d)
                (tex $bold1 $s $d)
                (tex $bold2 $s $d)
                (tex $boldcal $s $d)
                (virtual tradi-long $s $d)
                (virtual tradi-negate $s $d)
                (virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-std $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmsy msam msbm stmary wasy line
	   cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-std $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmbsy msam msbm stmary wasyb linew
	   cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))
    ((math-conc $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi xccsy xccam xccbm stmary wasy line
	   cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
	   cmbrsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

    ((math-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmsy msam msbm stmary wasy line
	       cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmbsy msam msbm stmary wasyb linew
	       cmsy eufm bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))
    ((math-conc-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi xccsy xccam xccbm stmary wasy line
	       cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
	       cmbrsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))

    ((math-alt $ecrm $ecti $cmr $cmmi $s $d)
     (alt-math $ecrm $ecti $cmr $cmmi cmsy msam msbm stmary wasy line
               cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-alt $ecrm $ecti $cmr $cmmi $s $d)
     (alt-math $ecrm $ecti $cmr $cmmi cmbsy msam msbm stmary wasyb linew
               cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))
    ((math-conc-alt $ecrm $ecti $cmr $cmmi $s $d)
     (alt-math $ecrm $ecti $cmr $cmmi xccsy xccam xccbm stmary wasy line
               cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright-alt $ecrm $ecti $cmr $cmmi $s $d)
     (alt-math $ecrm $ecti $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
               cmbrsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

    ((roman mr medium right $s $d) (math-var ecrm cmr cmmi $s $d))
    ((roman mr bold right $s $d) (bold-math-var ecbx cmbx cmmib $s $d))
    ((roman ms medium right $s $d) (math-var ecss cmss cmmi $s $d))
    ((roman ms bold right $s $d) (bold-math-var ecsx cmssbx cmmib $s $d))
    ((roman mt medium right $s $d) (math-var ectt cmtt cmmi $s $d))
    ((roman mr medium $a $s $d) (math-std ecrm cmr cmmi $s $d))
    ((roman mr bold $a $s $d) (bold-math-std ecbx cmbx cmmib $s $d))
    ((roman ms light $a $s $d) (math-bright cmbr cmbr cmbrmi $s $d))
    ((roman ms medium $a $s $d) (math-alt ecss ecsi cmss cmmi $s $d))
    ((roman ms bold $a $s $d) (math-alt ecsx ecso cmssbx cmmib $s $d))
    ((roman mt medium $a $s $d) (math-alt ectt ecst cmtt cmitt $s $d))

    ((roman trm medium $a $s $d) (math-var ecrm cmr cmmi $s $d))
    ((roman trm bold $a $s $d) (bold-math-var ecbx cmbx cmmib $s $d))
    ((roman tss medium $a $s $d) (math-var ecss cmss cmmi $s $d))
    ((roman tss bold $a $s $d) (bold-math-var ecsx cmssbx cmmib $s $d))
    ((roman ttt $a $b $s $d) (math-var ectt cmtt cmmi $s $d))
    ((roman bf $a $b $s $d) (bold-math-var ecbx cmbx cmmib $s $d))
    ((roman up $a $b $s $d) (math-var ecui cmu cmmi $s $d))
    ((roman it $a $b $s $d) (math-var ecti cmti cmmi $s $d))
    ((roman sl $a $b $s $d) (math-var ecsl cmsl cmmi $s $d))

    ((concrete mr medium right $s $d) (math-conc-var eorm ccr xcmmi $s $d))
    ((concrete mr bold right $s $d) (math-conc-var ecbx cmbx cmmib $s $d))
    ((concrete ms medium right $s $d) (math-alt ecss ecsi cmss cmmi $s $d))
    ((concrete ms bold right $s $d) (math-alt ecsx ecso cmssbx cmmib $s $d))
    ((concrete mt medium right $s $d) (math-alt ectt ecst cmtt cmmi $s $d))
    ((concrete mr medium $a $s $d) (math-conc eorm ccr xccmi $s $d))
    ((concrete mr bold $a $s $d) (bold-math-std ecbx cmbx cmmib $s $d))
    ((concrete ms light $a $s $d) (math-bright cmbr cmbr cmbrmi $s $d))
    ((concrete ms medium $a $s $d) (math-std ecss cmss cmssi $s $d))
    ((concrete ms bold $a $s $d) (math-std ecsx cmssbx cmmib $s $d))
    ((concrete mt medium $a $s $d) (math-std ectt cmtt cmitt $s $d))

    ((concrete trm medium $a $s $d) (math-conc-var eorm ccr xcmmi $s $d))
    ((concrete trm bold $a $s $d) (math-conc-var ecbx cmbx cmmib $s $d))
    ((concrete tss medium $a $s $d) (math-var ecss cmss cmmi $s $d))
    ((concrete tss bold $a $s $d) (math-var ecsx cmssbx cmmib $s $d))
    ((concrete ttt $a $b $s $d) (math-var ectt cmtt cmmi $s $d))
    ((concrete bf $a $b $s $d) (bold-math-var ecbx cmbx cmmib $s $d))
    ((concrete up $a $b $s $d) (math-var ecui cmu cmmi $s $d))
    ((concrete it $a $b $s $d) (math-var ecti cmti cmmi $s $d))
    ((concrete sl $a $b $s $d) (math-var ecsl cmsl cmmi $s $d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical fonts with special capital letters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font-rules
  `(((capital-math $name $ecrm $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
		   $cal $frak $bbb $upgreek $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (capital-math (tex $cmr $s $d)
		    (tex $cmmi $s $d)
		    (tex $name $s $d)
		    (tex $cmsy $s $d)
		    (tex $msam $s $d)
		    (tex $msbm $s $d)
		    (tex $stmary $s $d)
		    (tex $wasy $s $d)
		    (tex $line $s $d)
		    (tex $cmex $s $d)
		    (tex $cal $s $d)
		    (tex $frak $s $d)
		    (tex $bbb $s $d)
		    (tex $upgreek $s $d)
		    (tex $bold1 $s $d)
		    (tex $bold2 $s $d)
		    (tex $boldcal $s $d)
		    (virtual tradi-long $s $d)
		    (virtual tradi-negate $s $d)
		    (virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-capital $name $s $d)
     (capital-math $name ecrm cmr cmmi cmsy msam msbm stmary wasy line
		   cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-capital $name $s $d)
     (capital-math $name ecbx cmbx cmmib cmbsy msam msbm stmary wasyb linew
		   cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))

    ((cal mr medium $a $s $d) (math-capital cmsy $s $d))
    ((cal mr bold $a $s $d) (bold-math-capital cmbsy $s $d))
    ((cal* mr medium $a $s $d) (math-capital rsfs $s $d))
    ((cal** mr medium $a $s $d) (math-capital euxm $s $d))
    ((Bbb mr medium $a $s $d) (math-capital msbm $s $d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical fonts with special alpha-numerical letters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font-rules
  `(((alphanum-math $name $ecrm $cmr $cmmi $cmsy $msam $msbm
		    $stmary $wasy $line $cal $frak $bbb $upgreek
                    $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (alphanum-math (tex $cmr $s $d)
		     (tex $cmmi $s $d)
		     (tex $name $s $d)
		     (tex $cmsy $s $d)
		     (tex $msam $s $d)
		     (tex $msbm $s $d)
		     (tex $stmary $s $d)
		     (tex $wasy $s $d)
		     (tex $line $s $d)
		     (tex $cmex $s $d)
		     (tex $cal $s $d)
		     (tex $frak $s $d)
		     (tex $bbb $s $d)
		     (tex $upgreek $s $d)
		     (tex $bold1 $s $d)
		     (tex $bold2 $s $d)
		     (tex $boldcal $s $d)
		     (virtual tradi-long $s $d)
		     (virtual tradi-negate $s $d)
		     (virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-alphanum $name $s $d)
     (alphanum-math $name ecrm cmr cmmi cmsy msam msbm stmary wasy line
		    cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-alphanum $name $s $d)
     (alphanum-math $name ecbx cmbx cmmib cmbsy msam msbm stmary wasyb linew
		    cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmexb $s $d))

    ((Duerer mr medium slanted $s $d) (math-alphanum cdsl $s $d))
    ((Duerer mr medium italic $s $d) (math-alphanum cdi $s $d))
    ((Duerer mr medium $a $s $d) (math-alphanum cdr $s $d))
    ((Duerer mr bold $a $s $d) (math-alphanum cdb $s $d))
    ((Duerer ms medium $a $s $d) (math-alphanum cdss $s $d))
    ((Duerer mt medium $a $s $d) (math-alphanum cdtt $s $d))
    ((Euler mr medium $a $s $d) (math-alphanum eufm $s $d))
    ((Euler mr bold $a $s $d) (math-alphanum eufb $s $d))
    ((Bbb* mr medium slanted $s $d) (math-alphanum bbmsl $s $d))
    ((Bbb* mr medium $a $s $d) (math-alphanum bbm $s $d))
    ((Bbb* mr bold slanted $s $d) (math-alphanum bbmbxsl $s $d))
    ((Bbb* mr bold $a $s $d) (math-alphanum bbmbx $s $d))
    ((Bbb* ms medium italic $s $d) (math-alphanum bbmssi $s $d))
    ((Bbb* ms medium $a $s $d) (math-alphanum bbmss $s $d))
    ((Bbb* ms bold $a $s $d) (math-alphanum bbmssbx $s $d))
    ((Bbb* mt medium $a $s $d) (math-alphanum bbmsltt $s $d))
    ((Bbb** mr medium $a $s $d) (math-alphanum bbold $s $d))
    ((Bbb*** mr medium $a $s $d) (math-alphanum ocmr $s $d))
    ((Bbb**** mr medium $a $s $d) (math-alphanum dsrom $s $d))
    ((Bbb**** ms medium $a $s $d) (math-alphanum dsss $s $d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Support for the Euler New Roman fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font-rules
  `(((enr-math $name $ecrm $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
	       $cal $frak $bbb $upgreek $bold1 $bold2 $boldcal $cmex $s $d)
     (math
      (enr-math (tex $cmr $s $d)
		(tex $cmmi $s $d)
		(tex $name $s $d)
		(tex $cmsy $s $d)
		(tex $msam $s $d)
		(tex $msbm $s $d)
		(tex $stmary $s $d)
		(tex $wasy $s $d)
		(tex $line $s $d)
		(tex $cmex $s $d)
		(tex $cal $s $d)
		(tex $frak $s $d)
		(tex $bbb $s $d)
		(tex $upgreek $s $d)
		(tex $bold1 $s $d)
		(tex $bold2 $s $d)
		(tex $boldcal $s $d)
		(virtual tradi-long $s $d)
		(virtual tradi-negate $s $d)
		(virtual tradi-misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-enr $name $s $d)
     (enr-math $name ecrm cmr cmmi cmsy msam msbm stmary wasy line
	       cmsy eufm bbm grmn eurb eurb cmbsy cmex $s $d))
    ((bold-math-enr $name $s $d)
     (enr-math $name ecbx cmbx cmmib cmbsy msam msbm stmary wasyb linew
	       cmbsy eufb bbmbx grxn eurb eurb cmbsy cmexb $s $d))

    ((ENR mr medium $a $s $d) (math-enr eurm $s $d))
    ((ENR mr bold $a $s $d) (math-enr eurb $s $d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stix fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-font-rules
 '(((unicode-math $up $it $bup $bit $t $a $b $s $d)
    (unimath
     (unicode $up $s $d)
     (unicode $it $s $d)
     (unicode $bup $s $d)
     (unicode $bit $s $d)
     (roman $t $a $b $s $d)))

   ;;((math-bonum $t bold right $s $d)
   ;; (unicode-math texgyrebonum-bold texgyrebonum-bold
   ;;		  texgyrebonum-bold texgyrebonum-bold
   ;;		  $t $a $b $s $d))
   ((math-bonum $t $a right $s $d)
    (unicode-math texgyrebonum-math texgyrebonum-math
		  texgyrebonum-bold texgyrebonum-bold
		  $t $a $b $s $d))
   ;;((math-bonum $t bold $b $s $d)
   ;; (unicode-math texgyrebonum-bold texgyrebonum-bolditalic
   ;;		  texgyrebonum-bold texgyrebonum-bolditalic
   ;;		  $t $a $b $s $d))
   ((math-bonum $t $a $b $s $d)
    (unicode-math texgyrebonum-math texgyrebonum-italic
		  texgyrebonum-bold texgyrebonum-bolditalic
		  $t $a $b $s $d))

   ;;((math-pagella $t bold right $s $d)
   ;; (unicode-math texgyrepagella-bold texgyrepagella-bold
   ;;		  texgyrepagella-bold texgyrepagella-bold
   ;;		  $t $a $b $s $d))
   ((math-pagella $t $a right $s $d)
    (unicode-math texgyrepagella-math texgyrepagella-math
		  texgyrepagella-bold texgyrepagella-bold
		  $t $a $b $s $d))
   ;;((math-pagella $t bold $b $s $d)
   ;; (unicode-math texgyrepagella-bold texgyrepagella-bolditalic
   ;;		  texgyrepagella-bold texgyrepagella-bolditalic
   ;;		  $t $a $b $s $d))
   ((math-pagella $t $a $b $s $d)
    (unicode-math texgyrepagella-math texgyrepagella-italic
		  texgyrepagella-bold texgyrepagella-bolditalic
		  $t $a $b $s $d))

   ;;((math-schola $t bold right $s $d)
   ;; (unicode-math texgyreschola-bold texgyreschola-bold
   ;;		  texgyreschola-bold texgyreschola-bold
   ;;		  $t $a $b $s $d))
   ((math-schola $t $a right $s $d)
    (unicode-math texgyreschola-math texgyreschola-math
		  texgyreschola-bold texgyreschola-bold
		  $t $a $b $s $d))
   ;;((math-schola $t bold $b $s $d)
   ;; (unicode-math texgyreschola-bold texgyreschola-bolditalic
   ;;		  texgyreschola-bold texgyreschola-bolditalic
   ;;		  $t $a $b $s $d))
   ((math-schola $t $a $b $s $d)
    (unicode-math texgyreschola-math texgyreschola-italic
		  texgyreschola-bold texgyreschola-bolditalic
		  $t $a $b $s $d))

   ;;((math-termes $t bold right $s $d)
   ;; (unicode-math texgyretermes-bold texgyretermes-bold
   ;;		  texgyretermes-bold texgyretermes-bold
   ;;		  $t $a $b $s $d))
   ((math-termes $t $a right $s $d)
    (unicode-math texgyretermes-math texgyretermes-math
		  texgyretermes-bold texgyretermes-bold
		  $t $a $b $s $d))
   ;;((math-termes $t bold $b $s $d)
   ;; (unicode-math texgyretermes-bold texgyretermes-bolditalic
   ;;		  texgyretermes-bold texgyretermes-bolditalic
   ;;		  $t $a $b $s $d))
   ((math-termes $t $a $b $s $d)
    (unicode-math texgyretermes-math texgyretermes-italic
		  texgyretermes-bold texgyretermes-bolditalic
		  $t $a $b $s $d))

   ((math-stix $t bold right $s $d)
    (unicode-math STIX-Bold STIX-Bold
		  STIX-Bold STIX-Bold
		  $t bold $b $s $d))
   ((math-stix $t $a right $s $d)
    (unicode-math STIX-Regular STIX-Regular
		  STIX-Bold STIX-Bold
		  $t $a $b $s $d))
   ((math-stix $t bold $b $s $d)
    (unicode-math STIX-Bold STIX-BoldItalic
		  STIX-Bold STIX-BoldItalic
		  $t bold $b $s $d))
   ((math-stix $t $a $b $s $d)
    (unicode-math STIX-Regular STIX-Italic
		  STIX-Bold STIX-BoldItalic
		  $t $a $b $s $d))

   ((math-asana $t $a $b $s $d)
    (unicode-math Asana-Math Asana-Math
		  Asana-Math Asana-Math
		  $t $a $b $s $d))
   ((math-lucida $t $a $b $s $d)
    (unicode-math LucidaGrande LucidaGrande
		  LucidaGrande LucidaGrande
		  $t $a $b $s $d))
   ((math-apple $t $a $b $s $d)
    (unicode-math #{Apple Symbols}# #{Apple Symbols}#
		  #{Apple Symbols}# #{Apple Symbols}#
		  $t $a $b $s $d))

   ((math-dejavu ms bold right $s $d)
    (unicode-math DejaVuSans-Bold DejaVuSans-Bold
		  DejaVuSans-Bold DejaVuSans-Bold
		  ms bold $b $s $d))
   ((math-dejavu ms $a right $s $d)
    (unicode-math DejaVuSans DejaVuSans
		  DejaVuSans-Bold DejaVuSans-Bold
		  ms $a $b $s $d))
   ((math-dejavu ms bold $b $s $d)
    (unicode-math DejaVuSans-Bold DejaVuSans-BoldOblique
		  DejaVuSans-Bold DejaVuSans-BoldOblique
		  ms bold $b $s $d))
   ((math-dejavu ms $a $b $s $d)
    (unicode-math DejaVuSans DejaVuSans-Oblique
		  DejaVuSans-Bold DejaVuSans-BoldOblique
		  ms $a $b $s $d))

   ((math-dejavu mt bold right $s $d)
    (unicode-math DejaVuSansMono-Bold DejaVuSansMono-Bold
		  DejaVuSansMono-Bold DejaVuSansMono-Bold
		  mt bold $b $s $d))
   ((math-dejavu mt $a right $s $d)
    (unicode-math DejaVuSansMono DejaVuSansMono
		  DejaVuSansMono-Bold DejaVuSansMono-Bold
		  mt $a $b $s $d))
   ((math-dejavu mt bold $b $s $d)
    (unicode-math DejaVuSansMono-Bold DejaVuSansMono-BoldOblique
		  DejaVuSansMono-Bold DejaVuSansMono-BoldOblique
		  mt bold $b $s $d))
   ((math-dejavu mt $a $b $s $d)
    (unicode-math DejaVuSansMono DejaVuSansMono-Oblique
		  DejaVuSansMono-Bold DejaVuSansMono-BoldOblique
		  mt $a $b $s $d))

   ((math-dejavu $t bold right $s $d)
    (unicode-math DejaVuSerif-Bold DejaVuSerif-Bold
		  DejaVuSerif-Bold DejaVuSerif-Bold
		  $t bold $b $s $d))
   ((math-dejavu $t $a right $s $d)
    (unicode-math DejaVuSerif DejaVuSerif
		  DejaVuSerif-Bold DejaVuSerif-Bold
		  $t $a $b $s $d))
   ((math-dejavu $t bold $b $s $d)
    (unicode-math DejaVuSerif-Bold DejaVuSerif-BoldItalic
		  DejaVuSerif-Bold DejaVuSerif-BoldItalic
		  $t bold $b $s $d))
   ((math-dejavu $t $a $b $s $d)
    (unicode-math DejaVuSerif DejaVuSerif-Italic
		  DejaVuSerif-Bold DejaVuSerif-BoldItalic
		  $t $a $b $s $d))))
