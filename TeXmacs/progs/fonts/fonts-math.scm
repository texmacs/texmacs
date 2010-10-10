
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
  `(((math $ecrm $cmr $cmmi $cmsy $msam $msbm $stmary $wasy $line
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
	    (virtual long $s $d)
	    (virtual negate $s $d)
	    (virtual misc $s $d))
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
     (math $ecrm $cmr $cmmi cmbsy msam msbm stmaryb wasyb linew
	   cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))
    ((math-conc $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi xccsy xccam xccbm stmary wasy line
	   cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright $ecrm $cmr $cmmi $s $d)
     (math $ecrm $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
	   cmbrsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

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
		(virtual long $s $d)
		(virtual negate $s $d)
		(virtual misc $s $d))
      (rubber (tex-rubber rubber-cmex $cmex $s $d)
	      (tex-rubber rubber-stmary $stmary $s $d)
	      (tex-rubber rubber-wasy $wasy $s $d)
	      (tex-dummy-rubber (tex-rubber rubber-cmex $cmex $s $d)))
      (ec $ecrm $s $d)
      (ec ecrm $s $d)))

    ((math-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmsy msam msbm stmary wasy line
	       cmsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))
    ((bold-math-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmbsy msam msbm stmary wasyb linew
	       cmsy eufm bbmbx grxn cmbx cmmib cmbsy cmex $s $d))
    ((math-conc-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi xccsy xccam xccbm stmary wasy line
	       cmsy eufm bbm grmn cmbx cmmib cmbsy xccex $s $d))
    ((math-bright-var $ecrm $cmr $cmmi $s $d)
     (var-math $ecrm $cmr $cmmi cmbrsy cmbram cmbrbm stmary wasy line
	       cmbrsy eufm bbm grmn cmbx cmmib cmbsy cmex $s $d))

    ((roman mr medium $a $s $d) (math-std ecrm cmr cmmi $s $d))
    ((roman mr bold $a $s $d) (bold-math-std ecbx cmbx cmmib $s $d))
    ((roman ms light $a $s $d) (math-bright cmbr cmbr cmbrmi $s $d))
    ((roman ms medium $a $s $d) (math-std ecss cmss cmssi $s $d))
    ((roman ms bold $a $s $d) (math-std ecsx cmssbx cmmib $s $d))
    ((roman mt medium $a $s $d) (math-std ectt cmtt cmitt $s $d))
    ((roman trm medium $a $s $d) (math-var ecrm cmr cmmi $s $d))
    ((roman trm bold $a $s $d) (math-var ecbx cmbx cmmib $s $d))
    ((roman tss medium $a $s $d) (math-var ecss cmss cmmi $s $d))
    ((roman tss bold $a $s $d) (math-var ecsx cmssbx cmmib $s $d))
    ((roman ttt $a $b $s $d) (math-var ectt cmtt cmmi $s $d))
    ((roman bf $a $b $s $d) (bold-math-var ecbx cmbx cmmib $s $d))
    ((roman up $a $b $s $d) (math-var ecui cmu cmmi $s $d))
    ((roman it $a $b $s $d) (math-var ecti cmti cmmi $s $d))
    ((roman sl $a $b $s $d) (math-var ecsl cmsl cmmi $s $d))

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
		    (virtual long $s $d)
		    (virtual negate $s $d)
		    (virtual misc $s $d))
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
     (capital-math $name ecbx cmbx cmmib cmbsy msam msbm stmaryb wasyb linew
		   cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

    ((cal mr medium normal $s $d) (math-capital cmsy $s $d))
    ((cal mr bold normal $s $d) (bold-math-capital cmbsy $s $d))
    ((cal* mr medium normal $s $d) (math-capital rsfs $s $d))
    ((cal** mr medium normal $s $d) (math-capital euxm $s $d))
    ((Bbb mr medium normal $s $d) (math-capital msbm $s $d))))

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
		     (virtual long $s $d)
		     (virtual negate $s $d)
		     (virtual misc $s $d))
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
     (alphanum-math $name ecbx cmbx cmmib cmbsy msam msbm stmaryb wasyb linew
		    cmbsy eufb bbmbx grxn cmbx cmmib cmbsy cmex $s $d))

    ((Duerer mr medium normal $s $d) (math-alphanum cdr $s $d))
    ((Duerer mr medium slanted $s $d) (math-alphanum cdsl $s $d))
    ((Duerer mr medium italic $s $d) (math-alphanum cdi $s $d))
    ((Duerer mr bold normal $s $d) (math-alphanum cdb $s $d))
    ((Duerer ms medium normal $s $d) (math-alphanum cdss $s $d))
    ((Duerer mt medium normal $s $d) (math-alphanum cdtt $s $d))
    ((Euler mr medium normal $s $d) (math-alphanum eufm $s $d))
    ((Euler mr bold normal $s $d) (math-alphanum eufb $s $d))
    ((Bbb* mr medium normal $s $d) (math-alphanum bbm $s $d))
    ((Bbb* mr medium slanted $s $d) (math-alphanum bbmsl $s $d))
    ((Bbb* mr bold normal $s $d) (math-alphanum bbmbx $s $d))
    ((Bbb* mr bold slanted $s $d) (math-alphanum bbmbxsl $s $d))
    ((Bbb* ms medium normal $s $d) (math-alphanum bbmss $s $d))
    ((Bbb* ms medium italic $s $d) (math-alphanum bbmssi $s $d))
    ((Bbb* ms bold normal $s $d) (math-alphanum bbmssbx $s $d))
    ((Bbb* mt medium normal $s $d) (math-alphanum bbmsltt $s $d))
    ((Bbb** mr medium normal $s $d) (math-alphanum bbold $s $d))
    ((Bbb*** mr medium normal $s $d) (math-alphanum ocmr $s $d))
    ((Bbb**** mr medium normal $s $d) (math-alphanum dsrom $s $d))
    ((Bbb**** ms medium normal $s $d) (math-alphanum dsss $s $d))))

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
		(virtual long $s $d)
		(virtual negate $s $d)
		(virtual misc $s $d))
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
     (enr-math $name ecbx cmbx cmmib cmbsy msam msbm stmaryb wasyb linew
	       cmbsy eufb bbmbx grxn eurb eurb cmbsy cmex $s $d))

    ((ENR mr medium normal $s $d) (math-enr eurm $s $d))
    ((ENR mr bold normal $s $d) (math-enr eurb $s $d))))

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

   ((math-stix $t bold $b $s $d)
    (unicode-math STIXGeneralBol STIXGeneralBolIta
		  STIXGeneralBol STIXGeneralBolIta
		  $t bold $b $s $d))
   ((math-stix $t $a $b $s $d)
    (unicode-math STIXGeneral STIXGeneralItalic
		  STIXGeneralBol STIXGeneralBolIta
		  $t $a $b $s $d))))
