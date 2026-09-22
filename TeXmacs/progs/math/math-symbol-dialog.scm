;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-symbol-dialog.scm
;; DESCRIPTION : a window for inserting a mathematical symbol
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-symbol-dialog)
  (:use (math math-menu)))

;; The palettes of math-menu.scm are the source of truth for the symbols
;; TeXmacs has always had; this window arranges them in tabs and adds the
;; ones which came from the unicode-math list and are in no palette, see
;; langs/encoding/tmuniversaltounicode-extra.scm.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (math-symbols-common)
  (scrollable
    (padded
      (bold (text "Arithmetic and order"))
      (tile 16
        (symbol "<pm>") (symbol "<mp>") (symbol "<times>") (symbol "<div>")
        (symbol "<cdot>") (symbol "<ast>") (symbol "<circ>") (symbol "<bullet>")
        (symbol "<leq>") (symbol "<geq>") (symbol "<ll>") (symbol "<gg>")
        (symbol "<neq>") (symbol "<approx>") (symbol "<sim>") (symbol "<simeq>")
        (symbol "<equiv>") (symbol "<propto>") (symbol "<prec>") (symbol "<succ>"))
      ===
      (bold (text "Sets"))
      (tile 16
        (symbol "<in>") (symbol "<ni>") (symbol "<notin>") (symbol "<subset>")
        (symbol "<subseteq>") (symbol "<supset>") (symbol "<supseteq>")
        (symbol "<cup>") (symbol "<cap>") (symbol "<setminus>")
        (symbol "<emptyset>") (symbol "<varnothing>") (symbol "<infty>")
        (symbol "<partial>") (symbol "<nabla>") (symbol "<QED>"))
      ===
      (bold (text "Logic"))
      (tile 16
        (symbol "<forall>") (symbol "<exists>") (symbol "<nexists>")
        (symbol "<neg>") (symbol "<wedge>") (symbol "<vee>")
        (symbol "<Rightarrow>") (symbol "<Leftarrow>") (symbol "<Leftrightarrow>")
        (symbol "<vdash>") (symbol "<models>") (symbol "<bot>") (symbol "<top>"))
      ===
      (bold (text "Arrows"))
      (tile 16
        (symbol "<rightarrow>") (symbol "<leftarrow>") (symbol "<leftrightarrow>")
        (symbol "<mapsto>") (symbol "<hookrightarrow>") (symbol "<twoheadrightarrow>")
        (symbol "<uparrow>") (symbol "<downarrow>") (symbol "<nearrow>")
        (symbol "<searrow>") (symbol "<nwarrow>") (symbol "<swarrow>"))
      ===
      (bold (text "Greek"))
      (tile 16
        (symbol "<alpha>") (symbol "<beta>") (symbol "<gamma>") (symbol "<delta>")
        (symbol "<varepsilon>") (symbol "<zeta>") (symbol "<eta>") (symbol "<theta>")
        (symbol "<kappa>") (symbol "<lambda>") (symbol "<mu>") (symbol "<nu>")
        (symbol "<xi>") (symbol "<pi>") (symbol "<rho>") (symbol "<sigma>")
        (symbol "<tau>") (symbol "<phi>") (symbol "<chi>") (symbol "<psi>")
        (symbol "<omega>") (symbol "<Gamma>") (symbol "<Delta>") (symbol "<Theta>")
        (symbol "<Lambda>") (symbol "<Xi>") (symbol "<Pi>") (symbol "<Sigma>")
        (symbol "<Phi>") (symbol "<Psi>") (symbol "<Omega>"))
      ===
      (bold (text "Dots and spacing"))
      (tile 16 (link dots-menu)))))

(tm-widget (math-symbols-greek)
  (scrollable
    (padded
      (bold (text "Lowercase"))
      (tile 16 (link lower-greek-menu))
      ===
      (bold (text "Uppercase"))
      (tile 16 (link upper-greek-menu))
      ===
      (bold (text "Bold"))
      (tile 16 (link bold-greek-menu)))))

(tm-widget (math-symbols-letters)
  (scrollable
    (padded
      (bold (text "Calligraphic"))
      (tile 16 (link cal-menu))
      ===
      (bold (text "Fraktur"))
      (tile 16 (link frak-menu))
      ===
      (bold (text "Blackboard bold"))
      (tile 16 (link bbb-menu))
      ===
      (bold (text "Bold"))
      (tile 16 (link bold-alpha-menu))
      ===
      (bold (text "Bold upright"))
      (tile 16 (link bold-up-alpha-menu))
      ===
      (bold (text "Bold digits"))
      (tile 16 (link bold-num-menu))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<Angstrom>") (symbol "<Bbbgamma>") (symbol "<BbbGamma>")
          (symbol "<BbbPi>")))))

(tm-widget (math-symbols-operators)
  (scrollable
    (padded
      (bold (text "Binary operations"))
      (tile 16 (link binary-operation-menu))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<tieconcat>") (symbol "<fracslash>") (symbol "<upand>")
          (symbol "<divslash>") (symbol "<vysmblkcircle>")
          (symbol "<dotminus>") (symbol "<invlazys>")
          (symbol "<cupleftarrow>") (symbol "<cupdot>")
          (symbol "<circledequal>") (symbol "<barvee>")
          (symbol "<lozengeminus>") (symbol "<concavediamond>")
          (symbol "<concavediamondtickleft>")
          (symbol "<concavediamondtickright>") (symbol "<olessthan>")
          (symbol "<ogreaterthan>") (symbol "<vectimes>")
          (symbol "<dottimes>") (symbol "<btimes>"))
      ===
      (bold (text "Big operators"))
      (tile 16 (link big-operator-menu))
      ===
      (bold (text "Large operators from unicode-math"))
      (tile 16
          (symbol "<Bbbsum>") (symbol "<intclockwise>")
          (symbol "<varointclockwise>") (symbol "<ointctrclockwise>")
          (symbol "<bigbot>") (symbol "<bigtop>") (symbol "<bigcupdot>")
          (symbol "<bigtimes>") (symbol "<fint>") (symbol "<awint>")
          (symbol "<sqint>"))
      ===
      (bold (text "Named operators"))
      (tile 4 (link textual-operator-menu)))))

(tm-widget (math-symbols-relations)
  (scrollable
    (padded
      (bold (text "Relations"))
      (tile 16 (link binary-relation-menu-1))
      ===
      (bold (text "More relations"))
      (tile 16 (link binary-relation-menu-2))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<smallin>") (symbol "<smallni>") (symbol "<mathratio>")
          (symbol "<Colon>") (symbol "<dashcolon>")
          (symbol "<dotsminusdots>") (symbol "<kernelcontraction>")
          (symbol "<simneqq>") (symbol "<approxident>") (symbol "<backcong>")
          (symbol "<eqcolon>") (symbol "<arceq>") (symbol "<wedgeq>")
          (symbol "<veeeq>") (symbol "<stareq>") (symbol "<eqdef>")
          (symbol "<measeq>") (symbol "<questeq>") (symbol "<Equiv>")
          (symbol "<nlesssim>") (symbol "<ngtrsim>") (symbol "<nlessgtr>")
          (symbol "<ngtrless>") (symbol "<assert>") (symbol "<VDash>")
          (symbol "<origof>") (symbol "<imageof>") (symbol "<equalparallel>")
          (symbol "<eqless>") (symbol "<eqgtr>") (symbol "<sqsubsetneq>")
          (symbol "<sqsupsetneq>") (symbol "<DashVDash>")
          (symbol "<dashVdash>") (symbol "<multimapinv>")
          (symbol "<vlongdash>") (symbol "<dualmap>") (symbol "<Coloneq>")
          (symbol "<precneq>") (symbol "<succneq>") (symbol "<preceqq>")
          (symbol "<succeqq>") (symbol "<dashV>") (symbol "<Dashv>")
          (symbol "<DashV>") (symbol "<barV>") (symbol "<Vbar>")
          (symbol "<leqqslant>") (symbol "<geqqslant>"))
      ===
      (bold (text "Negations"))
      (tile 16 (link negation-menu-1))
      (tile 16 (link negation-menu-2)))))

(tm-widget (math-symbols-arrows)
  (scrollable
    (padded
      (bold (text "Horizontal"))
      (tile 16 (link horizontal-arrow-menu))
      ===
      (bold (text "Vertical and diagonal"))
      (tile 16 (link vertical-arrow-menu))
      ===
      (bold (text "Long"))
      (tile 16 (link long-arrow-menu))
      ===
      (bold (text "Extensible"))
      (tile 16 (link extensible-arrow-menu))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<twoheaduparrow>") (symbol "<twoheaddownarrow>")
          (symbol "<mapsup>") (symbol "<mapsdown>") (symbol "<Ldsh>")
          (symbol "<Rdsh>") (symbol "<updownarrows>") (symbol "<Nwarrow>")
          (symbol "<Nearrow>") (symbol "<Searrow>") (symbol "<Swarrow>")
          (symbol "<downuparrows>") (symbol "<rightthreearrows>")
          (symbol "<rightarrowonoplus>") (symbol "<Longmapsfrom>")
          (symbol "<Longmapsto>") (symbol "<longrightsquigarrow>")
          (symbol "<Mapsfrom>") (symbol "<Mapsto>")
          (symbol "<leftthreearrows>") (symbol "<longleftsquigarrow>")))))

(tm-widget (math-symbols-brackets)
  (scrollable
    (padded
      (bold (text "Large delimiters"))
      (tile 16 (link large-delimiter-menu))
      ===
      (bold (text "Opening"))
      (tile 16 (link left-delimiter-menu))
      ===
      (bold (text "Separators"))
      (tile 16 (link middle-delimiter-menu))
      ===
      (bold (text "Closing"))
      (tile 16 (link right-delimiter-menu))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<lbag>") (symbol "<lgroup>")
          (symbol "<rbag>") (symbol "<rgroup>")))))

(tm-widget (math-symbols-misc)
  (scrollable
    (padded
      (bold (text "Miscellaneous"))
      (tile 16 (link miscellaneous-symbol-menu))
      ===
      (bold (text "Dots"))
      (tile 16 (link dots-menu))
      ===
      (bold (text "From unicode-math"))
      (tile 16
          (symbol "<mathhyphen>") (symbol "<horizbar>")
          (symbol "<twolowline>") (symbol "<dprime>") (symbol "<trprime>")
          (symbol "<backdprime>") (symbol "<backtrprime>")
          (symbol "<qprime>") (symbol "<euro>") (symbol "<Eulerconst>")
          (symbol "<Planckconst>") (symbol "<Finv>") (symbol "<Game>")
          (symbol "<mitBbbD>") (symbol "<mitBbbd>") (symbol "<mitBbbe>")
          (symbol "<mitBbbi>") (symbol "<mitBbbj>") (symbol "<linefeed>")
          (symbol "<carriagereturn>") (symbol "<leftdasharrow>")
          (symbol "<rightdasharrow>") (symbol "<leftwhitearrow>")
          (symbol "<upwhitearrow>") (symbol "<rightwhitearrow>")
          (symbol "<downwhitearrow>") (symbol "<increment>") (symbol "<QED>")
          (symbol "<rightangle>") (symbol "<sinewave>")
          (symbol "<hermitmatrix>") (symbol "<measuredrightangle>")
          (symbol "<varlrtriangle>") (symbol "<turnednot>")
          (symbol "<inttop>") (symbol "<intbottom>") (symbol "<lparenuend>")
          (symbol "<lparenextender>") (symbol "<lparenlend>")
          (symbol "<rparenuend>") (symbol "<rparenextender>")
          (symbol "<rparenlend>") (symbol "<lbrackuend>")
          (symbol "<lbrackextender>") (symbol "<lbracklend>")
          (symbol "<rbrackuend>") (symbol "<rbrackextender>")
          (symbol "<rbracklend>") (symbol "<lbraceuend>")
          (symbol "<lbracemid>") (symbol "<lbracelend>")
          (symbol "<vbraceextender>") (symbol "<rbraceuend>")
          (symbol "<rbracemid>") (symbol "<rbracelend>")
          (symbol "<intextender>") (symbol "<sumtop>") (symbol "<sumbottom>")
          (symbol "<sqrtbottom>") (symbol "<obrbrak>") (symbol "<ubrbrak>")
          (symbol "<blanksymbol>") (symbol "<mathvisiblespace>")
          (symbol "<blockfull>") (symbol "<blockqtrshaded>")
          (symbol "<blockhalfshaded>") (symbol "<blockthreeqtrshaded>")
          (symbol "<smblksquare>") (symbol "<smwhtsquare>")
          (symbol "<hrectangleblack>") (symbol "<hrectangle>")
          (symbol "<bigblacktriangleup>") (symbol "<bigblacktriangledown>")
          (symbol "<mdlgblkdiamond>") (symbol "<smwhtcircle>")
          (symbol "<mdwhtsquare>") (symbol "<mdblksquare>")
          (symbol "<varspadesuit>") (symbol "<varclubsuit>")
          (symbol "<mdwhtcircle>") (symbol "<mdblkcircle>")
          (symbol "<mdsmwhtcircle>") (symbol "<diamondcdot>")
          (symbol "<mdsmblkcircle>") (symbol "<dottedsquare>")
          (symbol "<lgblkcircle>") (symbol "<mdblkdiamond>")
          (symbol "<mdwhtdiamond>") (symbol "<mdwhtlozenge>")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The window
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (math-symbols-widget)
  (resize "660px" "480px"
    (padded
      (tabs
        (tab (text "Common") (dynamic (math-symbols-common)))
        (tab (text "Greek") (dynamic (math-symbols-greek)))
        (tab (text "Letters") (dynamic (math-symbols-letters)))
        (tab (text "Operators") (dynamic (math-symbols-operators)))
        (tab (text "Relations") (dynamic (math-symbols-relations)))
        (tab (text "Arrows") (dynamic (math-symbols-arrows)))
        (tab (text "Brackets") (dynamic (math-symbols-brackets)))
        (tab (text "Miscellaneous") (dynamic (math-symbols-misc)))))))

(tm-define (open-math-symbols)
  (:synopsis "Open the window for inserting a mathematical symbol")
  (:interactive #t)
  (top-window math-symbols-widget "Mathematical symbols"))
