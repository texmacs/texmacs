;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-symbol-tools.scm
;; DESCRIPTION : a side tool and a window for inserting a mathematical symbol
;; COPYRIGHT   : (C) 2026  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-symbol-tools)
  (:use (math math-menu)))

;; The palettes of math-menu.scm are the source of truth for the symbols
;; TeXmacs has always had; the groups below arrange them and add the ones
;; which came from the unicode-math list and are in no palette, see
;; langs/encoding/tmuniversaltounicode-extra.scm.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaring a group of symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A group has a name and sections, each of them a title and the items of a
;; palette. The number of columns of a tile has to be known when the widget
;; is built, so the macro lays every group out once per width its callers
;; ask for: sixteen symbols a row for the window below, four for the side
;; tool, a dock being narrow. A section which begins with :narrow gets a
;; quarter of the columns, for items as wide as the name of an operator.

(define math-symbols-columns (list 16 4))

(define (math-symbols-section-body sec cols)
  (let* ((title (car sec))
         (narrow? (keyword? (cadr sec)))
         (items (if narrow? (cddr sec) (cdr sec)))
         (n (if narrow? (max 1 (quotient cols 4)) cols)))
    `((bold (text ,title)) (tile ,n ,@items))))

(define (math-symbols-group-body sections cols)
  (if (null? sections) (list)
      (let* ((body (math-symbols-section-body (car sections) cols))
             (rest (math-symbols-group-body (cdr sections) cols)))
        (if (null? rest) body (append body (list '===) rest)))))

(define-macro (define-math-symbols-group label . sections)
  `(begin
     ,@(map (lambda (cols)
              `(tm-widget (math-symbols-group group cols)
                 (:require (and (== group ,label) (== cols ,cols)))
                 (padded ,@(math-symbols-group-body sections cols))))
            math-symbols-columns)))

;; the master of the groups: each declaration below overrides it for its own
;; name and for the widths above
(tm-widget (math-symbols-group group cols)
  (text "no such group of symbols"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The groups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-math-symbols-group "Common"
  ("Arithmetic"
   (symbol "<pm>") (symbol "<mp>") (symbol "<times>") (symbol "<div>")
   (symbol "<cdot>") (symbol "<ast>") (symbol "<circ>") (symbol "<bullet>")
   (symbol "<leq>") (symbol "<geq>") (symbol "<ll>") (symbol "<gg>")
   (symbol "<neq>") (symbol "<approx>") (symbol "<sim>") (symbol "<simeq>")
   (symbol "<equiv>") (symbol "<propto>") (symbol "<prec>")
   (symbol "<succ>"))
  ("Sets"
   (symbol "<in>") (symbol "<ni>") (symbol "<notin>") (symbol "<subset>")
   (symbol "<subseteq>") (symbol "<supset>") (symbol "<supseteq>")
   (symbol "<cup>") (symbol "<cap>") (symbol "<setminus>")
   (symbol "<emptyset>") (symbol "<varnothing>") (symbol "<infty>")
   (symbol "<partial>") (symbol "<nabla>") (symbol "<QED>"))
  ("Logic"
   (symbol "<forall>") (symbol "<exists>") (symbol "<nexists>")
   (symbol "<neg>") (symbol "<wedge>") (symbol "<vee>")
   (symbol "<Rightarrow>") (symbol "<Leftarrow>")
   (symbol "<Leftrightarrow>") (symbol "<vdash>") (symbol "<models>")
   (symbol "<bot>") (symbol "<top>"))
  ("Arrows"
   (symbol "<rightarrow>") (symbol "<leftarrow>")
   (symbol "<leftrightarrow>") (symbol "<mapsto>")
   (symbol "<hookrightarrow>") (symbol "<twoheadrightarrow>")
   (symbol "<uparrow>") (symbol "<downarrow>") (symbol "<nearrow>")
   (symbol "<searrow>") (symbol "<nwarrow>") (symbol "<swarrow>"))
  ("Greek"
   (symbol "<alpha>") (symbol "<beta>") (symbol "<gamma>")
   (symbol "<delta>") (symbol "<varepsilon>") (symbol "<zeta>")
   (symbol "<eta>") (symbol "<theta>") (symbol "<kappa>")
   (symbol "<lambda>") (symbol "<mu>") (symbol "<nu>") (symbol "<xi>")
   (symbol "<pi>") (symbol "<rho>") (symbol "<sigma>") (symbol "<tau>")
   (symbol "<phi>") (symbol "<chi>") (symbol "<psi>") (symbol "<omega>")
   (symbol "<Gamma>") (symbol "<Delta>") (symbol "<Theta>")
   (symbol "<Lambda>") (symbol "<Xi>") (symbol "<Pi>") (symbol "<Sigma>")
   (symbol "<Phi>") (symbol "<Psi>") (symbol "<Omega>"))
  ("Dots"
   (link dots-menu)))

(define-math-symbols-group "Greek"
  ("Lowercase"
   (link lower-greek-menu))
  ("Uppercase"
   (link upper-greek-menu))
  ("Bold"
   (link bold-greek-menu)))

(define-math-symbols-group "Letters"
  ("Calligraphic"
   (link cal-menu))
  ("Fraktur"
   (link frak-menu))
  ("Blackboard bold"
   (link bbb-menu))
  ("Bold"
   (link bold-alpha-menu))
  ("Bold upright"
   (link bold-up-alpha-menu))
  ("Bold digits"
   (link bold-num-menu))
  ("From unicode-math"
   (symbol "<Angstrom>") (symbol "<Bbbgamma>") (symbol "<BbbGamma>")
   (symbol "<BbbPi>")))

(define-math-symbols-group "Operators"
  ("Binary"
   (link binary-operation-menu))
  ("From unicode-math"
   (symbol "<tieconcat>") (symbol "<fracslash>") (symbol "<upand>")
   (symbol "<divslash>") (symbol "<vysmblkcircle>") (symbol "<dotminus>")
   (symbol "<invlazys>") (symbol "<cupleftarrow>") (symbol "<cupdot>")
   (symbol "<circledequal>") (symbol "<barvee>") (symbol "<lozengeminus>")
   (symbol "<concavediamond>") (symbol "<concavediamondtickleft>")
   (symbol "<concavediamondtickright>") (symbol "<olessthan>")
   (symbol "<ogreaterthan>") (symbol "<vectimes>") (symbol "<dottimes>")
   (symbol "<btimes>"))
  ("Big operators"
   (link big-operator-menu))
  ("More big operators"
   (symbol "<Bbbsum>") (symbol "<intclockwise>")
   (symbol "<varointclockwise>") (symbol "<ointctrclockwise>")
   (symbol "<bigbot>") (symbol "<bigtop>") (symbol "<bigcupdot>")
   (symbol "<bigtimes>") (symbol "<fint>") (symbol "<awint>")
   (symbol "<sqint>"))
  ("Named operators" :narrow
   (link textual-operator-menu)))

(define-math-symbols-group "Relations"
  ("Relations"
   (link binary-relation-menu-1))
  ("More relations"
   (link binary-relation-menu-2))
  ("From unicode-math"
   (symbol "<smallin>") (symbol "<smallni>") (symbol "<mathratio>")
   (symbol "<Colon>") (symbol "<dashcolon>") (symbol "<dotsminusdots>")
   (symbol "<kernelcontraction>") (symbol "<simneqq>")
   (symbol "<approxident>") (symbol "<backcong>") (symbol "<eqcolon>")
   (symbol "<arceq>") (symbol "<wedgeq>") (symbol "<veeeq>")
   (symbol "<stareq>") (symbol "<eqdef>") (symbol "<measeq>")
   (symbol "<questeq>") (symbol "<Equiv>") (symbol "<nlesssim>")
   (symbol "<ngtrsim>") (symbol "<nlessgtr>") (symbol "<ngtrless>")
   (symbol "<assert>") (symbol "<VDash>") (symbol "<origof>")
   (symbol "<imageof>") (symbol "<equalparallel>") (symbol "<eqless>")
   (symbol "<eqgtr>") (symbol "<sqsubsetneq>") (symbol "<sqsupsetneq>")
   (symbol "<DashVDash>") (symbol "<dashVdash>") (symbol "<multimapinv>")
   (symbol "<vlongdash>") (symbol "<dualmap>") (symbol "<Coloneq>")
   (symbol "<precneq>") (symbol "<succneq>") (symbol "<preceqq>")
   (symbol "<succeqq>") (symbol "<dashV>") (symbol "<Dashv>")
   (symbol "<DashV>") (symbol "<barV>") (symbol "<Vbar>")
   (symbol "<leqqslant>") (symbol "<geqqslant>"))
  ("Negations"
   (link negation-menu-1) (link negation-menu-2)))

(define-math-symbols-group "Arrows"
  ("Horizontal"
   (link horizontal-arrow-menu))
  ("Vertical"
   (link vertical-arrow-menu))
  ("Long"
   (link long-arrow-menu))
  ("Extensible"
   (link extensible-arrow-menu))
  ("From unicode-math"
   (symbol "<twoheaduparrow>") (symbol "<twoheaddownarrow>")
   (symbol "<mapsup>") (symbol "<mapsdown>") (symbol "<Ldsh>")
   (symbol "<Rdsh>") (symbol "<updownarrows>") (symbol "<Nwarrow>")
   (symbol "<Nearrow>") (symbol "<Searrow>") (symbol "<Swarrow>")
   (symbol "<downuparrows>") (symbol "<rightthreearrows>")
   (symbol "<rightarrowonoplus>") (symbol "<Longmapsfrom>")
   (symbol "<Longmapsto>") (symbol "<longrightsquigarrow>")
   (symbol "<Mapsfrom>") (symbol "<Mapsto>") (symbol "<leftthreearrows>")
   (symbol "<longleftsquigarrow>")))

(define-math-symbols-group "Brackets"
  ("Large"
   (link large-delimiter-menu))
  ("Opening"
   (link left-delimiter-menu))
  ("Separators"
   (link middle-delimiter-menu))
  ("Closing"
   (link right-delimiter-menu))
  ("From unicode-math"
   (symbol "<lbag>") (symbol "<lgroup>") (symbol "<rbag>")
   (symbol "<rgroup>")))

(define-math-symbols-group "Miscellaneous"
  ("Miscellaneous"
   (link miscellaneous-symbol-menu))
  ("Dots"
   (link dots-menu))
  ("From unicode-math"
   (symbol "<mathhyphen>") (symbol "<horizbar>") (symbol "<twolowline>")
   (symbol "<dprime>") (symbol "<trprime>") (symbol "<backdprime>")
   (symbol "<backtrprime>") (symbol "<qprime>") (symbol "<euro>")
   (symbol "<Eulerconst>") (symbol "<Planckconst>") (symbol "<Finv>")
   (symbol "<Game>") (symbol "<mitBbbD>") (symbol "<mitBbbd>")
   (symbol "<mitBbbe>") (symbol "<mitBbbi>") (symbol "<mitBbbj>")
   (symbol "<linefeed>") (symbol "<carriagereturn>")
   (symbol "<leftdasharrow>") (symbol "<rightdasharrow>")
   (symbol "<leftwhitearrow>") (symbol "<upwhitearrow>")
   (symbol "<rightwhitearrow>") (symbol "<downwhitearrow>")
   (symbol "<increment>") (symbol "<QED>") (symbol "<rightangle>")
   (symbol "<sinewave>") (symbol "<hermitmatrix>")
   (symbol "<measuredrightangle>") (symbol "<varlrtriangle>")
   (symbol "<turnednot>") (symbol "<inttop>") (symbol "<intbottom>")
   (symbol "<lparenuend>") (symbol "<lparenextender>")
   (symbol "<lparenlend>") (symbol "<rparenuend>")
   (symbol "<rparenextender>") (symbol "<rparenlend>")
   (symbol "<lbrackuend>") (symbol "<lbrackextender>")
   (symbol "<lbracklend>") (symbol "<rbrackuend>")
   (symbol "<rbrackextender>") (symbol "<rbracklend>")
   (symbol "<lbraceuend>") (symbol "<lbracemid>") (symbol "<lbracelend>")
   (symbol "<vbraceextender>") (symbol "<rbraceuend>")
   (symbol "<rbracemid>") (symbol "<rbracelend>") (symbol "<intextender>")
   (symbol "<sumtop>") (symbol "<sumbottom>") (symbol "<sqrtbottom>")
   (symbol "<obrbrak>") (symbol "<ubrbrak>") (symbol "<blanksymbol>")
   (symbol "<mathvisiblespace>") (symbol "<blockfull>")
   (symbol "<blockqtrshaded>") (symbol "<blockhalfshaded>")
   (symbol "<blockthreeqtrshaded>") (symbol "<smblksquare>")
   (symbol "<smwhtsquare>") (symbol "<hrectangleblack>")
   (symbol "<hrectangle>") (symbol "<bigblacktriangleup>")
   (symbol "<bigblacktriangledown>") (symbol "<mdlgblkdiamond>")
   (symbol "<smwhtcircle>") (symbol "<mdwhtsquare>")
   (symbol "<mdblksquare>") (symbol "<varspadesuit>")
   (symbol "<varclubsuit>") (symbol "<mdwhtcircle>")
   (symbol "<mdblkcircle>") (symbol "<mdsmwhtcircle>")
   (symbol "<diamondcdot>") (symbol "<mdsmblkcircle>")
   (symbol "<dottedsquare>") (symbol "<lgblkcircle>")
   (symbol "<mdblkdiamond>") (symbol "<mdwhtdiamond>")
   (symbol "<mdwhtlozenge>")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A window with one tab per group
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define math-symbols-groups
  (list "Common" "Greek" "Letters" "Operators"
        "Relations" "Arrows" "Brackets" "Miscellaneous"))

(tm-widget (math-symbols-widget)
  (resize "820px" "560px"
    (padded
      (tabs
        (tab (text "Common") (scrollable (dynamic (math-symbols-group "Common" 16))))
        (tab (text "Greek") (scrollable (dynamic (math-symbols-group "Greek" 16))))
        (tab (text "Letters") (scrollable (dynamic (math-symbols-group "Letters" 16))))
        (tab (text "Operators") (scrollable (dynamic (math-symbols-group "Operators" 16))))
        (tab (text "Relations") (scrollable (dynamic (math-symbols-group "Relations" 16))))
        (tab (text "Arrows") (scrollable (dynamic (math-symbols-group "Arrows" 16))))
        (tab (text "Brackets") (scrollable (dynamic (math-symbols-group "Brackets" 16))))
        (tab (text "Miscellaneous")
             (scrollable
               (dynamic (math-symbols-group "Miscellaneous" 16))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A side tool with one group at a time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define math-symbols-shown "Common")

(tm-widget (math-symbols-tool-body win)
  (padded
    (hlist
      (text "Group:") //
      (enum (begin
              (set! math-symbols-shown answer)
              (refresh-now "math-symbols-tool"))
            math-symbols-groups
            math-symbols-shown "10em")
      >>>)
    ===
    (refreshable "math-symbols-tool"
      (scrollable
        (dynamic (math-symbols-group math-symbols-shown 4))))))

(tm-tool (math-symbols-tool win)
  (:name "Mathematical symbols")
  (dynamic (math-symbols-tool-body win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Opening them
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-math-symbols-tool)
  (:synopsis "Show the mathematical symbols in a side tool")
  (tool-select :right 'math-symbols-tool))

(tm-define (open-math-symbols)
  (:synopsis "Open the window for inserting a mathematical symbol")
  (:interactive #t)
  (top-window math-symbols-widget "Mathematical symbols"))
