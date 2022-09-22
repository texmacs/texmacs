
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-define.scm
;; DESCRIPTION : LaTeX definitions for TeXmacs extensions
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-define)
  (:use (convert latex latex-texmacs-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  ;; arrows and other symbols with limits
  (leftarrowlim "\\mathop{\\leftarrow}\\limits")
  (rightarrowlim "\\mathop{\\rightarrow}\\limits")
  (leftrightarrowlim "\\mathop{\\leftrightarrow}\\limits")
  (mapstolim "\\mathop{\\mapsto}\\limits")
  (longleftarrowlim "\\mathop{\\longleftarrow}\\limits")
  (longrightarrowlim "\\mathop{\\longrightarrow}\\limits")
  (longleftrightarrowlim "\\mathop{\\longleftrightarrow}\\limits")
  (longmapstolim "\\mathop{\\longmapsto}\\limits")
  (leftsquigarrowlim "\\mathop{\\leftsquigarrow}\\limits")
  (rightsquigarrowlim "\\mathop{\\rightsquigarrow}\\limits")
  (leftrightsquigarrowlim "\\mathop{\\leftrightsquigarrow}\\limits")
  (equallim "\\mathop{=}\\limits")
  (longequallim "\\mathop{\\longequal}\\limits")
  (Leftarrowlim "\\mathop{\\leftarrow}\\limits")
  (Rightarrowlim "\\mathop{\\rightarrow}\\limits")
  (Leftrightarrowlim "\\mathop{\\leftrightarrow}\\limits")
  (Longleftarrowlim "\\mathop{\\longleftarrow}\\limits")
  (Longrightarrowlim "\\mathop{\\longrightarrow}\\limits")
  (Longleftrightarrowlim "\\mathop{\\longleftrightarrow}\\limits")
  (cdotslim "\\mathop{\\cdots}\\limits")

  ;; rotated arrows and other symbols
  (mapsfrom (!group (mbox (rotatebox (!option "origin=c") "180"
                                     (!math (mapsto))))))
  (longmapsfrom (!group (mbox (rotatebox (!option "origin=c") "180"
                                         (!math (longmapsto))))))
  (mapmulti (!group (mbox (rotatebox (!option "origin=c") "180"
                                     (!math "\\multimap")))))
  (leftsquigarrow (!group (mbox (rotatebox (!option "origin=c") "180"
                                           (!math (rightsquigarrow))))))
  (upequal (!group (mbox (rotatebox (!option "origin=c") "90"
                                    (!math "=")))))
  (downequal (!group (mbox (rotatebox (!option "origin=c") "-90"
                                      (!math "=")))))
  (longupequal (!group (mbox (rotatebox (!option "origin=c") "90"
                                        (!math (longequal))))))
  (longdownequal (!group (mbox (rotatebox (!option "origin=c") "-90"
                                          (!math (longequal))))))
  (longupminus (!group (mbox (rotatebox (!option "origin=c") "90"
                                        (!math (longminus))))))
  (longdownminus (!group (mbox (rotatebox (!option "origin=c") "-90"
                                          (!math (longminus))))))
  (longuparrow (!group (mbox (rotatebox (!option "origin=c") "90"
                                        (!math (longrightarrow))))))
  (longdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                          (!math (longrightarrow))))))
  (longupdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                            (!math (longleftrightarrow))))))
  (Longuparrow (!group (mbox (rotatebox (!option "origin=c") "90"
                                        (!math (Longrightarrow))))))
  (Longdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                          (!math (Longrightarrow))))))
  (Longupdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                            (!math (Longleftrightarrow))))))
  (mapsup (!group (mbox (rotatebox (!option "origin=c") "90"
                                   (!math (mapsto))))))
  (mapsdown (!group (mbox (rotatebox (!option "origin=c") "-90"
                                     (!math (mapsto))))))
  (longmapsup (!group (mbox (rotatebox (!option "origin=c") "90"
                                       (!math (longmapsto))))))
  (longmapsdown (!group (mbox (rotatebox (!option "origin=c") "-90"
                                         (!math (longmapsto))))))
  (upsquigarrow (!group (mbox (rotatebox (!option "origin=c") "90"
                                         (!math (rightsquigarrow))))))
  (downsquigarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                           (!math (rightsquigarrow))))))
  (updownsquigarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                             (!math (leftrightsquigarrow))))))
  (hookuparrow (!group (mbox (rotatebox (!option "origin=c") "90"
                                        (!math (hookrightarrow))))))
  (hookdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                          (!math (hookrightarrow))))))
  (longhookuparrow (!group (mbox (rotatebox (!option "origin=c") "90"
                                            (!math (longhookrightarrow))))))
  (longhookdownarrow (!group (mbox (rotatebox (!option "origin=c") "-90"
                                              (!math (longhookrightarrow))))))
  (Backepsilon (!group (mbox (rotatebox (!option "origin=c") "180"
                                        "E"))))
  (Backsigma (!group (mbox (reflectbox (!math "\\Sigma")))))
  (Mho (!group (mbox (rotatebox (!option "origin=c") "180"
                                (!math "\\Omega")))))
  (btimes (!group (mbox (rotatebox (!option "origin=c") "90"
                                   (!math "\\ltimes")))))
  
  ;; asymptotic relations by Joris
  (nasymp "\\not\\asymp")
  (asympasymp "{\\asymp\\!\\!\\!\\!\\!\\!-}")
  (nasympasymp "{\\not\\asymp\\!\\!\\!\\!\\!\\!-}")
  (simsim "{\\approx\\!\\!\\!\\!\\!\\!-}")
  (nsimsim "{\\not\\approx\\!\\!\\!\\!\\!\\!-}")
  (triplesim "{\\approx\\!\\!\\!\\!\\!\\!\\sim}")
  (ntriplesim "{\\not\\approx\\!\\!\\!\\!\\!\\!\\sim}")
  (precprec "\\prec\\!\\!\\!\\prec")
  (precpreceq "\\preceq\\!\\!\\!\\preceq")
  (precprecprec "\\prec\\!\\!\\!\\prec\\!\\!\\!\\prec")
  (precprecpreceq "\\preceq\\!\\!\\!\\preceq\\!\\!\\!\\preceq")
  (succsucc "\\succ\\!\\!\\!\\succ")
  (succsucceq "\\succeq\\!\\!\\!\\succeq")
  (succsuccsucc "\\succ\\!\\!\\!\\succ\\!\\!\\!\\succ")
  (succsuccsucceq "\\succeq\\!\\!\\!\\succeq\\!\\!\\!\\succeq")
  (lleq "\\leq\\!\\!\\!\\leq")
  (llleq "\\leq\\!\\!\\!\\leq\\!\\!\\!\\leq")
  (ggeq "\\geq\\!\\!\\!\\geq")
  (gggeq "\\geq\\!\\!\\!\\geq\\!\\!\\!\\geq")

  ;; extra literal symbols
  (mathcatalan "C")
  (mathd "\\mathrm{d}")
  (mathD "\\mathrm{D}")
  (mathe "\\mathrm{e}")
  (matheuler "\\gamma")
  (mathGamma "\\Gamma")
  (mathlambda "\\lambda")
  (mathLaplace "\\Delta")
  (mathi "\\mathrm{i}")
  (mathpi "\\pi")
  (Alpha "\\mathrm{A}")
  (Beta "\\mathrm{B}")
  (Epsilon "\\mathrm{E}")
  (Eta "\\mathrm{H}")
  (Iota "\\mathrm{I}")
  (Kappa "\\mathrm{K}")
  (Mu "\\mathrm{M}")
  (Nu "\\mathrm{N}")
  (Omicron "\\mathrm{O}")
  (Chi "\\mathrm{X}")
  (Rho "\\mathrm{P}")
  (Tau "\\mathrm{T}")
  (Zeta "\\mathrm{Z}")

  ;; symbols from mathabx
  ;; NOTE: we avoid using the mathabx package because it tends
  ;; to be badly installed and incompatible with certain styles
  (divides "\\mathrel{|}")
  (ndivides "\\mathrel{\\nmid}")
  (asterisk "\\mathord{*}")
  (dottimes "\\mathbin{\\dot{\\times}}")
  (precdot "\\mathrel{\\prec\\!\\!\\cdot")

  ;; negations
  (nin "\\not\\in")
  (nni "\\not\\ni")
  (notni "\\not\\ni")
  (nequiv (!annotate "\\mathrel{\\not\\equiv}" (equiv)))
  (nleadsto (!annotate "\\not\\leadsto" (leadsto)))
  (napproxeq (!annotate "\\mathrel{\\not\\approxeq}" (approxeq)))
  (nprecapprox (!annotate "\\mathrel{\\not\\precapprox}" (precapprox)))
  (npreccurlyeq (!annotate "\\mathrel{\\not\\preccurlyeq}" (preccurlyeq)))
  (npreceqq (!annotate "\\mathrel{\\not\\preceqq}" (preceqq)))
  (nprecsim (!annotate "\\mathrel{\\not\\precsim}" (precsim)))
  (nsimeq (!annotate "\\mathrel{\\not\\simeq}" (simeq)))
  (nsubset (!annotate "\\mathrel{\\not\\subset}" (subset)))
  (napprox (!annotate "\\mathrel{\\not\\approx}" (approx)))
  (nsqsubset (!annotate "\\mathrel{\\not\\sqsubset}" (sqsubset)))
  (nsqsubseteq (!annotate "\\mathrel{\\not\\sqsubseteq}" (sqsubseteq)))
  (nsqsubseteqq (!annotate "\\mathrel{\\not\\sqsubseteqq}" (sqsubseteqq)))
  (nsqsupset (!annotate "\\mathrel{\\not\\sqsupset}" (sqsupset)))
  (nsqsupseteq (!annotate "\\mathrel{\\not\\sqsupseteq}" (sqsupseteq)))
  (nsqsupseteqq (!annotate "\\mathrel{\\not\\sqsupseteqq}" (sqsupseteqq)))
  (nsuccapprox (!annotate "\\mathrel{\\not\\succapprox}" (succapprox)))
  (nsucccurlyeq (!annotate "\\mathrel{\\not\\succcurlyeq}" (succcurlyeq)))
  (nsucceqq (!annotate "\\mathrel{\\not\\succeqq}" (succeqq)))
  (nsuccsim (!annotate "\\mathrel{\\not\\succsim}" (succsim)))

  ;; other extra symbols
  (oempty "\\circ")
  (exterior "\\wedge")
  (Exists "\\exists")
  (bigintwl "\\int")
  (bigointwl "\\oint")
  (bigintlim "\\int")
  (bigointlim "\\oint")
  (of ":")
  (suchthat ":")
  (barsuchthat "|")
  (point ".")
  (cdummy "\\cdot")
  (comma "{,}")
  (copyright "\\copyright")
  (bignone "")
  (nobracket "")
  (nospace "")
  (nocomma "")
  (noplus "")
  (nosymbol "")
  (dotminus "\\mathaccent95{-}")
  (dotpm "\\mathaccent95{\\pm}")
  (dotmp "\\mathaccent95{\\mp}")
  (dotamalg "\\mathaccent95{\\amalg}")
  (dotoplus "\\mathaccent95{\\oplus}")
  (dototimes "\\mathaccent95{\\otimes}")
  (dotast "\\mathaccent95{*}")
  (into "\\rightarrow")
  (longminus "{-\\!\\!-}")
  (longequal "{=\\!\\!=}")
  (longhookrightarrow "{\\lhook\\joinrel\\relbar\\joinrel\\rightarrow}")
  (longhookleftarrow "{\\leftarrow\\joinrel\\relbar\\joinrel\\rhook}")
  (triangleup "\\triangle")
  (tmprecdot "{\\prec\\hspace{-0.6em}\\cdot}\\;\\,")
  (preceqdot "{\\preccurlyeq\\hspace{-0.6em}\\cdot}\\;\\,")
  (llangle "{\\langle\\!\\langle}")
  (rrangle "{\\rangle\\!\\rangle}")
  (join "\\Join")
  (um "-")
  (upl "+")
  (upm "\\pm")
  (ump "\\mp")
  (assign ":=")
  (plusassign "+\\!\\!=")
  (minusassign "-\\!\\!=")
  (timesassign "\times\\!\\!=")
  (overassign "/\\!\\!=")
  (backassign "=:")
  (pplus (mathbin "+\\!\\!\\!\\!+"))
  (lflux "\\ll")
  (gflux "\\gg")
  (colons "\\,:\\,")
  (transtype "\\,:\\!!>")
  (tmxspace (hspace "1em"))
  (lebar (mathrel (Yleft)))
  (gebar (mathrel (Yright)))
  (leangle (mathrel (angle)))
  (geangle (mathrel (!group (mbox (reflectbox (!math (angle)))))))
  (anglege (mathrel (!group (mbox (rotatebox (!option "origin=c") "180"
                                             (!math (angle)))))))
  (anglele (mathrel (!group (mbox (rotatebox (!option "origin=c") "180"
                                             (!math (!recurse (geangle))))))))
  ;;(leqangle (mathrel (substack (!append (angle) "\\\\" (smash "-")))))
  (leqangle (mathrel (!append (angle) " \\llap "
                              (!group (raisebox "-1ex" (!math "-"))))))
  (geqangle (mathrel (!group (mbox (reflectbox (!math (!recurse (leqangle))))))))
  (legeangle (mathrel (substack (!append (leangle) "\\\\" (!recurse (anglege))))))
  (geleangle (mathrel (substack (!append (geangle) "\\\\" (!recurse (anglele))))))
  (udots "{\\mathinner{\\mskip1mu\\raise1pt\\vbox{\\kern7pt\\hbox{.}}\\mskip2mu\\raise4pt\\hbox{.}\\mskip2mu\\raise7pt\\hbox{.}\\mskip1mu}}")
  (subsetsim (underset (sim) (subset)))
  (supsetsim (underset (sim) (supset)))
  (rightmap (!group (!append (shortmid) "\\!\\!\\!-")))
  (leftmap (!group (!append "-\\!\\!\\!" (shortmid))))
  (leftrightmap (!group (!append (shortmid) "\\!\\!\\!-\\!\\!\\!"
                                 (shortmid))))
  (LRleftrightarrow (!group (!append (Lleftarrow) "\\!\\!\\!" (Rrightarrow))))
  (Llongleftarrow (!group (!append (Lleftarrow) "\\!" (equiv))))
  (Llongrightarrow (!group (!append (equiv) "\\!" (Rrightarrow))))
  (Llongleftrightarrow (!group (!append (Lleftarrow) "\\!" (equiv)
                                        "\\!" (Rrightarrow))))
  (threeleftarrows
   (mathrel (substack (!append (leftarrow) "\\\\[-0.6ex]"
                               (leftarrow) "\\\\[-0.6ex]"
                               (leftarrow)))))
  (fourleftarrows
   (mathrel (substack (!append (leftarrow) "\\\\[-0.6ex]"
                               (leftarrow) "\\\\[-0.6ex]"
                               (leftarrow) "\\\\[-0.6ex]"
                               (leftarrow)))))
  (threerightarrows
   (mathrel (substack (!append (rightarrow) "\\\\[-0.6ex]"
                               (rightarrow) "\\\\[-0.6ex]"
                               (rightarrow)))))
  (fourrightarrows
   (mathrel (substack (!append (rightarrow) "\\\\[-0.6ex]"
                               (rightarrow) "\\\\[-0.6ex]"
                               (rightarrow) "\\\\[-0.6ex]"
                               (rightarrow)))))
  (longleftrightarrows
   (mathrel (substack (!append (longleftarrow) "\\\\[-0.6ex]"
                               (longrightarrow)))))
  (longleftleftarrows
   (mathrel (substack (!append (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow)))))
  (longthreeleftarrows
   (mathrel (substack (!append (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow)))))
  (longfourleftarrows
   (mathrel (substack (!append (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow) "\\\\[-0.6ex]"
                               (longleftarrow)))))
  (longrightleftarrows
   (mathrel (substack (!append (longrightarrow) "\\\\[-0.6ex]"
                               (longleftarrow)))))
  (longrightrightarrows
   (mathrel (substack (!append (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow)))))
  (longthreerightarrows
   (mathrel (substack (!append (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow)))))
  (longfourrightarrows
   (mathrel (substack (!append (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow) "\\\\[-0.6ex]"
                               (longrightarrow))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  ;; Nullary macros
  (tmunsc "\\_")
  (emdash "---")
  (tmat "\\symbol{\"40}")
  (tmbsl "\\ensuremath{\\backslash}")
  (tmdummy "$\\mbox{}$")
  (TeXmacs "T\\kern-.1667em\\lower.5ex\\hbox{E}\\kern-.125emX\\kern-.1em\\lower.5ex\\hbox{\\textsc{m\\kern-.05ema\\kern-.125emc\\kern-.05ems}}")
  (madebyTeXmacs (footnote (!recurse (withTeXmacstext))))
  (withTeXmacstext
    (!append (!translate "This document has been produced using the GNU") " "
             (!group (!recurse (TeXmacs))) " " (!translate "text editor") " ("
             (!translate "see") " "
             (url "https://www.texmacs.org") ")"))
  (citewebsite
    (!append (!translate "This document has been written using") " GNU "
             (!group (!recurse (TeXmacs))) "; " (!translate "see") " "
             (url "https://www.texmacs.org") "."))
  (tmmade (!recurse (tikzframe (Backsigma))))
  (scheme "{\\sc Scheme}")
  (tmsep  ", ")
  (tmSep  "; ")
  (pari "{\\sc Pari}")
  (textdots "...")
  (filldots "{\\dotfill\\hfill\\hbox{}}")
  (infixand (text " and "))
  (infixor (text " or "))
  (infixiff (text " iff "))

  ;; Unary macros
  (tmrsub (ensuremath (!append "_{" (textrm 1) "}")))
  (tmrsup (textsuperscript 1))
  (tmverbatim (text (!group (ttfamily) (!group 1))))
  (tmtextrm (text (!group (rmfamily) (!group 1))))
  (tmtextsf (text (!group (sffamily) (!group 1))))
  (tmtexttt (text (!group (ttfamily) (!group 1))))
  (tmtextmd (text (!group (mdseries) (!group 1))))
  (tmtextbf (text (!group (bfseries) (!group 1))))
  (tmtextup (text (!group (upshape) (!group 1))))
  (tmtextsl (text (!group (slshape) (!group 1))))
  (tmtextit (text (!group (itshape) (!group 1))))
  (tmtextsc (text (!group (scshape) (!group 1))))
  (tmmathbf (ensuremath (!recurse (boldsymbol 1))))
  (tmmathmd (ensuremath 1))
  (tmop (ensuremath (operatorname 1)))
  (tmstrong (textbf 1))
  (tmem (!group "\\em " 1 "\\/"))
  (tmtt (texttt 1))
  (tmdate (today))
  (tmname (textsc 1))
  (tmsamp (textsf 1))
  (tmabbr 1)
  (tmdfn (textbf 1))
  (tmkbd (texttt 1))
  (tmvar (texttt 1))
  (tmacronym (textsc 1))
  (tmperson (textsc 1))
  (tmscript (text (scriptsize (!math 1))))
  (tmdef 1)
  (dueto (textup (textbf (!append "(" 1 ") "))))
  (op 1)
  (todo (!group (!append (color "red!75!black") "[To do: " 1 "]")))
  (tmoutput 1)
  (tmerrput (!append (color "red!50!black") 1))
  (tmtiming (!append (hfill) (footnotesize) (color "black!50") 1 (par)))
  (tmsubtitle (thanks (!append (textit (!translate "Subtitle:")) " " 1)))
  (tmrunningtitle (!append (!translate "Running title:") " " 1))
  (tmrunningauthor (!append (!translate "Running author:") " " 1))
  (tmaffiliation (!append (!nextline) 1))
  (tmemail (!append (!nextline) (textit (!translate "Email:")) " " (texttt 1)))
  (tmhomepage (!append (!nextline) (textit (!translate "Web:")) " " (texttt 1)))
  (tmfnaffiliation (thanks (!append (textit (!translate "Affiliation:")) " " 1)))
  (tmfnemail (thanks (!append (textit (!translate "Email:")) " " (texttt 1))))
  (tmfnhomepage (thanks (!append (textit (!translate "Web:")) " " (texttt 1))))
  (tmacmhomepage (titlenote (!append (textit (!translate "Web:")) " " 1)))
  (tmacmmisc (titlenote (!append (textit (!translate "Misc:")) " " 1)))
  (tmieeeemail (!append (textit (!translate "Email:")) " " 1))
  (tmnote (thanks (!append (textit (!translate "Note:")) " " 1)))
  (tmmisc (thanks (!append (textit (!translate "Misc:")) " " 1)))
  (citetexmacs
    (!append (!translate "This document has been written using") " GNU "
             (!group (!recurse (TeXmacs))) " " (cite 1) "."))
  (key (!append
         (fcolorbox "black" "gray!25!white"
                    (raisebox "0pt" (!option "5pt") (!option "0pt") (texttt 1)))
         (hspace "0.5pt")))
  (uhat (underaccent (hat) 1))
  (uwidehat (underaccent (widehat (hphantom 1)) 1))
  (utilde (underaccent (tilde) 1))
  (uwidetilde (underaccent (widetilde (hphantom 1)) 1))
  (uvec (underaccent (vec) 1))
  (ubreve (underaccent (breve) 1))
  (uinvbreve (underaccent (invbreve) 1))
  (ucheck (underaccent (check) 1))
  (uring (underaccent (ring) 1))
  (uacute (underaccent (acute) 1))
  (ugrave (underaccent (grave) 1))
  (underdot (underaccent (dot) 1))
  (uddot (underaccent (ddot) 1))
  (udddot (underaccent (dddot (hphantom 1)) 1))
  (uddddot (underaccent (ddddot (hphantom 1)) 1))
  (widespacing 1)
  (gb  (!append (texttt "[\\!\\![") 1 (texttt "]\\!\\!]")))
  (gbt (!append (texttt "[\\!\\![\\!\\![") 1 (texttt "]\\!\\!]\\!\\!]")))

  ;; With options
  (tmcodeinline ((!option "") (!group (ttfamily) (!group 2))))

  ;; Binary macros
  (tmcolor (!group (color 1) (!group 2)))
  (tmsummarizeddocumentation
   (trivlist (!append (item (!option "")) (mbox "") "\\large\\bf" 1)))
  (tmsummarizedgrouped (trivlist (!append (item (!option "[")) (mbox "") 1)))
  (tmsummarizedexplain
   (trivlist (!append (item (!option "")) (mbox "") "\\bf" 1)))
  (tmsummarizedplain (trivlist (!append (item (!option "")) (mbox "") 1)))
  (tmsummarizedtiny (trivlist (!append (item (!option "")) (mbox "") 1)))
  (tmsummarizedraw (trivlist (!append (item (!option "")) (mbox "") 1)))
  (tmsummarizedenv
   (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmsummarizedstd
   (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmsummarized
   (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))

  (tmdetaileddocumentation
   (trivlist (!append (item (!option "")) (mbox "") "\\large\\bf" 2)))
  (tmdetailedgrouped (trivlist (!append (item (!option "[")) (mbox "") 2)))
  (tmdetailedexplain
   (trivlist (!append (item (!option "")) (mbox "") "\\bf" 2)))
  (tmdetailedplain (trivlist (!append (item (!option "")) (mbox "") 2)))
  (tmdetailedtiny (trivlist (!append (item (!option "")) (mbox "") 2)))
  (tmdetailedraw (trivlist (!append (item (!option "")) (mbox "") 2)))
  (tmdetailedenv (trivlist (!append (item (!option "$\\circ$")) (mbox "") 2)))
  (tmdetailedstd (trivlist (!append (item (!option "$\\circ$")) (mbox "") 2)))
  (tmdetailed (trivlist (!append (item (!option "$\\circ$")) (mbox "") 2)))

  (tmfoldeddocumentation
   (trivlist (!append (item (!option "")) (mbox "") "\\large\\bf" 1)))
  (tmunfoldeddocumentation
   (trivlist (!append (item (!option "")) (mbox "")
                      (!group "\\large\\bf" 1) "\\\\"
                      (item (!option "")) (mbox "") 2)))
  (tmfoldedsubsession
   (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmunfoldedsubsession
   (trivlist (!append (item (!option "$\\circ$"))   (mbox "") 1 "\\\\"
                      (item (!option "")) (mbox "") 2 )))
  (tmfoldedgrouped
   (trivlist (!append (item (!option "["))  (mbox "") 1)))
  (tmunfoldedgrouped
   (trivlist (!append (item (!option "$\\lceil$"))  (mbox "") 1 "\\\\"
                      (item (!option "$\\lfloor$")) (mbox "") 2 )))
  (tmfoldedexplain (trivlist (!append (item (!option "")) "\\bf" 1)))
  (tmunfoldedexplain
   (trivlist (!append (item (!option "")) (mbox "")
                      (!group "\\bf" 1) "\\\\"
                      (item (!option "")) (mbox "") 2 )))
  (tmfoldedplain (trivlist (!append (item (!option "")) (mbox "") 1)))
  (tmunfoldedplain
   (trivlist (!append (item (!option "")) (mbox "") 1 "\\\\"
                      (item (!option "")) (mbox "") 2 )))
  (tmfoldedenv (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmunfoldedenv
   (trivlist (!append (item (!option "$\\circ$")) (mbox "") 1 "\\\\"
                      (item (!option "")) (mbox "") 2 )))
  (tmfoldedstd (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmunfoldedstd
   (trivlist (!append (item (!option "$\\circ$")) (mbox "") 1 "\\\\"
                      (item (!option "")) (mbox "") 2 )))
  (tmfolded (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1)))
  (tmunfolded (trivlist (!append (item (!option "$\\circ$")) (mbox "") 1 "\\\\"
                                 (item (!option "")) (mbox "") 2 )))
  (tminput
   (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                      (!group (!append (color "blue!50!black") (mbox "") 2)))))
  (tminputmath
   (trivlist (!append (item (!option 1)) (ensuremath 2))))
  (tmhlink  (!group (!append (color "blue") 1)))
  (tmaction (!group (!append (color "blue") 1)))
  (ontop (genfrac "" "" "0pt" "" 1 2))
  (subindex (index (!append 1 "!" 2)))
  (renderfootnote (footnotetext (!append (tmrsup 1) " " 2)))
  (renderfootnotestar (footnotetext (!append (tmrsup 1) " " 3)))
  (tmlinenumber (!append (custombinding 1)
                         (tmlinenote (footnotesize 1) 2 "0cm")))

  ;; Ternary macros
  (tmsession (!group (!append (tt) 3)))
  (tmfoldediomath
   (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                      (!group (!append (color "blue!50!black") (ensuremath 2))))))
  (tmunfoldediomath
   (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                      (!group (!append (color "blue!50!black") (ensuremath 2)))
                      (item (!option "")) (mbox "") 3)))
  (tmfoldedio
   (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                      (mbox "") (!group (!append (color "blue!50!black") 2)))))
  (tmunfoldedio
   (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                      (mbox "") (!group (!append (color "blue!50!black") 2))
                      (item (!option "")) (mbox "") 3)))
  (tmlinenote
   (!append (tmdummy)
            (marginpar (adjustbox
                        (!append "right=0cm, lap=" 2
                                 "-\\textwidth-\\marginparsep, raise=" 3)
                        1))))
  (subsubindex (index (!append 1 "!" 2 "!" 3)))
  (tmref 1)
  (glossaryentry (!append (item (!option (!append 1 (hfill)))) 2 (dotfill) 3))

  ;; Tetrary macros
  (tmscriptinput (fbox (!append (fbox (!append (sf) 2)) " "
                                (!append (tt) 3))))
  (tmscriptoutput (!append 4))
  (tmconverterinput (fbox (!append (fbox (!append (sf) 2)) " "
                                   (!append (tt) 3))))
  (tmconverteroutput (!append 4))
  (subsubsubindex (index (!append 1 "!" 2 "!" 3 "!" 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated extra macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (labeleqnum "\\addtocounter{equation}{-1}\\refstepcounter{equation}\\addtocounter{equation}{1})")
  (eqnumber (!append "\\hfill(\\theequation" (!recurse (labeleqnum)) ")"))
  (leqnumber (!append "(\\theequation" (!recurse (labeleqnum)) ")\\hfill"))
  (reqnumber (!append "\\hfill(\\theequation" (!recurse (labeleqnum)) ")"))
  (skey (!recurse (key (!append "shift-" 1))))
  (ckey (!recurse (key (!append "ctrl-" 1))))
  (akey (!recurse (key (!append "alt-" 1))))
  (mkey (!recurse (key (!append "meta-" 1))))
  (hkey (!recurse (key (!append "hyper-" 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra TeXmacs environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-environment
  ("proof"
   (!append (noindent) (textbf (!append (!translate "Proof") "\\ "))
            ---
            (hspace* (fill)) (!math (Box)) (medskip)))
  ("proof*"
   (!append (noindent) (textbf (!append 1 "\\ "))
            ---
            (hspace* (fill)) (!math (Box)) (medskip)))
  ("leftaligned"
   ((!begin "flushleft") ---))
  ("rightaligned"
   ((!begin "flushright") ---))
  ("quoteenv"
   ((!begin "quote") ---))
  ("tmcode"
   ((!option "")
    ((!begin "alltt") ---)))
  ("tmparmod"
   ((!begin "list" "" (!append "\\setlength{\\topsep}{0pt}"
                               "\\setlength{\\leftmargin}{" 1 "}"
                               "\\setlength{\\rightmargin}{" 2 "}"
                               "\\setlength{\\parindent}{" 3 "}"
                               "\\setlength{\\listparindent}{\\parindent}"
                               "\\setlength{\\itemindent}{\\parindent}"
                               "\\setlength{\\parsep}{\\parskip}"))
    (!append "\\item[]"
             ---)))
  ("tmparsep"
   (!append (begingroup) "\\setlength{\\parskip}{" 1 "}"
            ---
            (endgroup)))
  ("tmcompact"
   ((!begin "tmparsep" "0em") ---))
  ("tmcompressed"
   ((!begin "tmparsep" "0.25em") ---))
  ("tmamplified"
   ((!begin "tmparsep" "0.75em") ---))
  ("tmjumpin"
   ((!begin "tmparmod" "1.5em" "0pt" "-1.5em") ---))
  ("tmindent"
   ((!begin "tmparmod" "1.5em" "0pt" "0pt") ---))
  ("tmlisting"
   ((!begin "linenumbers") (!append (resetlinenumber) ---)))
  ("elsequation" ((!begin "eqnarray") (!append --- "&&")))
  ("elsequation*" ((!begin "eqnarray*") (!append --- "&&")))
  ("theglossary"
   ((!begin "list" "" (!append "\\setlength{\\labelwidth}{6.5em}"
                               "\\setlength{\\leftmargin}{7em}"
                               "\\small")) ---)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs list environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (latex-texmacs-itemize env lab)
  `(smart-table latex-texmacs-environment
     (,env
      ((!begin "itemize")
       (!append "\\renewcommand{\\labelitemi}{" ,lab "}"
                "\\renewcommand{\\labelitemii}{" ,lab "}"
                "\\renewcommand{\\labelitemiii}{" ,lab "}"
                "\\renewcommand{\\labelitemiv}{" ,lab "}"
                ---)))))

(define-macro (latex-texmacs-enumerate env lab)
  `(smart-table latex-texmacs-environment
     (,env ((!begin "enumerate" (!option ,lab)) ---))))

(define-macro (latex-texmacs-description env)
  `(smart-table latex-texmacs-environment
     (,env ((!begin "description") ---))))

(latex-texmacs-itemize "itemizeminus" "$-$")
(latex-texmacs-itemize "itemizedot" "$\\bullet$")
(latex-texmacs-itemize "itemizearrow" "$\\rightarrow$")
(latex-texmacs-enumerate "enumeratenumeric" "1.")
(latex-texmacs-enumerate "enumerateroman" "i.")
(latex-texmacs-enumerate "enumerateromancap" "I.")
(latex-texmacs-enumerate "enumeratealpha" "a{\\textup{)}}")
(latex-texmacs-enumerate "enumeratealphacap" "A.")
(latex-texmacs-description "descriptioncompact")
(latex-texmacs-description "descriptionaligned")
(latex-texmacs-description "descriptiondash")
(latex-texmacs-description "descriptionlong")
(latex-texmacs-description "descriptionparagraphs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra preamble definitions which are needed to export certain macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-preamble
  (newmdenv
   (!append (mdfsetup (!append "linecolor=black,linewidth=0.5pt,"
                               "skipabove=0.5em,skipbelow=0.5em,"
                               "hidealllines=true,innerleftmargin=0pt,"
                               "innerrightmargin=0pt,innertopmargin=0pt,"
                               "innerbottommargin=0pt" )) "\n"))
  (tikzframe
   (!append
    (!ignore (tikz))
    "\\newcommand{\\tikzframe}[1]{%\n"
    "  \\tikz[baseline=(X.base)]\n"
    "  \\node[draw=black,semithick,rectangle,inner sep=2pt,rounded corners=2pt]\n"
    "  (X) {#1};}\n"))
  (nonconverted
   (!append "\\newcommand{\\nonconverted}[1]{\\mbox{}}\n"))
  (tmkeywords
   (!append (newcommand (tmkeywords)
                        (!append (textbf (!translate "Keywords:")) " "))
            "\n"))
  (tmacm
   (!append (newcommand (tmacm)
                        (!append
                         (textbf
                          (!translate "A.C.M. subject classification:")) " "))
            "\n"))
  (tmarxiv
   (!append (newcommand (tmarxiv)
                        (!append
                         (textbf
                          (!translate "arXiv subject classification:")) " "))
            "\n"))
  (tmpacs
   (!append (newcommand (tmpacs)
                        (!append
                         (textbf
                          (!translate "P.A.C.S. subject classification:"))
                         " "))
            "\n"))
  (tmmsc
   (!append (newcommand (tmmsc)
                        (!append
                         (textbf
                          (!translate "A.M.S. subject classification:")) " "))
            "\n"))
  (fmtext (!append "\\newcommand{\\fmtext}[2][]{\\fntext[#1]{"
                   (!translate "Misc:") " #2}}\n"))
  (tdatetext (!append "\\newcommand{\\tdatetext}[2][]{\\tnotetext[#1]{"
                      (!translate "Date:") " #2}}\n"))
  (tmisctext (!append "\\newcommand{\\tmisctext}[2][]{\\tnotetext[#1]{"
                      (!translate "Misc:") " #2}}\n"))
  (tsubtitletext (!append "\\newcommand{\\tsubtitletext}[2][]{\\tnotetext[#1]{"
                          (!translate "Subtitle:") " #2}}\n"))
  (thankshomepage (!append "\\newcommand{\\thankshomepage}[2][]{\\thanks[#1]{"
                           (!translate "URL:") " #2}}\n"))
  (thanksemail (!append "\\newcommand{\\thanksemail}[2][]{\\thanks[#1]{"
                        (!translate "Email:") " #2}}\n"))
  (thanksdate (!append "\\newcommand{\\thanksdate}[2][]{\\thanks[#1]{"
                       (!translate "Date:") " #2}}\n"))
  (thanksamisc (!append "\\newcommand{\\thanksamisc}[2][]{\\thanks[#1]{"
                        (!translate "Misc:") " #2}}\n"))
  (thanksmisc (!append "\\newcommand{\\thanksmisc}[2][]{\\thanks[#1]{"
                       (!translate "Misc:") " #2}}\n"))
  (thankssubtitle (!append "\\newcommand{\\thankssubtitle}[2][]{\\thanks[#1]{"
                           (!translate "Subtitle:") " #2}}\n"))
  (qed (!append (providecommand "\\qed" (ensuremath (Box))) "\n"))
  (mho
   (!append
    "\\renewcommand{\\mho}{\\mbox{\\rotatebox[origin=c]{180}{$\\omega$}}}"))
  (invbreve
   (!append
    "\\usepackage[T3,T1]{fontenc}\n"
    "\\DeclareSymbolFont{tipa}{T3}{cmr}{m}{n}\n"
    "\\DeclareMathAccent{\\invbreve}{\\mathalpha}{tipa}{16}\n"))
  (custombinding
   (!append
    "\\newcounter{tmcounter}\n"
    "\\newcommand{\\custombinding}[1]{%\n"
    "  \\setcounter{tmcounter}{#1}%\n"
    "  \\addtocounter{tmcounter}{-1}%\n"
    "  \\refstepcounter{tmcounter}}\n"))
  (tmfloat
   (!append
    (!ignore (ifthenelse) (captionof) (widthof))
    "\\newcommand{\\tmfloatcontents}{}\n"
    "\\newlength{\\tmfloatwidth}\n"
    "\\newcommand{\\tmfloat}[5]{\n"
    "  \\renewcommand{\\tmfloatcontents}{#4}\n"
    "  \\setlength{\\tmfloatwidth}{\\widthof{\\tmfloatcontents}+1in}\n"
    "  \\ifthenelse{\\equal{#2}{small}}\n"
    ;; FIXME: the length test frequently produces an error:
    ;; '! Missing = inserted for \ifdim'.
    ;; I (Joris) did not manage to understand this LaTeX mess.
    ;;"    {\\ifthenelse{\\lengthtest{\\tmfloatwidth > \\linewidth}}\n"
    ;;"      {\\setlength{\\tmfloatwidth}{\\linewidth}}{}}\n"
    "    {\\setlength{\\tmfloatwidth}{0.45\\linewidth}}\n"
    "    {\\setlength{\\tmfloatwidth}{\\linewidth}}\n"
    "  \\begin{minipage}[#1]{\\tmfloatwidth}\n"
    "    \\begin{center}\n"
    "      \\tmfloatcontents\n"
    "      \\captionof{#3}{#5}\n"
    "    \\end{center}\n"
    "  \\end{minipage}}\n"))
  (addtocountergroup (!append "\\newcommand{\\addtocountergroup}[2]{}\n"))
  (groupcommoncounter (!append "\\newcommand{\\groupcommoncounter}[1]{}\n")))

;;(define-macro (latex-texmacs-long prim x l m r)
;;  `(smart-table latex-texmacs-preamble
;;     (,(string->symbol (substring prim 1 (string-length prim)))
;;      (!append
;;       "\\def" ,prim "fill@{\\arrowfill@" ,l ,m ,r "}\n"
;;       "\\providecommand{" ,prim "}[2][]{"
;;       "\\ext@arrow 0099" ,prim "fill@{#1}{#2}}\n"))))

(define-macro (latex-texmacs-long prim x l m r)
  `(smart-table latex-texmacs-preamble
     (,(string->symbol (substring prim 1 (string-length prim)))
      (!append
       "\\providecommand{" ,prim "}[2][]{"
       "\\mathop{" ,x "}\\limits_{#1}^{#2}}\n"))))

(latex-texmacs-long "\\xminus" "-"
                    "\\DOTSB\\relbar" "\\relbar" "\\DOTSB\\relbar")
(latex-texmacs-long "\\xleftrightarrow" "\\longleftrightarrow"
                    "\\leftarrow" "\\relbar" "\\rightarrow")
(latex-texmacs-long "\\xmapsto" "\\longmapsto"
                    "\\vdash" "\\relbar" "\\rightarrow")
(latex-texmacs-long "\\xmapsfrom" "\\leftarrow\\!\\!\\dashv"
                    "\\leftarrow" "\\relbar" "\\dashv")
(latex-texmacs-long "\\xequal" "="
                    "\\DOTSB\\Relbar" "\\Relbar" "\\DOTSB\\Relbar")
(latex-texmacs-long "\\xLeftarrow" "\\Longleftarrow"
                    "\\Leftarrow" "\\Relbar" "\\Relbar")
(latex-texmacs-long "\\xRightarrow" "\\Longrightarrow"
                    "\\Relbar" "\\Relbar" "\\Rightarrow")
(latex-texmacs-long "\\xLeftrightarrow" "\\Longleftrightarrow"
                    "\\Leftarrow" "\\Relbar" "\\Rightarrow")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plain style theorems
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (latex-texmacs-thmenv prim name before after . opt-mode)
  (let* ((head (if (null? opt-mode) (list) (list `(:mode ,(car opt-mode)))))
         (prim* (string-append prim "*"))
         (nonum (string-append "nn" prim))
         (thenonum (string-append "\\the" nonum)))
    `(smart-table latex-texmacs-env-preamble
       ,@head
       (,prim  (!append ,@before
                        (newtheorem ,prim (!translate ,name))
                        ,@after "\n"))
       (,prim* (!append (newcounter ,nonum) "\n"
                        "\\def" ,thenonum "{\\unskip}\n"
                        ,@before
                        (newtheorem ,prim* (!option ,nonum) (!translate ,name))
                        ,@after "\n")))))

(define-macro (latex-texmacs-theorem prim name)
  `(latex-texmacs-thmenv ,prim ,name () ()))

(define-macro (latex-texmacs-remark prim name)
  `(latex-texmacs-thmenv
    ,prim ,name ("{" (!recurse (theorembodyfont "\\rmfamily"))) ("}")))

(define-macro (latex-texmacs-exercise prim name)
  `(latex-texmacs-thmenv
    ,prim ,name ("{" (!recurse (theorembodyfont "\\rmfamily\\small"))) ("}")))

(latex-texmacs-theorem "theorem" "Theorem")
(latex-texmacs-theorem "proposition" "Proposition")
(latex-texmacs-theorem "lemma" "Lemma")
(latex-texmacs-theorem "corollary" "Corollary")
(latex-texmacs-theorem "axiom" "Axiom")
(latex-texmacs-theorem "definition" "Definition")
(latex-texmacs-theorem "notation" "Notation")
(latex-texmacs-theorem "conjecture" "Conjecture")
(latex-texmacs-remark "remark" "Remark")
(latex-texmacs-remark "note" "Note")
(latex-texmacs-remark "example" "Example")
(latex-texmacs-remark "convention" "Convention")
(latex-texmacs-remark "warning" "Warning")
(latex-texmacs-remark "acknowledgments" "Acknowledgments")
(latex-texmacs-remark "answer" "Answer")
(latex-texmacs-remark "question" "Question")
(latex-texmacs-exercise "exercise" "Exercise")
(latex-texmacs-exercise "problem" "Problem")
(latex-texmacs-exercise "solution" "Solution")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ornamented environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-env-preamble
  ("tmpadded" (!append (newmdenv (!option "") "tmpadded") "\n"))
  ("tmoverlined"
   (!append (newmdenv (!option "topline=true,innertopmargin=1ex")
                      "tmoverlined") "\n"))
  ("tmunderlined"
   (!append (newmdenv (!option "bottomline=true,innerbottommargin=1ex")
                      "tmunderlined") "\n"))
  ("tmbothlined"
   (!append (newmdenv (!option "topline=true,bottomline=true,innertopmargin=1ex,innerbottommargin=1ex")
                      "tmbothlined") "\n"))
  ("tmframed"
   (!append (newmdenv (!option "hidealllines=false,innertopmargin=1ex,innerbottommargin=1ex,innerleftmargin=1ex,innerrightmargin=1ex")
                      "tmframed") "\n"))
  ("tmornamented"
   (!append (newmdenv (!option "hidealllines=false,innertopmargin=1ex,innerbottommargin=1ex,innerleftmargin=1ex,innerrightmargin=1ex")
                      "tmornamented") "\n")))
