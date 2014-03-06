
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

(logic-table latex-texmacs-macro%
  ;; arrows with limits
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

  ;; asymptotic relations by Joris
  (nasymp "\\not\\asymp")
  (asympasymp "{\\asymp\\!\\!\\!\\!\\!\\!-}")
  (nasympasymp "{\\not\\asymp\\!\\!\\!\\!\\!\\!-}")
  (simsim "{\\approx\\!\\!\\!\\!\\!\\!-}")
  (nsimsim "{\\not\\approx\\!\\!\\!\\!\\!\\!-}")
  (npreccurlyeq "\\not\\preccurlyeq")
  (precprec "\\prec\\!\\!\\!\\prec")
  (precpreceq "\\preceq\\!\\!\\!\\preceq")
  (precprecprec "\\prec\\!\\!\\!\\prec\\!\\!\\!\\prec")
  (precprecpreceq "\\preceq\\!\\!\\!\\preceq\\!\\!\\!\\preceq")
  (succsucc "\\succ\\!\\!\\!\\succ")
  (succsucceq "\\succeq\\!\\!\\!\\succeq")
  (succsuccsucc "\\succ\\!\\!\\!\\succ\\!\\!\\!\\succ")
  (succsuccsucceq "\\succeq\\!\\!\\!\\succeq\\!\\!\\!\\succeq")
  (lleq "\\leq\\negmedspace\\negmedspace\\leq")
  (llleq "\\leq\\negmedspace\\negmedspace\\leq\\negmedspace\\negmedspace\\leq")
  (ggeq "\\geq\\negmedspace\\negmedspace\\geq")
  (gggeq "\\geq\\negmedspace\\negmedspace\\geq\\negmedspace\\negmedspace\\geq")

  ;; extra literal symbols
  (btimes "{\\mbox{\\rotatebox[origin=c]{90}{$\\ltimes$}}}")
  (Backepsilon "{\\mbox{\\rotatebox[origin=c]{180}{E}}}")
  (Mho "{\\mbox{\\rotatebox[origin=c]{180}{$\\Omega$}}}")
  (mapmulti "{\\mbox{\\rotatebox[origin=c]{180}{$\\multimap$}}}")
  (mathcatalan "C")
  (mathd "\\mathrm{d}")
  (mathD "\\mathrm{D}")
  (mathe "\\mathrm{e}")
  (matheuler "\\gamma")
  (mathlambda "\\lambda")
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

  ;; other extra symbols
  (exterior "\\wedge")
  (Exists "\\exists")
  (bigintwl "\\int")
  (bigointwl "\\oint")
  (asterisk "*")
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
  (nin "\\not\\in")
  (nni "\\not\\ni")
  (notni "\\not\\ni")
  (nequiv "\\not\\equiv")
  (nleadsto "\\not\\leadsto")
  (dotamalg "\\mathaccent95{\\amalg}")
  (dottimes "\\mathaccent95{\\times}")
  (dotoplus "\\mathaccent95{\\oplus}")
  (dototimes "\\mathaccent95{\\otimes}")
  (into "\\rightarrow")
  (longequal "{=\\!\\!=}")
  (longhookrightarrow "{\\lhook\\joinrel\\relbar\\joinrel\\rightarrow}")
  (longhookleftarrow "{\\leftarrow\\joinrel\\relbar\\joinrel\\rhook}")
  (longdownarrow "\\downarrow")
  (longuparrow "\\uparrow")
  (triangleup "\\triangle")
  (precdot "{\\prec\\hspace{-0.6em}\\cdot}\\;\\,")
  (preceqdot "{\\preccurlyeq\\hspace{-0.6em}\\cdot}\\;\\,")
  (llangle "{\\langle\\!\\langle}")
  (rrangle "{\\rangle\\!\\rangle}")
  (join "\\Join")
  (um "-")
  (upl "+")
  (upm "\\pm")
  (ump "\\mp")
  (upequal "{\\mbox{\\rotatebox[origin=c]{90}{$=$}}}")
  (assign ":=")
  (plusassign "+\\!\\!=")
  (minusassign "-\\!\\!=")
  (timesassign "\times\\!\\!=")
  (overassign "/\\!\\!=")
  (lflux "\\ll")
  (gflux "\\gg")
  (colons "\\,:\\,")
  (transtype "\\,:\\!!>")
  (udots "{\\mathinner{\\mskip1mu\\raise1pt\\vbox{\\kern7pt\\hbox{.}}\\mskip2mu\\raise4pt\\hbox{.}\\mskip2mu\\raise7pt\\hbox{.}\\mskip1mu}}"))
