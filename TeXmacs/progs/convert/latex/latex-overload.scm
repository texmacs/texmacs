
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : latex-overload.scm
;; DESCRIPTION : LaTeX re-definitions for specific styles/packages
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert latex latex-overload)
  (:use (convert latex latex-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Letter and article styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:require (latex-has-style? "letter"))
  (appendix ""))

(define-macro (latex-texmacs-section name inside style)
  `(smart-table latex-texmacs-macro
     (:require (latex-has-style? ,style))
     (,name (!append (medskip) (bigskip) "\n\n" (noindent) (textbf ,inside)))))

(define-macro (latex-texmacs-paragraph name inside style)
  `(smart-table latex-texmacs-macro
     (:require (latex-has-style? ,style))
     (,name (!append (smallskip) "\n\n" (noindent) (textbf ,inside)))))

(latex-texmacs-section chapter (!append "\\huge " 1) "article")
(latex-texmacs-section chapter (!append "\\huge " 1) "letter")
(latex-texmacs-section section (!append "\\LARGE " 1) "letter")
(latex-texmacs-section subsection (!append "\\Large " 1) "letter")
(latex-texmacs-section subsubsection (!append "\\large " 1) "letter")
(latex-texmacs-paragraph paragraph 1 "letter")
(latex-texmacs-paragraph subparagraph 1 "letter")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; amsthm package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:require (latex-depends? "amsthm"))
  (qed #f))

(smart-table latex-texmacs-environment
  (:require (or (latex-depends? "amsthm")
                (latex-has-texmacs-style? "amsart")))
  ("proof" #f))

(define-macro (ams-texmacs-theorem abbr full)
  `(smart-table latex-texmacs-env-preamble
     (:require (latex-depends? "amsthm"))
     (,abbr (!append "\\theoremstyle{plain}\n"
                     (newtheorem ,abbr (!translate ,full)) "\n"))))

(define-macro (ams-texmacs-remark abbr full)
  `(smart-table latex-texmacs-env-preamble
     (:require (latex-depends? "amsthm"))
     (,abbr (!append "\\theoremstyle{remark}\n"
                     (newtheorem ,abbr (!translate ,full)) "\n"))))

(define-macro (ams-texmacs-exercise abbr full)
  `(smart-table latex-texmacs-env-preamble
     (:require (latex-depends? "amsthm"))
     (,abbr (!append "\\newtheoremstyle{indent-exercise}{3pt}{3pt}"
                     "{\\small}{\\parindent}{\\bf\\small}{.}{.5em}{}{}\n"
                     "\\theoremstyle{indent-exercise}\n"
                     (newtheorem ,abbr (!translate ,full)) "\n"))))

(ams-texmacs-theorem "theorem" "Theorem")
(ams-texmacs-theorem "proposition" "Proposition")
(ams-texmacs-theorem "lemma" "Lemma")
(ams-texmacs-theorem "corollary" "Corollary")
(ams-texmacs-theorem "axiom" "Axiom")
(ams-texmacs-theorem "definition" "Definition")
(ams-texmacs-theorem "notation" "Notation")
(ams-texmacs-theorem "conjecture" "Conjecture")
(ams-texmacs-remark "remark" "Remark")
(ams-texmacs-remark "note" "Note")
(ams-texmacs-remark "example" "Example")
(ams-texmacs-remark "convention" "Convention")
(ams-texmacs-remark "acknowledgments" "Acknowledgments")
(ams-texmacs-remark "warning" "Warning")
(ams-texmacs-remark "answer" "Answer")
(ams-texmacs-remark "question" "Question")
(ams-texmacs-exercise "exercise" "Exercise")
(ams-texmacs-exercise "problem" "Problem")
(ams-texmacs-exercise "solution" "Solution")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Framed session package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(smart-table latex-texmacs-macro
  (:require (latex-has-texmacs-package? "framed-session"))
  (tmerrput ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={red!15},linecolor={red!50!black}"))
             (!append (color "red!50!black") 1)))
  (tmfoldedsubsession
   ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={rgb:white,10;red,9;green,4;yellow,2},linecolor={black!50}"))
    (trivlist (!append (item (!option "$\\bullet$")) (mbox "") 1))))
  (tmunfoldedsubsession
   (!append
    ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={rgb:white,10;red,9;green,4;yellow,2},linecolor={black!50}"))
     (trivlist (!append (item (!option "$\\circ$")) (mbox "") 1)))
    ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={rgb:white,50;red,9;green,4;yellow,2},linecolor={black!50}"))
     (trivlist (!append (item (!option "")) (mbox "") 2 )))))
  (tminput
   ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
    (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                       (!group (!append (color "blue!50!black") (mbox "") 2))))))
  (tminputmath
   ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
    (trivlist (!append (item (!option 1)) (mbox "") (ensuremath 2)))))
  

  (tmfoldediomath
   ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
    (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                       (mbox "") (!group (!append (color "blue!50!black") (ensuremath 2)))))))
  (tmunfoldediomath
   (!append ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
             (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                                (mbox "") (!group (!append (color "blue!50!black") (ensuremath 2))))))
            ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor=white,linewidth=0pt"))
             (trivlist (!append (item (!option "")) (mbox "") 3)))))
  (tmfoldedio
   ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
    (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                       (mbox "") (!group (!append (color "blue!50!black") 2))))))
  (tmunfoldedio
   (!append ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor={yellow!15},linecolor={black!15}"))
             (trivlist (!append (item (!option (!append (color "rgb:black,10;red,9;green,4;yellow,2") 1)))
                                (mbox "") (!group (!append (color "blue!50!black") 2)))))
            ((!begin "tmframed" (!option "skipabove=0,skipbelow=0,backgroundcolor=white,linewidth=0pt"))
             (trivlist (!append (item (!option "")) (mbox "") 3))))))
