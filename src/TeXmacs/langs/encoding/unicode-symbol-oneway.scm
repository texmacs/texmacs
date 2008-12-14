;; One-way conversion of some Unicode characters to TeXmacs symbols.

;; (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.

;; Those symbols have a sensible unicode translation, so they can be exported.
;; But another symbol was chosen for the importation of the corresponding
;; unicode character. So the exportation is one-way only.

;; NOTE: This file must be reversed when loaded by the translator.
;;       The order (symbol unicode) is used for consistence with other tables.

;;; Angle brackets

;; These characters are canonically equivalent to CJK punctuations, but MathML
;; retained their mathematic use for backwards compatibility (though recent
;; revisions of Unicode explicitely discourage this).

("<langle>"	"#2329")
("<rangle>"	"#232A")

;;; Ambiguous characters

;; Omega and MathML both agree that <barwedge> should be exported as #2305
;; (PROJECTIVE), but this symbol must also be used to display NAND (until a
;; nand symbol is defined, at least).

("<barwedge>"	"#22BC") ; NAND
