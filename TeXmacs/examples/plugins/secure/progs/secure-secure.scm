
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : secure-secure.scm
;; DESCRIPTION : Secure routines for 'secure' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (secure-secure)
  (:use (utils plugins plugin-eval)))

(tm-define (latexer s)
  (:type (-> tree object))
  (:synopsis "convert LaTeX string to TeXmacs tree using plugin")
  (:secure #t)
  (plugin-eval "secure" "default" (tree->string s)))
