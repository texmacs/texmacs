
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : secure-secure.scm
;; DESCRIPTION : Secure routines for 'secure' plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (secure-secure)
  (:use (texmacs plugin plugin-cmd))
  (:export latexer))

(tm-define (latexer s)
  (:type (-> tree object))
  (:synopsis "convert LaTeX string to TeXmacs tree using plugin")
  (:secure #t)
  (plugin-eval "secure" "default" (tree->string s)))
