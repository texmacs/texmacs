
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-coq.scm
;; DESCRIPTION : Initialize Coq plugin
;; COPYRIGHT   : (C) 2011  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure coq
  (:require (url-exists-in-path? "coqtop"))
  (:launch "tm_coq")
  (:session "Coq"))

(when (url-exists-in-path? "coqtop")
  (lazy-format (convert coq init-coqml) vernac))

(tm-cond-expand (supports-coq?)
  (texmacs-modes
    (in-coq-style% (style-has? "coq-style")))

  (lazy-keyboard (coq-kbd) in-coq-style?))
