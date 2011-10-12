
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

(texmacs-modes
  (in-coq-style% (style-has? "coq-style")))

(lazy-keyboard (coq-kbd) in-coq-style?)

(when (url-exists-in-path? "coq_to_texmacs")
  (define-format coq-vernacular
    (:name "Coq vernacular")
    (:suffix "v"))

  (converter coq-vernacular-file stm-file
    (:shell "coq_to_texmacs" from to)))

(plugin-configure coq
  (:require (url-exists-in-path? "coq_texmacs"))
  (:launch "coq_texmacs"))
