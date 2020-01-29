
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pari.scm
;; DESCRIPTION : Initialize pari plugin
;; COPYRIGHT   : (C) 1999-2020  Joris van der Hoeven, Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cas-supports-completions-set! must-be-pari) (noop)) ;; obsolete

(define (pari-commander s)
  (string-append (char->string #\002)
		 "special:" s
		 (char->string #\005) "\n"))

(plugin-configure pari
  (:macpath "PariGP*" "Contents/Resources/bin")
  (:winpath "PARI" ".")
  (:require (url-exists-in-path? "gp"))
  (:launch "gp --texmacs")
  (:session "Pari")
  (:scripts "Pari")
  (:tab-completion #t)
  (:commander ,pari-commander))

(when (supports-pari?)
  (import-from (pari-menus))
  (plugin-input-converters pari))
