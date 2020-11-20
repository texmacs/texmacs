
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mmshell.scm
;; DESCRIPTION : Initialize mmshell plugin
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mmshell-serialize lan t)
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
	(string-append (escape-verbatim w) "\n")))))

(plugin-configure mmshell
  (:winpath "mmshell*" "bin")
  (:winpath "Mmshell*" "bin")
  (:require (url-exists-in-path? "mmshell"))
  (:serializer ,mmshell-serialize)
  (:launch "mmshell --texmacs")
  (:launch "optimized" "mmshell --optimize --texmacs")
  (:session "Mmshell")
  (:scripts "Mmshell"))

(texmacs-modes
  (in-mmshell% (== (get-env "prog-language") "mmshell"))
  (in-prog-mmshell% #t in-prog% in-mmshell%)
  (in-mmshell-math% #t in-mmshell% in-math%)
  (mmshell-scripts-math% #t mmshell-scripts% in-math%))

(lazy-keyboard (mmshell-edit) in-prog-mmshell?)

(when (supports-mmshell?)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (mmshell-input) mmshell)
  (import-from (dynamic session-menu))
  (plugin-approx-command-set! "mmshell" ""))
