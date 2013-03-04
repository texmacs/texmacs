
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mathemagix.scm
;; DESCRIPTION : Initialize mathemagix plugin
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven, 2012  Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mathemagix-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
	(string-append (escape-verbatim w) "\n")))))

(define mathemagix-launcher
  (if #f;;(url-exists-in-path? "mmi")
      "mmi --texmacs"
      "mmx-light --texmacs"))

(plugin-configure mathemagix
  (:winpath "Mathemagix" "bin")
  (:require (or (url-exists-in-path? "mmi")
                (url-exists-in-path? "mmx-light")))
  (:serializer ,mathemagix-serialize)
  (:launch ,mathemagix-launcher)
  (:session "Mathemagix")
  (:scripts "Mathemagix"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing Mathemagix programs (even if Mathemagix is not installed)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-mathemagix% (== (get-env "prog-language") "mathemagix"))
  (in-prog-mathemagix% #t in-prog% in-mathemagix%)
  (in-mathemagix-math% #t in-mathemagix% in-math%)
  (mathemagix-scripts-math% #t mathemagix-scripts% in-math%))

(lazy-keyboard (mathemagix-edit) in-prog-mathemagix?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization if Mathemagix is supported
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (supports-mathemagix?)

(import-from (utils plugins plugin-convert))
(lazy-input-converter (mathemagix-input) mathemagix)
(import-from (dynamic session-menu))
(import-from (mathemagix-kbd))
(import-from (mathemagix-menus))
(plugin-approx-command-set! "mathemagix" "")

) ;; end when (supports-mathemagix?)
