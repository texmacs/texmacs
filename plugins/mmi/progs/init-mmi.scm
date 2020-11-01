
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-mmi.scm
;; DESCRIPTION : Initialize mmi plugin
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (mmi-serialize lan t)
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
	(string-append (escape-verbatim w) "\n")))))

(plugin-configure mmi
  (:winpath "mmi*" "bin")
  (:winpath "Mmi*" "bin")
  (:require (url-exists-in-path? "mmi"))
  (:serializer ,mmi-serialize)
  (:launch "mmi --texmacs")
  (:launch "optimized" "mmi --optimize --texmacs")
  (:session "Mmi")
  (:scripts "Mmi"))

(texmacs-modes
  (in-mmi% (== (get-env "prog-language") "mmi"))
  (in-prog-mmi% #t in-prog% in-mmi%)
  (in-mmi-math% #t in-mmi% in-math%)
  (mmi-scripts-math% #t mmi-scripts% in-math%))

(lazy-keyboard (mmi-edit) in-prog-mmi?)

(when (supports-mmi?)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (mmi-input) mmi)
  (import-from (dynamic session-menu))
  (plugin-approx-command-set! "mmi" ""))
