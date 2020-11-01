
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-caas.scm
;; DESCRIPTION : Initialize caas plugin
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (caas-serialize lan t)
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
	(string-append (escape-verbatim w) "\n")))))

(plugin-configure caas
  (:winpath "caas*" "bin")
  (:winpath "Caas*" "bin")
  (:require (url-exists-in-path? "caas"))
  (:serializer ,caas-serialize)
  (:launch "caas --texmacs")
  (:session "Caas")
  (:scripts "Caas"))

(texmacs-modes
  (in-caas% (== (get-env "prog-language") "caas"))
  (in-prog-caas% #t in-prog% in-caas%)
  (in-caas-math% #t in-caas% in-math%)
  (caas-scripts-math% #t caas-scripts% in-math%))

(lazy-keyboard (caas-edit) in-prog-caas?)

(when (supports-caas?)
  (import-from (utils plugins plugin-convert))
  (lazy-input-converter (caas-input) caas)
  (import-from (dynamic session-menu))
  (plugin-approx-command-set! "caas" ""))
