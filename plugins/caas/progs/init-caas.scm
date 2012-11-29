
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-caas.scm
;; DESCRIPTION : Initialize caas plugin
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven, Gregoire Lecerf
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-caas% (== (get-env "prog-language") "caas"))
  (in-prog-caas% #t in-prog% in-caas%)
  (in-caas-math% #t in-caas% in-math%)
  (caas-scripts-math% #t caas-scripts% in-math%))

;;(lazy-keyboard (caas-edit) in-prog-caas?)

(define (caas-initialize)
  ;;(import-from (utils plugins plugin-convert))
  ;;(lazy-input-converter (caas-input) caas)
  ;;(import-from (dynamic session-menu))
  ;;(import-from (caas-kbd))
  ;;(import-from (caas-menus))
  ;;(plugin-approx-command-set! "caas" "")
  ;;(if (caas-scripts?)
  ;;    (init-add-package "caas"))
  (noop))

(define (caas-serialize lan t)
  (import-from (utils plugins plugin-cmd))
  (with u (pre-serialize lan t)
    (with v (texmacs->code u)
      (with w (string-replace v "\n" "/{CR}/")
	(string-append (escape-verbatim w) "\n")))))

(plugin-configure caas
  (:winpath "Caas\\bin")
  (:require (url-exists-in-path? "caas"))
  (:serializer ,caas-serialize)
  (:initialize (caas-initialize))
  (:launch "caas --texmacs")
  (:session "Caas")
  (:scripts "Caas"))

(tm-define (script-numeric-evaluation-command)
  (:mode in-caas?)
  "")
