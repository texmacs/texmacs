
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-pnambic.scm
;; DESCRIPTION : Initialize pnambic plugin
;; COPYRIGHT   : (C) 2005  Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(plugin-configure pnambic
  (:require (url-exists-in-path? "pnambic"))
  (:launch "pnambic ~/.TeXmacs/plugins/pnambic/in ~/.TeXmacs/plugins/pnambic/out")
  (:session "Pnambic"))
