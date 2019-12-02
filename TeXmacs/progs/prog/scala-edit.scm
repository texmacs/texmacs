;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scala-edit.scm
;; DESCRIPTION : editing Scala programs
;; COPYRIGHT   : (C) 2019  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scala-edit)
  (:use (prog prog-edit)))

(tm-define (get-tabstop)
  (:mode in-prog-scala?)
  2)

(tm-define (program-compute-indentation doc row col)
  (:mode in-prog-scala?)
  (get-tabstop))

(tm-define (kbd-paste)
  (:mode in-prog-scala?)
  (clipboard-paste-import "scala" "primary"))
