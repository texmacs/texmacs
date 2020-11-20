
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : mmshell-edit.scm
;; DESCRIPTION : editing mmshell programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (mmshell-edit)
  (:use (mmx-indent)))

(tm-define (kbd-variant t forward?)
  (:require (and (in-prog-mmshell?) (not (inside? 'session))))
  (mmx-indent))

(tm-define (insert-return)
  (:mode in-prog-mmshell?)
  (insert-raw-return)
  (mmx-indent))
