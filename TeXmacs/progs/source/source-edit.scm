
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-edit.scm
;; DESCRIPTION : editing source code
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (source source-edit))

(tm-define (make-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))

(tm-define (kbd-return)
  (:inside hybrid)
  (activate-hybrid #f))

(tm-define (kbd-return)
  (:inside latex)
  (activate-latex))

(tm-define (kbd-return)
  (:inside symbol)
  (activate-symbol))

(tm-define (kbd-return)
  (:inside inactive)
  (activate))

(tm-define (kbd-tab)
  (:inside hybrid)
  (activate-hybrid #t))

(tm-define (kbd-tab)
  (:inside inactive tuple attr)
  (insert-argument #t))

(tm-define (kbd-tab)
  (:mode in-source?)
  (insert-argument #t))

(tm-define (structured-insert forwards?)
  (:inside hybrid)
  (activate-hybrid #t))
