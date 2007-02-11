
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : source-edit.scm
;; DESCRIPTION : editing source code
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

(tm-define (kbd-return)
  (:inside compound)
  (activate-compound))

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
