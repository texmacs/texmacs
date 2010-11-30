
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing command tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-latex)
  (make 'latex)
  (set-message "Type a latex command followed by return" "latex"))

(tm-define (activate-compound)
  (with-innermost t 'compound
    (when (not (tree-is? t :up 'inactive))
      (tree-set! t `(inactive ,t)))
    (activate)))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #f))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'compound))
  (activate-compound))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'latex))
  (activate-latex))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'symbol))
  (activate-symbol))

(tm-define (kbd-enter t forwards?)
  (:require (tree-is? t 'inactive))
  (activate))

(tm-define (kbd-variant t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #t))

(tm-define (kbd-variant t forwards?)
  (:require (tree-in? t '(inactive tuple attr)))
  (insert-argument forwards?))

(tm-define (kbd-variant t forwards?)
  (:require (in-source?))
  (insert-argument forwards?))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (tree-is? t 'hybrid))
  (activate-hybrid #t))

(tm-define (inactive-toggle t)
  (if (or (tree-is? t 'inactive) (tree-is? t :up 'inactive))
      (activate)
      (tree-set t `(inactive ,t))))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'hybrid) (tree-is? t :up 'inactive)))
  (activate-hybrid #f))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'compound) (tree-is? t :up 'inactive)))
  (activate-compound))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'latex) (tree-is? t :up 'inactive)))
  (activate-latex))

(tm-define (inactive-toggle t)
  (:require (and (tree-is? t 'symbol) (tree-is? t :up 'inactive)))
  (activate-symbol))
