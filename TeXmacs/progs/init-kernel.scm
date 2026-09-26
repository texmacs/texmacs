
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-kernel.scm
;; DESCRIPTION : Initialization of the TeXmacs kernel
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Loaded by init-s7.scm or init-guile.scm, once the module system and the
;; kernel's compatibility module are loaded; common to both interpreters.
;; The initialization continues with init-texmacs.scm.

(define remote-client-list (list))
(define tm-interactive-hook tm-interactive)

(inherit-modules (kernel boot abbrevs)
                 (kernel boot debug) (kernel boot srfi)
                 (kernel boot ahash-table) (kernel boot prologue))
(inherit-modules (kernel library base) (kernel library list)
                 (kernel library tree) (kernel library content)
                 (kernel library patch))
(inherit-modules (kernel regexp regexp-match) (kernel regexp regexp-select))
(inherit-modules (kernel logic logic-rules) (kernel logic logic-query)
                 (kernel logic logic-data))
(inherit-modules (kernel texmacs tm-define)
                 (kernel texmacs tm-preferences) (kernel texmacs tm-modes)
                 (kernel texmacs tm-plugins) (kernel texmacs tm-secure)
                 (kernel texmacs tm-convert) (kernel texmacs tm-dialogue)
                 (kernel texmacs tm-language) (kernel texmacs tm-file-system)
                 (kernel texmacs tm-states))
(inherit-modules (kernel gui gui-markup)
                 (kernel gui menu-define) (kernel gui menu-widget)
                 (kernel gui kbd-define)
                 (kernel gui speech-define)
                 (kernel gui kbd-handlers)
                 (kernel gui menu-test)
                 (kernel old-gui old-gui-widget)
                 (kernel old-gui old-gui-factory)
                 (kernel old-gui old-gui-form)
                 (kernel old-gui old-gui-test))
(lazy-define (kernel gui menu-convert) make-menu-widget**)
