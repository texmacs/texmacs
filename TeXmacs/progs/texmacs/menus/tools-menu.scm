
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tools-menu.scm
;; DESCRIPTION : the tools menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus tools-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus for formats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clipboard-preference-item fm name routine)
  `(,name (,routine ,fm)))

(define-macro (clipboard-preference-menu-promise routine)
  (define (item fm name) (clipboard-preference-item fm name routine))
  (with fun (if (== routine 'clipboard-set-export)
		converter-from-menu converter-to-menu)
    `(menu-dynamic ,@(fun "texmacs-snippet" "-snippet" #t item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Tools menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-extend test-menu
  (-> "Test"
      ("Tree" (show-tree))
      ("Path" (show-path))
      ("Cursors" (show-cursor))
      ("Selection" (show-selection))
      ("Environment" (show-env))
      ---
      ("Error" (oops))
      ("Test" (edit-test))
      ("Timings" (bench-print-all))
      ("Backtrace errors" (debug-enable 'backtrace 'debug))
      ("Memory information" (show-meminfo))))

(menu-bind tools-menu
  (-> "Execute"
      ("Execute system command" (interactive system))
      ("Evaluate scheme expression" (interactive footer-eval)))
  (-> "Selections"
      (-> "Import"
	  (promise (clipboard-preference-menu-promise clipboard-set-import)))
      (-> "Export"
	  (promise (clipboard-preference-menu-promise clipboard-set-export))))
  (-> "Update"
      ("Image links" (image-gc))
      ("Inclusions" (inclusions-gc))
      ("Styles" (style-clear-cache)))
  (-> "Web"
      ("Create web site" (tmweb-interactive-build)))
  (-> "Project"
      (link project-manage-menu))
  (if (nnull? (test-menu))
      ---
      (link test-menu)))
