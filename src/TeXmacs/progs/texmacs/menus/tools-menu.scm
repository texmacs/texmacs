
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tools-menu.scm
;; DESCRIPTION : the tools menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
      ;;("Keymaps" (show-keymaps))
      ---
      ("Error" (oops))
      ("Test" (edit-test))
      ("Timings" (bench-print-all))
      ("Memory information" (show-meminfo))))

(menu-bind tools-menu
  (-> "Execute"
      ("Execute system command" (interactive system "Command"))
      ("Evaluate scheme expression" (interactive footer-eval "Command")))
  (-> "Selections"
      (-> "Import"
	  (promise (clipboard-preference-menu-promise clipboard-set-import)))
      (-> "Export"
	  (promise (clipboard-preference-menu-promise clipboard-set-export))))
  (-> "Update"
      ("Image links" (postscript-gc))
      ("Inclusions" (inclusions-gc))
      ("Styles" (style-clear-cache)))
  (-> "Web"
      ("Create web site" ...
       (choose-file "Source directory" "directory" 'tmweb-build-from)))
  (if (nnull? (test-menu))
      ---
      (link test-menu)))
