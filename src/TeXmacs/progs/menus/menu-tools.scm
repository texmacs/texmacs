
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-tools.scm
;; DESCRIPTION : the tools menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (menus menu-tools))

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

(menu-bind tools-menu
  (-> "Execute"
      ("Execute system command" ... (interactive '("Command:") 'system))
      ("Evaluate scheme expression" ...
       (interactive '("Command:") 'footer-eval)))
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
  (if (not (null? (menu-get 'test-menu)))
      ---
      (link test-menu)))
