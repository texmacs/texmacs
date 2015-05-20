
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : developer-menu.scm
;; DESCRIPTION : Menu items for developer mode
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;; Things to do:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus developer-menu))

(use-modules (prog scheme-tools) (prog scheme-menu)
             (doc apidoc) (doc apidoc-widgets)
             (language natural))

(menu-bind developer-menu
  (group "Scheme")
  (link scheme-menu)
  ---
  (group "Translations")
  (link translations-menu)
  ---
  (group "Documentation")
  (link apidoc-menu)
  ---
  (group "Configuration")
  ((replace "Open %1" (verbatim "my-init-texmacs.scm"))
   (load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm")))
  ((replace "Open %1" (verbatim "my-init-buffer.scm"))
   (load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm")))
  ((replace "Open %1" (verbatim "preferences.scm"))
   (load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/system/preferences.scm"))))
