
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscellaneous extra routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scm-load-buffer u)
   (load-document u) 
   (if (not (url-exists? u)) 
;; save empty file & reload so that it is recognized as scheme code, not plain tm doc  
      (begin (buffer-save u) (revert-buffer-revert))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized keyboards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (notify-keyboard-tool var val)
  (update-bottom-tools))

(define-preferences
  ("custom keyboard" "" (lambda args (noop)))
  ("keyboard tool" "off" notify-keyboard-tool))

(tm-define (set-custom-keyboard kbd)
  (with s (serialize-texmacs-snippet kbd)
    (set-preference "custom keyboard" s)))

(tm-define (get-custom-keyboard)
  (with s (get-preference "custom keyboard")
    (parse-texmacs-snippet s)))

(tm-define (get-the-keyboard)
  (with s (get-custom-keyboard)
    (if (not (tm-equal? s "")) s
        (get-keyboard))))

(tm-menu (custom-keyboard-toolbar)
  (hlist (glue #f #f 0 248)
    >>
    (refreshable "custom-keyboard"
      (invisible (get-the-keyboard))
      (texmacs-output
       `(with "bg-color" "#404040" ,(get-the-keyboard))
       '(style "new-gui")))
    >>))

(tm-define (has-custom-keyboard?)
  (== (get-preference "keyboard tool") "on"))

(tm-define (toggle-custom-keyboard)
  (:check-mark "*" has-custom-keyboard?)
  (with on? (not (has-custom-keyboard?))
    (set-boolean-preference "keyboard tool" on?)
    (refresh-now "custom-keyboard")))

(tm-widget (custom-keyboard-widget cmd)
  (refreshable "custom-keyboard"
    (invisible (get-the-keyboard))
    (texmacs-output
     `(with "bg-color" "#404040" ,(get-the-keyboard))
     '(style "new-gui"))))

(tm-define (open-custom-keyboard)
  (:interactive #t)
  (dialogue-window custom-keyboard-widget noop "Custom keyboard"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The developer menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
   (scm-load-buffer 
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-texmacs.scm")))
  ((replace "Open %1" (verbatim "my-init-buffer.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/progs/my-init-buffer.scm")))
  ((replace "Open %1" (verbatim "preferences.scm"))
   (scm-load-buffer
    (url-concretize "$TEXMACS_HOME_PATH/system/preferences.scm")))
  ---
  (group "Custom keyboard")
  ("Show keyboard" (toggle-custom-keyboard))
  ("Open keyboard" (open-custom-keyboard))
  (when (selection-active-any?)
    ("Set keyboard" (set-custom-keyboard (tm->tree (selection-tree)))))
  (when (not (tm-equal? (get-custom-keyboard) ""))
    ("Reset keyboard" (set-custom-keyboard (tm->tree ""))))
  (assuming (side-tools?)
    ---
    (group "Experimental side tools")
    ("Reset left" (close-tools :left))
    ("Reset right" (close-tools :right))
    ("Buffer left" (tool-select :left 'buffer-tool))
    ("Buffer right" (tool-select :right 'buffer-tool))
    ("Context" (tool-select :right 'context-tool))
    ("Invalid" (tool-toggle :right 'invalid-tool))
    (-> "Test"
        ("Sections" (tool-select :right 'sections-tool))
        ("Subsections" (tool-select :right 'subsections-tool)))
    ;;(-> "Color"
    ;;    ("Color" (tool-select :right '(color-tool "Background color")))
    ;;    ("Pattern" (tool-select :right '(pattern-tool "Background pattern")))
    ;;    ("Gradient" (tool-select :right '(gradient-tool "Background gradient")))
    ;;    ("Picture" (tool-select :right '(picture-tool "Background picture"))))
    ))
