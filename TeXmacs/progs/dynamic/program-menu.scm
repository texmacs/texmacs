
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : program-menu.scm
;; DESCRIPTION : menus for programs
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic program-menu)
  (:use (dynamic program-edit)
        (generic generic-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (supported-programs-menu)
  (for (name (session-list))
    (let* ((menu-name (session-name name))
           (l (connection-variants name)))
      (assuming (== l (list "default"))
        ((eval menu-name) (make-program name "default")))
      (assuming (!= l (list "default"))
        (-> (eval menu-name)
            (for (variant l)
              ((eval variant) (make-program name variant))))))))

(menu-bind insert-program-menu
  (when (and (style-has? "std-dtd") (in-text?))
    ("Scheme" (make-program "scheme" "default"))
    ---
    (link supported-programs-menu)
    ---
    ("Other" (interactive make-program))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the Programs menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind program-input-menu
  (when (in-plugin-with-converters?)
    ("Mathematical input" (toggle-program-math-input)))
  ("Multiline input" (toggle-program-multiline-input)))

(menu-bind program-output-menu
  (if (in-scheme?)
      ("Pretty tree output" (toggle-program-scheme-trees))
      ("Pretty scheme tree output" (toggle-program-scheme-strees))
      ("Mathematical output" (toggle-program-scheme-math))
      ---)
  ("Show timings" (toggle-program-output-timings)))

(menu-bind program-program-menu
  ("Clear all fields" (program-clear-all))
  ("Fold all fields" (program-fold-all))
  ("Unfold all fields" (program-unfold-all))
  ---
  ("Evaluate fields in order" (toggle-session-program))
  ---
  ("Create subprogram" (prog-field-insert-fold (focus-tree)))
  ("Split program" (program-split)))

(menu-bind program-evaluate-menu
  ("Evaluate" (program-evaluate))
  ("Evaluate all" (program-evaluate-all))
  ("Evaluate above" (program-evaluate-above))
  ("Evaluate below" (program-evaluate-below)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Program menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-program-language)
  (with lan (get-env "prog-language")
    (or (session-name lan) "Scheme")))

(tm-define (standard-options l)
  (:require (in? l prog-field-tags))
  (list "framed-program" "ring-program"))

(tm-menu (focus-tag-menu t)
  (:require (prog-field-context? t))
  (inert ((eval (focus-program-language)) (noop) (noop)))
  (when (alternate-context? t)
    ((check "Unfolded" "v" (alternate-second? (focus-tree)))
     (alternate-toggle (focus-tree))))
  (assuming (focus-has-preferences? t)
    (-> "Preferences"
        (dynamic (focus-preferences-menu t))))
  ("Describe" (set-message "Not yet implemented" "")))

(tm-menu (focus-move-menu t)
  (:require (prog-field-context? t))
  ("Previous field" (traverse-previous))
  ("Next field" (traverse-next))
  ("First field" (traverse-first))
  ("Last field" (traverse-last)))

(tm-define (focus-can-insert-remove? t)
  (:require (prog-field-context? t))
  #t)

(tm-menu (focus-insert-menu t)
  (:require (prog-field-context? t))
  ("Insert field above" (prog-field-insert (focus-tree) #f))
  ("Insert field below" (prog-field-insert (focus-tree) #t))
  ("Insert text field above" (prog-field-insert-text (focus-tree) #f))
  ("Insert text field below" (prog-field-insert-text (focus-tree) #t))
  ---
  ("Remove previous field" (prog-field-remove (focus-tree) #f))
  ("Remove next field" (prog-field-remove (focus-tree) #t))
  ("Remove banner" (prog-field-remove-banner (focus-tree)))
  ("Remove last field" (prog-field-remove-extreme (focus-tree) #t)))

(tm-menu (focus-hidden-menu t)
  (:require (prog-field-context? t)))

(tm-menu (focus-extra-menu t)
  (:require (prog-field-context? t))
  ---
  (-> "Input options" (link program-input-menu))
  (-> "Output options" (link program-output-menu))
  (-> "Program" (link program-program-menu))
  ---
  (-> "Evaluate" (link program-evaluate-menu))
  ("Interrupt execution" (plugin-interrupt))
  ("Close program" (plugin-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programs icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-second-name t)
  (:require (prog-field-context? t))
  "Unfold")

(tm-define (alternate-second-icon t)
  (:require (prog-field-context? t))
  "tm_alternate_both.xpm")

(tm-menu (focus-tag-icons t)
  (:require (prog-field-context? t))
  (dynamic (focus-toggle-icons t))
  (mini #t (inert ((eval (focus-program-language)) (noop))))
  (assuming (focus-has-preferences? t)
    (=> (balloon (icon "tm_focus_prefs.xpm") "Preferences for tag")
	(dynamic (focus-preferences-menu t))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (focus-help)))

(tm-menu (focus-move-icons t)
  (:require (prog-field-context? t))
  ((balloon (icon "tm_similar_first.xpm") "Go to first similar tag")
   (traverse-first))
  ((balloon (icon "tm_similar_previous.xpm") "Go to previous similar tag")
   (traverse-previous))
  ((balloon (icon "tm_similar_next.xpm") "Go to next similar tag")
   (traverse-next))
  ((balloon (icon "tm_similar_last.xpm") "Go to last similar tag")
   (traverse-last)))

(tm-menu (focus-insert-icons t)
  (:require (prog-field-context? t))
  ((balloon (icon "tm_insert_up.xpm") "Insert field above")
   (structured-insert-up))
  ((balloon (icon "tm_insert_down.xpm") "Insert field below")
   (structured-insert-down))
  ((balloon (icon "tm_delete_up.xpm") "Remove field above")
   (prog-field-remove (focus-tree) #f))
  ((balloon (icon "tm_delete_down.xpm") "Remove field below")
   (prog-field-remove (focus-tree) #t)))

(tm-menu (focus-hidden-icons t)
  (:require (prog-field-context? t)))

(tm-menu (focus-extra-icons t)
  (:require (prog-field-context? t))
  (glue #f #f 8 0)
  (=> (balloon (icon "tm_plugin_input.xpm") "Input options")
      (link program-input-menu))
  (=> (balloon (icon "tm_plugin_output.xpm") "Output options")
      (link program-output-menu))
  (=> (balloon (icon "tm_session_session.xpm") "Program commands")
      (link program-program-menu))
  (glue #f #f 10 0)
  (=> (balloon (icon "tm_go.xpm") "Evaluate fields")
      (link program-evaluate-menu))
  (if (!= (get-env "prog-language") "scheme")
      ((balloon (icon "tm_stop.xpm") "Interrupt execution")
       (plugin-interrupt))
      ((balloon (icon "tm_clsession.xpm") "Close program")
       (plugin-stop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind program-help-icons
  ;; Each plugin appends its own entry
  )
