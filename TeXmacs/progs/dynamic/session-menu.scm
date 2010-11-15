
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : session-menu.scm
;; DESCRIPTION : menus for sessions
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic session-menu)
  (:use (dynamic session-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (supported-sessions-menu)
  (let* ((dummy (lazy-plugin-force))
         (l (list-sort supported-sessions-list string<=?)))
    (for (name l)
      (let* ((menu-name (ahash-ref supported-sessions-table name))
             (l (ahash-ref connection-varlist name))
             (fun (lambda (v) (supported-sessions-menu-variant name v))))
        (assuming (not l)
          ((eval menu-name) (make-session name "default")))
        (assuming l
          (-> (eval menu-name)
              (for (variant l)
                ((eval variant) (make-session name variant)))))))))

(menu-bind insert-session-menu
  (when (and (style-has? "std-dtd") (in-text?))
    ("Scheme" (make-session "scheme" "default"))
    ---
    (link supported-sessions-menu)
    ---
    ("Other" (interactive make-session))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Submenus of the Sessions menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind session-input-menu
  (when (in-plugin-with-converters?)
    ("Mathematical input" (toggle-session-math-input)))
  ("Multiline input" (toggle-session-multiline-input)))

(menu-bind session-output-menu
  (if (in-scheme?)
      ("Pretty tree output" (toggle-session-scheme-trees))
      ("Mathematical output" (toggle-session-scheme-math))
      ---)
  ("Show timings" (toggle-session-output-timings)))

(menu-bind session-field-menu
  ("Insert field above" (field-insert #f))
  ("Insert field below" (field-insert #t))
  ("Insert text field above" (field-insert-text #f))
  ("Insert text field below" (field-insert-text #t))
  ---
  ("Remove previous field" (field-remove #f))
  ("Remove next field" (field-remove #t))
  ("Remove banner" (field-remove-banner))
  ("Remove last field" (field-remove-extreme #t)))

(menu-bind session-session-menu
  ("Clear all fields" (session-clear-all))
  ("Fold all fields" (session-fold-all))
  ("Unfold all fields" (session-unfold-all))
  ---
  ("Create subsession" (field-insert-fold))
  ("Split session" (session-split)))

(menu-bind session-evaluate-menu
  ("Evaluate" (session-evaluate))
  ("Evaluate all" (session-evaluate-all))
  ("Evaluate above" (session-evaluate-above))
  ("Evaluate below" (session-evaluate-below)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Session menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind session-menu
  (-> "Input options" (link session-input-menu))
  (-> "Output options" (link session-output-menu))
  (-> "Field" (link session-field-menu))
  (-> "Session" (link session-session-menu))
  ---
  (-> "Evaluate" (link session-evaluate-menu))
  ("Interrupt execution" (plugin-interrupt))
  ("Close session" (plugin-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind session-icons
  (glue #f #f 8 0)
  (=> (balloon (icon "tm_plugin_input.xpm") "Input options")
      (link session-input-menu))
  (=> (balloon (icon "tm_plugin_output.xpm") "Output options")
      (link session-output-menu))
  (=> (balloon (icon "tm_session_field.xpm") "Field commands")
      (link session-field-menu))
  (=> (balloon (icon "tm_session_session.xpm") "Session commands")
      (link session-session-menu))
  (glue #f #f 10 0)
  (=> (balloon (icon "tm_go.xpm") "Evaluate fields")
      (link session-evaluate-menu))
  (if (!= (get-env "prog-language") "scheme")
      ((balloon (icon "tm_stop.xpm") "Interrupt execution")
       (plugin-interrupt))
      ((balloon (icon "tm_clsession.xpm") "Close session")
       (plugin-stop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind session-help-icons
  ;; Each plugin appends its own entry
  )
