
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
  (:use (dynamic session-edit)
        (dynamic program-edit)
        (generic generic-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remote plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (select-remote-plugin-widget quit)
  (let* ((where "")
         (set-where
          (lambda (new-where)
            (when new-where
              (set! where new-where)
              (refresh-now "supported-plugins")
              (refresh-now "enter-server"))))
         (key-press
          (lambda (what)
            (when what
              (set! where (car what))
              (when (== (cadr what) "return")
                (set-where where)))))
         (encode
          (lambda (triple)
            (with (p v cmd) triple
              (with s (upcase-first p)
                (if (== v "default") s
                    (string-append s " (" v ")"))))))
         (decode
          (lambda (name)
            (and-with l (list-remote-plugins where)
              (with t (list-find (cadr l) (lambda (u) (== (encode u) name)))
                (list (car t) (string-append where "/" (cadr t)))))))
         (plugin-list
          (lambda ()
            (with l (list-remote-plugins where)
              (if (not l) (list)
                  (map encode (cadr l)))))))
    (padded
      (hlist
        (resize "300px" "400px"
          (vlist
            (bold (text "Remote servers"))
            ===
            (refreshable "remote-servers"
              (choice (set-where answer) (remote-connection-servers) where))))
        // // //
        (resize "300px" "400px"
          (vlist
            (bold (text "Supported plug-ins"))
            ===
            (refreshable "supported-plugins"
              (scrollable
                (choice (quit (decode answer)) (plugin-list) ""))))))
      === ===
      (hlist
        (text "Server:") //
        (refreshable "enter-server"
          (input (key-press answer) "search-server" (list where) "1w"))
        // // //
        (explicit-buttons
          ("Add"
           (detect-remote-plugins where)
           (refresh-now "remote-servers")
           (refresh-now "supported-plugins")) //
           ("Update"
            (update-remote-plugins where)
            (refresh-now "supported-plugins")) //
            ("Remove"
             (remove-remote-plugins where)
             (refresh-now "remote-servers")
             (refresh-now "supported-plugins")))))))

(tm-define (open-remote-plugin-selector name call-back)
  (:interactive #t)
  (dialogue-window select-remote-plugin-widget call-back name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (supported-sessions-menu)
  (for (name (session-list))
    (let* ((menu-name (session-name name))
           (l (local-connection-variants name)))
      (assuming (nnull? l)
        (assuming (== l (list "default"))
          ((eval menu-name) (make-session name "default")))
        (assuming (!= l (list "default"))
          (-> (eval menu-name)
              (for (variant l)
                ((eval variant) (make-session name variant)))))))))

(menu-bind insert-session-menu
  (when (and (style-has? "std-dtd") (in-text?))
    ("Scheme" (make-session "scheme" "default"))
    ---
    (link supported-sessions-menu)
    ---
    ("Remote" (open-remote-plugin-selector
               "Start remote session"
               (lambda (x) (apply make-session x))))
    ("Other" (interactive make-session))
    (assuming (or (os-mingw?) (os-win32?))
      ("Manual path" (interactive set-manual-path)))))

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
      ("Pretty scheme tree output" (toggle-session-scheme-strees))
      ("Mathematical output" (toggle-session-scheme-math))
      ---)
  ("Show timings" (toggle-session-output-timings)))

(menu-bind session-session-menu
  ("Clear all fields" (session-clear-all))
  ("Fold all fields" (session-fold-all))
  ("Unfold all fields" (session-unfold-all))
  ---
  ("Evaluate fields in order" (toggle-session-program))
  ---
  ("Create subsession" (field-insert-fold (focus-tree)))
  ("Split session" (session-split)))

(menu-bind session-evaluate-menu
  ("Evaluate" (session-evaluate))
  ("Evaluate all" (session-evaluate-all))
  ("Evaluate above" (session-evaluate-above))
  ("Evaluate below" (session-evaluate-below)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Session menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-session-language)
  (with lan (get-env "prog-language")
    (or (session-name lan) "Scheme")))

(tm-define (standard-options l)
  (:require (in? l field-tags))
  (list "framed-session" "ring-session" "large-formulas"))

(tm-menu (focus-tag-menu t)
  (:require (field-context? t))
  (inert ((eval (focus-session-language)) (noop) (noop)))
  (when (alternate-context? t)
    ((check "Unfolded" "v" (alternate-second? (focus-tree)))
     (alternate-toggle (focus-tree))))
  (assuming (focus-has-preferences? t)
    (-> "Preferences"
        (dynamic (focus-preferences-menu t))))
  ("Describe" (set-message "Not yet implemented" "")))

(tm-menu (focus-move-menu t)
  (:require (field-context? t))
  ("Previous field" (traverse-previous))
  ("Next field" (traverse-next))
  ("First field" (traverse-first))
  ("Last field" (traverse-last)))

(tm-define (focus-can-insert-remove? t)
  (:require (field-context? t))
  #t)

(tm-menu (focus-insert-menu t)
  (:require (field-context? t))
  ("Insert field above" (field-insert (focus-tree) #f))
  ("Insert field below" (field-insert (focus-tree) #t))
  ("Insert text field above" (field-insert-text (focus-tree) #f))
  ("Insert text field below" (field-insert-text (focus-tree) #t))
  ---
  ("Remove previous field" (field-remove (focus-tree) #f))
  ("Remove next field" (field-remove (focus-tree) #t))
  ("Remove banner" (field-remove-banner (focus-tree)))
  ("Remove last field" (field-remove-extreme (focus-tree) #t)))

(tm-menu (focus-hidden-menu t)
  (:require (field-context? t)))

(tm-menu (focus-extra-menu t)
  (:require (field-context? t))
  ---
  (-> "Input options" (link session-input-menu))
  (-> "Output options" (link session-output-menu))
  (-> "Session" (link session-session-menu))
  ---
  (-> "Evaluate" (link session-evaluate-menu))
  ("Interrupt execution" (plugin-interrupt))
  ("Close session" (plugin-stop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-second-name t)
  (:require (field-context? t))
  "Unfold")

(tm-define (alternate-second-icon t)
  (:require (field-context? t))
  "tm_alternate_both.xpm")

(tm-menu (focus-tag-icons t)
  (:require (field-context? t))
  (dynamic (focus-toggle-icons t))
  (mini #t (inert ((eval (focus-session-language)) (noop))))
  (assuming (focus-has-preferences? t)
    (=> (balloon (icon "tm_focus_prefs.xpm") "Preferences for tag")
	(dynamic (focus-preferences-menu t))))
  ((balloon (icon "tm_focus_help.xpm") "Describe tag")
   (focus-help)))

(tm-menu (focus-move-icons t)
  (:require (field-context? t))
  ((balloon (icon "tm_similar_first.xpm") "Go to first similar tag")
   (traverse-first))
  ((balloon (icon "tm_similar_previous.xpm") "Go to previous similar tag")
   (traverse-previous))
  ((balloon (icon "tm_similar_next.xpm") "Go to next similar tag")
   (traverse-next))
  ((balloon (icon "tm_similar_last.xpm") "Go to last similar tag")
   (traverse-last)))

(tm-menu (focus-insert-icons t)
  (:require (field-context? t))
  ((balloon (icon "tm_insert_up.xpm") "Insert field above")
   (structured-insert-up))
  ((balloon (icon "tm_insert_down.xpm") "Insert field below")
   (structured-insert-down))
  ((balloon (icon "tm_delete_up.xpm") "Remove field above")
   (field-remove (focus-tree) #f))
  ((balloon (icon "tm_delete_down.xpm") "Remove field below")
   (field-remove (focus-tree) #t)))

(tm-menu (focus-hidden-icons t)
  (:require (field-context? t)))

(tm-menu (focus-extra-icons t)
  (:require (field-context? t))
  (glue #f #f 8 0)
  (=> (balloon (icon "tm_plugin_input.xpm") "Input options")
      (link session-input-menu))
  (=> (balloon (icon "tm_plugin_output.xpm") "Output options")
      (link session-output-menu))
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
