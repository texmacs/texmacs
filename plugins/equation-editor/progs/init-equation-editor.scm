;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-equation-editor.scm
;; DESCRIPTION : plugin that enables using TeXmacs as a graphical
;;               equation editor for external programs 
;; COPYRIGHT   : (C) 2016-2022  Philippe Joyez
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This plugin enables using TeXmacs as 
;; graphical "equation editor" which
;; can be called from other applications
;; (notably from within Inkscape using
;; the "Texmacs extension" which can be installed
;; in Inkscape from Texmacs convert->images preferences).
;; 
;; In this mode the equation to be edited 
;; is loaded from a temporary tm file and displayed with 
;; a toolbar with "done" and "cancel" buttons.
;; When the "done" button is pressed 
;; an updated svg image of the equation (containing the
;; texmacs representation of the equation) is created 
;;
;; The external application can communicate with texmacs 
;; (and this plugin) in two ways:
;; a) launching texmacs with command line 
;; options -x "(lazy-plugin-force)(equ-edit-cmdline)"
;; (see below) and using stdin/stdout pipes
;; b) connecting with an already running texmacs instance
;; using texmacs' socket server feature  
;; (enabled by the preference "equation-editor-server" 
;; controlled in the plugin's preference widget or
;; menu (in the Tools menu).
;; Using socket communication is much faster since it
;; spares the boot-up time of texmacs.
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(import-from (debug debug-menu))(debug-toggle "io")

(plugin-configure equation-editor 
  (:require #t)) ;set to #f if you want to permanently disable the plugin
  
;;;;;;;;;;;;
;; plugin's code for communication and operation
;;;;;;;;;;;;

;;(when (supports-equation-editor?) (display "equation-editor plugin loaded\n"))

;; Do (server-start) automatically at boot-up if preferences are set so
;; to enable incoming connections

(if (get-boolean-preference "equation-editor-server")
  (begin 
    (import-from (server server-base)) ;; define tm-service for use below
    (with srv (client-start "localhost")
      (if (== srv -1) 
        (begin (display "starting server\n") (server-start))
        (begin (display "found local server\n")(client-stop srv)
        )))

;; need a login for connecting to texmac's server
    (server-set-user-information "inkscape" "inkscape-modify-equation" "inkscape" "" "no")

;; service where inkscape connects, sending the path of a temporary file
;; with the equation to be edited and (possibly) a latex code of the equation
;; (for compatibility with latex-based inkscape extensions)
    (tm-service (remote-equ file latex)
(display "tm-service\n")
      (set! current-envelope envelope)
      (set! current-equ-url (system->url file))
        ;;(display* "remote-equ envelope = " current-envelope "\n")
        (load-buffer-in-new-window current-equ-url)
        (window-focus (buffer->window current-equ-url))
        ;(display* (window-to-buffer (current-window)) "\n")
        (equ-edit-start)
        ; sometimes, the buffer shown in the new window spuriously changes
        ; use this hack to be sure we show what we want
        (delayed (:idle 300) (if (not (== (buffer->window current-equ-url) (current-window))) (begin (window-set-buffer (current-window) current-equ-url) 
      (window-focus (buffer->window current-equ-url))))
      (if (== (url-suffix current-equ-url) "html")
      ; converting a LO Math equation from its mathml code
              (buffer-set-default-style))
        
      (if (!= latex "") 
          (insert (latex->texmacs (parse-latex (string-append "\\[" latex "\\]")))))
        )
    )
  ))


(define (current-envelope) #f)
(define (current-equ-url) #f)

(tm-define (equ-edit-cmdline)
  (:synopsis "Shows equation-edit toolbar in non-server mode; can be called on command line with -x \"(lazy-plugin-force)(equ-edit-cmdline)\"")
  (set! current-envelope #f) ;;envelope shall be #f if not acting as socket server
  (set! current-equ-url (system->url (cAr (program-arguments))))
  (equ-edit-start))

(define (equ-edit-done)
  (begin 
    (if (!= (current-buffer) current-equ-url) (switch-to-buffer current-equ-url))
    (select-all)
    (export-selection-as-graphics (url-glue current-equ-url ".svg"))
    (if (not current-envelope) (display "done\n") (server-return current-envelope #t) )
;; NB: when in non-server mode Inkscape watches for "done" in stdout           
    (equ-edit-end)))

(define (equ-edit-cancel)
  (begin 
    (if (not current-envelope ) (display "cancel\n") (server-return current-envelope  #f) )
    (equ-edit-end)))

(define (equ-edit-end) 
  (let ((stay (get-boolean-preference "equation-editor-server"))) ;; should we keep server running ?
;; we should also check (server-started?) but it's always false in qt-texmacs...
        (show-icon-bar 3 #f)
        (buffer-pretend-saved current-equ-url)
        (if (nnot current-envelope) (server-error current-envelope  "disconnect"))  ;;how to simply disconnect that client? 
        (if stay (begin   
          (if (> (length (window-list)) 1)  
              (begin (safely-kill-window) ))
          (if (== (length (buffer-list)) 1)  
              (begin (switch-to-buffer (buffer-new))))
          (buffer-close current-equ-url))
          (if (not current-envelope)  (quit) ))
  ))
  
(define (equ-edit-start)
;; ensure image export format is svg, override if not
    (if (not (svg-converter-available))
      (begin 
        (show-message "You invoked TeXmacs' equation editor plugin, but it cannot operate without a converter to SVG format. See Preferences > Plugins > Equation-editor"
        "Missing SVG converter")
        (equ-edit-cancel ))
      (begin 
        (if (!= (get-preference "texmacs->image:format") "svg")
          (begin 
            (set-preference "texmacs->image:format" "svg")
            ;; warn user one way or the other
          ))
        (equ-edit-toolbar))))


;;;;;;;;;;;;
;; Toolbar
;;;;;;;;;;;;

(define (equ-edit-toolbar)
;;  "Define and displays control buttons in the toolbar"
  (menu-bind texmacs-extra-icons 
       ((balloon "Done" "update equation in client") (equ-edit-done ) )
       ((balloon "Cancel" "abandon modifying equation") (equ-edit-cancel ))
       ((check (balloon "sansSerif" "") "v" (font-sserif?)) (toggle-sserif)))
  (show-icon-bar 3 #t))

(define (font-sserif?) (== (get-init "font-family") "ss"))
(define (toggle-sserif) 
  (if (== (get-init "font-family") "ss") (init-default "font-family") (init-env "font-family" "ss")))

;;;;;;;;;;;;
;; settings widget and preferences
;;;;;;;;;;;;

;; is operation in socket server mode preferred?
(define-preferences 
  ("equation-editor-server" "on" noop)) ;; actual pref value set in preference widget

(define (svg-converter-available)
  (nnot (converter-search "pdf-file" "svg-file")))

(define (inkscape-detected)
  (if (os-mingw?) #t (url-exists-in-path? "inkscape"))) ;;no reliable way on windows

(define (inkscape-prefs-dir)
  (url-concretize (url-append (if (os-mingw?)
  (system->url "$APPDATA") (system->url
  "~/.config"))(string->url "inkscape"))))
  
(define (inkscape-extension)
  (url-append (inkscape-prefs-dir) (string->url "extensions/texmacs/texmacs.inx")))

(define (inkscape-prefs-found)
  (url-exists? (inkscape-prefs-dir)))

(define (inkscape-extension-installed)
  (url-exists? (inkscape-extension)))
  
(define (inkscape-ready?)
 (and (inkscape-detected) (inkscape-prefs-found) (svg-converter-available)))

(define (get-svg-converter)
  (if (os-mingw?) 
    (show-message "pdftocairo is bundled with TeXmacs at this address: https://github.com/slowphil/mingw-w64-texmacs/releases"
      "'pdftocairo' is needed for co-operating with Inkscape, but it is not present.")
    (if (os-macos?) 
      (show-message "Please install either 'pdf2svg', 'inkscape' or 'pdftocairo' (included in poppler) from MacPorts, Fink or Homebrew"
        "missing svg converter")
      (show-message "Please install either 'pdf2svg', 'inkscape' or 'pdftocairo' (included in poppler-utils) using your distribution package management system"
        "missing svg converter"))))
(define (start-inkscape)
  (if (os-mingw?)
    (show-message "Make sure Inkscape is installed on your system, and then start it once." "missing Inkscape preferences")
    (shell "inkscape")))

(define (install-inkscape-extension)
  (let ((dest-dir (escape-shell (url-concretize 
          (url-append 
            (if (os-mingw?) (system->url "$APPDATA")  (system->url "~/.config"))
            (string->url "inkscape/extensions")))))
         (source-dir (escape-shell (url-concretize (url-unix "$TEXMACS_PATH" "plugins/equation-editor/misc/inkscape_extension")))))
         (if (os-mingw?)
           (system (string-append "xcopy " source-dir " " dest-dir "\\ /S /Y"))
           (system (string-append "mkdir -p " dest-dir " ; cp -r " source-dir "/texmacs " dest-dir)))
       (refresh-now "equ-ed-config")))
       
(define (install-libreoffice-extension)
;; use OS file associations to open extension installer in libreoffice
  (with lo-ext (escape-shell (url->string (url-complete (url-append (url-concretize (url-unix "$TEXMACS_PATH" "plugins/equation-editor/misc/")) (url-wildcard "SVG_and_Texmacs-L*.oxt")) "fr")))
  (if (os-mingw?) 
    (system (string-append "start \"\" " lo-ext))
    (if (os-macos?) 
      (system (string-append "open " lo-ext))
      (system (string-append "xdg-open " lo-ext))))))
      
(define (get-equation-editor-server)
  (get-boolean-preference "equation-editor-server"))

(define (set-equation-editor-server on?)
  (set-boolean-preference "equation-editor-server" on?)
  (refresh-now "equ-ed-config"))
  

(tm-widget (equation-editor-preferences-widget . cmd)
    ======
    (bold (centered  (text "Equation Editor for Inkscape or LibreOffice")))
    ===
    (refreshable "equ-ed-config"
     (if (not (svg-converter-available))
      (hlist  (text "Svg converter not available") >>> 
               (explicit-buttons ("get converter" (get-svg-converter))) //
               (explicit-buttons ("Help" (load-help-buffer "main/convert/man-graphics-export")))))
           
    (if (svg-converter-available) 
    ======
    (bold (text "Inkscape"))
    ===        
          ;; tests in "assuming" not refreshed, must use "if" construct instead         
      (if (not (inkscape-detected)) 
             (centered (text "Inkscape not detected") ))
      (if (and (inkscape-detected) (not (inkscape-prefs-found)))
        (aligned (meti (hlist  // (text "Inkscape user prefs found") >>> 
            (if (not (inkscape-prefs-found))
              (explicit-buttons (
                            (eval (if (os-mingw?) "help" "create (start Inkscape)"))
                  (start-inkscape))) ))
            (inert (toggle (noop) (inkscape-prefs-found))))))
      (if (inkscape-ready?)
        (aligned (meti (text "Inkscape extension installed")           
              (inert (toggle (noop) (inkscape-extension-installed)))))
        ===
    (centered  (explicit-buttons ("install/update extension" (install-inkscape-extension)) ) ))
      ======
    (bold (text "LibreOffice"))
    ===        
     (centered (explicit-buttons ("install/update LibreOffice extension" (install-libreoffice-extension)) ) )
    ===
     (centered (text "-------------------------------------"))
    ===
      (aligned (meti (hlist  // (text "Enable socket communications (faster)"))              
              (toggle (set-equation-editor-server answer)
                (get-equation-editor-server))))
    )
    )
  
    ======
        (centered
         (hlist  /// (explicit-buttons ("Refresh" (refresh-now "equ-ed-config")))
             /// (explicit-buttons ("Help" (load-help-buffer "equation-editor")))))
  )


(if (not (defined? 'plugins-with-pref-widget)) ;see https://savannah.gnu.org/patch/?10177
;; no preference tab widgets for plugins: setup a menu in the tools menu

  (begin 

    (tm-define (open-equation-editor-widget)
      (:interactive #t)
        (dialogue-window equation-editor-preferences-widget noop "Equation editor plugin settings"))
      

    (tm-menu (equation-editor-menu)
      (if (not (svg-converter-available))
            ("get svg converter" (get-svg-converter)))          
      (if (svg-converter-available)
        ("server mode" (toggle-preference "equation-editor-server"))
        (-> "External clients"
          (if (inkscape-ready?) ("install/update Inkscape extension" (install-inkscape-extension)))
          (if (not (inkscape-ready?)) 
            (when #f  ("Inkscape extension not instalable" (noop)))
            (if (not (inkscape-detected))
              (when #f  ("(Inkscape not detected)" (noop))))
            (if (and (inkscape-detected) (not (inkscape-prefs-found)))
              ("create Inkscape user prefs (start inkscape)" (start-inkscape))))
          ("install/update LibreOffice extension" (install-libreoffice-extension))))
      ("Help" (load-help-buffer "equation-editor")))

    (delayed (:idle 2000)
      (tm-menu (tools-menu)
        (former)
        ---
        (if (use-popups?) 
          ("Equation editor plugin"
            (open-equation-editor-widget)))
        (if (use-menus?)
          (-> "Equation editor plugin"
            (link equation-editor-menu)))
      ))
  
))
