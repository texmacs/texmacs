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

(define initialized-equation-editor? #f)

(tm-define (init-equation-editor)
  (when (not initialized-equation-editor?)
    (when (get-boolean-preference "equation-editor")
      (import-from (equation-editor))
      (set! initialized-equation-editor? #t))))

(init-equation-editor)

(tm-define (notify-equation-editor . args)
  (init-equation-editor))

(define-preferences ("equation-editor" "off" notify-equation-editor))
