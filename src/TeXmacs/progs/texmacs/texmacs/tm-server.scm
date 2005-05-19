
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-server.scm
;; DESCRIPTION : server wide properties and resource management
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-server)
  (:use (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-default-look-and-feel)
  (if (os-win32?) "windows" "emacs"))

(define (notify-look-and-feel var val)
  (set-message "Restart in order to let the new look and feel take effect"
	       "configure look and feel"))

(define (notify-language var val)
  (set-output-language val)
  (if (and (has-view?) (== (the-buffer) (stree->tree '(document ""))))
      (init-language val))
  (cond ((or (== val "russian") (== val "ukrainian"))
	 (notify-preference "cyrillic input method"))))

(define (notify-security var val)
  (cond ((== val "accept no scripts") (set-script-status 0))
	((== val "prompt on scripts") (set-script-status 1))
	((== val "accept all scripts") (set-script-status 2))))

(define (notify-autosave var val)
  (if (has-view?) ; delayed-autosave would crash at initialization time
      (delayed-autosave)))

(define (notify-bibtex-command var val)
  (set-bibtex-command val))

(define-preferences
  ("profile" "beginner" (lambda args (noop)))
  ("look and feel" (get-default-look-and-feel) notify-look-and-feel)
  ("language" (get-locale-language) notify-language)
  ("security" "prompt on scripts" notify-security)
  ("autosave" "120" notify-autosave)
  ("bibtex command" "bibtex" notify-bibtex-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of some built-in routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (system cmd)
  (:argument cmd "Command"))

(tm-property (footer-eval cmd)
  (:argument cmd "Command"))

(tm-property (replace-start-forward what by)
  ;; FIXME: should go elsewhere
  (:argument what "Replace")
  (:argument by "Replace by"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Killing buffers, windows and TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (conditional-kill-buffer confirm)
  (if (yes? confirm) (kill-buffer)))

(tm-define (safely-kill-buffer)
  (if (buffer-unsaved?)
      (interactive conditional-kill-buffer
	"The buffer has not been saved. Really close it?")
      (kill-buffer)))

(tm-define (safely-kill-window)
  (if (<= (get-nr-windows) 1) (safely-quit-TeXmacs) (kill-window)))

(tm-define (conditional-quit-TeXmacs confirm)
  (if (yes? confirm) (quit-TeXmacs)))

(tm-define (safely-quit-TeXmacs)
  (if (exists-unsaved-buffer?)
      (interactive conditional-quit-TeXmacs
	"There are unsaved files. Really quit?")
      (quit-TeXmacs)))
