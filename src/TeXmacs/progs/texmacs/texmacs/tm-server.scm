
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
  (:use (texmacs texmacs tm-document))
  (:export
    conditional-kill-buffer safely-kill-buffer safely-kill-window
    conditional-quit-TeXmacs safely-quit-TeXmacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-look-and-feel var val)
  (if preferences-initialization-flag
      (set-message "Restart in order to let the new look and feel take effect"
		   "configure look and feel")))

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

(define-preferences
  ("profile" "beginner" (lambda args (noop)))
  ("look and feel" "emacs" notify-look-and-feel)
  ("language" (get-locale-language) notify-language)
  ("security" "prompt on scripts" notify-security)
  ("autosave" "120" notify-autosave))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Killing buffers, windows and TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (conditional-kill-buffer confirm)
  (if (yes? confirm) (kill-buffer)))

(define (safely-kill-buffer)
  (if (buffer-unsaved?)
      (interactive
       '("The buffer has not been saved. Really close it?")
       'conditional-kill-buffer)
      (kill-buffer)))

(define (safely-kill-window)
  (if (<= (get-nr-windows) 1) (safely-quit-TeXmacs) (kill-window)))

(define (conditional-quit-TeXmacs confirm)
  (if (yes? confirm) (quit-TeXmacs)))

(define (safely-quit-TeXmacs)
  (if (exists-unsaved-buffer?)
      (interactive
       '("There are unsaved files. Really quit?")
       'conditional-quit-TeXmacs)
      (quit-TeXmacs)))
