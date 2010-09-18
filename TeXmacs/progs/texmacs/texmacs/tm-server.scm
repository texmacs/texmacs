
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-server.scm
;; DESCRIPTION : server wide properties and resource management
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs texmacs tm-server)
  (:use (generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-default-interactive-questions)
  (if (or (like-macos?) (like-windows?)) "popups" "footer"))

(define (notify-look-and-feel var val)
  (set-message "Restart in order to let the new look and feel take effect"
	       "configure look and feel"))

(define (notify-language var val)
  (set-output-language val)
  (if (and (has-view?) (== (buffer-tree) (stree->tree '(document ""))))
      (init-language val))
  (cond ((or (== val "bulgarian") (== val "russian") (== val "ukrainian"))
	 (notify-preference "cyrillic input method"))))

(define (notify-scripting-language var val)
  (if (has-view?)
      (if (== val "none")
	  (init-default "prog-scripts")
	  (init-env "prog-scripts" val))))

(define (notify-security var val)
  (cond ((== val "accept no scripts") (set-script-status 0))
	((== val "prompt on scripts") (set-script-status 1))
	((== val "accept all scripts") (set-script-status 2))))

(define (notify-bibtex-command var val)
  (set-bibtex-command val))

(define (notify-tool var val)
  ;; FIXME: the menus sometimes don't get updated,
  ;; but the fix below does not work
  (if (has-view?) (notify-change 1)))

(define-preferences
  ("profile" "beginner" (lambda args (noop)))
  ("look and feel" "default" notify-look-and-feel)
  ("detailed menus" "detailed" noop)
  ("interactive questions" (get-default-interactive-questions) noop)
  ("language" (get-locale-language) notify-language)
  ("semantic editing" "off" (lambda args (noop)))
  ("security" "prompt on scripts" notify-security)
  ("bibtex command" "bibtex" notify-bibtex-command)
  ("scripting language" "none" notify-scripting-language)
  ("debugging tool" "off" notify-tool)
  ("linking tool" "off" notify-tool)
  ("versioning tool" "off" notify-tool)
  ("remote connections" "off" notify-tool))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Properties of some built-in routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (system cmd)
  (:argument cmd "System command"))

(tm-property (footer-eval cmd)
  (:argument cmd "Scheme command"))

(define (symbol<=? s1 s2)
  (string<=? (symbol->string s1) (symbol->string s2)))

(define (get-function-list)
  (list-sort (map car (ahash-table->list ovl-table)) symbol<=?))

(define (get-interactive-function-list)
  (let* ((funs (get-function-list))
	 (pred? (lambda (fun) (not (not (property fun :arguments))))))
    (list-filter funs pred?)))

(tm-define (exec-interactive-command cmd)
  (:argument  cmd "Interactive command")
  (:proposals cmd (cons "" (map symbol->string
				(get-interactive-function-list))))
  (interactive (eval (string->symbol cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Killing buffers, windows and TeXmacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (safely-kill-buffer)
  (dialogue
    (if (or (not (buffer-unsaved?))
	    (dialogue-confirm?
	     "The buffer has not been saved. Really close it?" #f))
	(kill-buffer))))

(tm-define (safely-kill-window)
  (if (<= (get-nr-windows) 1)
      (safely-quit-TeXmacs)
      (kill-window)))

(tm-define (safely-quit-TeXmacs)
  (dialogue
    (if (or (not (exists-unsaved-buffer?))
	    (dialogue-confirm?
	     "There are unsaved files. Really quit?" #f))
	(quit-TeXmacs))))
