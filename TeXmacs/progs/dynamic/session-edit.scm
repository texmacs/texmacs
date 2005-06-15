
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : session-edit.scm
;; DESCRIPTION : editing routines for sessions
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic session-edit)
  (:use (dynamic fold-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-session-math-input)
  (:synopsis "Toggle mathematical input in sessions.")
  (:check-mark "v" session-math-input?)
  (session-use-math-input (not (session-math-input?))))

(define session-multiline-input #f)

(tm-define (session-multiline-input?)
  session-multiline-input)

(tm-define (toggle-session-multiline-input)
  (:synopsis "Toggle multi-line input in sessions.")
  (:check-mark "v" session-multiline-input?)
  (set! session-multiline-input (not session-multiline-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-left)
  (:inside input)
  (session-go-left))

(tm-define (kbd-right)
  (:inside input)
  (session-go-right))

(tm-define (kbd-up)
  (:inside input)
  (session-go-up))

(tm-define (kbd-down)
  (:inside input)
  (session-go-down))

(tm-define (kbd-page-up)
  (:inside input)
  (session-go-page-up))

(tm-define (kbd-page-down)
  (:inside input)
  (session-go-page-down))

(tm-define (kbd-remove forward?)
  (:inside input)
  (session-remove forward?))

(tm-define (kbd-tab)
  (:inside input)
  (:require (plugin-supports-completions? (get-env "prog-language")))
  (if (session-complete-try?) (noop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured keyboard editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (traverse-generic-context? t)
  (:case input math)
  #f)

(tm-define (traverse-document-context? t)
  (:case document)
  (:require (tree-is? t :up 'input))
  #f)

(tm-define (traverse-left)
  (:inside input)
  (go-to-remain-inside go-to-previous-word 'input))

(tm-define (traverse-right)
  (:inside input)
  (go-to-remain-inside go-to-next-word 'input))

(tm-define (traverse-up)
  (:inside input)
  (session-go-up))

(tm-define (traverse-down)
  (:inside input)
  (session-go-down))

(tm-define (traverse-previous)
  (:inside input)
  (session-go-up))

(tm-define (traverse-next)
  (:inside input)
  (session-go-down))

(tm-define (structured-generic-context? t)
  (:case input math)
  #f)

(tm-define (structured-left)
  (:inside input)
  (noop))

(tm-define (structured-right)
  (:inside input)
  (noop))

(tm-define (structured-up)
  (:inside input)
  (go-to-remain-inside session-go-up 'session))

(tm-define (structured-down)
  (:inside input)
  (go-to-remain-inside session-go-down 'session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (make-session type name)
  (:argument type "Session type")
  (:default  type "scheme")
  (:argument name "Session name")
  (:default  name "default"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The return key in session input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (session-get-input-string)
  (let* ((t (tree-innermost 'input))
	 (input (tree->stree (tree-ref t 2)))
	 (s (verbatim-serialize (get-env "prog-language") input)))
    (substring s 0 (- (string-length s) 1))))

(define (session-process-input)
  (let ((lan (get-env "prog-language"))
	(ses (get-env "prog-session")))
    (if (plugin-supports-input-done? lan)
	(let* ((s (escape-quotes (session-get-input-string)))
	       (cmd (string-append "(input-done? \"" s "\")"))
	       (r (tree->stree (connection-cmd lan ses cmd))))
	  (if (== r "#f")
	      (insert-return)
	      (process-input)))
	(process-input))))

(tm-define (kbd-return)
  (:inside input)
  (if (session-multiline-input?)
      (insert-return)
      (session-process-input)))

(tm-define (kbd-shift-return)
  (:inside input)
  (if (session-multiline-input?)
      (session-process-input)
      (insert-return)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert forwards?)
  (:inside input)
  (if forwards?
      (session-fold-input)
      (unfold)))

(tm-define (structured-insert-up)
  (:inside input)
  (session-insert-input-above))

(tm-define (structured-insert-down)
  (:inside input)
  (session-insert-input-below))

(tm-define (structured-remove forwards?)
  (:inside input)
  (session-remove-input forwards?))
