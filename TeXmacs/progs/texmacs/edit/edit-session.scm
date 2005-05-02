
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edit-session.scm
;; DESCRIPTION : editing routines for sessions
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-session))

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
;; The return key in session input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (session-get-input-string)
  (let* ((p (search-upwards "input"))
	 (input (tree->stree (tm-subtree (rcons p 2))))
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

(tm-define (session-return)
  (:synopsis "Pressing return in session input.")
  (if (session-multiline-input?)
      (insert-return)
      (session-process-input)))

(tm-define (session-shift-return)
  (:synopsis "Pressing shift-return in session input.")
  (if (session-multiline-input?)
      (session-process-input)
      (insert-return)))
