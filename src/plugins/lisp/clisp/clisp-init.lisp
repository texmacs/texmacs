
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : clisp-init.lisp
;; DESCRIPTION : Initialize clisp plugin
;; COPYRIGHT   : (C) 2004 Michael Graffam
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf tm-data-begin "")
(setf tm-data-end "")

(setf *PROMPT-BODY* "channel:promptCLisp> ")
(setf *PROMPT-FINISH* "")

(load "lisp/tmlib.lisp")
