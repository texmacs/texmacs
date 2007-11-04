
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODULE      : clisp-init.lisp
;; DESCRIPTION : Initialize clisp plugin
;; COPYRIGHT   : (C) 2004 Michael Graffam
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf tm-data-begin "")
(setf tm-data-end "")

(setf *PROMPT-BODY* "channel:promptCLisp> ")
(setf *PROMPT-FINISH* "")

(load "lisp/tmlib.lisp")
