
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : extra.scm
;; DESCRIPTION : miscellaneous extra functions and macros
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot extra))

(define-public (list-break l pred?)
  "Break @l at the first element satisfying @pred?."
  ;; Adaptation of "break" (SRFI-26)
  (let rec ((l l))
    (cond ((null? l) (values '() '()))
	  ((pred? (car l)) (values '() l))
	  (else (receive (first last) (rec (cdr l))
		  (values (cons (car l) first) last))))))

(define-public (quit-TeXmacs-scheme) (noop))

(define-public-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-public-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))
