
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : base.scm
;; DESCRIPTION : important base routines
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (tools base))

(define-macro (define-public-macro head . body)
  `(define-public ,(car head)
     ;; FIXME: why can't we use procedure->macro for a non-memoizing variant?
     (procedure->memoizing-macro
      (lambda (cmd env)
	(apply (lambda ,(cdr head) ,@body) (cdr cmd))))))

(export define-public-macro)
