
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrevs.scm
;; DESCRIPTION : useful abbreviations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot abbrevs)
  (:export == with tree->list))

(define == equal?)

(define-macro (with var val . body)
  (if (pair? var)
      `(apply (lambda ,var ,@body) ,val)
      `(let ((,var ,val)) ,@body)))

(define (tree->list t)
  (cons (tree-get-label t) (tree-get-children t)))
