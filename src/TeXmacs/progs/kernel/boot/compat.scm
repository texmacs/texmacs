
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat.scm
;; DESCRIPTION : for compatability
;; COPYRIGHT   : (C) 2003  David Allouche, Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot compat)
  (:export exec-file))

(debug-set! stack 1000000)

;;; make eval from guile>=1.6.0 backwards compatible
(catch 'wrong-number-of-args
       (lambda () (eval 1))
       (lambda arg
	 (let ((default-eval eval))
	   (set! eval (lambda (form . env)
			(cond ((null? form) (list))
			      ((null? env) (primitive-eval form))
			      (else (default-eval form (car env)))))))))

;;; for old-style initialization files
(define (exec-file . args)
  (noop))
