
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-keywords.scm
;; DESCRIPTION : special keywords for scheme programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-keywords)
  (:use (utils misc tm-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scheme-keyword-arity-table* (make-ahash-table))

(define (scheme-keyword-set-arity* x nr)
  (cond ((symbol? x) (scheme-keyword-set-arity* (symbol->string x) nr))
	((string? x) (ahash-set! scheme-keyword-arity-table* x nr))
	((list? x) (for-each (cut scheme-keyword-set-arity* <> nr) x))))

(scheme-keyword-set-arity* nullary-indent 0)
(scheme-keyword-set-arity* unary-indent   1)
(scheme-keyword-set-arity* binary-indent  2)
(scheme-keyword-set-arity* ternary-indent 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (scheme-keyword-get-arity* s)
  (:synopsis "get number of arguments before body of the scheme keyword @s")
  (if (symbol? s)
      (scheme-keyword-get-arity* (symbol->string s))
      (ahash-ref scheme-keyword-arity-table* s)))
