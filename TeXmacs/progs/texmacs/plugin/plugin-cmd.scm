
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plugin-cmd.scm
;; DESCRIPTION : Commanding applications from TeXmacs and vice versa
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs plugin plugin-cmd)
  (:export pre-serialize verbatim-serialize generic-serialize plugin-serialize
	   plugin-serializer-set! format-command plugin-commander-set!
	   plugin-eval
	   plugin-supports-completions-set! plugin-supports-completions?
	   plugin-supports-input-done-set! plugin-supports-input-done?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-serializer (make-ahash-table))

(define (pre-serialize lan t)
  (cond ((func? t 'document 1) (pre-serialize lan (cadr t)))
	((and (func? t 'var_expand 2) (== (cadr t) "math"))
	 (pre-serialize lan (plugin-math-input (list 'tuple lan (caddr t)))))
	((func? t 'math 1)
	 (pre-serialize lan (plugin-math-input (list 'tuple lan (cadr t)))))
	(else t)))

(define (verbatim-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append
     (escape-verbatim (texmacs->verbatim (object->tree u))) "\n")))

(define (generic-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append (char->string #\002) "verbatim:"
		   (escape-generic (texmacs->verbatim (object->tree u)))
		   (char->string #\005))))

(define (plugin-serialize lan t)
  (with fun (ahash-ref plugin-serializer lan)
    (if fun
	(fun lan t)
	(verbatim-serialize lan t))))

(define (plugin-serializer-set! lan val)
  (ahash-set! plugin-serializer lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-commander (make-ahash-table))

(define (default-format-command s)
  (string-append (char->string #\020) s "\n"))

(define (format-command lan s)
  (with fun (ahash-ref plugin-commander lan)
    (if fun
	(fun s)
	(default-format-command s))))

(define (plugin-commander-set! lan val)
  (ahash-set! plugin-commander lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation + simplification of documents with one paragraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-output-simplify t)
  (cond ((func? t 'document 1) (plugin-output-simplify (cadr t)))
	((func? t 'with) (rcons (cDr t) (plugin-output-simplify (cAr t))))
	(else t)))

(define (plugin-eval name session t)
  (let* ((u (connection-eval name session (object->tree t)))
	 (v (plugin-output-simplify (tree->object u))))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-completions (make-ahash-table))

(define (plugin-supports-completions-set! key)
  (ahash-set! plugin-supports-completions key #t))

(define (plugin-supports-completions? key)
  (ahash-ref plugin-supports-completions key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing whether more input is needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-input-done (make-ahash-table))

(define (plugin-supports-input-done-set! key)
  (ahash-set! plugin-supports-input-done key #t))

(define (plugin-supports-input-done? key)
  (ahash-ref plugin-supports-input-done key))
