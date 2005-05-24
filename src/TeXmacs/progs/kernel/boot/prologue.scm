
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prologue.scm
;; DESCRIPTION : subroutines which are not well implemented in guile
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot prologue)
  (:use (kernel boot ahash-table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional support for loading modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define module-loaded-table (make-ahash-table))

(define-public (list->module module)
  (let* ((aux (lambda (s) (string-append "/" (symbol->string s))))
	 (name* (apply string-append (map aux module)))
	 (name (substring name* 1 (string-length name*)))
	 (u (url "$GUILE_LOAD_PATH" (string-append name ".scm"))))
    (url-materialize u "r")))

(define-public (module-load module*)
  (if (list? module*)
      (let* ((module (list->module module*))
	     (loaded (ahash-ref module-loaded-table module)))
	(ahash-set! module-loaded-table module #t)
	;(if (not loaded) (display* "TeXmacs] Loading module " module* "\n"))
	(if (not loaded) (load-module module)))))

;; FIXME: why does this not work?
;(define-public (module-load name)
;  (module-use! (current-module) (resolve-module name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Work around broken 'symbol-property'
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define symbol-procedure-table (make-ahash-table))
(define symbol-property-table (make-ahash-table))

(define-public (set-symbol-procedure! symb proc)
  (ahash-set! symbol-procedure-table symb proc))

(define-public (symbol-procedure symb)
  (ahash-ref symbol-procedure-table symb))

(define-public (set-symbol-prop! symb prop val)
  (ahash-set! symbol-property-table (list symb prop) val))

(define-public (symbol-prop symb prop)
  (ahash-ref symbol-property-table (list symb prop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sorting lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-sort-insert x l comp?)
  (cond ((null? l) (list x))
	((comp? x (car l)) (cons x l))
	(else (cons (car l) (list-sort-insert x (cdr l) comp?)))))

(define-public (list-sort l comp?)
  "Sort @l using the comparison @comp?."
  ;; Should be replaced by built-in 'sort' routine later on (Guile > 1.3.4)
  (if (null? l) l
      (let ((r (list-sort (cdr l) comp?)))
	(list-sort-insert (car l) r comp?))))
