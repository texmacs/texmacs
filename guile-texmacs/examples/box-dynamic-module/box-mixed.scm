;;; examples/box-dynamic-module/box-mixed.scm -- Scheme module using some
;;;   functionality from the shared library libbox-module, but do not
;;;   export procedures from the module.

;;; Commentary:

;;; This is the Scheme module box-mixed.  It uses some functionality
;;; from the shared library libbox-module, but does not export it.

;;; Code:

;;; Author: Thomas Wawrzinek
;;; Date: 2001-06-08
;;; Changed: 2001-06-14 by martin, some commenting, cleanup and integration.

(define-module (box-mixed))

;; First, load the library.
;;
(load-extension "libbox-module" "scm_init_box")

;; Create a list of boxes, each containing one element from ARGS.
;;
(define (make-box-list . args)
  (map (lambda (el)
	 (let ((b (make-box)))
	   (box-set! b el) b))
       args))

;; Map the procedure FUNC over all elements of LST, which must be a
;; list of boxes.  The result is a list of freshly allocated boxes,
;; each containing the result of an application of FUNC.
(define (box-map func lst)
  (map (lambda (el)
	 (let ((b (make-box)))
	   (box-set! b (func (box-ref el)))
	   b))
       lst))

;; Export the procedures, so that they can be used by others.
;;
(export make-box-list box-map)

;;; End of file.
