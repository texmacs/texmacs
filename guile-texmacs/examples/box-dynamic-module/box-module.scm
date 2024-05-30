;;; examples/box-dynamic-module/box-module.scm -- Scheme module exporting
;;;   some functionality from the shared library libbox-module.

;;; Commentary:

;;; This is the Scheme part of the dynamic library module (box-module).
;;; When you do a (use-modules (box-module)) in this directory,
;;; this file gets loaded and will load the compiled extension.

;;; Code:

;;; Author: Martin Grabmueller
;;; Date: 2001-06-06

(define-module (box-module))

;; First, load the library.
;;
(load-extension "libbox-module" "scm_init_box")

;; Then export the procedures which should be visible to module users.
;;
(export make-box box-ref box-set!)

;;; End of file.
