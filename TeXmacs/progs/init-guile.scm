
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-guile.scm
;; DESCRIPTION : Guile-specific start of the initialization
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The initialization file of TeXmacs on Guile (scheme_init_file in
;; guile_tm.cpp). It sets up Guile, loads the module system and the kernel's
;; compatibility module, then the common init-kernel.scm and init-texmacs.scm.

(define boot-start (texmacs-time))

(cond ((os-mingw?)
       (debug-set! stack 0))
      ((os-macos?)
       (debug-set! stack 2000000))
      (else
       (debug-set! stack 1000000)))

(define developer-mode?
  (equal? (cpp-get-preference "developer tool" "off") "on"))

(if developer-mode?
    (debug-enable 'backtrace 'debug))

(define (%new-read-hook sym) (noop)) ; for autocompletion

(define-public macro-keywords '(define-macro define-public-macro 
                                tm-define-macro))
(define-public def-keywords
  `(define-public provide-public
    tm-define tm-menu menu-bind tm-widget ,@macro-keywords))

(define old-read read)
(define (new-read port)
  "A redefined reader which stores line number and file name in symbols."
  ;; FIXME: handle overloaded definitions
  (let ((form (old-read port)))
    (if (and (pair? form) (member (car form) def-keywords))
        (let* ((l (source-property form 'line))
               (c (source-property form 'column))
               (f (source-property form 'filename))
               (sym  (if (pair? (cadr form)) (caadr form) (cadr form))))
          (if (symbol? sym) ; Just in case
              (let ((old (or (symbol-property sym 'defs) '()))
                    (new `(,f ,l ,c)))
                (%new-read-hook sym)
                (if (and (member (car form) macro-keywords)
                         (not (member sym def-keywords)))
                    (set! def-keywords (cons sym def-keywords)))
                (if (not (member new old))
                    (set-symbol-property! sym 'defs (cons new old)))))))
    form))

(define old-primitive-load primitive-load)
(define (new-primitive-load filename)
  (if (member (scheme-dialect) (list "guile-a" "guile-b"))
      (old-primitive-load filename)
      ;; We explicitly circumvent guile's decision to set the current-reader
      ;; to #f inside ice-9/boot-9.scm, try-module-autoload
      (with-fluids ((current-reader read))
                   (old-primitive-load filename))))

(if developer-mode?
    (begin
      (module-export! (current-module)
                      '(%new-read-hook old-read new-read def-keywords))
      (set! read new-read)
      (module-export! (current-module)
                      '(old-primitive-load new-primitive-load))
      (set! primitive-load new-primitive-load)))

;; TODO: scheme file caching using (set! primitive-load ...) and
;; (set! %search-load-path)

;;(debug-enable 'backtrace 'debug)
;; (define load-indent 0)
;; (define old-primitive-load primitive-load)
;; (define (new-primitive-load . x)
;;   (for-each display (make-list load-indent "  "))
;;   (display "Load ") (apply display x) (display "\n")
;;   (set! load-indent (+ load-indent 1))
;;   (apply old-primitive-load x)
;;   (set! load-indent (- load-indent 1))
;;   (for-each display (make-list load-indent "  "))
;;   (display "Done\n"))
;; (set! primitive-load new-primitive-load)

;(display "Booting TeXmacs kernel functionality\n")
(if (and (os-mingw?) (string= (gui-version) "qt4"))
    (load "kernel/boot/boot.scm")
    (load (url-concretize "$TEXMACS_PATH/progs/kernel/boot/boot.scm")))
(inherit-modules (kernel boot compat))

;; The common initialization
(define (load-init-file name)
  (if (and (os-mingw?) (string= (gui-version) "qt4"))
      (load name)
      (load (url-concretize (string-append "$TEXMACS_PATH/progs/" name)))))
(load-init-file "init-kernel.scm")
(load-init-file "init-texmacs.scm")
