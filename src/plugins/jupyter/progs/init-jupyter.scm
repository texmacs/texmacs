
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-jupyter.scm
;; DESCRIPTION : Initialize Jupyter plugin
;; COPYRIGHT   : (C) 2019  Massimiliano Gubinelli
;; COPYRIGHT   : (C) 2021  Jeroen Wouters
;; COPYRIGHT   : (C) 2025  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with Jupyter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (check-jupyter?)
  (and (url-exists-in-path? (python-command))
       (with cmd (string-append (python-command)
                                " -m pip show jupyter_client 2> /dev/null")
         (> (string-length (var-eval-system cmd)) 10))))

(plugin-configure jupyter  ;; defines supports-jupyter?
  (:require (check-jupyter?)))

(define (jupyter-find-kernels)
  (let* ((u "$TEXMACS_PATH/plugins/jupyter/tm_jupyter/kernels.py")
         (cmd (string-append (python-command) " " (system-url->string u)))
         (val (eval-system cmd))
         (l (string-split (string-replace val "\r" "") #\newline)))     
    (list-filter l (lambda (k) (!= "" k)))))

(tm-define (jupyter-launcher kernel)
  (let* ((u "$TEXMACS_PATH/plugins/jupyter/tm_jupyter/client.py")
         (cmd (string-append (python-command) " " (system-url->string u))))
    (if (== kernel "") cmd (string-append cmd " --kernel=" kernel))))

;;(display* "Jupyter -> " (supports-jupyter?) "\n")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugin and kernel name conversion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (jupyter->plugin session)
  ;; FIXME: there should really be a mechanism for plugins to customise
  ;; the way the `prog-lang` environment variable is derived from
  ;; the plugin and session name
  (with s (tm->string session)
    (cond ((string-starts? s "python") "python")
          ((string-starts? s "julia") "julia")
          ((string-starts? s "ir") "r")
          ((== s "default") "python")
          (else s))))

(tm-define (plugin->jupyter-prefix lan)
  ;; TODO: add more lanuages; put in a table
  (cond ((== lan "python") "python")
        ((== lan "julia") "julia")
        ((== lan "r") "ir")
        (else #f)))

(tm-define (plugin->jupyter lang)
  ;; TODO: Should not return just the first match      
  (and-with pref (plugin->jupyter-prefix lang)
    (with l (filter (lambda (k) (string-starts? k pref))
                    (jupyter-find-kernels))
      (and (nnull? l) (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle native plugins that can also be run through Jupyter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (run-via-jupyter? lan)
  (and lan (with pref (string-append lan "-via-jupyter")
             (get-boolean-preference pref))))

(tm-define (run-via-jupyter lan enable?)
  (with pref (string-append lan "-via-jupyter")
    (set-boolean-preference pref enable?)
    (reinit-plugin-single lan)))

(set! alt-launcher
      (lambda (langname)
        (let ((kernel (plugin->jupyter langname)))
          (if (and kernel (run-via-jupyter? (jupyter->plugin kernel)))
              (jupyter-launcher kernel)
              #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle plugins that are only supported through Jupyter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (jupyter-serialize lan t)
    (with u (pre-serialize lan t)
      (with s (texmacs->code (stree->tree u) "SourceCode")
        (string-append s "\n<EOF>\n"))))

(tm-define (make-jupyter-launcher kernel)
  (with lan (jupyter->plugin kernel)
    `(plugin-configure ,(string->symbol lan)
       (:require (supports-jupyter?))
       (:launch ,(jupyter-launcher kernel))
       (:serializer ,(list 'unquote 'jupyter-serialize))
       (:session ,(upcase-first lan))
       (:tab-completion #t))))

(define (kernel-ok? kernel)
  (with lan (jupyter->plugin kernel)
    (when (string? lan) (set! lan (string->symbol lan)))
    (nin? lan (plugin-list))))

(define ok-kernels
  (list-filter (jupyter-find-kernels) kernel-ok?))

(define-public-macro (make-jupyter-launchers dummy)
  `(begin ,@(map make-jupyter-launcher ok-kernels)))

(make-jupyter-launchers #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load further tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(display* "kernels = " ok-kernels "\n")

(when (supports-jupyter?)
  (import-from (jupyter-kernels)))
