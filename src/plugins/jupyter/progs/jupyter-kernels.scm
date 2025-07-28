(texmacs-module (jupyter-kernels))

(define jupyter-kernel-table (make-ahash-table))


(tm-define (jupyter-register-kernel uuid lan)
   (ahash-set! jupyter-kernel-table uuid lan))
   
(tm-define (jupyter-list-kernels)
  (ahash-table->list jupyter-kernel-table))

;; `make-session` insert a session with a temporary banner
;; it calls `session-feed` to start the plugin and direct the first output to the banner area

;; if input is sent from a session without a running plugin a new process is also started, from `field-process-input` using `session-feed`

;; store a list of session tree paths together with kernel uuids that are running their commands
;; change the input submission routines (`session-feed`?) to check for the kernel uuid of the corresponding session element

;; How to deal with exec folds and script fields? `script-feed` is called from `alternate-toggle` in a `script-input` tag (exec fold) and from  `script-modified-eval` in a `script-eval tag (script field)

;; implement input done?
