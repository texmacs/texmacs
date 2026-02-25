;; Generic test helpers — prepended to every scenario script.
;; No server connection required.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fail! label msg)
  (display* "  TM FAIL [" label "]: " msg "\n")
  (quit-TeXmacs-code 1))

(define (pass! label)
  (display* "  TM OK [" label "]\n"))

(define (check! label ok? msg) (if ok? (pass! label) (fail! label msg)))

(define (on-error label)
  (lambda (err)
    (fail! label (if (string? err) err (object->string err)))))
