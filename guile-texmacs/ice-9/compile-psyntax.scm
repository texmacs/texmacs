(use-modules (ice-9 syncase))

;; XXX - We need to be inside (ice-9 syncase) since psyntax.ss calls
;; `eval' int he `interaction-environment' aka the current module and
;; it expects to have `andmap' there.  The reason for this escapes me
;; at the moment.
;;
(define-module (ice-9 syncase))

(define source (list-ref (command-line) 1))
(define target (list-ref (command-line) 2))

(let ((in (open-input-file source))
      (out (open-output-file (string-append target ".tmp"))))
  (with-fluids ((expansion-eval-closure
		 (module-eval-closure (current-module))))
    (let loop ((x (read in)))
      (if (eof-object? x)
	  (begin
	    (close-port out)
	    (close-port in))
	  (begin
	    (write (sc-expand3 x 'c '(compile load eval)) out)
	    (newline out)
	    (loop (read in)))))))

(system (format #f "mv -f ~s.tmp ~s" target target))
