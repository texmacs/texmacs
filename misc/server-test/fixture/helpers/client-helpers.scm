;; Client-side test helpers — prepended to feature/load scenario scripts.
;; Assumes test-helpers.scm (fail!, pass!, check!, on-error) is already loaded.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Server-side evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Execute a Scheme datum server-side via remote-eval (requires admin login).
;; expr — a Scheme datum (list) to evaluate on the server.
;; cont — called with the return value on success; errors call fail!.
(define (server-eval server expr cont)
  (client-remote-eval server
    (list 'remote-eval expr)
    cont
    (on-error "server-eval")))

;; (setup-test server form ...)
;;
;; Execute fixture forms on the server (fire-and-forget).
;; The body is quasiquoted — use ,var to splice client-side values.
;; Errors call fail! and abort the test.
;;
;; Example:
;;   (define alice (string-append "alice-" test-seed))
;;   (setup-test server
;;     (fixture-create-account ,alice "Alice" "TeXmacs123!" "a@h" #f)
;;     (let ((rid (fixture-create-file ,alice "f.tm" '())))
;;       (fixture-share ,alice "admin" rid "localhost")))
(define-macro (setup-test server . body)
  (list 'server-eval server (list 'quasiquote (cons 'begin body)) '(lambda (_) #t)))

;; (with-server-eval r server expr body ...)
;;
;; Evaluate expr server-side and bind the result to r in body.
;; r may be a symbol or a list for destructuring (via `with`).
;; expr is a Scheme datum — build it with quasiquote when client-side
;; values need to be embedded.
;;
;; Example:
;;   (define alice (string-append "alice-" test-seed))
;;   (with-server-eval (uid file-rid) server
;;     `(with-user #t
;;        (list (server-find-user ,alice)
;;              (car (db-search '(("type" "file"))))))
;;     (check! "alice-exists" uid "uid is #f"))
(define-macro (with-server-eval r server expr . body)
  `(server-eval ,server ,expr
                (lambda (msg) (with ,r msg ,@body))))

(define (timeout) (display* "TeXmacs test timed out.\n") (quit-TeXmacs-code 1))
