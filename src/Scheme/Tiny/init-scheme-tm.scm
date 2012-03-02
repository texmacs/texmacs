; initialization for littlescheme
; specific to texmacs


;;(display "initialization for LittleScheme/TeXmacs\n")
;;(load (url-concretize "$TEXMACS_PATH/progs/init-scheme.scm"))

(define (reload) 
 (load (url-concretize "$TEXMACS_PATH/progs/init-scheme-tm.scm")))


(define (module->path name)
  (url-concretize (string-append
  (foldr (lambda (a b) (string-append (atom->string a) 
          "/" (atom->string b))) "$TEXMACS_PATH/progs" name) ".scm")))
  

(define module-list '())

(define (module-name module) (car module))
(define (module-env module) (cadr module))
(define (module-symbols module) (caddr module))
(define (set-module-symbols! module symbols) (set-car! (cddr module) symbols))
(define (set-module-env! module env) (set-car! (cdr module) env))


(define-macro (create-module head . body)
    `(let ((current-module-int (cons ,head (list  () ()))))
	(define (current-module) current-module-int)
    (set! module-list (cons (current-module) module-list))
    ,@body
	;; snapshot current environment
    (let ((bind-symbol (lambda (name) (cons name (eval name))))) 
      (set-module-env! (current-module) (map bind-symbol (module-symbols (current-module))))
    )
	(current-module)))

(define-macro (export . names)
  `(begin 
    (display "Exporting ") (display (quote ,names)) (display "\n")
	(set-module-symbols! (current-module) (append (list ,@names ) (module-symbols (current-module))))))

(define (import-module module)
  (cons 'begin
    (map (lambda (binding) `(define ,(car binding) ,(cadr binding))) 
	     (module-env module))))

(define (load-module name)
  (display "Loading: ") (display name) (display "\n")
  (create-module name
    (load (module->path name))))

(define (resolve-module name)
   (cond
      ((assoc name module-list) => (lambda (x) (display "Module already loaded.\n") x))
	  (#t (load-module name))))
	  
(define (do-use-module name)
  (display "Using module ") (display name) (display "\n")
  (let ((module (resolve-module name)))
    (import-module module)))	 

(define-macro (use-module m) (do-use-module m))

(define-macro (use-modules . l) `(begin ,@(map do-use-module l)))
  
(define-macro (define-public head . body)
  (let ((name (if (pair? head) (car head) head)))
    `(begin (define ,head ,@body) (export (quote ,name)))))

(define (current-module) '(*top-level-module* () ())) 

(define-macro (define-module name) 
  `(when (not (eq? ,name (module-name (current-module)))) 
    (display "Module name mismatch: ") (display ,name ) (display "\n")))

(define re-export export)


(define-macro (inherit-modules . which-list)
  (define (module-exports which)
    (let* ((m (resolve-module which))
	       (m-public (module-symbols m)))
      m-public))
  (let ((l (apply append (map module-exports which-list))))
    `(begin
       (use-modules ,@which-list)
       (re-export ,@l))))
  
;;;;;;;;;; testing

(create-module '(test) 
  (define-public a 10) (define-public b 30) (define c 20)  )

(create-module '(test2) 
  (define-public a1 10) (define-public b1 30) (define c1 20)  )

(display "so far so good\n")


;;;;;; compatibility functions

(define-macro (with label value . body)
  `(let ((,label ,value)) ,@body))


(define map-in-order map)

;;(define (call-with-output-string func) 
;;  (with port (open-output-string "") (func port) (get-output-string port)))

(define (call-with-output-string func) 
  (let ((port (open-output-string ""))) (func port) (get-output-string port)))

  

	