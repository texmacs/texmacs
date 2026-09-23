
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-define-test.scm
;; DESCRIPTION : Test suite for tm-define
;; COPYRIGHT   : (C) 2021  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-define-test)
  (:use (kernel texmacs tm-define)))

(define (regtest-procedure-name)
  (regression-test-group
   "procedure" "procedure"
   procedure-name :none
   (test "procedures defined via define-public" string->float string->float)
   (test "procedures defined via glue symbols" utf8->cork utf8->cork)
   (test "procedures defined via tm-define" regtest-tm-define regtest-tm-define)
   (test "invalid input" 1 #f)))

(define (regtest-procedure-symbol-name)
  (regression-test-group
   "procedure" "symbol"
   procedure-symbol-name :none
   (test "glue procedure" system 'system)
   (test "tm-defined" exec-interactive-command
                      'exec-interactive-command)
   (test "anonymous function" (lambda (x) (+ x 1)) #f)
   (test "invalid input" 1 #f)))


(define (regtest-procedure-symbol-name-more)
  (regression-test-group
   "procedure" "symbol, more"
   procedure-symbol-name :none
   (test "glue procedure with punctuation" url-exists? 'url-exists?)
   (test "glue procedure with an arrow" utf8->cork 'utf8->cork)
   (test "symbol" 'foo 'foo)
   (test "string" "foo" 'foo)
   (test "mode predicate" in-math? 'in-math?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloading, properties and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmdt-sign x) 'other)
(tm-define (tmdt-sign x)
  (:require (> x 0))
  'positive)
(tm-define (tmdt-sign x)
  (:require (< x 0))
  (list 'negative (former x)))

(tm-define (tmdt-prop x y)
  (:synopsis "Test synopsis")
  (:argument x "First")
  (:argument y "Second")
  (list x y))

(tm-define (tmdt-plain x) x)
(tm-property (tmdt-plain x) (:interactive #t))

(tm-define-macro (tmdt-swap a b)
  (:synopsis "Swap two expressions")
  `(list ,b ,a))

(define (regtest-tm-define-overloading)
  (regression-test-group
   "tm-define" "overloading"
   :none :none
   (test "no condition applies" (tmdt-sign 0) 'other)
   (test "later definition with a true condition" (tmdt-sign 1) 'positive)
   (test "former calls the previous definition" (tmdt-sign -1)
         '(negative other))
   (test "tm-defined procedures are global"
         (procedure? (with-let (rootlet) tmdt-sign)) #t)
   (test "all definitions are recorded"
         (length (ahash-ref tm-defined-table 'tmdt-sign)) 3)
   (test "the defining module is recorded"
         (car (ahash-ref tm-defined-module 'tmdt-sign))
         '(kernel texmacs tm-define-test))))

(define (regtest-tm-define-properties)
  (regression-test-group
   "tm-define" "properties"
   :none :none
   (test "synopsis of a procedure" (property tmdt-prop :synopsis)
         '("Test synopsis"))
   (test "synopsis of a symbol" (property 'tmdt-prop :synopsis)
         '("Test synopsis"))
   (test "arguments" (property tmdt-prop :arguments) '(x y))
   (test "missing property" (property tmdt-plain :synopsis) #f)
   (test "tm-property" (property tmdt-plain :interactive) '(#t))
   (test "compute-interactive-args"
         (compute-interactive-args tmdt-prop)
         '(("First" "string") ("Second" "string")))
   (test "interactive-title from the synopsis" (interactive-title tmdt-prop)
         "Test synopsis")
   (test "interactive-title from the name" (interactive-title tmdt-plain)
         "Interactive command 'tmdt-plain'")
   (test "tm-define-macro" (tmdt-swap 1 2) '(2 1))
   (test "tm-define-macro in a quasiquoted template"
         (let ((x 1)) `(tmdt-swap ,x 2)) '(tmdt-swap 1 2))))

(define (regtest-tm-define-modes)
  (regression-test-group
   "tm-define" "modes"
   :none :none
   (test "texmacs-mode-mode of a symbol" (texmacs-mode-mode 'in-math?) 'in-math%)
   (test "texmacs-mode-mode of a predicate" (texmacs-mode-mode in-math?)
         'in-math%)
   (test "texmacs-mode-mode of an anonymous procedure"
         (texmacs-mode-mode (lambda () #t)) 'unknown%)
   (test "mode predicates are procedures" (procedure? in-text?) #t)
   (test "texmacs-submode?" (texmacs-submode? 'in-math% 'always%) #t)))

(tm-define (regtest-tm-define)
  (let ((n (+ (regtest-procedure-name)
              (regtest-procedure-symbol-name)
              (regtest-procedure-symbol-name-more)
              (regtest-tm-define-overloading)
              (regtest-tm-define-properties)
              (regtest-tm-define-modes))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of tm-define: ok\n")))
