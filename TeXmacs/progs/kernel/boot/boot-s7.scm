
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : boot-s7.scm
;; DESCRIPTION : some global variables, public macros, on-entry, on-exit and
;;               initialization of the TeXmacs module system
;; COPYRIGHT   : (C) 2020  Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (s7-scheme?) #t)
(define has-look-and-feel? (lambda (x) (== x "emacs")))

;; TeXmacs expects Guile's list?, which only holds for proper lists. Like the
;; exported definitions (see define-public), it is bound in the rootlet
(varlet (rootlet) 'list? proper-list?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirect standard output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define original-display display)
(define original-write write)

(define (display . l)
  "display one object on the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-display l)
      (tm-output (display-to-string (car l)))))

(define (write . l)
  "write an object to the standard output or a specified port."
  (if (or (null? l) (not (null? (cdr l))))
      (apply original-write l)
      (tm-output (object->string (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(varlet (rootlet) 'temp-value #f)

;; setup the main modules
(with-let (rootlet)
  (define *texmacs-module* (rootlet))
  ;(define *current-module* (rootlet))
  (define *module-name* '(texmacs))
  (define *modules* (make-hash-table)))

(define *texmacs-user-module* (curlet))
(set! *current-module* *texmacs-user-module*)
(set! *module-name* '(texmacs-user))
(define *exports* '())

(set! (*modules* '(texmacs)) *texmacs-module*)
(set! (*modules* '(texmacs-user)) *texmacs-user-module*)

(define (current-module) *current-module*)

(define-macro (export . symbols)
    `(set! *exports* (append ',symbols *exports*)))

;; s7 caches, for each symbol, the id of the let that holds its most recent
;; local binding, and a lookup skips the lets that are newer than that; lets
;; get increasing ids when they are created. Entering a let with with-let
;; gives it a fresh, highest id. Call this once, right after the kernel is
;; loaded: the user module then becomes newer than the kernel modules, where
;; the kernel symbols are bound, so that lookups of kernel symbols skip it on
;; their way to the rootlet. Do not enter the user module afterwards (hence
;; eval in tm-define-macro): it would become newer than the modules loaded
;; so far, and lookups from their code of the names bound in the user module
;; (such as define, see compat-s7.scm) would scan their environment.
(define (renumber-user-module!)
  (with-let *texmacs-user-module* (curlet)))

(define-macro (with-module module . body)
  `(let ((m ,module)) (with-let m
     (let-temporarily (((*texmacs-module* '*current-module*) (curlet)))
     ,@body))))

;; Public definitions are made in their module and published in the rootlet,
;; which is where the other modules find them. use-modules sees them there,
;; with the same value, and does not copy them (import-bindings!). So the
;; other modules share one binding of each public name, and the user module
;; stays small; lookups of public names skip the (newer) module lets and end
;; in the rootlet. The defining module keeps its own binding, as before: a
;; set! of a public variable there is not seen by the other modules.
;; With TM_PUBLISH_LOG set, publications which replace another value are
;; reported (a name defined public by two modules, or an s7 builtin).
(define publish-log? (getenv "TM_PUBLISH_LOG"))
(define (publish-binding! name value)
  (when (and publish-log?
             (defined? name (rootlet))
             (not (eq? ((rootlet) name) value)))
    (format *stderr* "PUBLISH-REPLACES ~A in ~A~%" name
            ((*texmacs-module* '*current-module*) '*module-name*)))
  (varlet (rootlet) name value))

(define-macro (define-public head . body)
  (let ((name (if (pair? head) (car head) head)))
    `(begin
       (define ,head ,@body)
       (export ,name)
       ,@(if (symbol? name) `((publish-binding! ',name ,name)) '()))))

(define-macro (provide-public head . body)
  (if (or (and (symbol? head) (not (defined? head)))
	  (and (pair? head) (symbol? (car head)) (not (defined? (car head)))))
      `(define-public ,head ,@body)
      '(noop)))

(define-macro (define-public-macro head . body)
  (let ((name (if (pair? head) (car head) head)))
    `(begin
       (define-macro ,head ,@body)
       (export ,name)
       ,@(if (symbol? name) `((publish-binding! ',name ,name)) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Module handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module-available? module)
  (if (hash-table-ref *modules* module) #t #f))

(define (list->module module)
  (let* ((aux (lambda (s) (string-append "/" (symbol->string s))))
     (name* (apply string-append (map aux module)))
     (name (substring name* 1 (string-length name*)))
     (u (url-unix "$GUILE_LOAD_PATH" (string-append name ".scm")))
     ;; FIXME: should use %load-path instead of $GUILE_LOAD_PATH
     )
    (url-materialize u "r")))

(define (module-load module)
  (if (list? module)
      (let ((module-file (list->module module))
             (loaded (hash-table-ref *modules* module)))
    (when (not loaded)
        ;;(display "TeXmacs] Loading module ") (display module) (display "\n")
        (with-module (sublet (hash-table-ref *modules* '(texmacs-user))
                             '*exports* ()
                             '*module-file* module-file)
                (load *module-file* (curlet)))))))

(define (module-provide module)
  (if (not (module-available? module)) (module-load module)))

(define (resolve-module module)
  (module-provide module)
    (hash-table-ref *modules* module))

;; s7 (since version 11) refuses to varlet a symbol which is already bound
;; in the target let, so we update existing bindings in place
;; A binding which target already sees, with the same value, through its
;; outlets (typically a public definition, published in the rootlet) is not
;; copied: the copy would not change what lookups return, and it would move
;; the symbol's cached binding into target, so that lookups from elsewhere
;; would have to scan
(define (import-bindings! target entries)
  (for-each (lambda (entry)
              (cond ((defined? (car entry) target #t)
                     (let-set! target (car entry) (cdr entry)))
                    ((and (defined? (car entry) target)
                          (eq? (let-ref target (car entry)) (cdr entry)))
                     (noop))
                    (else
                     (varlet target (car entry) (cdr entry)))))
            entries))

(define-macro (use-modules . modules)
  `(map (lambda (module)
    (let* ((m (resolve-module module))
           (ex (m '*exports*))
           (exx (map (lambda (entry) (if (member (car entry) ex) entry (values))) m)))
        (import-bindings! (*texmacs-module* '*current-module*) exx)))
      ',modules))

(define-macro (import-from . modules)
  `(use-modules ,@modules))

(define-macro (re-export . symbols)
  `(export ,@symbols))

;; The exports of the inherited modules are collected when the form is
;; evaluated, not when it is expanded: macros are expanded at read time, and
;; since s7 11 loading a file during the expansion silently ends the load of
;; the file being read
(define (re-export-modules! which-list)
  (let ((cur (*texmacs-module* '*current-module*))
        (l (apply append
                  (map (lambda (which) ((resolve-module which) '*exports*))
                       which-list))))
    (let-set! cur '*exports* (append l (cur '*exports*)))))

(define-macro (inherit-modules . which-list)
  `(begin
     (use-modules ,@which-list)
     (re-export-modules! ',which-list)))

(define-macro (texmacs-module name . options)
  (#_define (transform action)
    (cond ((not (pair? action)) (noop))
	  ((equal? (car action) :use) (cons 'use-modules (cdr action)))
	  ((equal? (car action) :inherit) (cons 'inherit-modules (cdr action)))
	  ((equal? (car action) :export)
	   (display "Warning] The option :export is no longer supported\n")
	   (display "       ] Please use tm-define instead\n"))
	  (else '(noop))))
  (let ((l (map transform options)))
    ;;(display "loading ") (display name) (display "\n")
    `(begin
        (define *module-name* ',name)
        (define *exports* ())
        (hash-table-set! *modules* ',name (current-module))
       ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; On-entry and on-exit macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (quit-TeXmacs-scheme) (noop))

(define-macro (on-entry . cmd)
  `(begin ,@cmd))

(define-macro (on-exit . cmd)
  `(set! quit-TeXmacs-scheme (lambda () ,@cmd (,quit-TeXmacs-scheme))))

