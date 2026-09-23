
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-autocomplete.scm
;; DESCRIPTION : Autocompletion in scheme sessions
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; We provide very rudimentary autocompletion in scheme sessions using a prefix
;; tree of all publicly defined symbols. Things TO-DO are:
;;
;;  - Truly index all code with an indexer, instead of patching scheme's read.
;;  - Be aware of context: create new ptrees on the fly based on the
;;    environment. This needs online parsing and will be difficult. 
;;  - Suggest parameters in function calls.
;;  - Provide an alternative interface using (non-modal) popups or greyed out
;;    text after the cursor.
;;  - Add a layer decoupling from the specific scheme implementation for
;;    better portability.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-autocomplete)
  (:use (utils library ptrees) (prog glue-symbols)))

(define completions (make-ptree))

; Subroutine for all-used-symbols
(define (obarray-fold-sub module prev)
  (with mi (module-public-interface module)
    (append
      (map 
        (lambda (x) (symbol->string (car x)))
        (list-filter
          (ahash-table->list (module-obarray module))
          (lambda (x) 
            (or (not mi) (!= #f (module-local-variable mi (car x)))))))
      prev)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public scheme-completions-built? #f)

; redefine
(tm-define (all-used-modules) *modules*)
; anyway the previous definition was wrong.
; (tm-define (all-used-modules)
;  (cons (current-module) (module-uses (current-module))))

;;FIXME: this function is implementation dependent, move it in the compat layer
;;also move some other parts of this module
(tm-define (all-used-symbols)
  (map symbol->string (append
     ;; add tm-defined symbols
     (map car tm-defined-table)
    ;; add all other exported symbols
     (apply append (map (lambda (m)
       (let ((e ((cdr m) '*exports*)))
         (if (undefined? e) (values) e))) *modules*)))))

;(tm-define (all-used-symbols)
;  (list-fold obarray-fold-sub '() (all-used-modules)))

(tm-define (scheme-completions-add str)
  (set! completions (pt-add completions str)))

(tm-define (scheme-completions-add-list lst)
  (set! completions (pt-add-list completions lst)))

(tm-define (scheme-completions-rebuild)
  (display "Texmacs] Populating autocompletions tree... ")
  (let ((start (texmacs-time))
        (symbols (append (all-used-symbols) (all-glued-symbols))))
    (scheme-completions-add-list symbols)
    (display* (length symbols) " symbols in "
              (- (texmacs-time) start) " msec\n")
    (set! scheme-completions-built? #t)))

(tm-define (scheme-completions-dump)
  (pt-words-below (pt-find completions "")))

(tm-define (scheme-completions root)
  (:synopsis "Provide the completions for @root as needed by custom-complete")
  `(tuple ,root 
     ,@(map string->tmstring 
            (pt-words-below (pt-find completions (tmstring->string root))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for new-read. See init-texmacs.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%read-symbol-hook sym)
  (scheme-completions-add (symbol->string sym)))

(if developer-mode? (set! %new-read-hook %read-symbol-hook ))
