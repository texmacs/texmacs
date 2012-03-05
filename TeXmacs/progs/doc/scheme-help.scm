
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-help.scm
;; DESCRIPTION : Automatically generated scheme documentation
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc scheme-help)
  (:use (generic generic-menu)))  ; FIXME: set this to the real dependency


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A first attempt at automatically generated documentation.
;; TODO: (among many other things)
;;  - Prettier display of results.
;;  - Add list of modules and functions per module.
;;  - Directly jump to the line of definition of a function.
;;  - Make the names of the functions links to special pages with synopsis,
;;    function definition, etc.
;;  - When clicking on a module, parse the scheme file loaded and make defined
;;    symbols clickable.
;;  - Devise a sensible method of documenting modules (keyword in texmacs-module ?)
;;  - ...
;;


; A hash of character -> (list of symbols starting with that character)
(define indexed-commands (make-ahash-table)) 

(define (symbols>? x y)
  (string>? (symbol->string x) (symbol->string y)))

(define (append-command-alphabetically cmd)
  "Used to build the hash @indexed-commands from entries in @tm-defined-name"
  (with _cmd (symbol->string cmd)
    (with _first (substring _cmd 0 1)
      ; It would be nicer to avoid the check, but ahash-ref returns #f if the
      ; entry is empty and we would have improper lists.
      (if (ahash-ref indexed-commands _first)
          (ahash-set! indexed-commands _first
                      (cons _cmd (ahash-ref indexed-commands _first)))
          (ahash-set! indexed-commands _first (list _cmd))))))


(define (module->$link l)
  "Builds a link to a module. The argument @l is a list like (generic generic-menu)"
  ($link
    (string-concatenate (list (url-concretize "$TEXMACS_PATH/progs/")
                              (string-join (map symbol->string l) "/")
                              ".scm"))
    (string-join (map symbol->string l) ".") ))

;; See progs/doc/docgrep.scm for more examples of usage of $blah macros
(define (links-to-modules-of cmd)
  "Builds links to all texmacs-modules where @cmd is defined"
  ($para ($strong `(scm ,cmd)) " is defined in " $lf
    `(indent ,($para
      ($for (mods (ahash-ref tm-defined-module (string->symbol cmd)))
         (module->$link mods) ", ")))))

(define (output-commands-folded-section handle)
  ($folded-documentation (car handle)
    ($para
      ($for (cmd (cdr handle))
          (links-to-modules-of cmd)))))

(define (sort-by-car a b) 
  (string<? (car a) (car b)))

(define (all-commands-index)
  (begin
    ; HACK: sorting here sorts the values of ahash-table->list
    (map append-command-alphabetically 
         (sort (map car (ahash-table->list tm-defined-table)) symbols>?))
    (let 
      ((sorted (sort (reverse (ahash-table->list indexed-commands)) sort-by-car)))
      ($tmdoc
        ($tmdoc-title "All TeXmacs commands")
          ($para 
              "This is an alphabetical list of TeXmacs's public interface. "
              "Because of contextual overloading, a name can be defined several "
              "times in the code. Click on a module name to open it.")
          ($para
            ($for (entry sorted)
              (output-commands-folded-section entry)))))))                          


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(tm-define (help-all-scheme-commands)
  (:synopsis "Opens a help buffer with a list of all commands defined using tm-define")
  (cursor-history-add (cursor-path))
  (set-help-buffer "All commands, alphabetically" (all-commands-index)))

