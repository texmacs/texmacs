;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : sapi.scm
;; DESCRIPTION : Automatically generated scheme documentation
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file provides the tmfs interface to the scheme API help.
;; Queries accepted by this handler are of type
;;
;;    tmfs://sapi/type=(symbol|module)&what=(name|)
;;
;; An empty "what" parameter means to list all items of the specified type.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(texmacs-module (doc sapi)
  (:use (kernel gui gui-markup) 
        (doc sapi-markup) (doc sapi-funcs) (doc collect)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; A hash of character -> (list of symbols starting with that character)
(define indexed-commands (make-ahash-table)) 

(define (symbols>? x y)
  (string>? (symbol->string x) (symbol->string y)))

(define (append-command-alphabetically cmd)
  "Used to build the hash @indexed-commands from entries in @tm-defined-name"
  (with _cmd (symbol->string cmd)
    (with _first (substring _cmd 0 1)
      (if (ahash-ref indexed-commands _first)
          (ahash-set! indexed-commands _first
                      (cons _cmd (ahash-ref indexed-commands _first)))
          (ahash-set! indexed-commands _first (list _cmd))))))

(define (links-to-modules-of cmd)
  "Builds links to all texmacs-modules where @cmd is defined"
  ($para ($strong `(scm ,cmd)) " is defined in " $lf
    `(indent ,($para
      ($for (mod (ahash-ref tm-defined-module (string->symbol cmd)))
         ($module-doc-link mod) ", ")))))

(define (output-commands-folded-section handle)
  ($folded-documentation (car handle)
    ($para
      ($for (cmd (cdr handle))
        (links-to-modules-of cmd)))))

(define (sort-by-car a b) 
  (string<? (car a) (car b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer contents for the different requests.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro ($tmsapidoc . l)
  (with lan (get-output-language)
    ($quote
      `(document
         (TeXmacs ,(texmacs-version))
         (style (tuple "tmdoc" "scheme-api"))
         (body ($unquote ($block ,@l)))
         (initial (collection (associate "language" ,lan)))))))

(define ($doc-all-symbols-buffer)
  (map append-command-alphabetically 
       (sort (map car (ahash-table->list tm-defined-table)) symbols>?))
  (with sorted (sort (reverse (ahash-table->list indexed-commands)) sort-by-car)
    ($tmsapidoc
      ($tmdoc-title "All TeXmacs commands")
      ($para 
        "This is an alphabetical list of TeXmacs's public interface. "
        "Because of contextual overloading, a name can be defined several "
        "times in the code. Click on a module name to open its documentation.")
      ($para
        ($for (entry sorted)
          (output-commands-folded-section entry))))))

(define ($doc-symbol-buffer ssym)
  ($tmsapidoc
    ($tmdoc-title (string-append "Documentation for " ssym))
    ($explain-doc* (doc-scm-cache) ssym)))

(define ($doc-all-modules-buffer)
  ($tmsapidoc ($tmdoc-title "All TeXmacs modules")
    ($doc-module-traverse '())))

(define ($doc-module-buffer smod)
  ($tmsapidoc ($tmdoc-title (string-append "Documentation for " smod))
    (if (is-real-module? (string->module smod))
      `(document (doc-module-header ,smod "Description TO-DO")
                 ,($module-exported (string->module smod)))
      `(document (doc-module-header ,smod "")
                 ,($doc-module-traverse (string->module smod))))))

(define ($query-not-implemented query)
 ($tmdoc 
   ($tmdoc-title "Request unknown or not implemented")
   ($para query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (sapi query)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what")))
    (tm->stree
      (cond ((== type "symbol")
             (if (== what "") ($doc-all-symbols-buffer))
                              ($doc-symbol-buffer what))
            ((== type "module")
             (if (== what "") ($doc-all-modules-buffer)
                              ($doc-module-buffer what)))
            (else ($query-not-implemented query))))))

(tmfs-title-handler (sapi query doc)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what")))
    (cond ((== type "symbol")
           (if (== what "") "Browse all symbols"
               (string-append "Documentation for " what)))
          ((== type "module") "Browse all modules"
               (string-append "Documentation for module " what))
          (else "Unknow request"))))

(tmfs-permission-handler (sapi name type) #t)

(tm-define (sapi-all-symbols)
  (:synopsis "Opens a help buffer with a list of all tm-defined symbols")
  (cursor-history-add (cursor-path))
  (load-buffer "tmfs://sapi/type=symbol&what="))

(tm-define (sapi-all-modules)
  (:synopsis "Opens a help buffer with a list of all TeXmacs modules")
  (cursor-history-add (cursor-path))
  (load-buffer "tmfs://sapi/type=module&what="))
