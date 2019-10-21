;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc.scm
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
;;    tmfs://apidoc/type=(symbol|module)&what=(name|)
;;
;; An empty "what" parameter means to list all items of the specified type.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(texmacs-module (doc apidoc)
  (:use (kernel gui gui-markup) 
        (doc apidoc-markup) (doc apidoc-funcs) (doc apidoc-collect)))

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

(tm-define-macro ($tmapidoc . l)
  (with lan (get-output-language)
    ($quote
      `(document
         (TeXmacs ,(texmacs-version))
         (style (tuple "tmdoc" "scheme-api" ,lan))
         (body ($unquote ($block ,@l)))))))

(define ($doc-all-symbols-buffer)
  (map append-command-alphabetically 
       (sort (map car (ahash-table->list tm-defined-table)) symbols>?))
  (with sorted (sort (reverse (ahash-table->list indexed-commands)) sort-by-car)
    ($tmapidoc
      ($tmdoc-title (replace "All TeXmacs commands"))
      ($para 
        "This is an alphabetical list of TeXmacs's public interface. "
        "Because of contextual overloading, a name can be defined several "
        "times in the code. Click on a module name to open its documentation.")
      ($para
        ($for (entry sorted)
          (output-commands-folded-section entry))))))

(define ($doc-symbol-buffer ssym)
  ($tmapidoc
    ($tmdoc-title (replace "Documentation for %1" ssym))
    ($doc-explain-scm* ssym)))

(define ($doc-all-modules-buffer)
  ($tmapidoc ($tmdoc-title (replace "All TeXmacs modules"))
    ($doc-module-traverse '())))

(define ($doc-module-buffer smod)
  (with m (string->module smod)
    (if (is-real-module? m)
        ($tmapidoc
          ($tmdoc-title (replace "Documentation for %1" smod))
          `(document
            ,($doc-module-header m)
            ,($doc-module-exported m)))
        ($tmapidoc
          ($tmdoc-title (replace "Module family %1" smod))
          ($doc-module-traverse m)))))

(define ($query-not-implemented query)
 ($tmdoc 
   ($tmdoc-title (translate "Request unknown or not implemented"))
   ($para query)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tmfs-load-handler (apidoc query)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what")))
    (tm->stree
      (cond ((== type "symbol")
             (if (== what "") ($doc-all-symbols-buffer)
                              ($doc-symbol-buffer what)))
            ((== type "module")
             (if (== what "") ($doc-all-modules-buffer)
                              ($doc-module-buffer what)))
            (else ($query-not-implemented query))))))

(tmfs-title-handler (apidoc query doc)
  (let* ((type (query-ref query "type"))
         (what (query-ref query "what")))
    (cond ((== type "symbol")
           (if (== what "") (replace "Help - Browse all symbols")
               (replace "Help - Documentation for %1" what)))
          ((== type "module")
           (if (== what "") (replace "Help - Browse all modules")
               (replace "Help - Documentation for module %1" what)))
          (else (replace "Help - Unknow request")))))

(tmfs-permission-handler (apidoc name type) #t)

(tm-define (apidoc-all-symbols)
  (:synopsis "Opens a help buffer with a list of all tm-defined symbols")
  (cursor-history-add (cursor-path))
  (load-document "tmfs://apidoc/type=symbol&what="))

(tm-define (apidoc-all-modules)
  (:synopsis "Opens a help buffer with a list of all TeXmacs modules")
  (cursor-history-add (cursor-path))
  (load-document "tmfs://apidoc/type=module&what="))
