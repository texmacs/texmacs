;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc-markup.scm
;; DESCRIPTION : Markup for automatically collected scheme documentation
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Most of the procedures in this file are wrappers to the procedures in 
;; apidoc-funcs which will be used in <extern> and <action> tags in the scheme 
;; documentation. Most of them are needed for example in the style file
;; packages/documentation/scheme-api.ts.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-markup)
  (:use (doc apidoc-funcs)
        (kernel gui gui-markup)))

(tm-define (doc-module-synopsis tname)
  ;TODO
  (:secure #t)
  `(concat ,(string-append "synopsis for " (tree->string tname))))

(tm-define (doc-module-family tname)
  (:secure #t)
  `(concat ,(module->string (cDr (string->module (tree->string tname))))))

(tm-define (doc-module-dependencies tname)
  (:secure #t)
  ($module-dependencies (string->module (tree->string tname))))

(tm-define (doc-module-source-link tname)
  (:secure #t)
  ($module-source-link (string->module (tree->string tname))))

(tm-define (doc-module-doc-link tname)
  (:secure #t)
  ($module-doc-link (string->module (tree->string tname))))

(tm-define (doc-module-count-exported tname)
  (:secure #t)
  (number->string 
    (module-count-exported (string->module (tree->string tname)))))

(tm-define (doc-module-count-undocumented tname)
  (:secure #t)
  (number->string 
    (module-count-undocumented (string->module (tree->string tname)))))

(tm-define (doc-module-traverse troot)
  (:secure #t)
  ($doc-module-traverse (string->module (tree->string troot))))

(tm-define (doc-symbol-symbol tsym)
  (:secure #t)
  (tree->string tsym))

(tm-define (doc-symbol-synopsis tsym)
  (:secure #t)
    (doc-symbol-synopsis* (tree->symbol tsym)))

(tm-define (doc-symbol-code tsym)
  (:secure #t)
  ($doc-symbol-code (tree->symbol tsym)))

(tm-define (doc-symbol-template tsym)
  (:secure #t)
  ($doc-symbol-template (tree->symbol tsym) #t ""))

