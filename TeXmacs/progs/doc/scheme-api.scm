;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-api.scm
;; DESCRIPTION : Automated documentation for the scheme api
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The contents of this file are preliminary and simple. Things TO-DO are:
;;  - this list 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc scheme-api)
  (:use (convert rewrite init-rewrite)))

(tm-define (list-uniq l)
  (:synopsis "Returns a list without any duplicate items.")
  (cond ((null? l) '())
        ((member (car l) (cdr l)) (list-uniq (cdr l)))
        (else (cons (car l) (list-uniq (cdr l)))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions related to modules:

(define (string->module str)
  (map string->symbol (string-split str #\.)))

(define (module->string module)
  (if (list? module)
      (string-join (map symbol->string module) ".")
      (symbol->string module)))

(define (module->name module)
  (symbol->string (cAr module)))

(define (tree->symbol t)
  (string->symbol (tree->string t)))

(define (symbol->tree s)
  (string->tree (symbol->string s)))

(define (module-leq? x y)
  (string<=? (module->string x) (module->string y)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute information related to a module

(define (module-source-path module)
  (string-concatenate
     (list (url-concretize "$TEXMACS_PATH/progs/")
           (string-join (map symbol->string module) "/")
            ".scm")))

(define (module-doc-path module)
  (string-concatenate
     (list "api/"
           (string-join (map symbol->string module) "/")
           ".en.tm")))

(define ($module-source-link module)
  "Builds a link to the source code for @module"
  (let ((pm (module-source-path module)))
    ($link pm
      (if (url-exists? pm)
          (string-append "progs/"
             (string-join (map symbol->string module) "/") ".scm")
          (string-append "Path not found"))) ))

(define ($module-doc-link module)
  "Builds a link to the documentation for @module"
  (let ((pm (module-doc-path module)))
    ($link (string-append (url-concretize "$TEXMACS_PATH/doc/") pm)
      (module->string module))))

(define (module-dependencies module)
 `(concat
   ,@(list-intersperse 
       (map (lambda (x) ($module-doc-link (module-name x))) 
            (module-uses (resolve-module module)))
       ", ")))

(define (count-exported module)
  (apply + (map (lambda (x) (if (member module (cdr x)) 1 0))
                (ahash-table->list tm-defined-module))))

(define (count-undocumented module)
  ; TODO
  -1)

(define (module-exported module)
  (map car (list-filter 
             (ahash-table->list tm-defined-module)
             (lambda (x) (member module (cdr x))))))

(define (tm-exported? sym)
  ; FIXME: look in global symbols, etc.
  (not (eq? #f (ahash-ref tm-defined-table sym))))

(define (list-modules)
  (list-sort
    (list-uniq (apply append (map cdr (ahash-table->list tm-defined-module))))
    module-leq?))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; To be used in packages/documentation/scheme-api.ts

(tm-define (doc-module-synopsis tname)
  ;TODO
  (:secure #t)
  (with name (tree->string tname)
    `(concat ,(string-append "synopsis for " name))))

(tm-define (doc-module-family tname)
  (:secure #t)
  (with name (tree->string tname)
    `(concat ,(module->string (cDr (string->module name))))))

(tm-define (doc-module-dependencies tname)
  (:secure #t)
  (with module (string->module (tree->string tname))
    (module-dependencies module)))

(tm-define (doc-module-source-link tname)
  (:secure #t)
  (with name (tree->string tname)
    ($module-source-link (string->module name))))

(tm-define (doc-module-doc-link tname)
  (:secure #t)
  (with name (tree->string tname)
    ($module-doc-link (string->module name))))

(tm-define (doc-module-count-exported tname)
  (:secure #t)
  (with module (string->module (tree->string tname))
    (number->string (count-exported module))))

(tm-define (doc-module-count-undocumented tname)
  (:secure #t)
  (with module (string->module (tree->string tname))
    (number->string (count-undocumented module))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FIXME:

(tm-define ($doc-module-branches root)
  `(branch ,root ,root))

(tm-define ($doc-module-traverse root)
  (:secure #t)
  `(traverse (document ,@($doc-module-branches (tree->string root)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To be used in symbols documentation

(tm-define (doc-symbol-symbol tsym)
  (:secure #t)
  (tree->string tsym))

(tm-define (doc-symbol-synopsis* sym)
  (:secure #t)
  (with prop (property sym :synopsis)
    (if (list? prop)
        (car prop)
        "No synopsis available")))

(tm-define (doc-symbol-synopsis tsym)
  (:secure #t)
    (doc-symbol-synopsis* (tree->symbol tsym)))

(tm-define (doc-symbol-code* sym)
  (:secure #t)
  (if (tm-exported? sym) 
      (object->string (procedure-sources (eval sym)))
      "Symbol not found"))

(tm-define (doc-symbol-code tsym)
  (:secure #t)
  (with sym (tree->symbol tsym)
    (doc-symbol-code* sym)))

(tm-define (doc-symbol-template* sym)
  `(explain
     (document
       (concat
        (scm ,(symbol->string sym))
        (explain-synopsis ,(doc-symbol-synopsis* sym))))
     (document "Description..."
      (folded-explain
        (document (with "color" "dark green" (em "Example..."))) 
        (document (scm-code "")))
      (folded-explain
       (document (with "color" "dark green" (em "Source code...")))
       (document (scm-code ,(doc-symbol-code* sym))))
      )))

(tm-define (doc-symbol-template tsym)
  (:secure #t)
  (with sym (tree->symbol tsym)
    (doc-symbol-template* sym)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module browser widget

(tm-define module-widget-modules 
  (map module->string (list-modules)))

(tm-define module-widget-symbols
  (map symbol->string (module-exported '(guile))))

(tm-define module-widget-selected "")

(tm-define (module-widget-update-symbols module)
  (set! module-widget-symbols (map symbol->string (module-exported module))))

(tm-widget (module-symbols-widget)
  (scrollable (choice (set! selected-symbol answer)
                      module-widget-symbols
                      "")))

; TO-DO: load real documentation
(tm-widget (symbol-doc-widget)
  (texmacs-output
    `(document ,(doc-symbol-template* module-widget-selected))))

(tm-widget (symbol-doc-buttons)
 (bottom-buttons >>
   ("Insert template" (insert (doc-symbol-template* 
                                (string->symbol module-widget-selected))))))

(tm-widget (module-widget)
  (vertical
    (hsplit
      (scrollable 
        (choice (module-widget-update-symbols (string->module answer))
                module-widget-names
                ""))
      (refresh module-symbols-widget))
    ===
    ;(refresh symbol-doc-widget)
    (refresh symbol-doc-buttons)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handy..

(tm-define (doc-symbol-template** ssym)
  (with sym (string->symbol ssym)
    (insert (doc-symbol-template* sym))))

(tm-define (show-module-widget)
  (top-window module-widget "Pick module and symbol..."))

(define (get-current-doc-module)
  (let ((tt (select (buffer-tree) '(doc-module-header 0))))
    (if (null? tt)
      '(guile) ; FIXME: don't depend on guile for this
      (string->module (tree->string (car tt))))))

(define (exp-modules)
  (map symbol->string (module-exported (get-current-doc-module))))

(tm-define (ask-insert-symbol-doc ssym)
  (:argument ssym "Symbol")
  (:proposals ssym (exp-modules))
  ;(:check-mark "*" (symbol-documented?)) ; right?
  (doc-symbol-template** ssym))

; There is probably a faster way?
(tm-define (editing-module-doc?)
  (!= '() (select (buffer-tree) '(doc-module-header)) ))

(kbd-map
  (:require (editing-module-doc?))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

(menu-bind tools-menu
  (:require (editing-module-doc?))
  (former)
  ("Insert symbol documentation..." (interactive ask-insert-symbol-doc))
  ("Open module browser..." (show-module-widget)))

