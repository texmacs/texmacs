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
  (list-fold-right
    (lambda (x r) (if (member x r) r (cons x r))) '() l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions related to modules:

(define (string->module str)
  (if (== str "") '()
      (map string->symbol (string-split str #\.))))

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

(define (module-source-path module full?)
  (string-concatenate
     (list (if full? (url-concretize "$TEXMACS_PATH/progs/") "progs/")
           (string-join (map symbol->string module) "/")
            ".scm")))

(define (module-doc-path module full?)
  (string-concatenate
     (list (if full? (url-concretize "$TEXMACS_PATH/doc/api/progs/")
                     "api/progs/")
           (string-join (map symbol->string module) "/")
           ".en.tm")))

(define (module-doc-index-path dir full?)
  (string-concatenate
    (list (if full? (url-concretize "$TEXMACS_PATH/doc/api/progs/")
                     "api/progs/")
           (string-join (map symbol->string dir) "/")
           "/index.en.tm")))

(define ($module-source-link module)
  ($link (module-source-path module #t) (module-source-path module #f)))

(define ($module-doc-link module)
  ($link (module-doc-path module #t) (module->string module)))

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

(define (list-submodules root)
  (let ((l (length root)))
    (list-filter (list-modules) 
      (lambda (x)
        (and (>= (length x) l) (== root (list-head x l)))))))


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
  (module-dependencies (string->module (tree->string tname))))

(tm-define (doc-module-source-link tname)
  (:secure #t)
  ($module-source-link (string->module (tree->string tname))))

(tm-define (doc-module-doc-link tname)
  (:secure #t)
  ($module-doc-link (string->module (tree->string tname))))

(tm-define (doc-module-count-exported tname)
  (:secure #t)
  (number->string (count-exported (string->module (tree->string tname)))))

(tm-define (doc-module-count-undocumented tname)
  (:secure #t)
  (number->string (count-undocumented (string->module (tree->string tname)))))

(define (is-module-dir? m depth)
  (> (- (length m) depth) 1))

(define ($doc-module-branch m depth) 
  (cond ((null? m) '())
        ((is-module-dir? m depth)
         (with d (sublist m depth (+ 1 depth))
           `(branch ,(module->string d) ,(module-doc-index-path d #t))))
        (else `(branch ,(module->string (list-tail m depth))
                       ,(module-doc-path m #t)))))

; The branches returned are like a 'ls' in the directory:
; both actual modules and subdirectories are returned, but nothing else
(define ($doc-module-branches l depth done)
  (let* (;(done (if (null? args) '() (car args)))
         (m (if (null? l) '() (car l)))
         (d (if (is-module-dir? m depth)
                (sublist (car l) depth (+ 1 depth))
                '())))
    (if (null? m) '()
        (if (member d done)
            ($doc-module-branches (cdr l) depth done)
            (cons ($doc-module-branch m depth)
                  ($doc-module-branches (cdr l) depth `(,@done ,d)))))))

(tm-define ($doc-module-traverse root)
  `(traverse (document
     ,@($doc-module-branches (list-submodules root) (length root) '()))))

(tm-define (doc-module-traverse troot)
  (:secure #t)
  ($doc-module-traverse (string->module (tree->string troot))))

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

(tm-define mw-all-modules (map module->string (list-modules)))
(tm-define mw-all-symbols (map symbol->string (module-exported '())))
(tm-define mw-module "text.text-menu")
(tm-define mw-symbol "")

(tm-define (mw-update-symbols module)
  (set! mw-all-symbols (map symbol->string (module-exported module))))

(tm-widget (module-symbols-widget)
  (scrollable (choice (set! mw-symbol answer) mw-all-symbols "")))

; TO-DO: load real documentation
(tm-widget (symbol-doc-widget)
  (texmacs-output
    (doc-symbol-template* (string->symbol mw-symbol))))

(tm-widget (symbol-doc-buttons)
 (explicit-buttons >>
   ("Insert template" (insert (doc-symbol-template* 
                                (string->symbol mw-symbol))))))

(tm-widget (module-widget)
  (vertical
    (hsplit
      (scrollable 
        (choice (mw-update-symbols (string->module answer))
                mw-all-modules
                ""))
      (refresh module-symbols-widget))
    ===
    (refresh symbol-doc-widget)
    ===
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
      '()
      (string->module (tree->string (car tt))))))

(define (exp-modules)
  (map symbol->string (module-exported (get-current-doc-module))))

(tm-define (ask-insert-symbol-doc ssym)
  (:argument ssym "Symbol")
  (:proposals ssym (exp-modules))
  ;(:check-mark "*" (symbol-documented?)) ; right?
  (doc-symbol-template** ssym))

(kbd-map
  (:require (in-tmdoc?))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

(menu-bind tools-menu
  (:require (in-tmdoc?))
  (former)
  ("Insert symbol documentation..." (interactive ask-insert-symbol-doc))
  ("Open module browser..." (show-module-widget)))

