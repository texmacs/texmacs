;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : sapi-funcs.scm
;; DESCRIPTION : Routines for documentation of the scheme api
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This file contains procedures needed for the display of documentation
;; collected using module (doc collect).
;; (...)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc sapi-funcs)
  (:use (convert rewrite init-rewrite) (doc collect)))

(tm-define (list-uniq l)
  (:synopsis "Returns a list without any duplicate items.")
  (list-fold-right
    (lambda (x r) (if (member x r) r (cons x r))) '() l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions related to modules:

(tm-define (string->module str)
  (:synopsis "Returns a valid list of symbols for a module given as a string")
  (if (== str "") '()
      (map string->symbol (string-split str #\.))))

(tm-define (module->string module)
  (:synopsis "Formats a module in list format (some module) as some.module")
  (if (list? module)
      (string-join (map symbol->string module) ".")
      (symbol->string module)))

(define (module->name module)
  "Retrieves the name of the file for @module, without any extension"
  (symbol->string (cAr module)))

(define (tree->symbol t)
  (string->symbol (tree->string t)))

(define (symbol->tree s)
  (string->tree (symbol->string s)))

(define (module-leq? x y)
  (string<=? (module->string x) (module->string y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compute and display information related to a module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (is-real-module? module)
  (url-exists? 
    (url-concretize 
      (string-append
        "$TEXMACS_PATH/progs/"
        (string-join (map symbol->string module) "/")
        ".scm"))))

(define (module-source-path module full?)
  (string-concatenate
     (list (if full? (url-concretize "$TEXMACS_PATH/progs/") "progs/")
           (string-join (map symbol->string module) "/")
            ".scm")))

(define (module-doc-path module)
  (string-append "tmfs://sapi/type=module&what=" (module->string module)))

(tm-define ($module-source-link module)
  ($link (module-source-path module #t) (module-source-path module #f)))

(tm-define ($module-doc-link module)
  ($link (module-doc-path module) (module->string module)))

(tm-define ($module-dependencies module)
 `(concat
   ,@(list-intersperse 
       (map (lambda (x) ($module-doc-link (module-name x))) 
            (module-uses (resolve-module module)))
       ", ")))

(tm-define (module-count-exported module)
  (apply + (map (lambda (x) (if (member module (cdr x)) 1 0))
                (ahash-table->list tm-defined-module))))

; FIXME! handle the case of no exported symbols
(tm-define ($module-exported module)
  (for-each 
    (lambda (sym)
      (with ref (ahash-ref tm-defined-module (car sym))
        (if (and ref (member module ref))
          ($explain-doc* (doc-scm-cache) (symbol->string (car sym))))))
    (ahash-table->list tm-defined-table)))

(tm-define (module-count-undocumented module)
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
  (with l (length root)
    (list-filter (list-modules) 
      (lambda (x) (and (>= (length x) l) (== root (list-head x l)))))))

(define (is-module-dir? m depth)
  (> (- (length m) depth) 1))

(define ($doc-module-branch m depth) 
  (cond ((null? m) '())
        ((is-module-dir? m depth)
         (with d (sublist m depth (+ 1 depth))
           `(branch ,(module->string d) ,(module-doc-path d))))
        (else `(branch ,(module->string (list-tail m depth))
                       ,(module-doc-path m)))))

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
  `(traverse 
     (document
       ,@($doc-module-branches (list-submodules root) (length root) '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-symbol-synopsis* sym)
  (with prop (property sym :synopsis)
    (if (list? prop) (car prop) "No synopsis available")))

(tm-define (doc-symbol-code* sym)
  (if (tm-exported? sym) 
      (object->string (procedure-sources (eval sym)))
      "Symbol not found"))

(tm-define ($doc-symbol-template sym)
  `(explain
     (document
       (concat
        (scm ,(symbol->string sym))
        (explain-synopsis ,(doc-symbol-synopsis* sym))))
     (document ""
       (unfolded-explain
         (document (with "color" "dark green" (em "Example..."))) 
         (document (scm-code "")))
       (unfolded-explain
         (document (with "color" "dark green" (em "Source code...")))
         (document (scm-code ,(doc-symbol-code* sym)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieval and display of documentation from the cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (docgrep-new-window what)
  (let* ((query (list->query (list (cons "type" "doc") (cons "what" what))))
         (name (string-append "tmfs://grep/" query)))
    (buffer-load name)
    (open-buffer-in-window name (buffer-get name) "")))

(tm-define (docgrep-in-doc-secure what)
  (:synopsis "Search in documentation. Secure routine to use in 'action tags.")
  (:secure #t)
  (docgrep-new-window what))

(define ($explain-not-found key)
  `(document
    ,(string-append "Documentation for " key " not found. ")
    ,($doc-symbol-template (string->symbol key))
    (action "Search in documentation..." 
            ,(string-append "(docgrep-in-doc-secure \"" key "\")"))))

(tm-define ($explain-doc* cache key)
  (if (persistent-has? cache key)
      (tree->stree (parse-texmacs-snippet (persistent-get cache key)))
      ($explain-not-found key)))

(tm-define ($explain-doc cache key)
  (:synopsis "Return a document with the documentation for @key in @cache")
  `(document
     (surround
       (freeze (concat (locus (id "__doc__popup__") "")
                       (use-package "tmdoc-markup")))
     "" (document ,($explain-doc* cache key)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; module browser widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define mw-all-modules (map module->string (list-modules)))
(tm-define mw-all-symbols (map symbol->string (module-exported '())))
(tm-define mw-module "")
(tm-define mw-symbol "")

(tm-define (mw-update-symbols module)
  (set! mw-all-symbols (map symbol->string (module-exported module))))

(tm-widget (module-symbols-widget)
  (scrollable (choice (set! mw-symbol answer) mw-all-symbols "")))

; TO-DO: load real documentation
(tm-widget (symbol-doc-widget)
  (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
    (texmacs-input
     ,($explain-doc (doc-scm-cache) (symbol->string sym))
     (noop) #f)))
;;    ($doc-symbol-template (string->symbol mw-symbol))))

(tm-widget (symbol-doc-buttons)
 (explicit-buttons >>
   ("Insert template"
    (insert ($doc-symbol-template (string->symbol mw-symbol))))))

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
  (insert ($doc-symbol-template (string->symbol ssym))))

(kbd-map
  (:require (in-tmdoc?))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

(menu-bind tools-menu
  (:require (in-tmdoc?))
  (former)
  ("Insert symbol documentation..." (interactive ask-insert-symbol-doc))
  ("Open module browser..." (show-module-widget)))

