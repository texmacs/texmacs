;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : apidoc-funcs.scm
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
;; As usual, procedures prefixed with a dollar sign return strees for display.
;; Most of the time they have an unprefixed counterpart which does the work.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-funcs)
  (:use (convert rewrite init-rewrite) (doc apidoc-collect)))

(tm-define (list-uniq l)
  (:synopsis "Returns a list without any duplicate items.")
  (list-fold-right
    (lambda (x r) (if (member x r) r (cons x r))) '() l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions related to modules:

(tm-define (string->module str)
  (:synopsis "Returns the list representation of a module given as a string")
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
;; TODO: write abstract interface to decouple from TeXmacs/Guile/whatever
;; specifics.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (guile?)
  (with sd (scheme-dialect)
    (and (string? sd) (== "guile" (string-take sd 5)))))

(tm-define (is-real-module? module)
  (url-exists? 
    (string->url 
      (string-append "$TEXMACS_PATH/progs/"
                     (string-join (map symbol->string module) "/")
                     ".scm"))))

(define (module-source-path module full?)
  (string-concatenate
     (list (if full? (url-concretize "$TEXMACS_PATH/progs/") "")
           (string-join (map symbol->string module) "/")
            ".scm")))

(define (module-doc-path module)
  (string-append "tmfs://apidoc/type=module&what=" (module->string module)))

(tm-define ($module-source-link module)
  ($link (module-source-path module #t) (module-source-path module #f)))

(tm-define ($module-doc-link module)
  ($link (module-doc-path module) (module->string module)))

(tm-define (module-dependencies module)
  (throw 'not-implemented
  "TODO: implement when the module system is decoupled from guile's"))

(tm-define (module-dependencies module)
  (:require (guile?))
  (list-filter
    (map module-name (module-uses (resolve-module module)))
    (lambda (x) (and (!= x '(guile)) (!= x '(guile-user))))))

(tm-define ($module-dependencies module)
 (cons 'concat
   (list-intersperse (map $module-doc-link (module-dependencies module))
                     ", ")))

(define (module-description m)
  "Description TO-DO")

; (tm-define-macro name ...) creates symbol name$impl.
; FIXME: symbol? fails for these symbols when read from tm-defined-module.
; Some problem of scoping?
(define (tm-demacroify sym)
  (if (not (symbol? sym))
    (display "WTF?")
    (with str (symbol->string sym)
      (if (and (> (string-length str) 5) (== "$impl" (string-take-right str 5)))
          (string->symbol (string-drop-right str 5))
          sym))))

(define (module-exported-texmacs module)
  (if (null? module) '()
      (map (lambda (x) (tm-demacroify (car x)))
           (list-filter (ahash-table->list tm-defined-module)
                        (lambda (x) (member module (cdr x)))))))

; FIXME! remove non-exported symbols (but %module-public-interface seems not
; to work (always empty?))
(define (module-exported-guile module)
  (catch #t
    (lambda ()
      (map car (ahash-table->list (module-obarray 
                                    (module-ref (resolve-module module)
                                                '%module-public-interface)))))
    (lambda (key . args) '())))

(tm-define (module-exported module)
  (:synopsis "List of exported symbols in @module")
  (list-uniq (append (module-exported-texmacs module) 
                     (module-exported-guile module))))

(tm-define (module-count-exported module)
  (length (module-exported module)))

(tm-define (module-count-undocumented module)
  ; TODO
  -1)

(tm-define ($doc-module-exported module)
  (with l (module-exported module)
    (with fun (lambda (sym)
                (if (symbol? sym)
                  (list ($doc-explain-scm* (symbol->string sym)))
                  '()))
      (if (null? l)
       '(document "No symbols exported")
       `(document (subsection "Symbol documentation")
                  ,@(append-map fun l))))))

(define (tm-exported? sym)
  (and (symbol? sym) (ahash-ref tm-defined-table sym)))

; FIXME: traverse directories rather than using tm-defined-module
; (will all modules be listed?)
(define _list-modules_ #f) ; 
(define (list-modules)
  (if (== #f _list-modules_)
      (set! _list-modules_
        (list-sort
          (list-filter
            (list-uniq 
              (append-map cdr (ahash-table->list tm-defined-module)))
            (lambda (x) (!= x '(guile-user)))) ; HACK
          module-leq?)))
  _list-modules_)

(define (list-submodules root)
  (with l1 (length root)
    (list-sort
      (list-uniq
        (list-fold 
          (lambda (cur done)
            (with l2 (length cur)
              (cond ((or (< l2 l1) (== cur root)) done) 
                    ((== root (list-head cur l1))
                     (if (== l2 l1) 
                         (cons (list-head cur l1) done)
                         (cons (list-head cur (+ l1 1)) done)))
                    (else done))))
          '() (list-modules)))
      module-leq?)))

(define ($doc-module-branch m)
  `(branch ,(symbol->string (cAr m)) ,(module-doc-path m)))

(define ($doc-module-branches lst)
  (append-map (lambda (m) (list ($doc-module-branch m))) lst))

(tm-define ($doc-module-traverse root)
  `(traverse (document ,@($doc-module-branches (list-submodules root)))))

(tm-define ($submodules->gtree m)
  (with fun (lambda (x) (symbol->string (cAr x)))
    `(tree ,(if (null? m) "()" (fun m)) ,@(map fun (list-submodules m)))))

(tm-define ($doc-module-header m)
  `(doc-module-header ,(module->string m) ,(module-description m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbols documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ($doc-symbol-properties sym)
  (let ((line (symbol-property sym 'line))
        (filename (symbol-property sym 'filename)))
    (if (and line filename)
      `(hlink ,(string-append (basename filename) ":" (number->string line))
              ,filename)
      (translate "[symbol properties not found]"))))

(tm-define (doc-symbol-synopsis* sym)
  (with prop (property sym :synopsis)
    (if (list? prop) (car prop) "No synopsis available")))

(tm-define (doc-symbol-code* sym)
  (cond ((and (tm-exported? sym) (procedure? (eval sym)))
         (object->string (procedure-sources (eval sym))))
        ((and (defined? sym) 
              (procedure? (eval sym))
              (procedure-source (eval sym)))
              => (lambda (x) x))
        (else "Symbol not found or not a procedure")))

(tm-define ($doc-symbol-template sym message)
  `(explain
     (document
       (concat
        (scm ,(symbol->string sym))
        (explain-synopsis ,(doc-symbol-synopsis* sym))))
     (document ,message
       (folded-explain
         (document (with "color" "dark green" (em "Definition...")))
         (document (scm-code ,(doc-symbol-code* sym)))))))

(tm-define-macro ($ismall . l)
  ($quote `(small (with "font-shape" "italic" ($unquote ($inline ,@l))))))

(tm-define ($doc-symbol-extra sym . docurl)
  ($inline
    '(htab "")
     (if (nnull? docurl)
       ($inline ($ismall ($link (car docurl) (translate "Open doc."))) " | ")
       "")
    ($ismall (translate "Go to") " " ($doc-symbol-properties sym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retrieval and display of documentation from the cache
;; FIXME: this is not the right place for this.
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
  ($para
    ($doc-symbol-template (string->symbol key)
      ($inline "Documentation unavailable. Search"
        `(action " in the manual" 
                 ,(string-append "(docgrep-in-doc-secure \"" key "\")"))
         ", or go to the definition in "
         ($doc-symbol-properties (string->symbol key))))))

(define (doc-explain-sub entries)
  (if (or (null? entries) (not (func? (car entries) 'entry))) '()
    (with (key lan url doc) (cdar entries)
      (cons `(document ,doc ,($doc-symbol-extra (string->symbol key) url))
             (doc-explain-sub (cdr entries))))))

(tm-define ($doc-explain-scm* key)
  (with docs (doc-retrieve (doc-scm-cache) key (get-output-language))
    (if (null? docs) ($explain-not-found key) 
      `(document ,@(doc-explain-sub docs)))))

(tm-define ($doc-explain-scm key)
  (:synopsis "Return a document with the scheme documentation for @key")
  `(document
     (surround
       (freeze (concat (locus (id "__doc__popup__") "")
                       (use-package "tmdoc-markup")))
     "" ,($doc-explain-scm* key))))

(tm-define ($doc-explain-macro* key)
  (with docs (doc-retrieve (doc-macro-cache) key (get-output-language))
    (if (null? docs) ($explain-not-found key) (doc-explain-sub docs))))


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

(tm-widget (symbol-doc-widget)
  (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
    (texmacs-input
     ,($doc-explain-scm (symbol->string sym))
     (noop) #f)))

(tm-widget (symbol-doc-buttons)
 (explicit-buttons >>
   ("Insert template"
    (insert ($doc-symbol-template (string->symbol mw-symbol) "")))))

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

(define (go-to-line n)
  (with-innermost t 'document
    (with p (tree-cursor-path t)
      (tree-go-to t n 0))))

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
  (insert ($doc-symbol-template (string->symbol ssym) "")))

(kbd-map
  (:require (in-tmdoc?))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

(menu-bind tools-menu
  (:require (in-tmdoc?))
  (former)
  ("Insert symbol documentation..." (interactive ask-insert-symbol-doc))
  ("Open module browser..." (show-module-widget)))

