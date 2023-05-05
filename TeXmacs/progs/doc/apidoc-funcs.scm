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
;; collected using module (doc apidoc-collect).
;; As usual, procedures prefixed with a dollar sign return strees for display.
;; Most of the time they have an unprefixed counterpart which does the work.
;;
;; TODO:
;;  - use the code indexer when it's ready and ditch ad-hoc parsing made here
;;  - fix the implementation of refresh-widget to fix the module browser
;;  - this list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (doc apidoc-funcs)
  (:use (convert rewrite init-rewrite) (doc apidoc-collect) 
        (kernel gui gui-markup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversions related to modules:

(tm-define (string->module str)
  (:synopsis "Returns the list representation of a module given as a string")
  (if (or (not (string? str)) (== str "")) '()
      (map string->symbol (string-split str #\.))))

(tm-define (module->string module)
  (:synopsis "Formats a module in list format (some module) as some.module")
  (cond ((list? module)
         (string-join (map symbol->string module) "."))
        ((symbol? module)
         (symbol->string module))
        (else "")))

(define (module->name module)
  "Retrieves the name of the file for @module, without extension"
  (symbol->string (cAr module)))

(define (module->path module)
  "Returns the full path of the given module, without extension"
  (url-concretize
    (string-append "$TEXMACS_PATH/progs/"
      (cond ((list? module)
             (string-join (map symbol->string module) "/"))
            ((symbol? module)
             (symbol->string module))
            (else "")))))

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
  (url-exists? (string->url (string-append (module->path module) ".scm"))))

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
  ;TODO
  '())

(tm-define ($module-dependencies module)
 (cons 'concat
   (list-intersperse (map $module-doc-link (module-dependencies module))
                     ", ")))

(define (module-description m)
  "Description TO-DO")

(define module-exported-cache (make-ahash-table))

; HACK: we use read (copying what's done in init-texmacs.scm) until the
; code indexer is implemented
(define (parse-form form f)
  "Set symbol properties and return the symbol."
  (and (pair? form) 
       (member (car form) def-keywords) ;def-keywords defined in init-texmacs.scm
       (let* ((l (source-property form 'line))
              (c (source-property form 'column))
              (sym  (if (pair? (cadr form)) (caadr form) (cadr form))))
         (and (symbol? sym) ; Just in case
              (with old (or (symbol-property sym 'defs) '())
                (if (not (member `(,f ,l ,c) old))
                    (set-symbol-property! sym 'defs (cons `(,f ,l ,c) old)))
                sym)))))

(tm-define (module-exported module)
  (:synopsis "List of exported symbols in @module")
  (or (ahash-ref module-exported-cache module)
      (and (is-real-module? module)
           (let* ((fname (module-source-path module #t))
                  (p (open-input-string (string-load fname)))
                  (defs '())
                  (add (lambda (f) 
                         (with pf (parse-form f fname)
                           (and (!= pf #f) (set! defs (rcons defs pf)))))))
             (letrec ((r (lambda () (with form (read p)
                                      (or (eof-object? form) 
                                          (begin (add form) (r)))))))
                     (r))
             (ahash-set! module-exported-cache module defs)))
      '()))

(tm-define (module-count-exported module)
  (length (module-exported module)))

(tm-define (module-count-undocumented module)
  (with l (module-exported module)
    (- (length l)
       (length
        (list-filter l
           (lambda (x)
             (and (symbol? x) 
                  (persistent-has? (doc-scm-cache) (symbol->string x)))))))))

(tm-define ($doc-module-exported module)
  (with l (module-exported module)
    (with fun (lambda (sym)
                (if (symbol? sym)
                    (list ($doc-explain-scm* (symbol->string sym)))
                    '()))
      (if (null? l)
          `(document ,(replace "No symbols exported"))
          `(document (subsection ,(replace "Symbol documentation"))
                     ,@(append-map fun l))))))

; WRONG! what about unloaded modules
(define (tm-exported? sym)
  (and (symbol? sym) (ahash-ref tm-defined-table sym)))

(define (dir-with-access? path) (url-test? path "dx"))

(define (list-submodules module)
  (with full (module->path module)
    (if (not (dir-with-access? full))
      '()
      (let* ((list-1 (url->list
                      (url-expand
                       (url-complete
                        (url-append full (url-wildcard "*")) "r"))))
             (list-2 (map (lambda (u)
                            (cond ((string-ends? (url->system u) ".scm")
                                   (string->symbol (string-drop-right (url->system (url-tail u)) 4)))
                                  ((dir-with-access? (url->system u))
                                   (string->symbol (url->system (url-tail u))))
                                  (else
                                   '())))
                          list-1))
             (list-3 (filter (lambda (s) (nnull? s))
                             list-2))
             (list-4 (map (lambda (s) (rcons module s)) list-3)))
        list-4))))

(tm-define (list-submodules-recursive ml)
  (:synopsis "Return all submodules, recursively, for module list @ml")
  (cond ((null? ml) '())
        ((npair? ml)
         (if (is-real-module? ml) (list ml)
             (list-submodules-recursive (list-submodules ml))))
        ((null? (cdr ml))
         (if (is-real-module? (car ml)) (list (car ml))
             (list-submodules-recursive (list-submodules (car ml)))))
        (else
         (if (is-real-module? (car ml))
             (cons (car ml) (list-submodules-recursive (cdr ml)))
             (append (list-submodules-recursive (list-submodules (car ml)))
                     (list-submodules-recursive (cdr ml)))))))

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

(define (build-link s def)
  (with (file line column) def
    (if (and file line column)
        (let ((lno (number->string line))
              (cno (number->string column)))
;          `(hlink ,(string-append (url->system (url-tail file)) ":" lno)
;                  ,(string-append file "?line=" lno "&column=" cno
;                                       "&select=" (symbol->string s)))
; hlink not working in alt-window?? replace with action :
          `(action ,(string-append (url->system (url-tail file)) ":" lno)
                  ,(string-append "(show-def \"" file "\" " lno " " cno
                                       " \"" (symbol->string s)"\")")))
        "")))

(tm-define (show-def file line col w)
  (:secure #t)
  ;(display* file "\n" line " " col "\n" w "\n")
  ;FIXME column is zero (sometimes, at least): cannot use it
  (load-buffer-in-new-window file) 
  (go-to-line line)
  (select-line)
  (select-word w (path->tree (selection-path)) col))


(tm-define ($doc-symbol-properties sym)
  (with defs (or (symbol-property sym 'defs) '((#f #f #f)))
    `(concat 
       ,@(list-intersperse 
          (map (lambda (x) (build-link sym x))
               (reverse (list-remove-duplicates defs)))
          " | "))))

(tm-define (doc-symbol-synopsis* sym)
  (with prop (property sym :synopsis)
    (if (list? prop) (car prop) (replace "No synopsis available"))))

(tm-define ($doc-symbol-code sym)
  `(document ; used to be folded-explain but caused bug 61989
     (with "font-series" "bold" "color" "dark green" (em ,(replace "Definition:")))
     (scm-code
       (document
        ,(cond ((and (tm-exported? sym) (procedure? (eval sym)))
                (object->string (procedure-sources (eval sym))))
               ((and (defined? sym) 
                     (procedure? (eval sym))
                     (procedure-source (eval sym)))
                 => object->string)
               (else (replace "Symbol not found or not a procedure")))))))

(tm-define ($doc-symbol-template sym code? message)
  (with contents (cons message (if code? (list ($doc-symbol-code sym)) '()))
    `(explain
      (document
        (concat (scm ,(symbol->string sym))
                (explain-synopsis ,(doc-symbol-synopsis* sym))))
      (document ,@contents))))

(tm-define ($doc-symbol-extra sym . docurl)
  ($inline
    '(htab "")
     (if (nnull? docurl)
       ($inline ($ismall ($link (car docurl) (replace "Open doc."))) " | ")
       "")
    ($ismall (replace "Go to") " " ($doc-symbol-properties sym))))

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

(define ($explain-scheme-not-found key)
  `(document
    ,($doc-symbol-template (string->symbol key) #t
      `(concat "Documentation unavailable. Search "
        (action "the manual"
                ,(string-append "(docgrep-in-doc-secure \"" key "\")"))
         ", or go to the definition in "
         ,($doc-symbol-properties (string->symbol key))))))

(define (doc-explain-sub entries scheme?)
  (if (or (nlist? entries) (null? entries) (not (func? (car entries) 'entry))) 
      '()
      (with (key lan url doc) (cdar entries)
        (cons (if scheme?
                  `(explain
                    ,(tm-ref doc 0)
                    (document 
                      ,(tm-ref doc 1)
                      ,($doc-symbol-code (string->symbol key))
                      ,($doc-symbol-extra (string->symbol key) url)))
                  `(explain ,(tm-ref doc 0) (document ,(tm-ref doc 1))))
              (doc-explain-sub (cdr entries) scheme?)))))
  
(tm-define ($doc-explain-scm* key)
  (with docs (doc-retrieve (doc-scm-cache) key (get-output-language))
    (if (null? docs)
        ($explain-scheme-not-found key) 
        `(document ,@(doc-explain-sub docs #t)))))

(tm-define ($doc-explain-scm key)
  (:synopsis "Return a document with the scheme documentation for @key")
  `(document
     ,($doc-explain-scm* key)
     (freeze (concat (locus (id "__doc__popup__") "")))))

(define ($explain-macro-not-found key)
  `(document
    ,($doc-symbol-template (string->symbol key) #f
      `(concat "Documentation unavailable. You may search "
        (action "the manual"
                ,(string-append "(docgrep-in-doc-secure \"" key "\")"))
         "."))))

(tm-define ($doc-explain-macro* key)
  (with docs (doc-retrieve (doc-macro-cache) key (get-output-language))
    (if (null? docs)
        ($explain-macro-not-found key)
        `(document ,@(doc-explain-sub docs #f)))))

(tm-define ($doc-explain-macro key)
  (:synopsis "Return a document with the documentation for macro @key")
  `(document
     ,($doc-explain-macro* key)
     (freeze (concat (locus (id "__doc__popup__") "")))))
