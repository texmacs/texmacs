
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
;;  - During compile time build lists of symbols exported in the glue and
;;    add them to the completions tree.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-autocomplete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Prefix trees for autocompletion in scheme sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pt-marker
  (lambda args 
    (if (null? args) (acons "" '() '()) (acons "" '() (car args)))))

(define (pt-terminal? pt)
  (== pt (pt-marker)))

(define (pt-word-mark? pt)
  (!= #f (assoc-ref pt "")))

(define (pt-add pt str)
  (if (== str "")  ; We are done with the input
    (cond ((== pt #f) (pt-marker))  ; The last letter was a new node
          ((pt-word-mark? pt) pt)   ; The word was already there
          (else (pt-marker pt)))    ; This was a new word
    (let ((char (string-take str 1))
          (rest (string-drop str 1))
          (npt (if (== pt #f) '() pt)))
      (assoc-set! npt char (pt-add (assoc-ref npt char) rest)) )))

(define (pt-find pt str)
  (if (== str "") pt
    (let* ((char (string-take str 1))
           (rest (string-drop str 1))
           (val  (assoc-ref pt char)))
      (if (== val #f) #f (pt-find val rest)))))

(define (pt-add-list pt l)
  (if (null? l) pt
      (pt-add-list (pt-add pt (car l)) (cdr l))))

(define (pt-has? pt str)
  (!= #f (pt-find pt str)))

(define (pt-has-list? pt l) 
  "Check whether a given ptree @pt contains all items in the list @l"
  (list-fold (lambda (val prior) (and (pt-has? pt val) prior)) #t l))

(define (pt-words-below-sub pt step)
  (cond ((or (null? pt) (== #f pt)) '())
        ((pt-terminal? pt) 
         `(,step))
        (else (append (pt-words-below-sub (cdar pt)
                                          (string-append step (caar pt)))
                      (pt-words-below-sub (cdr pt) step)) )))

(define (pt-words-below pt)
  (pt-words-below-sub pt ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pt-symbols '()) ; to store all the completions
(define-public scheme-completions-built? #f)

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

(tm-define (all-used-modules)
  (cons (current-module) (module-uses (current-module))))

(tm-define (all-used-symbols)
  (list-fold obarray-fold-sub '() (all-used-modules)))

(tm-define (scheme-completions-add str)
  (set! pt-symbols (pt-add pt-symbols str)))

(tm-define (scheme-completions-add-list lst)
  (set! pt-symbols (pt-add-list pt-symbols lst)))

(tm-define (scheme-completions-rebuild)
  (display "Texmacs] Populating autocompletions tree... ")
  (let ((start (texmacs-time))
        (symbols (all-used-symbols)))
    (scheme-completions-add-list symbols)
    ;(scheme-completions-add-list (all-glued-symbols))
    (display* (length symbols) " symbols in "
              (- (texmacs-time) start) " msec\n")
    (set! scheme-completions-built? #t)))

(tm-define (scheme-completions root)
  (:synopsis "Provide the completions for @root as needed by custom-complete")
  `(tuple ,root 
     ,@(map string->tmstring 
            (pt-words-below (pt-find pt-symbols (tmstring->string root))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hook for new-read. See init-texmacs.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (%read-symbol-hook sym)
  (scheme-completions-add (symbol->string sym)))

(if developer-mode? (set! %new-read-hook %read-symbol-hook ))