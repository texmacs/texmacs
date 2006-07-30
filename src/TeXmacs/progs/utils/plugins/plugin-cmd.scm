
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plugin-cmd.scm
;; DESCRIPTION : Commanding applications from TeXmacs and vice versa
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils plugins plugin-cmd)
  (:use (utils library tree)
	(utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asynchronous evaluation of expressions and memorizing results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-serial-handle 0)
(define plugin-source (make-ahash-table))
(define plugin-results (make-ahash-table))
(define plugin-time-stamps (make-ahash-table))
(define plugin-current (make-ahash-table))

(define (plugin-result-set! handle doc)
  (ahash-set! plugin-results handle doc)
  (ahash-set! plugin-time-stamps handle (texmacs-time)))

(define (plugin-async-new name session channel)
  "Create a new handle for obtaining data from the plug-in"
  (set! plugin-serial-handle (+ plugin-serial-handle 1))
  (with handle (number->string plugin-serial-handle)
    (ahash-set! plugin-source handle (list name session channel))
    (plugin-result-set! handle (stree->tree '(document "")))
    (ahash-set! plugin-current (list name session channel) handle)
    handle))

(tm-define (plugin-async-start name session)
  "Start an asynchronous connection"
  (if (connection-declared? name)
      (with status (connection-status name session)
	(cond ((== status 0)
	       (with handle (plugin-async-new name session "output")
		 (with message (connection-start name session #f)
		   (if (== message "ok") handle
		       (string-append "error: " message)))))
	      ((== status 2)
	       (string-append "error: continuing#" name "#session"))
	      (else (string-append "error: " name "#is busy"))))
      (string-append "error: plug-in '" name "' not declared")))

(tm-define (plugin-async-feed name session t)
  "Evaluate tree @t for plug-in @name and return unique handle or error"
  (with status (connection-status name session)
    (cond ((in? status '(3 1)) (string-append "error: " name "#is busy"))
	  ((== status 0)
	   (with message (connection-start name session #t)
	     (if (== message "ok")
		 (plugin-async-feed name session t)
		 (string-append "error: " message))))
	  ((== status 2)
	   (with handle (plugin-async-new name session "output")
	     (connection-write name session t)
	     handle)))))

(define (plugin-async-active? handle)
  "Is the evaluation still going on?"
  (with source (ahash-ref plugin-source handle)
    (and source (with (name session channel) source
		  (and (== (ahash-ref plugin-current source) handle)
		       (== (connection-status name session) 3))))))

(define (plugin-async-append handle flag?)
  (with (name session channel) (ahash-ref plugin-source handle)
    (if flag? (set! channel "error"))
    (let* ((doc1 (ahash-ref plugin-results handle))
	   (doc2 (connection-read name session channel)))
      (if (and (== (tree-label doc2) 'document)
	       (== (tree-ref doc2 (- (tree-arity doc2) 1)) (string->tree "")))
	  (set! doc2 (tree-range doc2 0 (- (tree-arity doc2) 1))))
      (if (and (== (tree-label doc2) 'document)
	       (> (tree-arity doc2) 0))
	  (begin
	    (if flag? (set! doc2 (tm->tree `(document (errput ,doc2)))))
	    (if (== doc1 (stree->tree '(document "")))
		(plugin-result-set! handle doc2)
		(plugin-result-set! handle (tree-append doc1 doc2))))))))

(tm-define (plugin-async-retrieve handle)
  "Obtain current result of evaluation in @handle"
  (with source (ahash-ref plugin-source handle)
    (if (== (ahash-ref plugin-current source) handle)
	(with (name session channel) (ahash-ref plugin-source handle)
	  (plugin-async-append handle #f)
	  (if (== channel "output") (plugin-async-append handle #t)))))
  (ahash-ref plugin-results handle))

(tm-define (mutate-plugin-result handle)
  (:secure #t)
  (with-mutator t
    (let* ((doc (plugin-async-retrieve handle))
	   (t1 (mutator-time))
	   (t2 (ahash-ref plugin-time-stamps handle))
	   (u (tree-up t 3)))
      (cond ((not doc) (noop))
	    ((plugin-async-active? handle)
	     (if (<= t1 t2) (tree-set t doc)))
	    ((and u (== (tree-label u) 'output))
	     (with (name session channel) (ahash-ref plugin-source handle)
	       (tree-assign (tree-up t 2) doc)
	       (start-input name session (tree->path u))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-serializer (make-ahash-table))

(tm-define (pre-serialize lan t)
  (cond ((func? t 'document 1) (pre-serialize lan (cadr t)))
	((func? t 'math 1)
	 (pre-serialize lan (plugin-math-input (list 'tuple lan (cadr t)))))
	(else t)))

(tm-define (verbatim-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append
     (escape-verbatim (texmacs->verbatim (stree->tree u))) "\n")))

(tm-define (generic-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append (char->string #\002) "verbatim:"
		   (escape-generic (texmacs->verbatim (stree->tree u)))
		   (char->string #\005))))

(tm-define (plugin-serialize lan t)
  (with fun (ahash-ref plugin-serializer lan)
    (if fun
	(fun lan t)
	(verbatim-serialize lan t))))

(tm-define (plugin-serializer-set! lan val)
  (ahash-set! plugin-serializer lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-commander (make-ahash-table))

(define (default-format-command s)
  (string-append (char->string #\020) s "\n"))

(tm-define (format-command lan s)
  (with fun (ahash-ref plugin-commander lan)
    (if fun
	(fun s)
	(default-format-command s))))

(tm-define (plugin-commander-set! lan val)
  (ahash-set! plugin-commander lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some subroutines for mathematical content
;; FIXME: these should be moved into table-edit.scm and math-edit.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cell-context-inside-sub? t which)
  (or (and (list? which) (tree-in? t which))
      (and (nlist? which) (tree-is? t which))
      (and (tree-in? t '(table tformat document))
	   (cell-context-inside-sub? (tree-up t) which))))

(define (cell-context-inside? t which)
  (and (tree-is? t 'cell)
       (tree-is? t :up 'row)
       (cell-context-inside-sub? (tree-ref t :up :up)  which)))

(tm-define (formula-context? t)
  (with u (tree-up t)
    (and u (or (tree-in? u '(math equation equation*))
	       (match? u '(with "mode" "math" :1))
	       (cell-context-inside? u '(eqnarray eqnarray*))))))

(tm-define (in-var-math?)
  (let* ((t1 (tree-innermost formula-context? #t))
	 (t2 (tree-innermost 'text)))
    (and (nnot t1) (or (not t2) (tree-inside? t1 t2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation + simplification of document fragments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (plugin-output-std-simplify name t)
  (cond ((or (func? t 'document 0) (func? 'concat 0)) "")
	((or (func? t 'document 1) (func? t 'concat 1))
	 (plugin-output-simplify name (cadr t)))
	((and (or (func? t 'document) (func? t 'concat))
	      (in? (cadr t) '("" " " "  ")))
	 (plugin-output-simplify name (cons (car t) (cddr t))))
	((and (or (func? t 'document) (func? t 'concat))
	      (in? (cAr t) '("" " " "  ")))
	 (plugin-output-simplify name (cDr t)))
	((match? t '(with "mode" "math" :1))
	 `(math ,(plugin-output-simplify name (cAr t))))
	((func? t 'with)
	 (rcons (cDr t) (plugin-output-simplify name (cAr t))))
	(else t)))

(tm-define (plugin-output-simplify name t)
  (plugin-output-std-simplify name t))

(tm-define (plugin-eval name session t . opts)
  (cond ((null? opts)
	 (let* ((u (connection-eval name session t))
		(v (plugin-output-simplify name (tree->stree u))))
	   v))
	((== (car opts) :math-input)
	 (if (plugin-supports-math-input-ref name)
	     (set! t (plugin-math-input (list 'tuple name t))))
	 (apply plugin-eval (cons* name session t (cdr opts))))
	((== (car opts) :math-correct)
	 (with r (apply plugin-eval (cons* name session t (cdr opts)))
	   (if (and (in-var-math?) (tm-func? r 'math 1)) (cadr r) r)))
	(else
	 (texmacs-error "plugin-eval" "unrecognized option ~S" (car opts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-completions (make-ahash-table))

(tm-define (plugin-supports-completions-set! key)
  (ahash-set! plugin-supports-completions key #t))

(tm-define (plugin-supports-completions? key)
  (ahash-ref plugin-supports-completions key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing whether more input is needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-input-done (make-ahash-table))

(tm-define (plugin-supports-input-done-set! key)
  (ahash-set! plugin-supports-input-done key #t))

(tm-define (plugin-supports-input-done? key)
  (ahash-ref plugin-supports-input-done key))
