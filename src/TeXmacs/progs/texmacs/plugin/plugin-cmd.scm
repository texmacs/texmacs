
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

(texmacs-module (texmacs plugin plugin-cmd)
  (:export plugin-async-feed plugin-async-retrieve
           pre-serialize verbatim-serialize generic-serialize plugin-serialize
	   plugin-serializer-set! format-command plugin-commander-set!
	   plugin-eval
	   plugin-supports-completions-set! plugin-supports-completions?
	   plugin-supports-input-done-set! plugin-supports-input-done?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; asynchronous evaluation of expressions and memorizing results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-serial-handle 0)
(define plugin-source (make-ahash-table))
(define plugin-results (make-ahash-table))
(define plugin-current (make-ahash-table))

(define (plugin-async-new name session channel)
  "Create a new handle for obtaining data from the plug-in"
  (set! plugin-serial-handle (+ plugin-serial-handle 1))
  (with handle (number->string plugin-serial-handle)
    (ahash-set! plugin-source handle (list name session channel))
    (ahash-set! plugin-results handle (object->tree '(document "")))
    (ahash-set! plugin-current (list name session channel) handle)
    handle))

(define (plugin-async-feed name session t)
  "Evaluate tree @t for plug-in @name and return unique handle or error"
  (with status (connection-status name session)
    (cond ((in? status '(3 1)) "error: busy")
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
  (and (ahash-ref plugin-source handle)
       (with (name session channel) (ahash-ref plugin-source handle)
	 (and (== (ahash-ref (list name session channel)) handle)
	      (== (connection-status name session) 3)))))

(define (plugin-async-append doc1 name session channel flag?)
  (with doc2 (connection-read name session channel)
    (if (and (== (tree-get-label doc2) 'document)
	     (not (== doc2 (object->tree '(document "")))))
	(begin
	  (if flag? (set! doc2 (tree1 'document (tree1 'errput doc2))))
	  (if (== doc1 (object->tree '(document "")))
	      doc2
	      (tree-append doc1 doc2)))
	doc1)))

(define (plugin-async-retrieve handle)
  "Obtain current result of evaluation in @handle"
  (with source (ahash-ref plugin-source handle)
    (if (== (ahash-ref plugin-current source) handle)
	(with (name session channel) (ahash-ref plugin-source handle)
	  (with doc (ahash-ref plugin-results handle)
	    (set! doc (plugin-async-append doc name session channel #f))
	    (if (== channel "output")
		(set! doc (plugin-async-append doc name session "error" #t)))
	    (ahash-set! plugin-results handle doc)))))
  (ahash-ref plugin-results handle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; serialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-serializer (make-ahash-table))

(define (pre-serialize lan t)
  (cond ((func? t 'document 1) (pre-serialize lan (cadr t)))
	((func? t 'math 1)
	 (pre-serialize lan (plugin-math-input (list 'tuple lan (cadr t)))))
	(else t)))

(define (verbatim-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append
     (escape-verbatim (texmacs->verbatim (object->tree u))) "\n")))

(define (generic-serialize lan t)
  (with u (pre-serialize lan t)
    (string-append (char->string #\002) "verbatim:"
		   (escape-generic (texmacs->verbatim (object->tree u)))
		   (char->string #\005))))

(define (plugin-serialize lan t)
  (with fun (ahash-ref plugin-serializer lan)
    (if fun
	(fun lan t)
	(verbatim-serialize lan t))))

(define (plugin-serializer-set! lan val)
  (ahash-set! plugin-serializer lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-commander (make-ahash-table))

(define (default-format-command s)
  (string-append (char->string #\020) s "\n"))

(define (format-command lan s)
  (with fun (ahash-ref plugin-commander lan)
    (if fun
	(fun s)
	(default-format-command s))))

(define (plugin-commander-set! lan val)
  (ahash-set! plugin-commander lan val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluation + simplification of documents with one paragraph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plugin-output-simplify t)
  (cond ((func? t 'document 1) (plugin-output-simplify (cadr t)))
	((func? t 'with) (rcons (cDr t) (plugin-output-simplify (cAr t))))
	(else t)))

(define (plugin-eval name session t)
  (let* ((u (connection-eval name session (object->tree t)))
	 (v (plugin-output-simplify (tree->object u))))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tab completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-completions (make-ahash-table))

(define (plugin-supports-completions-set! key)
  (ahash-set! plugin-supports-completions key #t))

(define (plugin-supports-completions? key)
  (ahash-ref plugin-supports-completions key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; testing whether more input is needed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-supports-input-done (make-ahash-table))

(define (plugin-supports-input-done-set! key)
  (ahash-set! plugin-supports-input-done key #t))

(define (plugin-supports-input-done? key)
  (ahash-ref plugin-supports-input-done key))
