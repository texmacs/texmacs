
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plugin-cmd.scm
;; DESCRIPTION : Commanding applications from TeXmacs and vice versa
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
	     (if (<= t1 t2) (tree-set! t doc)))
	    ((and u (== (tree-label u) 'output))
	     (with (name session channel) (ahash-ref plugin-source handle)
	       (with p (tree-up t 2)
		 (tree-assign! p doc)
		 (start-input name session (tree->path u)))))))))

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
	       (match? u '(with "mode" "math" :%1))
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
	((match? t '(with "mode" "math" :%1))
	 `(math ,(plugin-output-simplify name (cAr t))))
	((func? t 'with)
	 (rcons (cDr t) (plugin-output-simplify name (cAr t))))
	(else t)))

(tm-define (plugin-output-simplify name t)
  (plugin-output-std-simplify name t))

(tm-define (plugin-preprocess name session t opts)
  ;;(display* "Preprocess " t "\n")
  (if (null? opts) t
      (begin
	(if (and (== (car opts) :math-input)
		 (plugin-supports-math-input-ref name))
	    (set! t (plugin-math-input (list 'tuple name t))))
	(plugin-preprocess name session t (cdr opts)))))

(tm-define (plugin-postprocess name session r opts)
  ;;(display* "Postprocess " r "\n")
  (if (null? opts) r
      (begin
	(if (== (car opts) :simplify-output)
	    (set! r (plugin-output-simplify name r)))
	(plugin-postprocess name session r (cdr opts)))))

(tm-define (plugin-eval name session t . opts)
  (with u (plugin-preprocess name session t opts)
    (with r (tree->stree (connection-eval name session u))
      (plugin-postprocess name session r (cons :simplify-output opts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Asynchroneous evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-eval-table (make-ahash-table))

(define (plugin-eval-insert name session t progress return post)
  (let* ((key (cons name session))
	 (val (list t progress return post))
	 (old (ahash-ref plugin-eval-table key))
	 (new (rcons (or old '()) val)))
    (if (and (not old) (== (connection-status name session) 0))
	(set! new (cons (list :restart ignore ignore identity) new)))
    (ahash-set! plugin-eval-table key new)
    (if (not old)
	(plugin-eval-loop name session))))

(define (plugin-eval-loop name session)
  (let* ((key (cons name session))
	 (busy? (lambda () (ahash-ref plugin-eval-table key)))
	 (wait 1)
	 (output #f))
    (delayed
      (:while (busy?))
      (:pause ((lambda () wait)))
      (:do (set! wait (min (* 2 wait) 2500)))
      (let* ((status (connection-status name session))
	     (old (ahash-ref plugin-eval-table key))
	     (new (and (nnull? (cdr old)) (cdr old))))
	;;(display* "Status " name ": " status "\n")
	(with (input progress return post) (car old)
	  (cond ((and (== status 0) (== input :restart))
		 (connection-start name session #f)
		 (set! wait 1)
		 (set! output '(document)))
		((and (== status 2) (not output))
		 ;;(display* "Write " name " <- " input "\n")
		 (connection-write name session input)
		 (set! wait 1)
		 (set! output '(document)))
		((and (== status 3) output)
		 (with r (tm->stree (connection-read name session "output"))
		   ;;(display* "Read " name " -> " r "\n")
		   (when (and (func? r 'document) (!= r '(document "")))
		     (if (== (cAr r) "") (set! r (cDr r)))
		     (set! wait 1)
		     (set! output (append output (cdr r)))
		     (when (== (connection-status name session) 3)
		       (progress output)))))
		((and (== status 2) output)
		 (with result output
		   (set! wait 1)
		   (set! output #f)
		   (ahash-set! plugin-eval-table key new)
		   (return (post result))))
		(else
		 (set! wait 1)
		 (set! output #f)
		 (ahash-set! plugin-eval-table key new)
		 (return '(script-dead)))))))))

(tm-define (plugin-async-eval name session t . opts)
  (with u (plugin-preprocess name session t opts)
    (with post (cut plugin-postprocess name session <> opts)
      (if dialogue-break
	  (dialogue-user local-continue
	    (with return (dialogue-machine local-continue)
	      (plugin-eval-insert name session u ignore return post)))
	  (texmacs-error "dialogue-ask" "Not in dialogue")))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Command for numeric evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-approx-command (make-ahash-table))

(tm-define (plugin-approx-command-set! key val)
  (ahash-set! plugin-approx-command key val))

(tm-define (plugin-approx-command-ref key)
  (ahash-ref plugin-approx-command key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New connection management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define connection-pending (make-ahash-table))
(define connection-starts (make-ahash-table))
(define connection-prompts (make-ahash-table))

(define (pending-ref lan ses)
  (or (ahash-ref connection-pending (list lan ses)) '()))

(define (pending-set lan ses l)
  (ahash-set! connection-pending (list lan ses) l))

(define (pending-encode in out next opts)
  (list (if (tm? in) (tm->stree in) in)
	(tree->tree-pointer out)
	(tree->tree-pointer next)
	opts))

(define (pending-decode l)
  (list (car l)
	(tree-pointer->tree (cadr l))
	(tree-pointer->tree (caddr l))
	(cadddr l)))

(define (pending-detach l)
  (tree-pointer-detach (cadr l))
  (tree-pointer-detach (caddr l)))

(define (var-tree-children t)
  (with r (tree-children t)
    (if (and (nnull? r) (tree-empty? (cAr r))) (cDr r) r)))

(define (var-connection-status lan ses)
  (if (!= lan "scheme")
      (connection-status lan ses)
      2))

(define (var-connection-start lan ses again?)
  (if (!= lan "scheme")
      (connection-start lan ses again?)))

(define (var-connection-write lan ses t)
  (if (!= lan "scheme")
      (connection-write lan ses t)
      (delayed
	(connection-notify-status lan ses 3)
	(with r (package-evaluate lan ses t)
	  (if (not (func? r 'document))
	      (set! r (tree 'document r)))
	  (connection-notify lan ses "output" r))
	(connection-notify-status lan ses 2))))

(define (connection-do lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with (in out next opts) (pending-decode (car l))
	(with status (var-connection-status lan ses)
	  ;;(display* "Send " lan ", " ses ", " in "\n")
	  (cond ((== in :start)
		 (if (== status 0)
		     (var-connection-start lan ses #f)
		     (connection-next lan ses)))
		((tree-empty? in)
		 (connection-next lan ses))
		((== status 0)
		 (with p (pending-encode :start (tree "") (tree "") '())
		   (pending-set lan ses (cons p l))
		   (connection-do lan ses)))
		(#t
		 (tree-set out :up 0 (connection-prompt lan ses))
		 (var-connection-write lan ses in)
		 (ahash-set! connection-starts (list lan ses)
			     (texmacs-time)))))))))

(define (connection-next lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with (in out next opts) (pending-decode (car l))
	(when (and (tm-func? out 'document)
		   (tm-func? (tree-ref out :last) 'script-busy))
	  (let* ((t1 (ahash-ref connection-starts (list lan ses)))
		 (t2 (texmacs-time))
		 (dt (- t2 (or t1 t2)))
		 (ts (if (< dt 1000)
			 (string-append (number->string dt) " msec")
			 (string-append (number->string (/ dt 1000.0))
					" sec"))))
	    (if (and (in? :timings opts) (>= dt 1))
		(tree-set (tree-ref out :last) `(timing ,ts))
		(tree-remove! out (- (tree-arity out) 1) 1))))
	(when (tree-empty? out)
	  (tree-set! out '(document)))
	(pending-detach (car l))
	(pending-set lan ses (cdr l))
	(connection-do lan ses)))))

(define (connection-cancel lan ses dead?)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with (in out next opts) (pending-decode (car l))
	(when (and (tm-func? out 'document)
		   (tm-func? (tree-ref out :last) 'script-busy))
	  (tree-assign (tree-ref out :last)
		       (if dead? '(script-dead) '(script-interrupted))))
	(pending-detach (car l))
	(pending-set lan ses (cdr l))
	(connection-cancel lan ses dead?)))))

(define (connection-output t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1)))
      (if (tm-func? u 'document)
	  (tree-insert! t i (var-tree-children u))))))

(define (connection-errput t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1))
	  (tree-insert! t i '((errput (document)))))
      (connection-output (tree-ref t i 0) u))))

(tm-define (connection-prompt lan ses)
  (with p (ahash-ref connection-prompts (list lan ses))
    (if p (tree-copy p) (string-append (upcase-first lan) "] "))))

(tm-define (connection-feed lan ses in out next opts)
  (set! in (plugin-preprocess lan ses in opts))
  (with l (pending-ref lan ses)
    (pending-set lan ses (rcons l (pending-encode in out next opts)))
    (tree-assign! out '(document (script-busy)))
    (if (null? l) (connection-do lan ses))))

(tm-define (connection-notify lan ses ch t)
  ;;(display* "Notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with (in out next opts) (pending-decode (car l))
	(cond ((== ch "output")
	       (connection-output out t))
	      ((== ch "error")
	       (connection-errput out t))
	      ((== ch "prompt")
	       (ahash-set! connection-prompts (list lan ses) (tree-copy t))
	       (if (and (null? (cdr l)) (tree-empty? (tree-ref next 1)))
	       	   (tree-set! next 0 (tree-copy t))))
	      ((and (== ch "input") (null? (cdr l)))
	       (tree-set! next 1 t)))))))

(tm-define (connection-notify-status lan ses st)
  ;;(display* "Notify status " lan ", " ses ", " st "\n")
  (when (== st 0)
    (ahash-remove! connection-starts (list lan ses))
    (ahash-remove! connection-prompts (list lan ses))
    (connection-cancel lan ses #t))
  (when (== st 2)
    (connection-next lan ses)))

(tm-define (connection-break)
  (connection-interrupt)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (connection-cancel lan ses #f)))
