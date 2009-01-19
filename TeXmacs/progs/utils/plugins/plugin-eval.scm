
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : plugin-eval.scm
;; DESCRIPTION : Evaluation via plugins
;; COPYRIGHT   : (C) 1999-2009  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils plugins plugin-eval)
  (:use (utils library tree)
	(utils library cursor)))

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
;; New connection management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-pending (make-ahash-table))
(define plugin-started (make-ahash-table))
(define plugin-prompts (make-ahash-table))

(define (pending-ref lan ses)
  (or (ahash-ref plugin-pending (list lan ses)) '()))

(define (pending-set lan ses l)
  (ahash-set! plugin-pending (list lan ses) l))

(define (plugin-status lan ses)
  (if (!= lan "scheme")
      (connection-status lan ses)
      2))

(define (plugin-start lan ses again?)
  (if (!= lan "scheme")
      (connection-start lan ses again?)))

(define (plugin-write lan ses t)
  (if (!= lan "scheme")
      (connection-write lan ses t)
      (delayed
	(plugin-notify-status lan ses 3)
	(with r (package-evaluate lan ses t)
	  (if (not (func? r 'document))
	      (set! r (tree 'document r)))
	  (plugin-notify lan ses "output" r))
	(plugin-notify-status lan ses 2))))

(define (plugin-do lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with status (plugin-status lan ses)
	(cond ((and (> (length (car l)) 2) (== (second (car l)) :start))
	       (if (== status 0)
		   (plugin-start lan ses #f)
		   (plugin-next lan ses)))
	      ((== status 0)
	       (with p (silent-encode :start noop '())
		 (pending-set lan ses (cons p l))
		 (plugin-do lan ses)))
	      (#t
	       ((first (caar l)) lan ses)))))))

(define (plugin-next lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      ((third (caar l)) lan ses)
      (pending-set lan ses (cdr l))
      (plugin-do lan ses))))

(define (plugin-cancel lan ses dead?)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      ((fourth (caar l)) lan ses dead?)
      (pending-set lan ses (cdr l))
      (plugin-cancel lan ses dead?))))

(tm-define (plugin-prompt lan ses)
  (with p (ahash-ref plugin-prompts (list lan ses))
    (if p (tree-copy p) (string-append (upcase-first lan) "] "))))

(tm-define (plugin-feed lan ses do notify next cancel args)
  (with l (pending-ref lan ses)
    (pending-set lan ses (rcons l (cons (list do notify next cancel) args)))
    (if (null? l) (plugin-do lan ses))))

(tm-define (plugin-notify lan ses ch t)
  ;;(display* "Notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (if (== ch "prompt")
	  (ahash-set! plugin-prompts (list lan ses) (tree-copy t)))
      ((second (caar l)) lan ses ch t))))

(tm-define (plugin-notify-status lan ses st)
  ;;(display* "Notify status " lan ", " ses ", " st "\n")
  (when (== st 0)
    (ahash-remove! plugin-started (list lan ses))
    (ahash-remove! plugin-prompts (list lan ses))
    (plugin-cancel lan ses #t))
  (when (== st 2)
    (plugin-next lan ses)))

(tm-define (plugin-interrupt)
  (connection-interrupt)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (plugin-cancel lan ses #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (session-encode in out next opts)
  (list (list session-do session-notify session-next session-cancel)
        (if (tm? in) (tm->stree in) in)
	(tree->tree-pointer out)
	(tree->tree-pointer next)
	opts))

(define (session-decode l)
  (list (second l)
	(tree-pointer->tree (third l))
	(tree-pointer->tree (fourth l))
	(fifth l)))

(define (session-detach l)
  (tree-pointer-detach (third l))
  (tree-pointer-detach (fourth l)))

(tm-define (session-feed lan ses in out next opts)
  (set! in (plugin-preprocess lan ses in opts))
  (tree-assign! out '(document (script-busy)))
  (with x (session-encode in out next opts)
    (apply plugin-feed `(,lan ,ses ,@(car x) ,(cdr x)))))

(tm-define (session-do lan ses)
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      ;;(display* "Session do " lan ", " ses ", " in "\n")
      (if (tree-empty? in)
	  (plugin-next lan ses)
	  (begin
	    (plugin-write lan ses in)
	    (tree-set out :up 0 (plugin-prompt lan ses))
	    (ahash-set! plugin-started (list lan ses) (texmacs-time)))))))

(tm-define (session-next lan ses)
  ;;(display* "Session next " lan ", " ses "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (when (and (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(let* ((t1 (ahash-ref plugin-started (list lan ses)))
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
      (session-detach (car l)))))	

(define (var-tree-children t)
  (with r (tree-children t)
    (if (and (nnull? r) (tree-empty? (cAr r))) (cDr r) r)))

(define (session-output t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1)))
      (if (tm-func? u 'document)
	  (tree-insert! t i (var-tree-children u))))))

(define (session-errput t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1))
	  (tree-insert! t i '((errput (document)))))
      (session-output (tree-ref t i 0) u))))

(tm-define (session-notify lan ses ch t)
  ;;(display* "Session notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (cond ((== ch "output")
	     (session-output out t))
	    ((== ch "error")
	     (session-errput out t))
	    ((== ch "prompt")
	     (if (and (null? (cdr l)) (tree-empty? (tree-ref next 1)))
		 (tree-set! next 0 (tree-copy t))))
	    ((and (== ch "input") (null? (cdr l)))
	     (tree-set! next 1 t))))))

(tm-define (session-cancel lan ses dead?)
  ;;(display* "Session cancel " lan ", " ses ", " dead? "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (when (and (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(tree-assign (tree-ref out :last)
		     (if dead? '(script-dead) '(script-interrupted))))
	(session-detach (car l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Silent evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (silent-encode in return opts)
  (list (list silent-do silent-notify silent-next silent-cancel)
        (if (tm? in) (tm->stree in) in)
	(tree 'document)
	(tree 'document)
	return
	opts))

(define (silent-decode l)
  (list (second l)
	(third l)
	(fourth l)
	(fifth l)
	(sixth l)))

(tm-define (silent-feed lan ses in return opts)
  (set! in (plugin-preprocess lan ses in opts))
  (with ret (lambda (x)
	      (return (if (npair? x) x
			  (cons (plugin-postprocess lan ses (car x) opts)
				(plugin-postprocess lan ses (cdr x) opts)))))
    (with x (silent-encode in ret opts)
      (apply plugin-feed `(,lan ,ses ,@(car x) ,(cdr x))))))

(tm-define (silent-feed* lan ses in return opts)
  (with ret (lambda (x)
	      (return (cond ((== x :dead) '(script-dead))
			    ((== x :interrupted) '(script-interrupted))
			    ((!= (tm-arity (cdr x)) 0)
			     `(with "color" "red" ,(cdr x)))
			    (else (car x)))))
    (silent-feed lan ses in ret opts)))

(tm-define (silent-do lan ses)
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      ;;(display* "Silent do " lan ", " ses ", " in "\n")
      (if (tree-empty? in)
	  (plugin-next lan ses)
	  (plugin-write lan ses in)))))

(tm-define (silent-next lan ses)
  ;;(display* "Silent next " lan ", " ses "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      (return (cons (tm->stree out) (tm->stree err))))))

(define (silent-output t u)
  (when (and (tm-func? t 'document) (tm-func? u 'document))
    (tree-insert! t (tree-arity t) (var-tree-children u))))

(tm-define (silent-notify lan ses ch t)
  ;;(display* "Silent notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      (cond ((== ch "output")
	     (silent-output out t))
	    ((== ch "error")
	     (silent-output err t))))))

(tm-define (silent-cancel lan ses dead?)
  ;;(display* "Silent cancel " lan ", " ses ", " dead? "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      (return (if dead? :dead :interrupted)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (script-feed lan ses in out opts)
  (when (not (supports-scripts? lan))
    (with s (string-append "Error:#" lan "#is not a scripting language")
      (set-message s "Evaluate")))
  (when (supports-scripts? lan)
    (tree-set! out '(script-busy))
    (with ptr (tree->tree-pointer out)
      (with ret (lambda (r)
		  (with check (tree-pointer->tree ptr)
		    (tree-pointer-detach ptr)
		    (when (== check out)
		      (with-cursor (tree->path check :end)
			(tree-select out)
			(clipboard-cut "dummy")
			(if (and (in-var-math?) (tm-func? r 'math 1))
			    (set! r (cadr r)))
			(if (in? :declaration opts)
			    (insert in)
			    (insert r))))))
	(silent-feed* lan ses in ret opts)))))
