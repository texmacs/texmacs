
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
  ;;(display* "Simplify " t "\n")
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
  ;;(display* "Simplify " t "\n")
  (plugin-output-std-simplify name t))

(tm-define (plugin-preprocess name ses t opts)
  ;;(display* "Preprocess " t ", " opts "\n")
  (if (null? opts) t
      (begin
	(if (and (== (car opts) :math-input)
		 (plugin-supports-math-input-ref name))
	    (set! t (plugin-math-input (list 'tuple name t))))
	(plugin-preprocess name ses t (cdr opts)))))

(tm-define (plugin-postprocess name ses r opts)
  ;;(display* "Postprocess " r ", " opts "\n")
  (if (null? opts) r
      (begin
	(if (== (car opts) :simplify-output)
            (set! r (plugin-output-simplify name r)))
	(plugin-postprocess name ses r (cdr opts)))))

(tm-define (plugin-eval name ses t . opts)
  (with u (plugin-preprocess name ses t opts)
    ;;(display* "u= " u "\n")
    (with r (tree->stree (connection-eval name ses u))
      ;;(display* "r= " r "\n")
      (plugin-postprocess name ses r (cons :simplify-output opts)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New connection management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define plugin-pending (make-ahash-table))
(define plugin-started (make-ahash-table))
(define plugin-prompts (make-ahash-table))
(define plugin-author  (make-ahash-table))

(define (pending-set lan ses l)
  (ahash-set! plugin-pending (list lan ses) l))

(tm-define (pending-ref lan ses)
  (or (ahash-ref plugin-pending (list lan ses)) '()))

(define (plugin-status lan ses)
  (if (!= lan "scheme")
      (connection-status lan ses)
      2))

(define (plugin-set-author lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (ahash-set! plugin-author (list lan ses) (fifth (caar l))))))

(define (plugin-start lan ses)
  (when (!= lan "scheme")
    (plugin-set-author lan ses)
    (connection-start lan ses)))

(tm-define (plugin-write lan ses t)
  (ahash-set! plugin-started (list lan ses) (texmacs-time))
  (if (!= lan "scheme")
      (if (tm-func? t 'command 1)
	  (connection-write-string lan ses (cadr t))
	  (begin
	    (plugin-set-author lan ses)
	    (connection-write lan ses t)))
      (delayed
	(connection-notify-status lan ses 3)
	(with r (scheme-eval t)
	  (if (not (func? r 'document))
	      (set! r (tree 'document r)))
	  (connection-notify lan ses "output" r))
	(connection-notify-status lan ses 2))))

(define (plugin-do lan ses)
  (with l (pending-ref lan ses)
    (when (nnull? l)
      (with status (plugin-status lan ses)
	(cond ((and (> (length (car l)) 2) (== (second (car l)) :start))
	       (if (== status 0)
		   (plugin-start lan ses)
		   (plugin-next lan ses)))
	      ((== status 0)
	       (with author 0
		 (when (!= lan "scheme")
		   (set! author (new-author))
		   (start-slave author))
		 (with p (silent-encode :start noop '())
		   (set! p (cons (rcons (car p) author) (cdr p)))
		   (pending-set lan ses (cons p l))
		   (plugin-do lan ses))))
	      (#t
	       ((first (caar l)) lan ses)))))))

(tm-define (plugin-next lan ses)
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

(tm-define (plugin-timing lan ses)
  (with t (ahash-ref plugin-started (list lan ses))
    (if t (- (texmacs-time) t) 0)))

(tm-define (plugin-feed lan ses do notify next cancel args)
  (with l (pending-ref lan ses)
    (with author 0
      (when (!= lan "scheme")
	(set! author (new-author))
	(start-slave author))
      (with cb (list do notify next cancel author)
	(pending-set lan ses (rcons l (cons cb args)))
	(if (null? l) (plugin-do lan ses))))))

(tm-define (plugin-interrupt)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (if (== (connection-status lan ses) 3)
	(connection-interrupt lan ses))
    (plugin-cancel lan ses #f)))

(tm-define (plugin-stop)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (if (!= (connection-status lan ses) 0)
	(connection-stop lan ses))))

(define-public-macro (with-author a . body)
  (with old (gensym)
    `(if (not ,a)
         (begin ,@body)
         (with ,old (get-author)
           (set-author ,a)
           (with r (begin ,@body)
             (commit-changes)
             (set-author ,old)
             r)))))

(tm-define (connection-notify lan ses ch t)
  ;;(display* "Notify " lan ", " ses ", " ch ", " t "\n")
  (with-author (ahash-ref plugin-author (list lan ses))
    (with l (pending-ref lan ses)
      (when (nnull? l)
	(if (== ch "prompt")
	    (ahash-set! plugin-prompts (list lan ses) (tree-copy t)))
	((second (caar l)) lan ses ch t)))))

(tm-define (connection-notify-status lan ses st)
  ;;(display* "Notify status " lan ", " ses ", " st "\n")
  (with-author (ahash-ref plugin-author (list lan ses))
    (when (== st 0)
      (ahash-remove! plugin-started (list lan ses))
      (ahash-remove! plugin-prompts (list lan ses))
      (plugin-cancel lan ses #t))
    (when (== st 2)
      (plugin-next lan ses))))

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

(define (silent-do lan ses)
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      ;;(display* "Silent do " lan ", " ses ", " in "\n")
      (if (tree-empty? in)
	  (plugin-next lan ses)
	  (plugin-write lan ses in)))))

(define (silent-next lan ses)
  ;;(display* "Silent next " lan ", " ses "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      ;;(display* "Silent return " (tm->stree out) ", " (tm->stree err) "\n")
      (return (cons (tm->stree out) (tm->stree err))))))

(define (var-tree-children t)
  (with r (tree-children t)
    (if (and (nnull? r) (tree-empty? (cAr r))) (cDr r) r)))

(define (silent-output t u)
  (when (and (tm-func? t 'document) (tm-func? u 'document))
    (tree-insert! t (tree-arity t) (var-tree-children u))))

(define (silent-notify lan ses ch t)
  ;;(display* "Silent notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      (cond ((== ch "output")
	     (silent-output out t))
	    ((== ch "error")
	     (silent-output err t))))))

(define (silent-cancel lan ses dead?)
  ;;(display* "Silent cancel " lan ", " ses ", " dead? "\n")
  (with l (pending-ref lan ses)
    (with (in out err return opts) (silent-decode (car l))
      (return (if dead? :dead :interrupted)))))

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

(define (plugin-command-answer x)
  (if (tm-func? x 'document 1) (plugin-command-answer (cadr x))
      x))

(tm-define (plugin-command lan ses in return opts)
  (let* ((cmd `(command ,(format-command lan in)))
	 (ret (lambda (x)
		(and (pair? x)
		     (return (plugin-command-answer (car x))))))
	 (x (silent-encode cmd ret opts)))
    (apply plugin-feed `(,lan ,ses ,@(car x) ,(cdr x)))))
