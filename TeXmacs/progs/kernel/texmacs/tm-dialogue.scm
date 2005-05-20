
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-dialogue.scm
;; DESCRIPTION : Interactive dialogues between Scheme and C++
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-dialogue)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dialogues
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public dialogue-break #f)
(define-public dialogue-continue #f)
(define-public dialogue-return #f)

(define-public-macro (dialogue . body)
  `(begin
     (if dialogue-break (texmacs-error "dialogue" "Already in dialogue"))
     (call-with-current-continuation
      (lambda (cont)
	(set! dialogue-break cont)
	,@body
	(set! dialogue-break #f)
	(set! dialogue-continue #f)))
     (if dialogue-return (dialogue-return (noop)))))

(define-public (ask-string explain)
  (call-with-current-continuation
   (lambda (cont)
     (set! dialogue-continue cont)
     (tm-interactive (lambda (result)
		       (call-with-current-continuation
			(lambda (cont)
			  (set! dialogue-return cont)
			  (dialogue-continue result)))
		       (set! dialogue-return #f))
		     (list explain))
     (if (not dialogue-break)
	 (texmacs-error "ask-string" "Asked string outside a dialogue")
	 (dialogue-break (noop))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed execution of commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (delayed-sub body)
  (cond ((or (npair? body) (nlist? (car body)) (not (keyword? (caar body))))
	 `(lambda () ,@body #t))
	((== (caar body) :pause)
	 `(let* ((time (+ (texmacs-time) ,(cadar body)))
		 (proc ,(delayed-sub (cdr body))))
	    (lambda ()
	      (and (> (texmacs-time) time) (proc)))))
	((== (caar body) :idle)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (> (idle-time) ,(cadar body)) (proc)))))
	((== (caar body) :refresh)
	 (with sym (gensym)
	   `(begin
	      (define ,sym #f)
	      (with proc ,(delayed-sub (cdr body))
		(lambda ()
		  (and (!= ,sym (change-time))
		       (> (idle-time) ,(cadar body))
		       (proc)
		       (begin
			 (set! ,sym (change-time))
			 #f)))))))
	((== (caar body) :require)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and ,(cadar body) (proc)))))
	((== (caar body) :permanent)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (proc) (not ,(cadar body))))))
	(else (delayed-sub (cdr body)))))

(define-public-macro (delayed . body)
  `(exec-delayed ,(delayed-sub body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages and feedback on the status bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (set-temporary-message left right len)
  (set-message-temp left right #t)
  (delayed
    (:pause len)
    (recall-message)))

(define-public (texmacs-banner)
  (with tmv (string-append "GNU TeXmacs " (texmacs-version))
    (delayed
     (set-message "Welcome to GNU TeXmacs" tmv)
     (delayed
     (:pause 2500)
     (set-message "GNU TeXmacs falls under the GNU general public license" tmv)
     (delayed
     (:pause 2500)
     (set-message "GNU TeXmacs comes without any form of legal warranty" tmv)
     (delayed
     (:pause 2500)
     (set-message
      "More information about GNU TeXmacs can be found in the Help->About menu"
      tmv)
     (delayed
     (:pause 2500)
     (set-message "" ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define interactive-arg-table (make-ahash-table))

(tm-define (learn-interactive-arg fun nr value)
  (let* ((l1 (ahash-ref interactive-arg-table (list fun nr)))
	 (l2 (if l1 l1 '()))
	 (l3 (if (in? value l2) l2 (cons value l2))))
    (ahash-set! interactive-arg-table (list fun nr) l3)))

(define (compute-interactive-arg-text fun which)
  (with arg (property fun (list :argument which))
    (cond ((npair? arg) (upcase-first (symbol->string which)))
	  ((and (string? (car arg)) (null? (cdr arg))) (car arg))
	  ((string? (cadr arg)) (cadr arg))
	  (else (upcase-first (symbol->string which))))))

(define (compute-interactive-arg-type fun which)
  (with arg (property fun (list :argument which))
    (cond ((or (npair? arg) (npair? (cdr arg))) "string")
	  ((string? (car arg)) (car arg))
	  ((symbol? (car arg)) (symbol->string (car arg)))
	  (else "string"))))

(define (compute-interactive-arg-propositions fun which)
  (let* ((default (property fun (list :default which)))
	 (propositions (property fun (list :propositions which)))
	 (learned '()))
    (cond ((procedure? default) (list (default)))
	  ((procedure? propositions) (propositions))
	  (else '()))))

(define (compute-interactive-arg fun which)
  (cons (compute-interactive-arg-text fun which)
	(cons (compute-interactive-arg-type fun which)
	      (compute-interactive-arg-propositions fun which))))

(define (compute-interactive-args-try-hard fun)
  (with src (procedure-source fun)
    (if (and (pair? src) (== (car src) 'lambda)
	     (pair? (cdr src)) (list? (cadr src)))
	(map upcase-first (map symbol->string (cadr src)))
	'())))

(define (compute-interactive-args fun)
  (with args (property fun :arguments)
    (if (not args)
	(compute-interactive-args-try-hard fun)
	(map (lambda (which) (compute-interactive-arg fun which)) args))))

(define (build-interactive-arg s)
  (cond ((string-ends? s ":") s)
	((string-ends? s "?") s)
	(else (string-append s ":"))))

(define (build-interactive-args fun l nr)
  (cond ((null? l) l)
	((string? (car l))
	 (build-interactive-args
	  fun (cons (list (car l) "string") (cdr l)) nr))
	(else
	 (let* ((name (build-interactive-arg (caar l)))
		(type (cadar l))
		(pl (cddar l))
		(ql (if (null? pl) '("") pl))
		(kl (ahash-ref interactive-arg-table (list fun nr)))
		(ll (if kl kl '()))
		(props (if (<= (length ql) 1) (append ql ll) ql)))
	   (cons (cons name (cons type props))
		 (build-interactive-args fun (cdr l) (+ nr 1)))))))

(tm-define (interactive fun . args)
  (:synopsis "Call @fun with interactively specified arguments @args")
  (:interactive #t)
  (lazy-define-force fun)
  (if (null? args) (set! args (compute-interactive-args fun)))
  (tm-interactive fun (build-interactive-args fun args 0)))
