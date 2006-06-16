
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

(with-module texmacs-user ;; switch modules for old versions of Guile
  (define-public dialogue-break #f)
  (define-public dialogue-return #f)
  (define-public dialogue-error #f))

(define-public (dialogue-report-errors)
  (if dialogue-error
      (with error dialogue-error
	(set! dialogue-error #f)
	(apply throw error))))

(define-public-macro (dialogue . body)
  (cond
    (dialogue-break
     `(begin ,@body))
    (dialogue-return
     `(begin
	(exec-delayed (lambda () (dialogue ,@body)))
	(dialogue-return (noop))))
    (else
     `(begin
	(with-cc cont
	  (set! dialogue-break cont)
	  (catch #t
		 (lambda () ,@body)
		 (lambda err (set! dialogue-error err)))
	  (set! dialogue-break #f))
	(if dialogue-return (dialogue-return (noop)))
	(dialogue-report-errors)))))

(define-public ((dialogue-machine local-continue) result)
  (with-cc cont
    (set! dialogue-return cont)
    (local-continue result))
  (set! dialogue-return #f)
  (dialogue-report-errors))

(define-public-macro (dialogue-user local-continue . body)
  `(with local-break dialogue-break
     (set! dialogue-break #f)
     (with r (with-cc ,local-continue
	       ,@body
	       (local-break (noop)))
       (set! dialogue-break local-break)
       r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple questions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (dialogue-ask prompt)
  (if dialogue-break
      (dialogue-user local-continue
	(tm-interactive (dialogue-machine local-continue)
			(if (string? prompt)
			    (list (build-interactive-arg prompt))
			    (list prompt))))
      (texmacs-error "dialogue-ask" "Not in dialogue")))

(define (yes)
  (with lan (get-output-language)
    (cond ((== lan "french") "oui")
	  ((in? lan '("dutch" "german")) "ja")
	  ((in? lan '("italian" "spanish")) "si")
	  (else "yes"))))

(define (no)
  (with lan (get-output-language)
    (cond ((== lan "french") "non")
	  ((== lan "dutch") "nee")
	  ((== lan "german") "nein")
	  (else "no"))))

(define-public (dialogue-confirm? prompt default)
  (if default
      (yes? (dialogue-ask (list prompt "question" (yes) (no))))
      (yes? (dialogue-ask (list prompt "question" (no) (yes))))))

(define-public (dialogue-url prompt type)
  (if dialogue-break
      (dialogue-user local-continue
	(delayed
	  (choose-file (dialogue-machine local-continue) prompt type)))
      (texmacs-error "dialogue-ask" "Not in dialogue")))

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
	((== (caar body) :every)
	 `(let* ((time (+ (texmacs-time) ,(cadar body)))
		 (proc ,(delayed-sub (cdr body))))
	    (lambda ()
	      (and (> (texmacs-time) time)
		   (begin (set! time (+ (texmacs-time) ,(cadar body))) #t)
		   (proc)))))
	((== (caar body) :idle)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (> (idle-time) ,(cadar body)) (proc)))))
	((== (caar body) :refresh)
	 (with sym (gensym)
	   `(let* ((,sym #f)
		   (proc ,(delayed-sub (cdr body))))
	      (lambda ()
		(and (!= ,sym (change-time))
		     (> (idle-time) ,(cadar body))
		     (begin (set! ,sym (change-time)) #t)
		     (proc))))))
	((== (caar body) :require)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and ,(cadar body) (proc)))))
	((== (caar body) :while)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (if ,(cadar body) (begin (proc) #f) #t))))
	((== (caar body) :clean)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (proc) (begin ,(cadar body) #t)))))
	((== (caar body) :permanent)
	 `(with proc ,(delayed-sub (cdr body))
	    (lambda ()
	      (and (proc) (not ,(cadar body))))))
	(else (delayed-sub (cdr body)))))

(define-public-macro (delayed . body)
  (if dialogue-break
      `(dialogue-user local-continue
	 (exec-delayed
	  (with proc ,(delayed-sub body)
	    (lambda ()
	      (with r (proc)
		(if r ((dialogue-machine local-continue) (noop)))
		r)))))
      `(exec-delayed ,(delayed-sub body))))

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
     (:pause 5000)
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

(define (list-but l1 l2)
  (cond ((null? l1) l1)
	((in? (car l1) l2) (list-but (cdr l1) l2))
	(else (cons (car l1) (list-but (cdr l1) l2)))))

(define-public (learn-interactive-arg fun nr value)
  "Learn interactive @value for @nr-th argument of @fun"
  (if (procedure-name fun) (set! fun (procedure-name fun)))
  (let* ((l1 (ahash-ref interactive-arg-table (list fun nr)))
	 (l2 (if l1 l1 '()))
	 (l3 (cons value (list-but l2 (list value)))))
    (ahash-set! interactive-arg-table (list fun nr) l3)))

(define (learned-interactive-arg fun nr)
  (if (procedure-name fun) (set! fun (procedure-name fun)))
  (with l (ahash-ref interactive-arg-table (list fun nr))
    (if l l '())))

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

(define (compute-interactive-arg-proposals fun which)
  (let* ((default (property fun (list :default which)))
	 (proposals (property fun (list :proposals which)))
	 (learned '()))
    (cond ((procedure? default) (list (default)))
	  ((procedure? proposals) (proposals))
	  (else '()))))

(define (compute-interactive-arg fun which)
  (cons (compute-interactive-arg-text fun which)
	(cons (compute-interactive-arg-type fun which)
	      (compute-interactive-arg-proposals fun which))))

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
		(ql pl)
		;;(ql (if (null? pl) '("") pl))
		(ll (learned-interactive-arg fun nr))
		(rl (append ql (list-but ll ql)))
		(props (if (<= (length ql) 1) rl ql)))
	   (cons (cons name (cons type props))
		 (build-interactive-args fun (cdr l) (+ nr 1)))))))

(tm-define (interactive fun . args)
  (:synopsis "Call @fun with interactively specified arguments @args")
  (:interactive #t)
  (lazy-define-force fun)
  (if (null? args) (set! args (compute-interactive-args fun)))
  (with fun-args (build-interactive-args fun args 0)
    (if dialogue-break
	(dialogue-user local-continue
	  (tm-interactive
	   (lambda args*
	     (with r* (apply fun args*)
	       ((dialogue-machine local-continue) r*)
	       r*))
	   fun-args))
	(tm-interactive fun fun-args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store learned arguments from one session to another
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-learned)
  (let* ((l1 (ahash-table->list interactive-arg-table))
	 (pred? (lambda (l) (symbol? (caar l))))
	 (l2 (list-filter l1 pred?)))
    (save-object "$TEXMACS_HOME_PATH/system/interactive.scm" l2)))

(define (retrieve-learned)
  (if (url-exists? "$TEXMACS_HOME_PATH/system/interactive.scm")
      (with l (load-object "$TEXMACS_HOME_PATH/system/interactive.scm")
	(set! interactive-arg-table (list->ahash-table l)))))

(on-entry (retrieve-learned))
(on-exit (save-learned))
