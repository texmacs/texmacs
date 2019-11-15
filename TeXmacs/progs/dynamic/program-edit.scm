
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : program-edit.scm
;; DESCRIPTION : editing routines for programs
;; COPYRIGHT   : (C) 2001--2009  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic program-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils plugins plugin-cmd)
	(dynamic program-drd)
	(dynamic program-menu)
	(dynamic fold-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define program-math-input (make-ahash-table))

(define (program-key)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (cons lan ses)))

(tm-define (program-math-input?)
  (ahash-ref program-math-input (program-key)))

(tm-define (toggle-program-math-input)
  (:synopsis "Toggle mathematical input in programs.")
  (:check-mark "v" program-math-input?)
  (ahash-set! program-math-input (program-key) (not (program-math-input?)))
  (with-innermost t prog-field-context?
    (prog-field-update-math t)))

(define program-multiline-input (make-ahash-table))

(tm-define (program-multiline-input?)
  (ahash-ref program-multiline-input (program-key)))

(tm-define (set-program-multiline-input lan ses set?)
  (ahash-set! program-multiline-input (cons lan ses) set?))

(tm-define (toggle-program-multiline-input)
  (:synopsis "Toggle multi-line input in programs.")
  (:check-mark "v" program-multiline-input?)
  (ahash-set! program-multiline-input (program-key)
              (not (program-multiline-input?))))

(define program-output-timings (make-ahash-table))

(tm-define (program-output-timings?)
  (ahash-ref program-output-timings (program-key)))

(tm-define (toggle-program-output-timings)
  (:synopsis "Toggle output of evaluation timings.")
  (:check-mark "v" program-output-timings?)
  (ahash-set! program-output-timings (program-key)
              (not (program-output-timings?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific switch for session <-> program configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define program-session-mode (make-ahash-table))

(logic-table session-program%
  (session          program)
  (folded-io        folded-prog-io)
  (unfolded-io      unfolded-prog-io)
  (folded-io-math   folded-prog-io-math)
  (unfolded-io-math unfolded-prog-io-math))

(define (session-node->program-node u)
  (logic-ref session-program% (tree-label u)))

(define (program-node->session-node u)
  (with r (query `(session-program% 'x ,(tree-label u)))
    (if (nnull? r) (cdaar r) #f)))

(define (session->program t)
  (ahash-set! program-session-mode (program-key) #t)
  (tree-replace
    t session-node->program-node
    (lambda (u)
      (if (and (tree-compound? u) (not (tree-is? u 'session)))
        (tree-insert! u (tree-arity u) '("")))
      (if (tree-compound? t)
        (for-each (lambda (t) (session->program t)) (tree-children u)))
      (tree-assign-node! u (session-node->program-node u)))))

(define (program->session t)
  (ahash-set! program-session-mode (program-key) #f)
  (tree-replace
    t program-node->session-node
    (lambda (u)
      (if (and (tree-compound? u) (not (tree-is? u 'program)))
        (tree-remove! u (- (tree-arity u) 1) 1))
      (if (tree-compound? t)
        (for-each (lambda (t) (program->session t)) (tree-children u)))
      (tree-assign-node! u (program-node->session-node u)))))

(tm-define (program-session-mode?)
  (ahash-ref program-session-mode (program-key)))

(tm-define (toggle-session-program)
  (:synopsis "Toggle evaluation mode.")
  (:check-mark "v" program-session-mode?)
  (with t (tree-innermost '(program session))
    (when t
      (if (program-session-mode?)
        (program->session t)
        (session->program t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific switches for Scheme programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define program-scheme-trees #t)

(tm-define (program-scheme-trees?)
  program-scheme-trees)

(tm-define (toggle-program-scheme-trees)
  (:synopsis "Toggle pretty tree output in scheme programs.")
  (:check-mark "v" program-scheme-trees?)
  (set! program-scheme-trees (not program-scheme-trees)))

(define program-scheme-strees #t)

(tm-define (program-scheme-strees?)
  program-scheme-strees)

(tm-define (toggle-program-scheme-strees)
  (:synopsis "Toggle pretty scheme tree output in scheme programs.")
  (:check-mark "v" program-scheme-strees?)
  (set! program-scheme-strees (not program-scheme-strees)))

(define program-scheme-math #f)

(tm-define (program-scheme-math?)
  program-scheme-math)

(tm-define (toggle-program-scheme-math)
  (:synopsis "Toggle pretty math output in scheme programs.")
  (:check-mark "v" program-scheme-math?)
  (set! program-scheme-math (not program-scheme-math)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-newline s)
  (with l (string-tokenize-by-char s #\newline)
    (if (<= (length l) 1) s
	(tm->tree `(document ,@l)))))

(define (var-object->string t)
  (with s (object->string t)
    (if (== s "#<unspecified>") "" (replace-newline (string->tmstring s)))))

(define (eval-string-with-catch s)
  (catch #t
         (lambda () (eval (string->object s)))
         (lambda (key msg . err-msg)
           (let* ((msg (car err-msg))
                  (args (cadr err-msg))
                  (err-msg 
                    (if (list? args) (eval (apply format #f msg args)) msg)))
             (stree->tree `(errput ,err-msg))))))

(define (error-tree? t)
  (and (tree? t) (tree-is? t 'errput)))

(tm-define (program-scheme-eval t mode)
  ;; FIXME: unify with scheme-eval
  (let* ((s (texmacs->code t "iso-8859-1"))
	 (r (eval-string-with-catch s)))
    (cond ((and (tree? r) (error-tree? r) (program-scheme-trees?))
           (tree-copy r))
          ((and (tree? r) (program-scheme-trees?))
           (tree 'text (tree-copy r)))
          ((and (tm? r) (== mode :silent)
                (or (program-scheme-trees?) (program-scheme-strees?)))
           (tree-copy (tm->tree r)))
          ((and (tm? r) (program-scheme-strees?))
           (tree 'text (tree-copy (tm->tree r))))
          ((program-scheme-math?)
           (with m (cas->stree r)
                 (if (tm? m) (tree 'math (tm->tree m)) (var-object->string r))))
          (else (var-object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level evaluation management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (program-encode in out next opts)
  (list (list program-do program-notify program-next program-cancel)
        (if (tm? in) (tm->stree in) in)
	(tree->tree-pointer out)
	(tree->tree-pointer next)
	opts))

(define (program-decode l)
  (list (second l)
	(tree-pointer->tree (third l))
	(tree-pointer->tree (fourth l))
	(fifth l)))

(define (program-detach l)
  (tree-pointer-detach (third l))
  (tree-pointer-detach (fourth l)))

(define (program-coherent? out next)
  (and (prog-field-or-output-context? (tree-ref out :up))
       (prog-field-context? next)))

(define (program-do lan ses)
  (with l (pending-ref lan ses)
    (with (in out next opts) (program-decode (car l))
      ;;(display* "Program do " lan ", " ses ", " in "\n")
      (if (or (and (tree-empty? in) (!= lan "r"))
	      (not (program-coherent? out next)))
	  (plugin-next lan ses)
	  (begin
	    (plugin-write lan ses in :program)
	    (tree-set out :up 0 (plugin-prompt lan ses)))))))

(define (program-next lan ses)
  ;;(display* "Program next " lan ", " ses "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (program-decode (car l))
      (when (and (program-coherent? out next)
		 (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(let* ((dt (plugin-timing lan ses))
	       (ts (if (< dt 1000)
		       (string-append (number->string dt) " msec")
		       (string-append (number->string (/ dt 1000.0)) " sec"))))
	  (if (and (in? :timings opts) (>= dt 1))
	      (tree-set (tree-ref out :last) `(timing ,ts))
	      (tree-remove! out (- (tree-arity out) 1) 1))))
      (when (and (program-coherent? out next)
		 (tree-empty? out))
	(prog-field-remove-output (tree-ref out :up)))
      (program-detach (car l)))))

(define (var-tree-children t)
  (with r (tree-children t)
    (if (and (nnull? r) (tree-empty? (cAr r))) (cDr r) r)))

(define (program-output t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1)))
      (if (tm-func? u 'document)
	  (tree-insert! t i (var-tree-children u))))))

(define (program-errput t u)
  (when (tm-func? t 'document)
    (with i (tree-arity t)
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'script-busy))
	  (set! i (- i 1)))
      (if (and (> i 0) (tm-func? (tree-ref t (- i 1)) 'errput))
	  (set! i (- i 1))
	  (tree-insert! t i '((errput (document)))))
      (program-output (tree-ref t i 0) u))))

(define (program-notify lan ses ch t)
  ;;(display* "Program notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (program-decode (car l))
      (when (program-coherent? out next)
	(cond ((== ch "output")
	       (program-output out t))
	      ((== ch "error")
	       (program-errput out t))
	      ((== ch "prompt")
	       (if (and (== (length l) 1) (tree-empty? (tree-ref next 1)))
		   (tree-set! next 0 (tree-copy t))))
	      ((and (== ch "input") (null? (cdr l)))
	       (tree-set! next 1 t)))))))

(define (program-cancel lan ses dead?)
  ;;(display* "Program cancel " lan ", " ses ", " dead? "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (program-decode (car l))
      (when (and (program-coherent? out next)
		 (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(tree-assign (tree-ref out :last)
		     (if dead? '(script-dead) '(script-interrupted))))
      (program-detach (car l)))))

(tm-define (program-feed lan ses in out next opts)
  (set! in (plugin-preprocess lan ses in opts))
  (tree-assign! out '(document (script-busy)))
  (with x (program-encode in out next opts)
    (apply plugin-feed `(,lan ,ses ,@(car x) ,(cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-document-context? t)
  (and (tm-func? t 'document)
       (tm-func? (tree-ref t :up) 'program)))

(tm-define (subprogram-document-context? t)
  (or (and (tm-func? t 'document)
	   (tm-func? (tree-ref t :up) 'program))
      (and (tm-func? t 'document)
	   (tm-func? (tree-ref t :up) 'unfolded-subprogram)
	   (== (tree-index t) 1))))

(tm-define prog-field-tags
  '(input unfolded-prog-io folded-prog-io
    input-math unfolded-prog-io-math folded-prog-io-math))

(tm-define (prog-field-context? t)
  (and (tm? t)
       (tree-in? t prog-field-tags)
       (tm-func? (tree-ref t :up) 'document)
       (tree-in? (tree-ref t :up :up) '(program))))

(tm-define (prog-field-or-output-context? t)
  (and (tm? t)
       (tree-in? t (cons 'output prog-field-tags))
       (tm-func? (tree-ref t :up) 'document)
       (tree-in? (tree-ref t :up :up) '(program))))

(tm-define (prog-field-folded-context? t)
  (and (tree-in? t '(folded-prog-io folded-prog-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (prog-field-unfolded-context? t)
  (and (tree-in? t '(unfolded-prog-io unfolded-prog-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (prog-field-prog-context? t)
  (and (tree-in? t '(input folded-prog-io unfolded-prog-io))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (prog-field-math-context? t)
  (and (tree-in? t '(input-math folded-prog-io-math unfolded-prog-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (prog-field-input-context? t)
  (and (prog-field-context? t)
       (== (tree-down-index t) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prog-field-parameters kind)
  (let* ((var (string-append (get-env "prog-language") "-" kind))
         (gen (string-append "generic-" kind)))
    (search-parameters (if (style-has? var) var gen))))

(tm-define (standard-parameters l)
  (:require (== l "program"))
  (prog-field-parameters "program"))

(tm-define (standard-parameters l)
  (:require (== l "input"))
  (prog-field-parameters "input"))

(tm-define (standard-parameters l)
  (:require (== l "output"))
  (prog-field-parameters "output"))

(tm-define (standard-parameters l)
  (:require (== l "errput"))
  (prog-field-parameters "errput"))

(tm-define (standard-parameters l)
  (:require (== l "textput"))
  (prog-field-parameters "textput"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-ready? . err-flag?)
  (with lan (get-env "prog-language")
    (or (== lan "scheme")
	(connection-defined? lan)
	(begin
	  (if err-flag?
	      (set-message `(concat "undefined plugin: " (verbatim ,lan)) ""))
	  #f))))

(tm-define (program-status)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-program")))
    (cond ((== lan "scheme") 2)
	  ((not (connection-defined? lan)) 0)
	  (else (connection-status lan ses)))))

(tm-define (program-busy-message msg)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-program")))
    (with l (pending-ref lan ses)
      (for-each
       (lambda (x)
         (with (in out next opts) (program-decode x)
           (when (and (tm-func? out 'document)
                      (tm-func? (tree-ref out :last) 'script-busy))
             (tree-assign (tree-ref out :last) `(script-busy ,msg)))))
       l))))

(tm-define (program-alive?)
  (> (program-status) 1))

(tm-define (program-supports-completions?)
  (and (program-alive?)
       (plugin-supports-completions? (get-env "prog-language"))))

(tm-define (program-supports-input-done?)
  (and (program-alive?)
       (plugin-supports-input-done? (get-env "prog-language"))))

(define (prog-field-next* t forward?)
  (and-with u (tree-ref t (if forward? :next :previous))
    (cond ((prog-field-context? u) u)
          ((tree-in? u '(folded-subprogram unfolded-subprogram)) #f)
          (else (prog-field-next u forward?)))))

(define (prog-field-next t forward?)
  (and-with u (tree-ref t (if forward? :next :previous))
    (if (prog-field-context? u) u (prog-field-next u forward?))))

(define (prog-field-extreme t last?)
  (with u (tree-ref t :up (if last? :last :first))
    (if (prog-field-context? u) u
	(prog-field-next u (not last?)))))

(define (prog-field-insert-output t)
  (cond ((tm-func? t 'input)
	 (tree-insert! t 2 (list '(document) ""))
	 (tree-assign-node! t 'unfolded-prog-io))
	((tm-func? t 'input-math)
	 (tree-insert! t 2 (list '(document) ""))
	 (tree-assign-node! t 'unfolded-prog-io-math))))

(define (prog-field-remove-output t)
  (cond ((or (tm-func? t 'folded-prog-io) (tm-func? t 'unfolded-prog-io))
	 (tree-assign-node! t 'input)
	 (tree-remove! t 2 2))
	((or (tm-func? t 'folded-prog-io-math) (tm-func? t 'unfolded-prog-io-math))
	 (tree-assign-node! t 'input-math)
	 (tree-remove! t 2 2))
	((tm-func? t 'output)
	 (with p (tree-ref t :up)
	   (when (tree-is? p 'document)
	     (tree-remove! p (tree-index t) 1))))))

(define (prog-field-update-math t)
  (if (program-math-input?)
      (when (prog-field-prog-context? t)
	(if (tm-func? t 'input)
	    (tree-assign-node! t 'input-math)
	    (begin
	      (tree-assign-node! t 'folded-prog-io-math)
	      (tree-assign (tree-ref t 1) '(document "")))))
      (when (prog-field-math-context? t)
	(if (tm-func? t 'input-math)
	    (tree-assign-node! t 'input)
	    (begin
	      (tree-assign-node! t 'folded-prog-io)
	      (tree-assign (tree-ref t 1) '(document "")))))))

(define (prog-field-create t p forward?)
  (let* ((d (tree-ref t :up))
	 (i (+ (tree-index t) (if forward? 1 0)))
	 (l (if (program-math-input?) 'input-math 'input))
	 (b `(,l ,p (document ""))))
    (tree-insert d i (list b))
    (tree-ref d i)))

(define (program-forall-sub fun t)
  (for (u (tree-children t))
    (when (prog-field-context? u)
      (fun u))
    (when (and (tm-func? u 'unfolded-subprogram)
	       (tm-func? (tree-ref u 1) 'document))
      (program-forall-sub fun (tree-ref u 1)))))

(define (program-forall fun)
  (with-innermost t subprogram-document-context?
    (program-forall-sub fun t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-program lan ses)
  (let* ((ban `(output (document "")))
	 (l (if (program-math-input?) 'input-math 'input))
	 (p (plugin-prompt lan ses))
	 (in `(,l (document ,p) (document "")))
	 (s `(program ,lan ,ses (document ,ban ,in))))
    (insert-go-to s '(2 1 1 0 0))
    (ahash-set! program-session-mode (program-key) #t)
    (with-innermost t prog-field-input-context?
      (with u (tree-ref t :previous 0)
	(if (url-exists? (url-unix "$TEXMACS_STYLE_PATH"
				   (string-append lan ".ts")))
	    (add-style-package lan))
	(program-feed lan ses :start u t '())))))

(define (input-options t)
  (with opts '()
    (when (program-output-timings?) (set! opts (cons :timings opts)))
    (when (prog-field-math-context? t) (set! opts (cons :math-input opts)))
    opts))

(define (prog-field-process-input t)
  (when (program-ready? #t)
    (prog-field-insert-output t)
    (cond ((tm-func? t 'folded-prog-io)
	   (tree-assign-node! t 'unfolded-prog-io))
	  ((tm-func? t 'folded-prog-io-math)
	   (tree-assign-node! t 'unfolded-prog-io-math)))
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-program"))
	   (p (plugin-prompt lan ses))
	   (in (tree->stree (tree-ref t 1)))
	   (out (tree-ref t 2))
	   (opts (input-options t)))
      (with u (or (prog-field-next* t #t) (prog-field-create t p #t))
	(program-feed lan ses in out u opts)
	(tree-go-to u 1 :end)))))

(define (kbd-enter-sub t done?)
  (if (in? done? (list #f "#f"))
      (insert-return)
      (delayed
        (:idle 1)
        (program-evaluate))))

(tm-define (kbd-enter t shift?)
  (:require (prog-field-input-context? t))
  (cond ((xor (program-multiline-input?) shift?)
         (insert-return))
        ((program-supports-input-done?)
         (let* ((lan (get-env "prog-language"))
                (ses (get-env "prog-program"))
                (opts (input-options t))
                (st (tree->stree (tree-ref t 1)))
                (pre (plugin-preprocess lan ses st opts))
                (in (plugin-serialize lan pre))
                (rew (if (string-ends? in "\n") (string-drop-right in 1) in))
                (cmd (string-append "(input-done? " (string-quote rew) ")"))
                (ret (lambda (done?) (kbd-enter-sub t done?))))
           (plugin-command lan ses cmd ret '())))
        (else (program-evaluate))))

(tm-define (program-evaluate)
  (with-innermost t prog-field-input-context?
    (prog-field-process-input t)))

(tm-define (program-evaluate-all)
  (program-forall
    (lambda (t)
      (when (not (tree-empty? (tree-ref t 1)))
	(prog-field-process-input t)))))

(tm-define (program-evaluate-above)
  (with-innermost me prog-field-input-context?
    (program-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf? (tree->path t) (tree->path me))
	    (prog-field-process-input t)))))))

(tm-define (program-evaluate-below)
  (with-innermost me prog-field-input-context?
    (program-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf-eq? (tree->path me) (tree->path t))
	    (prog-field-process-input t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-horizontal t forwards?)
  (:require (prog-field-context? t))
  (with move (if forwards? go-right go-left)
    (go-to-remain-inside move prog-field-context? 1)))

(tm-define (kbd-extremal t forwards?)
  (:require (prog-field-context? t))
  (with move (if forwards? go-end-line go-start-line)
    (go-to-remain-inside move prog-field-context? 1)))

(define (prog-field-go-to-previous)
  (with-innermost t prog-field-context?
    (with u (tree-ref t :previous)
      (if (and u (prog-field-context? u))
	  (tree-go-to u 1 :end)
	  (go-to-previous-tag-same-argument prog-field-tags)))))

(define (prog-field-go-to-next)
  (with-innermost t prog-field-context?
    (with u (tree-ref t :next)
      (if (and u (prog-field-context? u))
	  (tree-go-to u 1 :start)
	  (go-to-next-tag-same-argument prog-field-tags))
      (go-end-line))))

(define (prog-field-go-up)
  (with p (cursor-path)
    (go-to-remain-inside go-up prog-field-context? 1)
    (when (== (cursor-path) p)
      (prog-field-go-to-previous))))

(define (prog-field-go-down)
  (with p (cursor-path)
    (go-to-remain-inside go-down prog-field-context? 1)
    (when (== (cursor-path) p)
      (prog-field-go-to-next))))

(tm-define (kbd-vertical t downwards?)
  (:require (prog-field-context? t))
  (if downwards? (prog-field-go-down) (prog-field-go-up)))

(tm-define (kbd-incremental t downwards?)
  (:require (prog-field-context? t))
  (for (n 0 5)
    (if downwards? (prog-field-go-to-next) (prog-field-go-to-previous))))

(tm-define (kbd-remove t forwards?)
  (:require (prog-field-input-context? t))
  (cond ((and (tree-cursor-at? t 1 :start) (not forwards?)) (noop))
        ((and (tree-cursor-at? t 1 :end) forwards?) (noop))
        (else (remove-text forwards?))))

(tm-define (kbd-remove t forwards?)
  (:require (and (prog-field-input-context? t) (selection-active-any?)))
  (clipboard-cut "nowhere")
  (clipboard-clear "nowhere"))

(tm-define (kbd-variant t forwards?)
  (:require (and (prog-field-context? t) (program-supports-completions?)))
  (let* ((lan (get-env "prog-language"))
         (ses (get-env "prog-program"))
         (cmd (session-complete-command t))
         (ret (lambda (x) (when x (custom-complete (tm->tree x))))))
    (when (!= cmd "")
      (plugin-command lan ses cmd ret '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured keyboard movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-context? t)
  (:require (and (tree-is? t 'document)
                 (prog-field-input-context? (tree-ref t :up))))
  #f)

(tm-define (traverse-horizontal t forwards?)
  (:require (prog-field-input-context? t))
  (with move (if forwards? go-to-next-word go-to-previous-word)
    (go-to-remain-inside move prog-field-context? 1)))

(tm-define (traverse-vertical t downwards?)
  (:require (prog-field-input-context? t))
  (if downwards? (prog-field-go-down) (prog-field-go-up)))

(tm-define (traverse-extremal t forwards?)
  (:require (prog-field-input-context? t))
  (with move (if forwards? prog-field-go-down prog-field-go-up)
    (go-to-repeat move)))

(tm-define (traverse-incremental t downwards?)
  (:require (prog-field-input-context? t))
  (if downwards? (prog-field-go-down) (prog-field-go-up)))

(tm-define (structured-horizontal t forwards?)
  (:require (prog-field-input-context? t))
  (noop))

(tm-define (structured-vertical t downwards?)
  (:require (prog-field-input-context? t))
  (with move (if downwards? prog-field-go-down prog-field-go-up)
    (go-to-remain-inside move 'program)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold and unfold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (prog-field-unfolded-context? t))
  (with i (tree-down-index t)
    (variant-set t (ahash-ref alternate-table (tree-label t)))
    (if (== i 2) (tree-go-to t 1 :end))))

(tm-define (alternate-toggle t)
  (:require (prog-field-folded-context? t))
  (variant-set t (ahash-ref alternate-table (tree-label t))))

(tm-define (prog-field-fold t)
  (when (prog-field-unfolded-context? t)
    (alternate-toggle t)))

(tm-define (prog-field-unfold t)
  (when (prog-field-folded-context? t)
    (alternate-toggle t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (prog-field-insert t* forwards?)
  (and-with t (tree-search-upwards t* prog-field-input-context?)
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-program"))
	   (p (plugin-prompt lan ses))
	   (t (prog-field-create t p forwards?)))
      (tree-go-to t 1 :end))))

(tm-define (prog-field-insert-text t* forward?)
  (and-with t (tree-search-upwards t* prog-field-input-context?)
    (let* ((d (tree-ref t :up))
	   (i (+ (tree-index t) (if forward? 1 0)))
	   (b `(textput (document ""))))
      (tree-insert d i (list b))
      (tree-go-to d i 0 :start))))

(tm-define (prog-field-remove-banner t*)
  (and-with t (tree-search-upwards t* program-document-context?)
    (when (tm-func? (tree-ref t 0) 'output)
      (tree-remove! t 0 1))))

(tm-define (prog-field-remove-extreme t* last?)
  (and-with t (tree-search-upwards t* prog-field-input-context?)
    (with u (prog-field-extreme t last?)
      (with v (prog-field-next t (not last?))
	(if (and (== u t) v)
	    (tree-go-to v 1 :end))
	(if (or (!= u t) v)
	    (tree-remove (tree-ref u :up) (tree-index u) 1))))))

(tm-define (prog-field-remove t* forwards?)
  (and-with t (tree-search-upwards t* prog-field-input-context?)
    (if forwards?
        (with u (prog-field-next t #t)
          (if u (begin
                  (tree-remove (tree-ref t :up) (tree-index t) 1)
                  (tree-go-to u 1 :start))
              (prog-field-remove-extreme t #t)))
        (with u (prog-field-next* t #f)
          (if u (tree-remove (tree-ref u :up) (tree-index u) 1)
              (prog-field-remove-banner t))))))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (prog-field-input-context? t))
  (if forwards? (prog-field-insert-fold t)))

(tm-define (structured-insert-vertical t downwards?)
  (:require (prog-field-input-context? t))
  (prog-field-insert t downwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (prog-field-input-context? t))
  (prog-field-remove t forwards?))

(tm-define (structured-remove-vertical t forwards?)
  (:require (prog-field-input-context? t))
  (prog-field-remove t forwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Program management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-clear-all)
  (program-forall prog-field-remove-output))

(tm-define (program-fold-all)
  (program-forall prog-field-fold))

(tm-define (program-unfold-all)
  (program-forall prog-field-unfold))

(tm-define (prog-field-insert-fold t*)
  (and-with t (tree-search-upwards t* prog-field-input-context?)
    (tree-set! t `(unfolded-subprogram (document "") (document ,t)))
    (tree-go-to t 0 :end)))

(tm-define (program-split)
  (with-innermost t program-document-context?
    (let* ((u (tree-ref t :up)) ;; program
	   (v (tree-ref u :up)) ;; document
	   (i (+ (tree-down-index t) 1))
	   (j (tree-index u))
	   (lan (tree-ref u 0))
	   (ses (tree-ref u 1)))
      (when (< i (tree-arity t))
	(tree-remove! u 0 2)
	(tree-split! u 0 i)
	(tree-split! v j 1)
	(tree-insert (tree-ref v j) 0 `(,lan ,ses))
	(tree-insert (tree-ref v (+ j 1)) 0 `(,lan ,ses))
	(tree-insert v (+ j 1) '((document "")))
	(tree-go-to v (+ j 1) :end)))))
