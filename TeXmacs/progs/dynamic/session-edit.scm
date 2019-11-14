
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : session-edit.scm
;; DESCRIPTION : editing routines for sessions
;; COPYRIGHT   : (C) 2001--2009  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic session-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils plugins plugin-cmd)
	(dynamic session-drd)
	(dynamic fold-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style package rules for sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (style-category p)
  (:require (in? p (list "framed-session" "ring-session" "large-formulas")))
  :session-theme)

(tm-define (style-category-precedes? x y)
  (:require (and (== x :session-theme)
                 (in? y (map symbol->string (plugin-list)))))
  #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define session-math-input (make-ahash-table))

(define (session-key)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (cons lan ses)))

(tm-define (session-math-input?)
  (ahash-ref session-math-input (session-key)))

(tm-define (toggle-session-math-input)
  (:synopsis "Toggle mathematical input in sessions.")
  (:check-mark "v" session-math-input?)
  (ahash-set! session-math-input (session-key) (not (session-math-input?)))
  (with-innermost t field-context?
    (field-update-math t)))

(define session-multiline-input (make-ahash-table))

(tm-define (session-multiline-input?)
  (ahash-ref session-multiline-input (session-key)))

(tm-define (set-session-multiline-input lan ses set?)
  (ahash-set! session-multiline-input (cons lan ses) set?))

(tm-define (toggle-session-multiline-input)
  (:synopsis "Toggle multi-line input in sessions.")
  (:check-mark "v" session-multiline-input?)
  (ahash-set! session-multiline-input (session-key)
              (not (session-multiline-input?))))

(define session-output-timings (make-ahash-table))

(tm-define (session-output-timings?)
  (ahash-ref session-output-timings (session-key)))

(tm-define (toggle-session-output-timings)
  (:synopsis "Toggle output of evaluation timings.")
  (:check-mark "v" session-output-timings?)
  (ahash-set! session-output-timings (session-key)
              (not (session-output-timings?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific switches for Scheme sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define session-scheme-trees #t)

(tm-define (session-scheme-trees?)
  session-scheme-trees)

(tm-define (toggle-session-scheme-trees)
  (:synopsis "Toggle pretty tree output in scheme sessions.")
  (:check-mark "v" session-scheme-trees?)
  (set! session-scheme-trees (not session-scheme-trees)))

(define session-scheme-strees #f)

(tm-define (session-scheme-strees?)
  session-scheme-strees)

(tm-define (toggle-session-scheme-strees)
  (:synopsis "Toggle pretty scheme tree output in scheme sessions.")
  (:check-mark "v" session-scheme-strees?)
  (set! session-scheme-strees (not session-scheme-strees)))

(define session-scheme-math #f)

(tm-define (session-scheme-math?)
  session-scheme-math)

(tm-define (toggle-session-scheme-math)
  (:synopsis "Toggle pretty math output in scheme sessions.")
  (:check-mark "v" session-scheme-math?)
  (set! session-scheme-math (not session-scheme-math)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme sessions
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

(tm-define (scheme-eval t mode)
  (let* ((s (texmacs->code t "iso-8859-1"))
	 (r (eval-string-with-catch s)))
    (cond ((and (tree? r) (error-tree? r) (session-scheme-trees?))
           (tree-copy r))
          ((and (tree? r) (session-scheme-trees?))
           (tree 'text (tree-copy r)))
          ((and (tm? r) (or (and (== mode :silent) (session-scheme-trees?))
                            (session-scheme-strees?)))
           (tree 'text (tree-copy (tm->tree r))))
          ((session-scheme-math?)
           (with m (cas->stree r)
                 (if (tm? m) (tree 'math (tm->tree m)) (var-object->string r))))
          (else (var-object->string r)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level evaluation management
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

(define (session-coherent? out next)
  (and (field-or-output-context? (tree-ref out :up))
       (field-context? next)))

(define (session-do lan ses)
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      ;;(display* "Session do " lan ", " ses ", " in "\n")
      (if (or (and (tree-empty? in) (!= lan "r"))
	      (not (session-coherent? out next)))
	  (plugin-next lan ses)
	  (begin
	    (plugin-write lan ses in :session)
	    (tree-set out :up 0 (plugin-prompt lan ses)))))))

(define (session-next lan ses)
  ;;(display* "Session next " lan ", " ses "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (when (and (session-coherent? out next)
		 (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(let* ((dt (plugin-timing lan ses))
	       (ts (if (< dt 1000)
		       (string-append (number->string dt) " msec")
		       (string-append (number->string (/ dt 1000.0)) " sec"))))
	  (if (and (in? :timings opts) (>= dt 1))
	      (tree-set (tree-ref out :last) `(timing ,ts))
	      (tree-remove! out (- (tree-arity out) 1) 1))))
      (when (and (session-coherent? out next)
		 (tree-empty? out))
	(field-remove-output (tree-ref out :up)))
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

(define (session-notify lan ses ch t)
  ;;(display* "Session notify " lan ", " ses ", " ch ", " t "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (when (session-coherent? out next)
	(cond ((== ch "output")
	       (session-output out t))
	      ((== ch "error")
	       (session-errput out t))
	      ((== ch "prompt")
	       (if (and (== (length l) 1) (tree-empty? (tree-ref next 1)))
		   (tree-set! next 0 (tree-copy t))))
	      ((and (== ch "input") (null? (cdr l)))
	       (tree-set! next 1 t)))))))

(define (session-cancel lan ses dead?)
  ;;(display* "Session cancel " lan ", " ses ", " dead? "\n")
  (with l (pending-ref lan ses)
    (with (in out next opts) (session-decode (car l))
      (when (and (session-coherent? out next)
		 (tm-func? out 'document)
		 (tm-func? (tree-ref out :last) 'script-busy))
	(tree-assign (tree-ref out :last)
		     (if dead? '(script-dead) '(script-interrupted))))
      (session-detach (car l)))))

(tm-define (session-feed lan ses in out next opts)
  (set! in (plugin-preprocess lan ses in opts))
  (tree-assign! out '(document (script-busy)))
  (with x (session-encode in out next opts)
    (apply plugin-feed `(,lan ,ses ,@(car x) ,(cdr x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-document-context? t)
  (and (tm-func? t 'document)
       (tm-func? (tree-ref t :up) 'session)))

(tm-define (subsession-document-context? t)
  (or (and (tm-func? t 'document)
	   (tm-func? (tree-ref t :up) 'session))
      (and (tm-func? t 'document)
	   (tm-func? (tree-ref t :up) 'unfolded-subsession)
	   (== (tree-index t) 1))))

(tm-define field-tags
  '(input unfolded-io folded-io input-math unfolded-io-math folded-io-math))

(tm-define (field-context? t)
  (and (tm? t)
       (tree-in? t field-tags)
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-or-output-context? t)
  (and (tm? t)
       (tree-in? t (cons 'output field-tags))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-folded-context? t)
  (and (tree-in? t '(folded-io folded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-unfolded-context? t)
  (and (tree-in? t '(unfolded-io unfolded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-prog-context? t)
  (and (tree-in? t '(input folded-io unfolded-io))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-math-context? t)
  (and (tree-in? t '(input-math folded-io-math unfolded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (field-input-context? t)
  (and (field-context? t)
       (== (tree-down-index t) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field-parameters kind)
  (let* ((var (string-append (get-env "prog-language") "-" kind))
         (gen (string-append "generic-" kind)))
    (search-parameters (if (style-has? var) var gen))))

(tm-define (standard-parameters l)
  (:require (== l "session"))
  (field-parameters "session"))

(tm-define (standard-parameters l)
  (:require (== l "input"))
  (field-parameters "input"))

(tm-define (standard-parameters l)
  (:require (== l "output"))
  (field-parameters "output"))

(tm-define (standard-parameters l)
  (:require (== l "errput"))
  (field-parameters "errput"))

(tm-define (standard-parameters l)
  (:require (== l "textput"))
  (field-parameters "textput"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-ready? . err-flag?)
  (with lan (get-env "prog-language")
    (or (== lan "scheme")
	(connection-defined? lan)
	(begin
	  (if err-flag?
	      (set-message `(concat "undefined plugin: " (verbatim ,lan)) ""))
	  #f))))

(tm-define (session-status)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (cond ((== lan "scheme") 2)
	  ((not (connection-defined? lan)) 0)
	  (else (connection-status lan ses)))))

(tm-define (session-busy-message msg)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (with l (pending-ref lan ses)
      (for-each
       (lambda (x)
         (with (in out next opts) (session-decode x)
           (when (and (tm-func? out 'document)
                      (tm-func? (tree-ref out :last) 'script-busy))
             (tree-assign (tree-ref out :last) `(script-busy ,msg)))))
       l))))

(tm-define (session-alive?)
  (> (session-status) 1))

(tm-define (session-supports-completions?)
  (and (session-alive?)
       (plugin-supports-completions? (get-env "prog-language"))))

(tm-define (session-supports-input-done?)
  (and (session-alive?)
       (plugin-supports-input-done? (get-env "prog-language"))))

(define (field-next* t forward?)
  (and-with u (tree-ref t (if forward? :next :previous))
    (cond ((field-context? u) u)
          ((tree-in? u '(folded-subsession unfolded-subsession)) #f)
          (else (field-next u forward?)))))

(define (field-next t forward?)
  (and-with u (tree-ref t (if forward? :next :previous))
    (if (field-context? u) u (field-next u forward?))))

(define (field-extreme t last?)
  (with u (tree-ref t :up (if last? :last :first))
    (if (field-context? u) u
	(field-next u (not last?)))))

(define (field-insert-output t)
  (cond ((tm-func? t 'input)
	 (tree-insert! t 2 (list '(document)))
	 (tree-assign-node! t 'unfolded-io))
	((tm-func? t 'input-math)
	 (tree-insert! t 2 (list '(document)))
	 (tree-assign-node! t 'unfolded-io-math))))

(define (field-remove-output t)
  (cond ((or (tm-func? t 'folded-io) (tm-func? t 'unfolded-io))
	 (tree-assign-node! t 'input)
	 (tree-remove! t 2 1))
	((or (tm-func? t 'folded-io-math) (tm-func? t 'unfolded-io-math))
	 (tree-assign-node! t 'input-math)
	 (tree-remove! t 2 1))
	((tm-func? t 'output)
	 (with p (tree-ref t :up)
	   (when (tree-is? p 'document)
	     (tree-remove! p (tree-index t) 1))))))

(define (field-update-math t)
  (if (session-math-input?)
      (when (field-prog-context? t)
	(if (tm-func? t 'input)
	    (tree-assign-node! t 'input-math)
	    (begin
	      (tree-assign-node! t 'folded-io-math)
	      (tree-assign (tree-ref t 1) '(document "")))))
      (when (field-math-context? t)
	(if (tm-func? t 'input-math)
	    (tree-assign-node! t 'input)
	    (begin
	      (tree-assign-node! t 'folded-io)
	      (tree-assign (tree-ref t 1) '(document "")))))))

(define (field-create t p forward?)
  (let* ((d (tree-ref t :up))
	 (i (+ (tree-index t) (if forward? 1 0)))
	 (l (if (session-math-input?) 'input-math 'input))
	 (b `(,l ,p (document ""))))
    (tree-insert d i (list b))
    (tree-ref d i)))

(define (session-forall-sub fun t)
  (for (u (tree-children t))
    (when (field-context? u)
      (fun u))
    (when (and (tm-func? u 'unfolded-subsession)
	       (tm-func? (tree-ref u 1) 'document))
      (session-forall-sub fun (tree-ref u 1)))))

(define (session-forall fun)
  (with-innermost t subsession-document-context?
    (session-forall-sub fun t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processing input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-session lan ses)
  (let* ((ban `(output (document "")))
	 (l (if (session-math-input?) 'input-math 'input))
	 (p (plugin-prompt lan ses))
	 (in `(,l (document ,p) (document "")))
	 (s `(session ,lan ,ses (document ,ban ,in))))
    (insert-go-to s '(2 1 1 0 0))
    (with-innermost t field-input-context?
      (with u (tree-ref t :previous 0)
	(if (url-exists? (url-unix "$TEXMACS_STYLE_PATH"
				   (string-append lan ".ts")))
	    (add-style-package lan))
	(session-feed lan ses :start u t '())))))

(define (input-options t)
  (with opts '()
    (when (session-output-timings?) (set! opts (cons :timings opts)))
    (when (field-math-context? t) (set! opts (cons :math-input opts)))
    opts))

(define (field-process-input t)
  (when (session-ready? #t)
    (field-insert-output t)
    (cond ((tm-func? t 'folded-io)
	   (tree-assign-node! t 'unfolded-io))
	  ((tm-func? t 'folded-io-math)
	   (tree-assign-node! t 'unfolded-io-math)))
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-session"))
	   (p (plugin-prompt lan ses))
	   (in (tree->stree (tree-ref t 1)))
	   (out (tree-ref t 2))
	   (opts (input-options t)))
      (with u (or (field-next* t #t) (field-create t p #t))
	(session-feed lan ses in out u opts)
	(tree-go-to u 1 :end)))))

(define (kbd-enter-sub t done?)
  (if (in? done? (list #f "#f"))
      (insert-return)
      (delayed
        (:idle 1)
        (session-evaluate))))

(tm-define (kbd-enter t shift?)
  (:require (field-input-context? t))
  (cond ((xor (session-multiline-input?) shift?)
         (insert-return))
        ((session-supports-input-done?)
         (let* ((lan (get-env "prog-language"))
                (ses (get-env "prog-session"))
                (opts (input-options t))
                (st (tree->stree (tree-ref t 1)))
                (pre (plugin-preprocess lan ses st opts))
                (in (plugin-serialize lan pre))
                (rew (if (string-ends? in "\n") (string-drop-right in 1) in))
                (cmd (string-append "(input-done? " (string-quote rew) ")"))
                (ret (lambda (done?) (kbd-enter-sub t done?))))
           (plugin-command lan ses cmd ret '())))
        (else (session-evaluate))))

(tm-define (session-evaluate)
  (with-innermost t field-input-context?
    (field-process-input t)))

(tm-define (session-evaluate-all)
  (session-forall
    (lambda (t)
      (when (not (tree-empty? (tree-ref t 1)))
	(field-process-input t)))))

(tm-define (session-evaluate-above)
  (with-innermost me field-input-context?
    (session-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf? (tree->path t) (tree->path me))
	    (field-process-input t)))))))

(tm-define (session-evaluate-below)
  (with-innermost me field-input-context?
    (session-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf-eq? (tree->path me) (tree->path t))
	    (field-process-input t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-horizontal t forwards?)
  (:require (field-context? t))
  (with move (if forwards? go-right go-left)
    (go-to-remain-inside move field-context? 1)))

(tm-define (kbd-extremal t forwards?)
  (:require (field-context? t))
  (with move (if forwards? go-end-line go-start-line)
    (go-to-remain-inside move field-context? 1)))

(define (field-go-to-previous)
  (with-innermost t field-context?
    (with u (tree-ref t :previous)
      (if (and u (field-context? u))
	  (tree-go-to u 1 :end)
	  (go-to-previous-tag-same-argument field-tags)))))

(define (field-go-to-next)
  (with-innermost t field-context?
    (with u (tree-ref t :next)
      (if (and u (field-context? u))
	  (tree-go-to u 1 :start)
	  (go-to-next-tag-same-argument field-tags))
      (go-end-line))))

(define (field-go-up)
  (with p (cursor-path)
    (go-to-remain-inside go-up field-context? 1)
    (when (== (cursor-path) p)
      (field-go-to-previous))))

(define (field-go-down)
  (with p (cursor-path)
    (go-to-remain-inside go-down field-context? 1)
    (when (== (cursor-path) p)
      (field-go-to-next))))

(tm-define (kbd-vertical t downwards?)
  (:require (field-context? t))
  (if downwards? (field-go-down) (field-go-up)))

(tm-define (kbd-incremental t downwards?)
  (:require (field-context? t))
  (for (n 0 5)
    (if downwards? (field-go-to-next) (field-go-to-previous))))

(tm-define (kbd-remove t forwards?)
  (:require (field-input-context? t))
  (cond ((and (tree-cursor-at? t 1 :start) (not forwards?)) (noop))
        ((and (tree-cursor-at? t 1 :end) forwards?) (noop))
        (else (remove-text forwards?))))

(tm-define (kbd-remove t forwards?)
  (:require (and (field-input-context? t) (selection-active-any?)))
  (clipboard-cut "nowhere")
  (clipboard-clear "nowhere"))

(tm-define (kbd-variant t forwards?)
  (:require (and (field-context? t) (session-supports-completions?)))
  (let* ((lan (get-env "prog-language"))
         (ses (get-env "prog-session"))
         (cmd (session-complete-command t))
         (ret (lambda (x) (when x (custom-complete (tm->tree x))))))
    (when (!= cmd "")
      (plugin-command lan ses cmd ret '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured keyboard movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (document-context? t)
  (:require (and (tree-is? t 'document)
                 (field-input-context? (tree-ref t :up))))
  #f)

(tm-define (traverse-horizontal t forwards?)
  (:require (field-input-context? t))
  (with move (if forwards? go-to-next-word go-to-previous-word)
    (go-to-remain-inside move field-context? 1)))

(tm-define (traverse-vertical t downwards?)
  (:require (field-input-context? t))
  (if downwards? (field-go-down) (field-go-up)))

(tm-define (traverse-extremal t forwards?)
  (:require (field-input-context? t))
  (with move (if forwards? field-go-down field-go-up)
    (go-to-repeat move)))

(tm-define (traverse-incremental t downwards?)
  (:require (field-input-context? t))
  (if downwards? (field-go-down) (field-go-up)))

(tm-define (structured-horizontal t forwards?)
  (:require (field-input-context? t))
  (noop))

(tm-define (structured-vertical t downwards?)
  (:require (field-input-context? t))
  (with move (if downwards? field-go-down field-go-up)
    (go-to-remain-inside move 'session)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold and unfold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (field-unfolded-context? t))
  (with i (tree-down-index t)
    (variant-set t (ahash-ref alternate-table (tree-label t)))
    (if (== i 2) (tree-go-to t 1 :end))))

(tm-define (alternate-toggle t)
  (:require (field-folded-context? t))
  (variant-set t (ahash-ref alternate-table (tree-label t))))

(tm-define (field-fold t)
  (when (field-unfolded-context? t)
    (alternate-toggle t)))

(tm-define (field-unfold t)
  (when (field-folded-context? t)
    (alternate-toggle t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (field-insert t* forwards?)
  (and-with t (tree-search-upwards t* field-input-context?)
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-session"))
	   (p (plugin-prompt lan ses))
	   (t (field-create t p forwards?)))
      (tree-go-to t 1 :end))))

(tm-define (field-insert-text t* forward?)
  (and-with t (tree-search-upwards t* field-input-context?)
    (let* ((d (tree-ref t :up))
	   (i (+ (tree-index t) (if forward? 1 0)))
	   (b `(textput (document ""))))
      (tree-insert d i (list b))
      (tree-go-to d i 0 :start))))

(tm-define (field-remove-banner t*)
  (and-with t (tree-search-upwards t* session-document-context?)
    (when (tm-func? (tree-ref t 0) 'output)
      (tree-remove! t 0 1))))

(tm-define (field-remove-extreme t* last?)
  (and-with t (tree-search-upwards t* field-input-context?)
    (with u (field-extreme t last?)
      (with v (field-next t (not last?))
	(if (and (== u t) v)
	    (tree-go-to v 1 :end))
	(if (or (!= u t) v)
	    (tree-remove (tree-ref u :up) (tree-index u) 1))))))

(tm-define (field-remove t* forwards?)
  (and-with t (tree-search-upwards t* field-input-context?)
    (if forwards?
        (with u (field-next t #t)
          (if u (begin
                  (tree-remove (tree-ref t :up) (tree-index t) 1)
                  (tree-go-to u 1 :start))
              (field-remove-extreme t #t)))
        (with u (field-next* t #f)
          (if u (tree-remove (tree-ref u :up) (tree-index u) 1)
              (field-remove-banner t))))))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (field-input-context? t))
  (if forwards? (field-insert-fold t)))

(tm-define (structured-insert-vertical t downwards?)
  (:require (field-input-context? t))
  (field-insert t downwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (field-input-context? t))
  (field-remove t forwards?))

(tm-define (structured-remove-vertical t forwards?)
  (:require (field-input-context? t))
  (field-remove t forwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-clear-all)
  (session-forall field-remove-output))

(tm-define (session-fold-all)
  (session-forall field-fold))

(tm-define (session-unfold-all)
  (session-forall field-unfold))

(tm-define (field-insert-fold t*)
  (and-with t (tree-search-upwards t* field-input-context?)
    (tree-set! t `(unfolded-subsession (document "") (document ,t)))
    (tree-go-to t 0 :end)))

(tm-define (session-split)
  (with-innermost t session-document-context?
    (let* ((u (tree-ref t :up)) ;; session
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (session-selection-one? t)
  (and (tree-in? t (cons* 'output 'textput
                          'folded-subsession 'unfolded-subsession
                          field-tags))
       (tree-up t)
       (session-document-context? (tree-up t))))

(define (session-selection?)
  (with l (selection-trees)
    (and (nnull? l) (forall? session-selection-one? l))))

(define (session-selection sel*)
  (let* ((sel (selection-as-document sel*))
         (doc (tree-up (tree-ref sel 0)))
         (ses (tree-up doc)))
    `(session ,(cDr (tm-children ses)) ,sel)))

(tm-define (clipboard-cut which)
  (:require (session-selection?))
  (let* ((l (selection-trees))
         (doc (tree-up (car l)))
         (ses (tree-up doc))
         (i (tree-index (car l)))
         (j (tree-index (cAr l)))
         (k (- (+ j 1) i))
         (n (tree-arity doc)))
    (clipboard-copy which)
    (if (= k n)
        (tree-cut ses)
        (begin
          (tree-remove doc i k)
          (with next (tree-ref doc (min i (- n (+ k 1))))
            (cond ((field-context? next)
                   (tree-go-to next 1 :start))
                  ((tree-in? next '(output textput))
                   (tree-go-to next 0 :start))
                  ((tree-in? next '(folded-subsession unfolded-subsession))
                   (tree-go-to next 0 :start))
                  (else (tree-go-to next :start))))))))

(tm-define (clipboard-copy which)
  (:require (session-selection?))
  (let* ((l (selection-trees))
         (doc (tree-up (car l)))
         (ses (tree-up doc))
         (sel `(session ,@(cDr (tm-children ses)) (document ,@l))))
    (clipboard-set which sel)))

(tm-define (inside-subsession-context? t)
  (and (tree-in? t '(folded-subsession unfolded-subsession))
       (== (tree-arity t) 2)
       (cursor-inside? (tree-ref t 1))))

(tm-define (clipboard-paste which)
  (:require (and (inside? 'session)
                 (tm-ref (clipboard-get which) 1)
                 (tree-is? (tm-ref (clipboard-get which) 1) 'session)))
  (let* ((ses (tree-innermost 'session))
         (sub (tree-innermost inside-subsession-context?))
         (ins (tree-ref (clipboard-get which) 1)))
    (when (and (== (tree-arity ses) 3)
               (== (tree-arity ins) 3)
               (tm-equal? (tm-ref ses 0) (tm-ref ins 0)))
      (let* ((doc (if sub (tree-ref sub 1) (tree-ref ses 2)))
             (ext (tree-ref ins 2))
             (i (tree-down-index doc)))
        (if (== (cursor-path) (tree->path doc i :end))
            (begin
              (tree-insert doc (+ i 1) (tree-children ext))
              (tree-go-to doc (+ i (tree-arity ext)) :end))
            (tree-insert doc i (tree-children ext)))))))
