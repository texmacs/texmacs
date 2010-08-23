
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
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define session-math-input #f)

(tm-define (session-math-input?)
  session-math-input)

(tm-define (toggle-session-math-input)
  (:synopsis "Toggle mathematical input in sessions.")
  (:check-mark "v" session-math-input?)
  (set! session-math-input (not session-math-input))
  (with-innermost t field-context?
    (field-update-math t)))

(define session-multiline-input #f)

(tm-define (session-multiline-input?)
  session-multiline-input)

(tm-define (toggle-session-multiline-input)
  (:synopsis "Toggle multi-line input in sessions.")
  (:check-mark "v" session-multiline-input?)
  (set! session-multiline-input (not session-multiline-input)))

(define session-scheme-trees #t)

(tm-define (session-scheme-trees?)
  session-scheme-trees)

(tm-define (toggle-session-scheme-trees)
  (:synopsis "Toggle pretty tree output in scheme sessions.")
  (:check-mark "v" session-scheme-trees?)
  (set! session-scheme-trees (not session-scheme-trees)))

(define session-scheme-math #f)

(tm-define (session-scheme-math?)
  session-scheme-math)

(tm-define (toggle-session-scheme-math)
  (:synopsis "Toggle pretty tree output in scheme sessions.")
  (:check-mark "v" session-scheme-math?)
  (set! session-scheme-math (not session-scheme-math)))

(define session-output-timings #f)

(tm-define (session-output-timings?)
  session-output-timings)

(tm-define (toggle-session-output-timings)
  (:synopsis "Toggle output of evaluation timings.")
  (:check-mark "v" session-output-timings?)
  (set! session-output-timings (not session-output-timings)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scheme sessions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replace-newline s)
  (with l (string-tokenize-by-char s #\newline)
    (if (<= (length l) 1) s
	(tm->tree `(document ,@l)))))

(define (var-object->string t)
  (with s (object->string t)
    (if (== s "#<unspecified>") "" (replace-newline (string-encode s)))))

(define (eval-string-with-catch s)
  (catch #t
	 (lambda () (eval (string->object s)))
	 (lambda (key msg . args)
	   key)))

(tm-define (scheme-eval t)
  (let* ((s (texmacs->verbatim (tm->tree t)))
	 (r (eval-string-with-catch s)))
    (cond ((and (tree? r) (session-scheme-trees?)) (tree-copy r))
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
      (if (or (tree-empty? in) (not (session-coherent? out next)))
	  (plugin-next lan ses)
	  (begin
	    (plugin-write lan ses in)
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
	   (tm-func? (tree-ref t :up) 'unfolded)
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
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-defined? . err-flag?)
  (with lan (get-env "prog-language")
    (or (== lan "scheme")
	(connection-defined? lan)
	(begin
	  (if err-flag?
	      (set-message (string-append "plugin '" lan "' not defined") ""))
	  #f))))

(tm-define (session-status)
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session")))
    (cond ((== lan "scheme") 2)
	  ((not (connection-defined? lan)) 0)
	  (else (connection-status lan ses)))))

(tm-define (session-alive?)
  (> (session-status) 1))

(tm-define (session-supports-completions?)
  (and (session-alive?)
       (plugin-supports-completions? (get-env "prog-language"))))

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
	(tree-assign-node! t 'folded-io-math)
	(tree-assign (tree-ref t 1) '(document "")))
      (when (field-math-context? t)
	(tree-assign-node! t 'folded-io)
	(tree-assign (tree-ref t 1) '(document "")))))

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
    (when (and (tm-func? u 'unfolded)
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
	(if (url-exists? (url "$TEXMACS_STYLE_PATH" (string-append lan ".ts")))
	    (init-add-package lan))
	(session-feed lan ses :start u t '())))))

(define (field-process-input t)
  (when (session-defined? #t)
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
	   (opts '()))
      (when (session-output-timings?) (set! opts (cons :timings opts)))
      (when (field-math-context? t) (set! opts (cons :math-input opts)))
      (with u (or (field-next t #t) (field-create t p #t))
	(session-feed lan ses in out u opts)
	(tree-go-to u 1 :end)))))

(tm-define (kbd-return)
  (:context field-input-context?)
  (if (session-multiline-input?)
      (insert-return)
      (session-evaluate)))

(tm-define (kbd-shift-return)
  (:context field-input-context?)
  (if (session-multiline-input?)
      (session-evaluate)
      (insert-return)))

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

(tm-define (kbd-left)
  (:context field-context?)
  (go-to-remain-inside go-left field-context? 1))

(tm-define (kbd-right)
  (:context field-context?)
  (go-to-remain-inside go-right field-context? 1))

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

(tm-define (kbd-up)
  (:context field-context?)
  (field-go-up))

(tm-define (kbd-down)
  (:context field-context?)
  (field-go-down))

(tm-define (kbd-page-up)
  (:context field-input-context?)
  (for (n 0 5)
    (field-go-to-previous)))

(tm-define (kbd-page-down)
  (:context field-input-context?)
  (for (n 0 5)
    (field-go-to-next)))

(tm-define (kbd-remove forward?)
  (:context field-input-context?)
  (with-innermost t field-input-context?
    (cond ((and (tree-cursor-at? t 1 :start) (not forward?)) (noop))
	  ((and (tree-cursor-at? t 1 :end) forward?) (noop))
	  (else (remove-text forward?)))))

(tm-define (kbd-tab)
  (:context field-input-context?)
  (:require (session-supports-completions?))
  (with-innermost t field-input-context?
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-session"))
	   (cmd (session-complete-command t))
	   (ret (lambda (x) (when x (custom-complete (tm->tree x))))))
      (when (!= cmd "")
	(plugin-command lan ses cmd ret '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured keyboard movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (field-input-simple-context? t)
  (and (nleaf? t)
       (simple-context? (tree-down t))
       (field-input-context? t)))

(tm-define (document-context? t)
  (:case document)
  (:require (field-input-context? (tree-ref t :up)))
  #f)

(tm-define (traverse-left)
  (:context field-input-context?)
  (go-to-remain-inside go-to-previous-word field-context? 1))

(tm-define (traverse-right)
  (:context field-input-context?)
  (go-to-remain-inside go-to-next-word field-context? 1))

(tm-define (traverse-up)
  (:context field-input-context?)
  (field-go-up))

(tm-define (traverse-down)
  (:context field-input-context?)
  (field-go-down))

(tm-define (traverse-previous)
  (:context field-input-context?)
  (field-go-up))

(tm-define (traverse-next)
  (:context field-input-context?)
  (field-go-down))

(tm-define (structured-left)
  (:context field-input-simple-context?)
  (noop))

(tm-define (structured-right)
  (:context field-input-simple-context?)
  (noop))

(tm-define (structured-up)
  (:context field-input-simple-context?)
  (go-to-remain-inside field-go-up 'session))

(tm-define (structured-down)
  (:context field-input-simple-context?)
  (go-to-remain-inside field-go-down 'session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold and unfold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fold)
  (:context field-unfolded-context?)
  (with-innermost t field-unfolded-context?
    (toggle-toggle t)
    (tree-go-to t 1 :end)))

(tm-define (unfold)
  (:context field-folded-context?)
  (with-innermost t field-folded-context?
    (toggle-toggle t)
    (tree-go-to t 1 :end)))

(tm-define (field-fold t)
  (when (field-unfolded-context? t)
    (toggle-toggle t)))

(tm-define (field-unfold t)
  (when (field-folded-context? t)
    (toggle-toggle t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (field-insert forwards?)
  (with-innermost t field-input-context?
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-session"))
	   (p (plugin-prompt lan ses))
	   (t (field-create t p forwards?)))
      (tree-go-to t 1 :end))))

(tm-define (field-insert-text forward?)
  (with-innermost t field-input-context?
    (let* ((d (tree-ref t :up))
	   (i (+ (tree-index t) (if forward? 1 0)))
	   (b `(textput (document ""))))
      (tree-insert d i (list b))
      (tree-go-to d i 0 :start))))

(tm-define (field-remove-banner)
  (with-innermost t session-document-context?
    (when (tm-func? (tree-ref t 0) 'output)
      (tree-remove! t 0 1))))

(tm-define (field-remove-extreme last?)
  (with-innermost t field-input-context?
    (with u (field-extreme t last?)
      (with v (field-next t (not last?))
	(if (and (== u t) v)
	    (tree-go-to v 1 :end))
	(if (or (!= u t) v)
	    (tree-remove (tree-ref u :up) (tree-index u) 1))))))

(tm-define (field-remove forwards?)
  (with-innermost t field-input-context?
    (if forwards?
	(with u (field-next t #t)
	  (if u (begin
		  (tree-remove (tree-ref t :up) (tree-index t) 1)
		  (tree-go-to u 1 :start))
	      (field-remove-extreme #t)))
	(with u (field-next t #f)
	  (if u (tree-remove (tree-ref u :up) (tree-index u) 1)
	      (field-remove-banner))))))

(tm-define (structured-insert forwards?)
  (:context field-input-context?)
  (if forwards? (field-insert-fold)))

(tm-define (structured-insert-up)
  (:context field-input-context?)
  (field-insert #f))

(tm-define (structured-insert-down)
  (:context field-input-context?)
  (field-insert #t))

(tm-define (structured-remove forwards?)
  (:context field-input-context?)
  (field-remove forwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-clear-all)
  (session-forall field-remove-output))

(tm-define (session-fold-all)
  (session-forall field-fold))

(tm-define (session-unfold-all)
  (session-forall field-unfold))

(tm-define (field-insert-fold)
  (with-innermost t field-input-context?
    (tree-set! t `(unfolded (document "") (document ,t)))
    (tree-go-to t 0 :end)))

(tm-define (session-split)
  (:context session-document-context?)
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
