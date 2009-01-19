
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
	(dynamic session-drd)
	(dynamic scripts-edit)
	(dynamic fold-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-session-math-input)
  (:synopsis "Toggle mathematical input in sessions.")
  (:check-mark "v" session-math-input?)
  (session-use-math-input (not (session-math-input?)))
  (with-innermost t io-context?
    (io-update-math t)))

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

(tm-define io-tags '(unfolded-io folded-io unfolded-io-math folded-io-math))

(tm-define (io-context? t)
  (and (tree-in? t io-tags)
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (io-folded-context? t)
  (and (tree-in? t '(folded-io folded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (io-unfolded-context? t)
  (and (tree-in? t '(unfolded-io unfolded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (io-prog-context? t)
  (and (tree-in? t '(folded-io unfolded-io))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (io-math-context? t)
  (and (tree-in? t '(folded-io-math unfolded-io-math))
       (tm-func? (tree-ref t :up) 'document)))

(tm-define (input-context? t)
  (and (io-context? t)
       (== (tree-down-index t) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (io-next t forward?)
  (and-with u (tree-ref t (if forward? :next :previous))
    (if (io-context? u) u (io-next u forward?))))

(define (io-extreme t last?)
  (with u (tree-ref t :up (if last? :last :first))
    (if (io-context? u) u
	(io-next u (not last?)))))

(define (io-update-math t)
  (if (session-math-input?)
      (when (io-prog-context? t)
	(tree-assign-node! t 'folded-io-math)
	(tree-assign (tree-ref t 1) '(document "")))
      (when (io-math-context? t)
	(tree-assign-node! t 'folded-io)
	(tree-assign (tree-ref t 1) '(document "")))))

(define (io-create t p forward?)
  (let* ((d (tree-ref t :up))
	 (i (+ (tree-index t) (if forward? 1 0)))
	 (l (if (session-math-input?) 'folded-io-math 'folded-io))
	 (b `(,l ,p (document "") (document))))
    (tree-insert d i (list b))
    (tree-ref d i)))

(define (session-forall-sub fun t)
  (for (u (tree-children t))
    (when (io-context? u)
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
	 (l (if (session-math-input?) 'folded-io-math 'folded-io))
	 (p (connection-prompt lan ses))
	 (in `(,l (document ,p) (document "") (document)))
	 (s `(session ,lan ,ses (document ,ban ,in))))
    (insert-go-to s '(2 1 1 0 0))
    (with-innermost t input-context?
      (with u (tree-ref t :previous 0)
	(if (url-exists? (url "$TEXMACS_STYLE_PATH" (string-append lan ".ts")))
	    (init-add-package lan))
	(session-feed lan ses :start u t '())))))

(define (io-process-input t)
  (if (tm-func? t 'folded-io)
      (tree-assign-node! t 'unfolded-io))
  (if (tm-func? t 'folded-io-math)
      (tree-assign-node! t 'unfolded-io-math))
  (let* ((lan (get-env "prog-language"))
	 (ses (get-env "prog-session"))
	 (p (connection-prompt lan ses))
	 (in (tree->stree (tree-ref t 1)))
	 (out (tree-ref t 2))
	 (opts '()))
    (when (session-output-timings?) (set! opts (cons :timings opts)))
    (when (io-math-context? t) (set! opts (cons :math-input opts)))
    (with u (or (io-next t #t) (io-create t p #t))
      (session-feed lan ses in out u opts)
      (tree-go-to u 1 :end))))

(tm-define (kbd-return)
  (:context input-context?)
  (if (session-multiline-input?)
      (insert-return)
      (session-evaluate)))

(tm-define (kbd-shift-return)
  (:context input-context?)
  (if (session-multiline-input?)
      (session-evaluate)
      (insert-return)))

(tm-define (session-evaluate)
  (with-innermost t input-context?
    (io-process-input t)))

(tm-define (session-evaluate-all)
  (session-forall
    (lambda (t)
      (when (not (tree-empty? (tree-ref t 1)))
	(io-process-input t)))))

(tm-define (session-evaluate-above)
  (with-innermost me input-context?
    (session-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf? (tree->path t) (tree->path me))
	    (io-process-input t)))))))

(tm-define (session-evaluate-below)
  (with-innermost me input-context?
    (session-forall
      (lambda (t)
	(when (not (tree-empty? (tree-ref t 1)))
	  (when (path-inf-eq? (tree->path me) (tree->path t))
	    (io-process-input t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-left)
  (:context io-context?)
  (go-to-remain-inside go-left io-context? 1))

(tm-define (kbd-right)
  (:context io-context?)
  (go-to-remain-inside go-right io-context? 1))

(define (io-go-up)
  (with p (cursor-path)
    (go-to-remain-inside go-up io-context? 1)
    (when (== (cursor-path) p)
      (go-to-previous-tag-same-argument io-tags))))

(define (io-go-down)
  (with p (cursor-path)
    (go-to-remain-inside go-down io-context? 1)
    (when (== (cursor-path) p)
      (go-to-next-tag-same-argument io-tags))))

(tm-define (kbd-up)
  (:context io-context?)
  (io-go-up))

(tm-define (kbd-down)
  (:context io-context?)
  (io-go-down))

(tm-define (kbd-page-up)
  (:context input-context?)
  (for (n 0 5)
    (go-to-next-inside go-to-previous-node io-context? 1)))

(tm-define (kbd-page-down)
  (:context input-context?)
  (for (n 0 5)
    (go-to-next-inside go-to-next-node io-context? 1)))

(tm-define (kbd-remove forward?)
  (:context input-context?)
  (with-innermost t input-context?
    (cond ((and (tree-cursor-at? t 1 :start) (not forward?)) (noop))
	  ((and (tree-cursor-at? t 1 :end) forward?) (noop))
	  (else (remove-text forward?)))))

(tm-define (kbd-tab)
  (:context input-context?)
  (:require (plugin-supports-completions? (get-env "prog-language")))
  (with-innermost t input-context?
    (session-complete-try? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured keyboard movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (io-input-simple-context? t)
  (and (nleaf? t)
       (simple-context? (tree-down t))
       (input-context? t)))

(tm-define (document-context? t)
  (:case document)
  (:require (input-context? (tree-ref t :up)))
  #f)

(tm-define (traverse-left)
  (:context input-context?)
  (go-to-remain-inside go-to-previous-word io-context? 1))

(tm-define (traverse-right)
  (:context input-context?)
  (go-to-remain-inside go-to-next-word io-context? 1))

(tm-define (traverse-up)
  (:context input-context?)
  (io-go-up))

(tm-define (traverse-down)
  (:context input-context?)
  (io-go-down))

(tm-define (traverse-previous)
  (:context input-context?)
  (io-go-up))

(tm-define (traverse-next)
  (:context input-context?)
  (io-go-down))

(tm-define (structured-left)
  (:context io-input-simple-context?)
  (noop))

(tm-define (structured-right)
  (:context io-input-simple-context?)
  (noop))

(tm-define (structured-up)
  (:context io-input-simple-context?)
  (go-to-remain-inside io-go-up 'session))

(tm-define (structured-down)
  (:context io-input-simple-context?)
  (go-to-remain-inside io-go-down 'session))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fold and unfold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (fold)
  (:context io-unfolded-context?)
  (with-innermost t io-unfolded-context?
    (toggle-toggle t)
    (tree-go-to t 1 :end)))

(tm-define (unfold)
  (:context io-folded-context?)
  (with-innermost t io-folded-context?
    (toggle-toggle t)
    (tree-go-to t 1 :end)))

(tm-define (io-fold t)
  (when (io-unfolded-context? t)
    (toggle-toggle t)))

(tm-define (io-unfold t)
  (when (io-folded-context? t)
    (toggle-toggle t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (io-insert forwards?)
  (with-innermost t input-context?
    (let* ((lan (get-env "prog-language"))
	   (ses (get-env "prog-session"))
	   (p (connection-prompt lan ses)))
      (tree-go-to (io-create t p forwards?) 1 :end))))

(tm-define (io-insert-text forward?)
  (with-innermost t input-context?
    (let* ((d (tree-ref t :up))
	   (i (+ (tree-index t) (if forward? 1 0)))
	   (b `(textput (document ""))))
      (tree-insert d i (list b))
      (tree-go-to d i 0 :start))))

(tm-define (io-remove-banner)
  (with-innermost t session-document-context?
    (when (tm-func? (tree-ref t 0) 'output)
      (tree-remove! t 0 1))))

(tm-define (io-remove-extreme last?)
  (with-innermost t input-context?
    (with u (io-extreme t last?)
      (with v (io-next t (not last?))
	(if (and (== u t) v)
	    (tree-go-to v 1 :end))
	(if (or (!= u t) v)
	    (tree-remove (tree-ref u :up) (tree-index u) 1))))))

(tm-define (io-remove forwards?)
  (with-innermost t input-context?
    (if forwards?
	(with u (io-next t #t)
	  (if u (begin
		  (tree-remove (tree-ref t :up) (tree-index t) 1)
		  (tree-go-to u 1 :start))
	      (io-remove-extreme #t)))
	(with u (io-next t #f)
	  (if u (tree-remove (tree-ref u :up) (tree-index u) 1)
	      (io-remove-banner))))))

(tm-define (structured-insert forwards?)
  (:context input-context?)
  (if forwards? (io-insert-fold)))

(tm-define (structured-insert-up)
  (:context input-context?)
  (io-insert #f))

(tm-define (structured-insert-down)
  (:context input-context?)
  (io-insert #t))

(tm-define (structured-remove forwards?)
  (:context input-context?)
  (io-remove forwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Session management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (session-clear-all)
  (session-forall (lambda (t) (tree-set! t 2 '(document)))))

(tm-define (session-fold-all)
  (session-forall io-fold))

(tm-define (session-unfold-all)
  (session-forall io-unfold))

(tm-define (io-insert-fold)
  (with-innermost t input-context?
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
