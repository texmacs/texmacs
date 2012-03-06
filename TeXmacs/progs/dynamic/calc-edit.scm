
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-edit.scm
;; DESCRIPTION : low level linking and evaluation routines for spread sheets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-edit)
  (:use (text tm-structure)
        (link locus-edit)
        (dynamic session-edit)
        (dynamic scripts-edit)
        (dynamic calc-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spreadsheet evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define calc-input (make-ahash-table))
(tm-define calc-output (make-ahash-table))
(tm-define calc-invalid (make-ahash-table))
(tm-define calc-todo (make-ahash-table))

(tm-define (calc-updated? t)
  (cond ((tree-atomic? t) #t)
        ((tree-is? t 'calc-ref)
         (with var (texmacs->string (tree-ref t 0))
           (not (ahash-ref calc-invalid var))))
        (else (list-and (map calc-updated? (tree-children t))))))

(tm-define (calc-get-input t)
  (tree-ref t 1))

(tm-define (calc-prefix-input prefix t)
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'cell-ref)
         (with name (texmacs->string (tree-ref t 0))
           (tm->tree `(calc-ref ,(string-append prefix name)))))
        (else (tree-map-children (cut calc-prefix-input prefix <>) t))))

(tm-define (calc-update-inputs lan ses prefix t)
  (cond ((tree-atomic? t) (noop))
        ((calc-inert-context? t)
         (let* ((raw-var (texmacs->string (tree-ref t 0)))
                (pre (if (calc-cell-context? t) prefix ""))
                (var (string-append pre raw-var))
                (old-val (ahash-ref calc-output var))
                (new-val (tree->stree (tree-ref t 1))))
           (when (!= new-val old-val)
             (ahash-set! calc-output var new-val)
             (ahash-set! calc-invalid var #t))))
        ((calc-toggle-context? t)
         (let* ((raw-var (texmacs->string (tree-ref t 0)))
                (pre (if (calc-cell-context? t) prefix ""))
                (var (string-append pre raw-var))
                (in-tree (calc-prefix-input prefix (calc-get-input t)))
                (in (tree->stree in-tree))
                (out-tree (tree-ref t 2))
                (out (tree->stree out-tree))
                (todo (list lan ses var in-tree out-tree)))
           (when (or (!= (ahash-ref calc-input var) in)
                     (!= (ahash-ref calc-output var) out)
                     (not (calc-updated? in-tree)))
             (ahash-set! calc-input var in)
             (ahash-set! calc-invalid var #t)
             (ahash-set! calc-todo var todo))))
        ((tree-is? t 'calc-table)
         (with x (texmacs->string (tree-ref t 0))
           (calc-update-inputs lan ses (string-append prefix x "-")
                               (tree-ref t 1))))
        ((tree-is? t 'with)
         ;; FIXME: the test should be replaced by else,
         ;; but this is currently very slow
         (with update
             (lambda (i)
               (let* ((lan2 (tree-child-env t i "prog-scripts" lan))
                      (ses2 (tree-child-env t i "prog-session" ses))
                      (lan3 (texmacs->string lan2))
                      (ses3 (texmacs->string ses2)))
                 (calc-update-inputs lan3 ses3 prefix (tree-ref t i))))
           (for-each update (.. 0 (tree-arity t)))))
         (else
           (for-each (cut calc-update-inputs lan ses prefix <>)
                     (tree-children t)))))

(tm-define (calc-repeat-update-inputs lan ses prefix t)
  (with n (ahash-size calc-invalid)
    ;;(display* "  Update inputs " n "\n")
    (calc-update-inputs lan ses prefix t)
    (when (!= n (ahash-size calc-invalid))
      (calc-repeat-update-inputs lan ses prefix t))))

(tm-define (calc-available? t)
  (cond ((tree-atomic? t) #t)
        ((tree-is? t 'calc-ref)
         (with var (texmacs->string (tree-ref t 0))
           (not (ahash-ref calc-todo var))))
        (else (list-and (map calc-available? (tree-children t))))))

(tm-define (calc-substitute t)
  (cond ((tree-atomic? t) t)
        ((tree-is? t 'calc-ref)
         (let* ((var (texmacs->string (tree-ref t 0)))
                (val (ahash-ref calc-output var)))
           (cond ((not val) t)
                 ((tm-equal? val "") (string->tree "0"))
                 (else (tm->tree `(concat "(" ,val ")"))))))
	(else (tree-map-children calc-substitute t))))

(tm-define (calc-reevaluate-output lan ses var in out)
  (when (calc-available? in)
    ;;(display* "Reevaluate output " t "\n")
    ;;(display* "src= " (calc-substitute in) "\n")
    (let* ((src (tm->tree (calc-substitute in)))
           (dest (tm->tree (scheme-eval src))))
      ;;(display* "var= " var "\n")
      ;;(display* "src= " src "\n")
      ;;(display* "dest= " dest "\n")
      (ahash-set! calc-output var dest)
      (ahash-remove! calc-todo var)
      (tree-set t 2 dest))))

(tm-define (calc-repeat-reevaluate-outputs)
  (with n (ahash-size calc-todo)
    (for (p (ahash-table->list calc-todo))
      (with (lan ses var in out) (cdr p)
        (calc-reevaluate-output lan ses var in out)))
    (when (!= n (ahash-size calc-todo))
      (calc-repeat-reevaluate-outputs))))

(tm-define (calc-reevaluate lan ses prefix t)
  (calc-repeat-update-inputs lan ses prefix t)
  (calc-repeat-reevaluate-outputs)
  (set! calc-invalid (make-ahash-table))
  (set! calc-todo (make-ahash-table)))

(tm-define (calc-scheme)
  (calc-reevaluate "scheme" "default" "" (buffer-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with the plug-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (calc-ready?)
  (let* ((lan (get-env "prog-scripts"))
	 (ses (get-env "prog-session")))
    (and (connection-defined? lan)
         (supports-scripts? lan))))

(tm-define (calc-feed lan ses var in out)
  (when (supports-scripts? lan)
    (tree-set! out '(script-busy))
    ;;(display* "Calc " var ", " lan ", " ses "\n")
    ;;(display* "Feed " in "\n")
    ;;(display* "Wait " out "\n")
    (with ptr (tree->tree-pointer out)
      (with ret (lambda (r)
                  ;;(display* "r= " r "\n")
                  (with check (tree-pointer->tree ptr)
                    (tree-pointer-detach ptr)
                    (when (== check out)
                      (tree-set! out r)
                      ;;(display* var " := " (tm->stree r) "\n")
                      (ahash-set! calc-output var (tm->stree r))
                      (ahash-remove! calc-todo var)
                      (delayed (calc-continue)))))
        ;;(display* var " := " (tm->stree in) " using " lan ", " ses "\n")
        (silent-feed* lan ses in ret '(:simplify-output))))))

(tm-define (calc-continue-first l)
  (cond ((null? l)
         (set! calc-invalid (make-ahash-table))
         (set! calc-todo (make-ahash-table)))
        ((with (lan ses var in out) (cdar l)
           (calc-available? in))
         (with (lan ses var in out) (cdar l)
           (with src (tm->tree (calc-substitute in))
             (calc-feed lan ses var src out))))
        (else
          (calc-continue-first (cdr l)))))

(tm-define (calc-continue)
  (with n (ahash-size calc-todo)
    (if (== n 0)
        (set! calc-invalid (make-ahash-table))
        (calc-continue-first (ahash-table->list calc-todo)))))

(tm-define (calc)
  (let* ((lan (get-init "prog-scripts"))
	 (ses (get-init "prog-session")))
    ;;(display* "Dependencies...\n")
    (calc-repeat-update-inputs lan ses "" (buffer-tree))
    ;;(display* "Evaluating...\n")
    (calc-continue)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'calc-output))
  (tree-assign-node t 'calc-input)
  (tree-go-to t 1 :end))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'calc-input))
  (tree-assign-node t 'calc-output)
  (tree-go-to t 2 :end)
  (calc))

(tm-define (kbd-enter t forwards?)
  (:require (calc-inert-context? t))
  (calc))

(tm-define (kbd-enter t forwards?)
  (:require (calc-toggle-context? t))
  (alternate-toggle t))
