
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : form-edit.scm
;; DESCRIPTION : Editing forms
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic form-edit)
  (:use (link locus-edit)
	(utils plugins plugin-cmd)
	(utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (form-ref id)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (with l (id->trees id)
    (and l (nnull? l) (car l))))

(tm-define (form-set! id new-tree)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (and-with old-tree (form-ref id)
    (tree-set! old-tree (tree-copy (tm->tree new-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (form-toggle)
  (:secure #t)
  (with-action t
    (cond ((== t (tree "false")) (tree-set! t "true"))
	  ((== t (tree "true")) (tree-set! t "false")))))

(define (form-alternative-context? t)
  (tree-in? t '(form-alternative form-button-alternative)))

(define ((form-matching-alternatives? t) u)
  (and (tm-func? u 'form-alternatives)
       (== (tree-ref t 0) (tree-ref u 0))))

(tm-define (form-alternative)
  (:secure #t)
  (with-cursor (tree->path (action-tree) :end)
    (with-innermost t form-alternative-context?
      (with-innermost u (form-matching-alternatives? t)
	(tree-assign (tree-ref u 1) (tree-copy (tree-ref t 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripts via forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (script-background-eval in . opts)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session")))
    (when (supports-scripts? lan)
      (dialogue
	(with r (apply plugin-async-eval (cons* lan session in opts))
	  (noop))))))

(tm-define (form->script cas-var id)
  (with cmd `(concat ,cas-var ":" ,(tree->stree (form-ref id)))
    ;; FIXME: only works for Maxima for the moment
    (script-background-eval cmd :math-input :simplify-output)))

(define (script-form-eval id in . opts)
  (let* ((lan (get-env "prog-scripts"))
	 (session (get-env "prog-session")))
    (when (supports-scripts? lan)
      (dialogue
	(with r (apply plugin-async-eval (cons* lan session in opts))
	  (form-set! id r))))))

(tm-define (script->form id cas-expr)
  (script-form-eval id cas-expr :math-input :simplify-output))
