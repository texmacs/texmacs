
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
	(utils plugins plugin-cmd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (form-ref id)
  (with l (id->trees id)
    (and l (nnull? l) (car l))))

(tm-define (form-set! id new-tree)
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
