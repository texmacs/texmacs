
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
;; The form prefix and delayed evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define form-prefix "")

(tm-define-macro (form-with prefix . body)
  (:secure #t)
  `(let ((new-prefix ,prefix)
	  (old-prefix form-prefix))
     (set! form-prefix new-prefix)
     (let ((result (begin ,@body)))
       (set! form-prefix old-prefix)
       result)))

(define (form-get-prefix opt-prefix)
  (cond ((null? opt-prefix) form-prefix)
	((tree? (car opt-prefix)) (tree->string (car opt-prefix)))
	(else (car opt-prefix))))

(tm-define-macro (form-delay . body)
  (:secure #t)
  `(delayed
     (:idle 1)
     ,@body))

(tm-define-macro (form-delayed . body)
  (with normal? (lambda (x) (or (npair? x) (not (keyword? (car x)))))
    (receive (mods cmds) (list-break body normal?)
      `(with form-delayed-prefix form-prefix
	 (delayed
	   ,@mods
	   (form-with form-delayed-prefix
	     ,@cmds))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (form-ref id . opt-prefix)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (let* ((prefix (form-get-prefix opt-prefix))
	 (l (id->trees (string-append prefix id))))
    (and l (nnull? l) (car l))))

(tm-define (form-set! id new-tree . opt-prefix)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (and-with old-tree (apply form-ref (cons id opt-prefix))
    (tree-set! old-tree (tree-copy (tm->tree new-tree)))))

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
	 (session (get-env "prog-session"))
	 (prefix form-prefix))
    (when (supports-scripts? lan)
      (dialogue
	(with r (apply plugin-async-eval (cons* lan session in opts))
	  (form-with prefix
	    (form-set! id r)))))))

(tm-define (script->form id cas-expr)
  (script-form-eval id cas-expr :math-input :simplify-output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stand-alone forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macrofy-list p i t args)
  (if (null? t) (values t args)
      (receive (h next) (macrofy p i (car t) args)
	(receive (t end) (macrofy-list p (+ i 1) (cdr t) next)
	  (values (cons h t) end)))))

(define (macrofy p i t args)
  (cond ((in? (list p i) '((form-toggle 1)
			   (form-button-toggle 1)
			   (form-alternatives 1)
			   (form-small-input 1)
			   (form-line-input 1)
			   (form-big-input 1)))
	 (with v (string-append "v" (number->string (length args)))
	   (values `(arg ,v) (rcons args (cons v t)))))
	((nlist? t) (values t args))
	(else (macrofy-list (car t) -1 t args))))

(define (macrofy-body t)
  (receive (m args) (macrofy #f #f (tm->stree t) '())
    (values `(macro ,@(map car args) ,m)
	    `(form-window ,@(map cdr args)))))

(define (stand-alone body)
  (let* ((style '(tuple "generic" "gui-form"))
	 (init '(collection (associate "window-bars" "false")
			    (associate "prog-scripts" "maxima"))))
    `(document (style ,style) (body ,body) (initial ,init))))

(define (stand-alone* body)
  (receive (def body*) (macrofy-body body)
    (let* ((style '(tuple "generic" "gui-form"))
	   (init `(collection (associate "window-bars" "false")
			      (associate "form-window" ,def)
			      (associate "prog-scripts" "maxima"))))
      `(document (style ,style) (body ,body*) (initial ,init)))))

(define form-show-counter 0)
(tm-define (form-show name body)
  (set! form-show-counter (+ form-show-counter 1))
  (let* ((doc (stand-alone body))
	 (doc* (stand-alone* body))
	 (geom (tree-extents doc))
	 (num (number->string form-show-counter))
	 (serial (string-append "Form " num)))
    (open-buffer-in-window name doc* geom)
    (set-aux name serial)))
