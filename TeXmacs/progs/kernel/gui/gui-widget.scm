
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-widget.scm
;; DESCRIPTION : Definition of widgets
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-widget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Call back management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-call-back-nr 0)
(define widget-call-back-table (make-ahash-table))

(tm-define (widget-call-back handle)
  (:secure #t)
  (and-with fun (ahash-ref widget-call-back-table handle)
    (fun)))

(tm-define (widget-new-call-back fun)
  (set! widget-call-back-nr (+ widget-call-back-nr 1))
  (ahash-set! widget-call-back-table widget-call-back-nr fun)
  (string-append "(widget-call-back "
		 (number->string widget-call-back-nr)
		 ")"))

(tm-define (widget-delete-call-backs start end)
  (when (< start end)
    (ahash-set! widget-call-back-table start #f)
    (widget-delete-call-backs (+ start 1) end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal widget fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define widget-serial-table (make-ahash-table))
(define widget-internal-table (make-ahash-table))

(tm-define (widget-internal-new serial id)
  (ahash-set! widget-serial-table serial id)
  (ahash-set! widget-internal-table id (make-ahash-table)))

(tm-define (widget-internal-delete serial id)
  (ahash-remove! widget-serial-table serial)
  (ahash-remove! widget-internal-table id))

(tm-define (widget-internal-set! id var val)
  (and-with t (ahash-ref widget-internal-table id)
    (ahash-set! t var val)))

(tm-define (widget-internal-ref id var)
  (and-with t (ahash-ref widget-internal-table id)
    (ahash-ref t var)))

(tm-define (internal-set! var val)
  (when (context-has? "form-prefix")
    (and-with serial (string-drop-right (get-env "form-prefix") 1)
      (and-with id (ahash-ref widget-serial-table serial)
	(widget-internal-set! id var val)))))

(tm-define (internal-ref var)
  (when (context-has? "form-prefix")
    (and-with serial (string-drop-right (get-env "form-prefix") 1)
      (and-with id (ahash-ref widget-serial-table serial)
	(widget-internal-ref id var)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-serial-number 0)

(tm-define (widget-armour id body)
  `(let* ((aux-id ,id)
	  (aux-num (number->string widget-serial-number))
	  (aux-serial (string-append "widget-" aux-num))
	  (aux-begin (+ widget-call-back-nr 1))
	  (aux-end (+ widget-call-back-nr 1))
	  (aux-result #f)
	  (internal-ref
	   (lambda (var)
	     (widget-internal-ref aux-id var)))
	  (internal-set!
	   (lambda (var val)
	     (widget-internal-set! aux-id var val)))
	  (dismiss
	   (lambda ()
	     (widget-delete-call-backs aux-begin aux-end)
	     (widget-internal-delete aux-serial aux-id)
	     (kill-window-and-buffer))))
     (set! widget-serial-number (+ widget-serial-number 1))
     (widget-internal-new aux-serial aux-id)
     (set! aux-result ,body)
     (set! aux-end (+ widget-call-back-nr 1))
     (tm->tree `(form ,aux-serial (document ,@aux-result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The widget prefix and delayed evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-prefix "")

(tm-define-macro (widget-with prefix . body)
  (:secure #t)
  `(let ((new-prefix ,prefix)
	  (old-prefix widget-prefix))
     (set! widget-prefix new-prefix)
     (let ((result (begin ,@body)))
       (set! widget-prefix old-prefix)
       result)))

(define (widget-get-prefix opt-prefix)
  (cond ((null? opt-prefix) widget-prefix)
	((tree? (car opt-prefix)) (tree->string (car opt-prefix)))
	(else (car opt-prefix))))

(tm-define-macro (widget-delay . body)
  (:secure #t)
  `(delayed
     (:idle 1)
     ,@body))

(tm-define-macro (widget-delayed . body)
  (with normal? (lambda (x) (or (npair? x) (not (keyword? (car x)))))
    (receive (mods cmds) (list-break body normal?)
      `(with widget-delayed-prefix widget-prefix
	 (delayed
	   ,@mods
	   (widget-with widget-delayed-prefix
	     ,@cmds))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input and output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (widget-ref id . opt-prefix)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (let* ((prefix (widget-get-prefix opt-prefix))
	 (l (id->trees (string-append prefix id))))
    (and l (nnull? l) (car l))))

(tm-define (widget-set! id new-tree . opt-prefix)
  (:secure #t)
  (if (tree? id) (set! id (tree->string id)))
  (and-with old-tree (apply widget-ref (cons id opt-prefix))
    (tree-set! old-tree (tree-copy (tm->tree new-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stand-alone widgets
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

(define widget-show-counter 0)
(tm-define (widget-show name body)
  (set! widget-show-counter (+ widget-show-counter 1))
  (let* ((doc (stand-alone body))
	 (doc* (stand-alone* body))
	 (geom (tree-extents doc))
	 (num (number->string widget-show-counter))
	 (serial (string-append "Widget " num)))
    (open-buffer-in-window name doc* geom)
    (set-aux name serial)))
