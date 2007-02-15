
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

(define widget-internal-table (make-ahash-table))

(tm-define (widget-internal-new handle)
  (ahash-set! widget-internal-table handle (make-ahash-table)))

(tm-define (widget-internal-delete handle)
  (ahash-remove! widget-internal-table handle))

(tm-define (widget-internal-set! handle var val)
  (and-with t (ahash-ref widget-internal-table handle)
    (ahash-set! t var val)))

(tm-define (widget-internal-ref handle var)
  (and-with t (ahash-ref widget-internal-table handle)
    (ahash-ref t var)))

(tm-define (internal-set! var val)
  (when (context-has? "widget-prefix")
    (and-with handle* (get-env "widget-prefix")
      (with handle (if (== handle* "") "" (string-drop-right handle* 1))
	(widget-internal-set! handle var val)))))

(tm-define (internal-ref var)
  (when (context-has? "widget-prefix")
    (and-with handle* (get-env "widget-prefix")
      (with handle (if (== handle* "") "" (string-drop-right handle* 1))
	(widget-internal-ref handle var)))))

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

(tm-define (widget-ref handle . opt-prefix)
  (:secure #t)
  (if (tree? handle) (set! handle (tree->string handle)))
  (let* ((prefix (widget-get-prefix opt-prefix))
	 (l (id->trees (string-append prefix handle))))
    (and l (nnull? l) (car l))))

(tm-define (widget-set! handle new-tree . opt-prefix)
  (:secure #t)
  (if (tree? handle) (set! handle (tree->string handle)))
  (and-with old-tree (apply widget-ref (cons handle opt-prefix))
    (tree-set! old-tree (tree-copy (tm->tree new-tree)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Armouring widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define widget-serial-number 0)

(tm-define (widget-armour body)
  `(let* ((aux-handle widget-serial-number)
	  (aux-num (number->string aux-handle))
	  (aux-serial (string-append "widget-" aux-num))
	  (aux-begin (+ widget-call-back-nr 1))
	  (aux-end (+ widget-call-back-nr 1))
	  (aux-result #f)
	  (internal-ref
	   (lambda (var)
	     (widget-internal-ref aux-handle var)))
	  (internal-set!
	   (lambda (var val)
	     (widget-internal-set! aux-handle var val)))
	  (dismiss
	   (lambda ()
	     (widget-delete-call-backs aux-begin aux-end)
	     (widget-internal-delete aux-handle)
	     (kill-window-and-buffer))))
     (set! widget-serial-number (+ widget-serial-number 1))
     (widget-internal-new aux-handle)
     (set! aux-result ,body)
     (set! aux-end (+ widget-call-back-nr 1))
     `(widget ,aux-serial (document ,@aux-result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stand-alone widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (macrofy-list p i t args)
  (if (null? t) (values t args)
      (receive (h next) (macrofy p i (car t) args)
	(receive (t end) (macrofy-list p (+ i 1) (cdr t) next)
	  (values (cons h t) end)))))

(define (macrofy p i t args)
  (cond ((in? (list p i) '((short-input 1)
			   (wide-input 1)
			   (block-input 1)
			   (hidden-input 1)
			   (toggle-box 1)
			   (toggle-button 1)))
	 (with v (string-append "v" (number->string (length args)))
	   (values `(arg ,v) (rcons args (cons v t)))))
	((nlist? t) (values t args))
	(else (macrofy-list (car t) -1 t args))))

(define (macrofy-body t)
  (receive (m args) (macrofy #f #f (tm->stree t) '())
    (values `(macro ,@(map car args) ,m)
	    `(widget-window ,@(map cdr args)))))

(define (stand-alone body)
  (let* ((style '(tuple "generic" "gui"))
	 (init '(collection (associate "window-bars" "false")
			    (associate "prog-scripts" "maxima"))))
    `(document (style ,style) (body ,body) (initial ,init))))

(define (stand-alone* body)
  (receive (def body*) (macrofy-body body)
    (let* ((style '(tuple "generic" "gui"))
	   (init `(collection (associate "window-bars" "false")
			      (associate "widget-window" ,def)
			      (associate "prog-scripts" "maxima"))))
      `(document (style ,style) (body ,body*) (initial ,init)))))

(tm-define (widget-build w)
  (tm->tree (eval (widget-armour (build-content w)))))

(tm-define (widget-popup name w)
  (let* ((body (tm->tree (eval (widget-armour (build-content w)))))
	 (doc (stand-alone body))
	 (doc* (stand-alone* body))
	 (geom (tree-extents doc)))
    (open-buffer-in-window name doc* geom)
    (set-aux name name)))
