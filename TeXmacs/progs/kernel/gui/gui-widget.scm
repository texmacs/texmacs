
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-widget.scm
;; DESCRIPTION : Definition of widgets
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

(define (stand-alone body)
  (let* ((style '(tuple "generic" "gui"))
	 (init '(collection (associate "window-bars" "false")
			    (associate "prog-scripts" "maxima"))))
    `(document (style ,style) (body ,body) (initial ,init))))

(tm-define (widget-build w)
  (tm->tree (eval (widget-armour (build-content w)))))

(tm-define (widget-popup name w)
  (let* ((body* (tm->tree (eval (widget-armour (build-content w)))))
	 (body `(document (disable-writability ,body*)))
	 (doc (stand-alone body))
	 (geom (tree-extents doc)))
    (open-buffer-in-window name doc geom)
    (set-aux name name)))
