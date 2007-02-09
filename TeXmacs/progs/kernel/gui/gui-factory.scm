
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-factory.scm
;; DESCRIPTION : Different widget construction routines
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-factory)
  (:use (kernel gui gui-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Building widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-widget w)
  (:synopsis "Build a lazy widget constructor from a scheme program @w")
  (cond ((list? w) `(list ,(build-widgets w)))
	((== w '-) `(list '(gui-vspace)))
	((== w '---) `(list '(gui-hrule)))
	((== w '>>>) `(list '(htab "1em")))
	((symbol? w) `(list ',w))
	(else `(list ,w))))

(tm-define (build-widgets ws)
  `(append ,@(map build-widget ws)))

(tm-define-macro (define-widget proto . body)
  `(tm-define ,proto
     ,(widget-armour "default" (build-widgets body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare new widget builders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (widget-options-sub l)
  (if (and (nnull? l) (keyword? (car l)))
      (with (options . args) (widget-options-sub (cdr l))
	(cons (cons (car l) options) args))
      (cons '() l)))

(tm-define (widget-options l)
  (widget-options-sub (cdr l)))

(tm-define-macro (tm-widget proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-widget w)
       (:case ,fun)
       (with ,(cons 'options args) (widget-options w)
	 ,@body))))

(tm-define-macro (tm-widget-macro proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-widget w)
       (:case ,fun)
       (with ,(cons 'options args) (widget-options w)
	 (build-widget (begin ,@body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (sequence . body)
  (build-widgets body))

(tm-widget (let bindings . body)
  `(let* ,bindings
     ,(build-widgets body)))

(tm-widget (quote x)
  `(list ',x))

(tm-define (horizontal l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-widget (horizontal . body)
  `(list (horizontal ,(build-widgets body))))

(tm-define (vertical l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'document l))))

(tm-widget (vertical . body)
  `(list (vertical ,(build-widgets body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other helper constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (command . cmds)
  `(list (widget-new-call-back (lambda () ,@cmds))))

(tm-widget (instruct . cmds)
  `(begin
     ,@cmds
     '()))

(tm-define (widget-entry name val type)
  (when (== val :auto)
    (set! val `(begin
		 (assoc-set! form-type ,name ,type)
		 (form-auto ,name ,type))))
  (when (== type "boolean")
    (set! val `(if ,val "true" "false")))
  val)

(tm-widget (entry name val type)
  `(list ,(widget-entry name val type)))

(tm-widget-macro (internal var val)
  `(instruct (widget-internal-set! aux-id ,var ,val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Aspect attributes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-aspect x)
  (cond ((== x :red) '("gui-toggle-color" "pastel red"))
	((== x :green) '("gui-toggle-color" "pastel green"))
	((== x :blue) '("gui-toggle-color" "pastel blue"))
	((== x :yellow) '("gui-toggle-color" "pastel yellow"))
	((== x :orange) '("gui-toggle-color" "pastel orange"))
	((== x :grey) '("gui-toggle-color" "light grey"))
	((== x :circle) '("gui-toggle-type" "circle"))
	((== x :square) '("gui-toggle-type" "square"))
	((== x :checked) '("gui-marker-type" "checked"))
	((== x :bullet) '("gui-marker-type" "bullet"))
	(else '())))

(tm-widget (aspect . body)
  (let* ((bindings (append-map build-aspect options))
	 (fun (lambda (x) `(with ,@bindings ,x)))
	 (builder (build-widgets body)))
    `(map ,fun ,builder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons, toggles, alternatives and input fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget-macro (action body . cmds)
  `(form-action (horizontal ,body) (command ,@cmds)))

(tm-widget-macro (button body . cmds)
  `(form-button (horizontal ,body) (command ,@cmds)))

(tm-widget-macro (toggle name val)
  `(form-toggle ,name (entry ,name ,val "boolean")))

(tm-widget-macro (button-toggle name val . body)
  `(form-button-toggle ,name (entry ,name ,val "boolean")
     (horizontal ,@body)))

(tm-widget-macro (alternatives name val . body)
  `(form-alternatives ,name (entry ,name ,val "string")
     (vertical ,@body)))

(tm-widget-macro (alternative name val)
  `(form-alternative ,name ,val))

(tm-widget-macro (button-alternative name val . body)
  `(form-button-alternative ,name ,val
     (horizontal ,@body)))

(tm-widget-macro (header . body)
  `(gui-centered-switch (horizontal ,@body)))

(tm-widget-macro (sheet name val . body)
  `(form-sheet ,name ,val
     (vertical ,@body)))

(tm-widget-macro (field name val)
  (with f (cond ((in? :short options) 'form-short-input)
		((in? :multiline options) 'form-big-input)
		(else 'form-line-input))
    `(,f ,name (entry ,name ,val "content"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabular constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget-macro (cell . body)
  (cond ((== body '(>>>)) `(gui-tab))
	(else `((quote cell) (horizontal ,@body)))))

(tm-widget-macro (row . body)
  (let* ((make-cell (lambda (x) (if (func? x 'cell) x `(cell ,x))))
	 (body* (map make-cell body)))
    `((quote row) ,@body*)))

(tm-widget-macro (table . body)
  (let* ((make-row (lambda (x) (if (func? x 'row) x `(row ,@x))))
	 (body* (map make-row body)))
    `((quote table) ,@body*)))

(tm-widget-macro (table-widget name . rows)
  `(,name (tformat (table ,@rows))))

(tm-widget-macro (raster . rows)
  (let* ((short? (in? :short options))
	 (name (if short? 'gui-normal-bar 'gui-normal-table)))
    `(table-widget ,name ,@rows)))

(tm-widget-macro (bar . cells)
  (let* ((short? (in? :short options))
	 (name (if short? 'gui-normal-bar 'gui-normal-table)))
    `(table-widget ,name (row ,@cells))))
