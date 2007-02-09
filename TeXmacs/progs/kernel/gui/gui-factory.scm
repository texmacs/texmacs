
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-factory.scm
;; DESCRIPTION : Factory of content builders
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
;; Building content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (build-content w)
  (:synopsis "Generate a content builder from a scheme program @w")
  (cond ((list? w) `(list ,(build-content-list w)))
	((== w '-) `(list '(gui-vspace)))
	((== w '---) `(list '(gui-hrule)))
	((== w '>>>) `(list '(htab "1em")))
	((symbol? w) `(list ',w))
	(else `(list ,w))))

(tm-define (build-content-list ws)
  `(append ,@(map build-content ws)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare new content builders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-options-sub l)
  (if (and (nnull? l) (keyword? (car l)))
      (with (options . args) (build-options-sub (cdr l))
	(cons (cons (car l) options) args))
      (cons '() l)))

(tm-define (build-options l)
  (build-options-sub (cdr l)))

(tm-define-macro (tm-build proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 ,@body))))

(tm-define-macro (tm-build-macro proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 (build-content (begin ,@body))))))

(tm-define-macro (tm-build-widget proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 (build-content-list ,(list 'quasiquote body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build (sequence . body)
  (build-content-list body))

(tm-build (let bindings . body)
  `(let* ,bindings
     ,(build-content-list body)))

(tm-build (quote x)
  `(list ',x))

(tm-define (horizontal l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-build (horizontal . body)
  `(list (horizontal ,(build-content-list body))))

(tm-define (vertical l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'document l))))

(tm-build (vertical . body)
  `(list (vertical ,(build-content-list body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fundamental interactive constructs for building widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build (command . cmds)
  `(list (widget-new-call-back (lambda () ,@cmds))))

(tm-build (instruct . cmds)
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

(tm-build (entry name val type)
  `(list ,(widget-entry name val type)))

(tm-build-macro (internal var val)
  `(instruct (widget-internal-set! aux-handle ,var ,val)))

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

(tm-build (aspect . body)
  (let* ((bindings (append-map build-aspect options))
	 (fun (lambda (x) `(with ,@bindings ,x)))
	 (builder (build-content-list body)))
    `(map ,fun ,builder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons, toggles, alternatives and input fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build-macro (action body . cmds)
  `(form-action (horizontal ,body) (command ,@cmds)))

(tm-build-macro (button body . cmds)
  `(form-button (horizontal ,body) (command ,@cmds)))

(tm-build-macro (toggle name val)
  `(form-toggle ,name (entry ,name ,val "boolean")))

(tm-build-macro (button-toggle name val . body)
  `(form-button-toggle ,name (entry ,name ,val "boolean")
     (horizontal ,@body)))

(tm-build-macro (alternatives name val . body)
  `(form-alternatives ,name (entry ,name ,val "string")
     (vertical ,@body)))

(tm-build-macro (alternative name val)
  `(form-alternative ,name ,val))

(tm-build-macro (button-alternative name val . body)
  `(form-button-alternative ,name ,val
     (horizontal ,@body)))

(tm-build-macro (header . body)
  `(gui-centered-switch (horizontal ,@body)))

(tm-build-macro (sheet name val . body)
  `(form-sheet ,name ,val
     (vertical ,@body)))

(tm-build-macro (field name val)
  (with f (cond ((in? :short options) 'form-short-input)
		((in? :multiline options) 'form-big-input)
		(else 'form-line-input))
    `(,f ,name (entry ,name ,val "content"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabular constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build-macro (cell . body)
  (cond ((== body '(>>>)) `(gui-tab))
	(else `((quote cell) (horizontal ,@body)))))

(tm-build-macro (row . body)
  (let* ((make-cell (lambda (x) (if (func? x 'cell) x `(cell ,x))))
	 (body* (map make-cell body)))
    `((quote row) ,@body*)))

(tm-build-macro (table . body)
  (let* ((make-row (lambda (x) (if (func? x 'row) x `(row ,@x))))
	 (body* (map make-row body)))
    `((quote table) ,@body*)))

(tm-build-macro (raster . rows)
  (let* ((short? (in? :short options))
	 (name (if short? 'gui-normal-bar 'gui-normal-table)))
    `(,name (tformat (table ,@rows)))))

(tm-build-macro (bar . cells)
  (let* ((short? (in? :short options))
	 (name (if short? 'gui-normal-bar 'gui-normal-table)))
    `(,name (tformat (table (row ,@cells))))))
