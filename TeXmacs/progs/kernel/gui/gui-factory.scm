
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
	((== w '-) `(list '(gui-medskip)))
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
  (cond ((== x :circular) '("box-shape" "circular"))
	((== x :square) '("box-shape" "square"))
	((== x :red) '("box-color" "pastel red"))
	((== x :green) '("box-color" "pastel green"))
	((== x :blue) '("box-color" "pastel blue"))
	((== x :yellow) '("box-color" "pastel yellow"))
	((== x :orange) '("box-color" "pastel orange"))
	((== x :grey) '("box-color" "light grey"))
	((== x :checked) '("marker-shape" "checked"))
	((== x :bullet) '("marker-shape" "bullet"))
	(else '())))

(tm-build (aspect . body)
  (let* ((bindings (append-map build-aspect options))
	 (fun (lambda (x) `(with ,@bindings ,x)))
	 (builder (build-content-list body)))
    `(map ,fun ,builder)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buttons, toggles, alternatives and input fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build-macro (short-input name val)
  `((quote short-input) ,name (entry ,name ,val "content")))

(tm-build-macro (input name val)
  `((quote wide-input) ,name (entry ,name ,val "content")))

(tm-build-macro (block-input name val)
  `((quote block-input) ,name (entry ,name ,val "content")))

(tm-build-macro (hidden-input name val . body)
  `((quote hidden-input) ,name (entry ,name ,val "string")
     (vertical ,@body)))

(tm-build-macro (button body . cmds)
  `((quote button) (horizontal ,body) (command ,@cmds)))

(tm-build-macro (toggle name val)
  `((quote toggle-box) ,name (entry ,name ,val "boolean")))

(tm-build-macro (toggle-button name val . body)
  `((quote toggle-button) ,name (entry ,name ,val "boolean")
    (horizontal ,@body)))

(tm-build-macro (radio name val)
  `((quote radio-box) ,name ,val))

(tm-build-macro (radio-button name val . body)
  `((quote radio-button) ,name ,val
     (horizontal ,@body)))

(tm-build-macro (header-bar . body)
  `((quote header-bar) (horizontal ,@body)))

(tm-build-macro (pagelet name val . body)
  `((quote pagelet) ,name ,val
     (vertical ,@body)))

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

(tm-build-macro (dense-raster . rows)
  `((quote dense-raster) (tformat (table ,@rows))))

(tm-build-macro (short-raster . rows)
  `((quote short-raster) (tformat (table ,@rows))))

(tm-build-macro (raster . rows)
  `((quote wide-raster) (tformat (table ,@rows))))

(tm-build-macro (dense-bar . cells)
  `((quote dense-raster) (tformat (table (row ,@cells)))))

(tm-build-macro (short-bar . cells)
  `((quote short-raster) (tformat (table (row ,@cells)))))

(tm-build-macro (bar . cells)
  `((quote wide-raster) (tformat (table (row ,@cells)))))
