
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

(define (build-options-sub l)
  (if (null? l) (cons '() '())
      (with (options . args) (build-options-sub (cdr l))
	(if (and (pair? (car l)) (keyword? (caar l)))
	    (cons (cons (car l) options) args)
	    (cons options (cons (car l) args))))))

(tm-define (build-options l)
  (build-options-sub (cdr l)))

(define (with-bindings l)
  (if (null? l) l
      (cons* (keyword->string (caar l)) (cadar l)
	     (with-bindings (cdr l)))))

(tm-define (build-with options builder)
  (if (null? options) builder
      (let* ((bindings (with-bindings options))
	     (fun (lambda (x) `(with ,@bindings ,x))))
	`(map ,fun ,builder))))

(tm-define (build-content w)
  (:synopsis "Generate a content builder from a scheme program @w")
  (cond ((list? w)
	 (with (options . body) (build-options w)
	   (with l (cons (car w) body)
	     (build-with options `(list ,(build-content-list l))))))
	((== w '---) `(list '(gui-hrule)))
	((== w '===) `(list '(gui-medskip)))
	((== w '>>>) `(list '(htab "1em")))
	((symbol? w) `(list ',w))
	(else `(list ,w))))

(tm-define (build-content-list ws)
  `(append ,@(map build-content ws)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare new content builders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (tm-build proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 (build-with options
           (begin ,@body))))))

(tm-define-macro (tm-build-macro proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 (build-with options
	   (build-content (begin ,@body)))))))

(tm-define-macro (tm-build-widget proto . body)
  (let* ((fun (car proto))
	 (args (cdr proto)))
    `(tm-define (build-content w)
       (:case ,fun)
       (with ,(cons 'options args) (build-options w)
	 (build-with options
	   (build-content-list ,(list 'quasiquote body)))))))

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

(tm-build (with . body)
  (let* ((bindings (with-bindings options))
	 (fun (lambda (x) `(with ,@bindings ,x)))
	 (builder (build-content-list body)))
    `(map ,fun ,builder)))

(tm-define (concat l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-build (concat . body)
  `(list (concat ,(build-content-list body))))

(tm-define (document l)
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'document l))))

(tm-build (document . body)
  `(list (document ,(build-content-list body))))

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
;; Buttons, toggles, alternatives and input fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build-macro (short-input name val)
  `((quote short-input) ,name (entry ,name ,val "content")))

(tm-build-macro (input name val)
  `((quote wide-input) ,name "0%" (entry ,name ,val "content")))

(tm-build-macro (block-input name val)
  `((quote block-input) ,name (entry ,name ,val "content")))

(tm-build-macro (canvas-input name y1 y2 val)
  `((quote canvas-input) ,name ,y1 ,y2 "0%" "0%"
    (entry ,name ,val "content")))

(tm-build-macro (hidden-input name val . body)
  `((quote hidden-input) ,name (entry ,name ,val "string")
    (document ,@body)))

(tm-build-macro (button body . cmds)
  `((quote button) (concat ,body) (command ,@cmds)))

(tm-build-macro (toggle name val)
  `((quote toggle-box) ,name (entry ,name ,val "boolean")))

(tm-build-macro (toggle-button name val . body)
  `((quote toggle-button) ,name (entry ,name ,val "boolean")
    (concat ,@body)))

(tm-build-macro (radio name val)
  `((quote radio-box) ,name ,val))

(tm-build-macro (radio-button name val . body)
  `((quote radio-button) ,name ,val
    (concat ,@body)))

(tm-build-macro (header-bar . body)
  `((quote header-bar) (concat ,@body)))

(tm-build-macro (pagelet name val . body)
  `((quote pagelet) ,name ,val
    (document ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tabular constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-build-macro (cell . body)
  (cond ((== body '(>>>)) `(gui-tab))
	(else `((quote cell) (concat ,@body)))))

(tm-build-macro (row . body)
  (let* ((make-cell (lambda (x) (if (func? x 'cell) x `(cell ,x))))
	 (body* (map make-cell body)))
    `((quote row) ,@body*)))

(tm-build-macro (table . body)
  (let* ((make-row (lambda (x) (if (func? x 'row) x `(row ,@x))))
	 (body* (map make-row body)))
    `((quote table) ,@body*)))

(define (range-start x)
  (cond ((number? x) (number->string x))
	((== x '*) "1")
	((list-2? x) (number->string (car x)))
	(else "1")))

(define (range-end x)
  (cond ((number? x) (number->string x))
	((== x '*) "-1")
	((list-2? x) (number->string (car x)))
	(else "-1")))

(define (table-attribute x)
  (cond ((string-starts? (keyword->string (car x)) "table-")
	 `(twith ,(keyword->string (car x)) ,(cadr x)))
	((string-starts? (keyword->string (car x)) "cell-")
	 `(cwith ,(range-start (cadr x)) ,(range-end (cadr x))
		 ,(range-start (caddr x)) ,(range-end (caddr x))
		 ,(keyword->string (car x)) ,(cadddr x)))
	(else #f)))

(tm-define-macro (tm-build-table name rows)
  `(with attrs (filter-map table-attribute options)
     (set! options (list-filter options (non table-attribute)))
     (list '(quote ,name)
	   (rcons (cons 'tformat attrs)
		  (cons 'table ,rows)))))

(tm-build-macro (dense-raster . rows)
  (tm-build-table dense-raster rows))

(tm-build-macro (short-raster . rows)
  (tm-build-table short-raster rows))

(tm-build-macro (raster . rows)
  (tm-build-table wide-raster rows))

(tm-build-macro (dense-bar . cells)
  (tm-build-table dense-raster (list cells)))

(tm-build-macro (short-bar . cells)
  (tm-build-table short-raster (list cells)))

(tm-build-macro (bar . cells)
  (tm-build-table wide-raster (list cells)))
