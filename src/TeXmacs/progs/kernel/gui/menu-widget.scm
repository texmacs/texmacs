
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-widget.scm
;; DESCRIPTION : routines for generating menus
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-widget)
  (:use (kernel gui menu-define) (kernel gui kbd-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-error . args)
  (apply tm-display-error args)
  (widget-text "Error" #f ""))
(define (make-menu-bad-format p e?)
  (make-menu-error "menu has bad format in " (object->string p)))

(define (pixel) 256) ; TODO: length arithmetic
(define (widget-empty) (widget-glue #f #f 0 0))
(define (make-menu-empty) (widget-harray '() -1))

(define (delay-command cmd)
  (object->command (lambda () (exec-delayed cmd))))

(define-macro (make-menu-command cmd)
  `(delay-command (lambda ()
		    (menu-before-action)
		    ,cmd
		    (menu-after-action))))

(define (kbd-find-shortcut what)
  (with r (kbd-find-inv-binding what)
    (if (string-contains? r "accent:")
	(begin
	  (set! r (string-replace r "accent:deadhat" "^"))
	  (set! r (string-replace r "accent:tilde" "~"))
	  (set! r (string-replace r "accent:acute" "'"))
	  (set! r (string-replace r "accent:grave" "`"))
	  (set! r (string-replace r "accent:umlaut" "\""))
	  (set! r (string-replace r "accent:abovedot" "."))
	  (set! r (string-replace r "accent:breve" "U"))
	  (set! r (string-replace r "accent:check" "C"))))
    r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-label p e? . opt)
  "Make widget for menu label @p."
  ;; Possibilities for p:
  ;;   <label> :: (balloon <label> <string>)
  ;;     Label with a popup balloon. The <string> is the balloon text.
  ;;   <label> :: (text <font desc> <string>)
  ;;     Label <string> drawn in black text of an arbitrary font.
  ;;     <font desc> :: ([family [class [series [shape [size [dpi]]]]]])
  ;;     Example default values are: family="roman", class="mr",
  ;;     series="medium", shape="normal", size=10, dpi=600.
  ;;   <label> :: <string>
  ;;     Simple menu label, its display style is controlled by tt? and e?
  ;;   <label> :: (icon <string>)
  ;;     Pixmap menu label, the <string> is the name of the pixmap.
  (let ((tt? (and (nnull? opt) (car opt)))
	(col (color (if e? "black" "dark grey"))))
    (cond ((string? p)			; "text"
	   (widget-menu-text p col (get-input-language) tt?))
  	  ((tuple? p 'balloon 2)        ; (balloon <label> "balloon text")
  	   (make-menu-label (cadr p) e? tt?))
  	  ((tuple? p 'text 2)		; (text <font desc> "text")
	   (widget-box (cadr p) (caddr p) col #t #t))
  	  ((tuple? p 'icon 1)		; (icon "name.xpm")
  	   (widget-xpm (cadr p) #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elementary menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-hsep)
  "Make @--- menu item."
  (widget-separator (* 2 (pixel)) (* 2 (pixel)) #f))

(define (make-menu-vsep)
  "Make @| menu item."
  (widget-separator (* 2 (pixel)) (* 2 (pixel)) #t))

(define (make-menu-group s)
  "Make @(group :string?) menu item."
  (widget-command-button-3
   (widget-empty) (make-menu-label s #f) (widget-empty)
   (object->command noop)
   #f #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-entry-button e? bar? check label short command)
  (if bar?
      (widget-command-button-1 (make-menu-label label e?)
			       command #f)
      (widget-command-button-3
       (if (== check "")
	   (widget-empty)
	   (widget-box '()
		       (cadr (assoc check '(("v" "<checked>")
					    ("o" "<circ>")
					    ("*" "<bullet>"))))
		       (color (if e? "black" "dark grey"))
		       #t #f))
       (make-menu-label label e?)
       (if (== short "")
	   (widget-empty)
	   (make-menu-label short e? #t))
       command
       e? #f)))

(define (make-menu-entry-shortcut label action opt-key)
  (cond (opt-key opt-key)
	((pair? label) "")
	(else (with source (promise-source action)
		(if source (kbd-find-shortcut source) "")))))

(define (make-menu-entry-check-sub result propose)
  (cond ((string? result) result)
	(result propose)
	(else "")))

(define (make-menu-entry-check opt-check action)
  (if opt-check
      (make-menu-entry-check-sub ((cadr opt-check)) (car opt-check))
      (with source (promise-source action)
	(cond ((not (and source (pair? source))) "")
	      (else (with prop (property (car source) :check-mark)
		      (make-menu-entry-check-sub
		       (and prop (apply (cadr prop) (cdr source)))
		       (and prop (car prop)))))))))

(define (make-menu-entry-dots label action)
  (with source (promise-source action)
    (if (and source (pair? source) (property (car source) :interactive))
	(menu-label-add-dots label)
	label)))

(define (make-menu-entry-attrs label action opt-key opt-check)
  (cond ((match? label '(shortcut :%1 :string?))
	 (make-menu-entry-attrs (cadr label) action (caddr label) opt-check))
	((match? label '(check :%1 :string? :%1))
	 (make-menu-entry-attrs (cadr label) action opt-key (cddr label)))
	(else (values label action opt-key opt-check))))

(define (make-menu-entry-sub p e? bar?)
  (receive
      (label action opt-key opt-check)
      (make-menu-entry-attrs (car p) (cAr p) #f #f)
    (make-menu-entry-button
     e? bar?
     (make-menu-entry-check opt-check action)
     (make-menu-entry-dots label action)
     (make-menu-entry-shortcut label action opt-key)
     (make-menu-command (if e? (apply action '()))))))

(define (make-menu-entry p e? bar?)
  "Make @:menu-wide-item menu item."
  (let ((button (make-menu-entry-sub p e? bar?))
	(label (car p)))
    (if (tuple? label 'balloon 2)
	(widget-balloon button
			(widget-text (caddr label) #t (get-input-language)))
	button)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-symbol-button e? symstring opt-symobj)
  (widget-command-button-1
   (widget-box '() symstring
	       (color (if e? "black" "dark grey"))
	       #t #f)
   (if opt-symobj
       (make-menu-command (insert opt-symobj))
       (make-menu-command (insert symstring)))
   #f))

(define (make-menu-symbol p e?)
  "Make @(symbol :string? :*) menu item."
  ;; Possibilities for p:
  ;;   <menu-symbol> :: (symbol <symbol-string> [<symbol-object>] [<shortcut>])
  ;;   <symbol-object> :: (<symbol-type> <symbol-name>)
  ;;   <symbol-type> :: left | right | mid | big
  ;;   <symbol-name> :: <string>
  (with (tag symstring . opt) p
    (let ((error? #f) (opt-shortcut #f) (opt-symobj #f))
      (for-each
       (lambda (x)
	 (cond (error? (noop))
	       ((pair? x) (set! opt-symobj x))
	       ((string? x) (set! opt-shortcut x))
	       (else (set! error? #t))))
       opt)
      (if error? (make-menu-error "invalid symbol attribute in " p)
	  (let ((sh (or opt-shortcut (kbd-find-shortcut symstring))))
	    (if (== sh "")
		(make-menu-symbol-button e? symstring opt-symobj)
		(widget-balloon
		 (make-menu-symbol-button e? symstring opt-symobj)
		 (make-menu-label (string-append "Keyboard equivalent: " sh)
				  e?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite menus and submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-horizontal p e?)
  "Make @(horizontal :menu-item-list) menu item."
  (widget-harray (make-menu-items (cadr p) e? #t) -1))

(define (make-menu-vertical p e?)
  "Make @(vertical :menu-item-list) menu item."
  (widget-vmenu (make-menu-items (cadr p) e? #f)))

(define (make-menu-submenu p e?)
  "Make @((:or -> =>) :menu-label :menu-item-list) menu item."
  (with (tag label . items) p
    (let ((button
	   ((cond ((== tag '=>) widget-pulldown-button-lazy)
		  ((== tag '->) widget-pullright-button-lazy))
	    (make-menu-label label e?)
	    (object->make-widget
	     (lambda () (make-menu-widget (list 'vertical items) e?))))))
      (if (tuple? label 'balloon 2)
	  (widget-balloon button
			  (widget-text (caddr label) #t (get-input-language)))
	  button))))

(define (make-menu-tile p e?)
  "Make @(tile :integer? :menu-item-list) menu item."
  (with (tag width . items) p
    (widget-tile (make-menu-items items e? #f) width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-if p e? bar?)
  "Make @(if :%1 :menu-item-list) menu items."
  (with (tag pred? . items) p
    (if (pred?) (make-menu-items-list items e? bar?) '())))

(define (make-menu-when p e? bar?)
  "Make @(when :%1 :menu-item-list) menu items."
  (with (tag pred? . items) p
    (make-menu-items-list items (and e? (pred?)) bar?)))

(define (make-menu-link p e? bar?)
  "Make @(link :%1) menu items."
  (with linked ((eval (cadr p)))
    (if linked (make-menu-items linked e? bar?)
	(make-menu-error "bad link: " (object->string (cadr p))))))

(define (make-menu-promise p e? bar?)
  "Make @(promise :%1) menu items."
  (with value ((cadr p))
    (if (match? value ':menu-item) (make-menu-items value e? bar?)
	(make-menu-error "promise did not yield a menu: " value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routines for making menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-items-list l e? bar?)
  "Make menu items for each element in @l and append results."
  (append-map (lambda (p) (make-menu-items p e? bar?)) l))

(define (make-menu-items p e? bar?)
  "Make menu items @p. The items are on a bar if @bar? and greyed if not @e?."
  (if (pair? p)
      (cond ((string? (car p)) (list (make-menu-entry p e? bar?)))
	    ((symbol? (car p))
	     (with result (ahash-ref make-menu-items-table (car p))
	       (if (or (not result) (not (match? (cdr p) (car result))))
		   (make-menu-items-list p e? bar?)
		   ((cadr result) p e? bar?))))
	    ((match? (car p) ':menu-wide-label)
	     (list (make-menu-entry p e? bar?)))
	    (else (make-menu-items-list p e? bar?)))
      (cond ((== p '---) (list (make-menu-hsep)))
	    ((== p '|) (list (make-menu-vsep)))
	    ((== p '()) p)
	    (else (list (make-menu-bad-format p e?))))))

(define-table make-menu-items-table
  (group (:string?) ,(lambda (p e? bar?) (list (make-menu-group (cadr p)))))
  (symbol (:string? :*) ,(lambda (p e? bar?) (list (make-menu-symbol p e?))))
  (link (:%1) ,(lambda (p e? bar?) (make-menu-link p e? bar?)))
  (horizontal (:*) ,(lambda (p e? bar?) (list (make-menu-horizontal p e?))))
  (vertical (:*) ,(lambda (p e? bar?) (list (make-menu-vertical p e?))))
  (-> (:menu-label :*) ,(lambda (p e? bar?) (list (make-menu-submenu p e?))))
  (=> (:menu-label :*) ,(lambda (p e? bar?) (list (make-menu-submenu p e?))))
  (tile (:integer? :*) ,(lambda (p e? bar?) (list (make-menu-tile p e?))))
  (if (:%1 :*) ,(lambda (p e? bar?) (make-menu-if p e? bar?)))
  (when (:%1 :*) ,(lambda (p e? bar?) (make-menu-when p e? bar?)))
  (promise (:%1) ,(lambda (p e? bar?) (make-menu-promise p e? bar?))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu expansion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (menu-expand-link p)
  "Expand menu link @p."
  (with linked ((eval (cadr p)))
    (if linked (menu-expand linked) p)))

(define (menu-expand-if p)
  "Expand conditional menu @p."
  (with (tag pred? . items) p
    (if (pred?) (menu-expand-list items) '())))

(define (menu-expand-promise p)
  "Expand promised menu @p."
  (with value ((cadr p))
    (if (match? value ':menu-item) (menu-expand value) p)))

(define (menu-expand-list l)
  "Expand links and conditional menus in list of menus @l."
  (map menu-expand l))

(tm-define (menu-expand p)
  (:type (-> object object))
  (:synopsis "Expand links and conditional menus in menu @p.")
  (cond ((npair? p) p)
	((string? (car p)) p)
	((symbol? (car p))
	 (with result (ahash-ref menu-expand-table (car p))
	   (if result ((car result) p) p)))
	((match? (car p) ':menu-wide-label) p)
	(else (menu-expand-list p))))

(define-table menu-expand-table
  (--- ,(lambda (p) `(--- ,@(menu-expand-list (cdr p)))))
  (| ,(lambda (p) `(| ,@(menu-expand-list (cdr p)))))
  (group ,(lambda (p) p))
  (symbol ,(lambda (p) p))
  (link ,menu-expand-link p)
  (horizontal ,(lambda (p) `(horizontal ,@(menu-expand-list (cdr p)))))
  (vertical ,(lambda (p) `(vertical ,@(menu-expand-list (cdr p)))))
  (-> ,(lambda (p) p))
  (=> ,(lambda (p) p))
  (tile ,(lambda (p) p))
  (if ,menu-expand-if)
  (when ,(lambda (p) p))
  (promise ,menu-expand-promise))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debugging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (set-trace-level! make-menu-entry-button)
; (set-trace-level! make-menu-entry-shortcut)
; (set-trace-level! make-menu-entry-check)

; (set-trace-point! make-menu-hsep          "case: ---")
; (set-trace-point! make-menu-vsep          "case: |")
; (set-trace-point! make-menu-group         "case: group")
; (set-trace-point! make-menu-items-list    "case: list")
; (set-trace-point! make-menu-if            "case: if")
; (set-trace-point! make-menu-when          "case: when")
; (set-trace-point! make-menu-submenu       "case: =>/->")
; (set-trace-point! make-menu-tile          "case: tile")
; (set-trace-point! make-menu-link          "case: link")
; (set-trace-point! make-menu-promise       "case: promise")
; (set-trace-point! make-menu-entry         "case: item")
; (set-trace-point! make-menu-symbol        "case: symbol")

; (set-trace-level! make-menu-widget)
; (set-trace-level! make-menu-entry-sub)
; (set-trace-level! make-menu-items)

; (set-trace-point! make-menu-horizontal    "case: horizontal")
; (set-trace-point! make-menu-vertical      "case: vertical")
; (set-trace-point! make-menu-linked-menu   "case: link")
; (set-trace-point! make-menu-promised-menu "case: promise")
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-main p e?)
  "Transform the menu @p into a widget."
  (with l (make-menu-items p e? #f)
    (cond ((null? l) (make-menu-empty))
	  ((and (list? l) (null? (cdr l))) (car l))
	  (else (make-menu-bad-format p e?)))))

(tm-define (make-menu-widget p e?)
  (:type (-> object widget))
  (:synopsis "Transform a menu into a widget.")
  (:argument p "a scheme object which represents the menu")
  (:argument e? "greyed menu if @e? is @#f")
  ((wrap-catch make-menu-main) p e?))
