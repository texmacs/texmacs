
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-widget.scm
;; DESCRIPTION : routines for generating menus
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven, David Allouche
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
  (widget-text "Error" (color "black") #t))

(define (make-menu-bad-format p e?)
  (make-menu-error "menu has bad format in " (object->string p)))

(define (make-menu-empty) (widget-hmenu '()))

(define (delay-command cmd)
  (object->command (lambda () (exec-delayed cmd))))

(define-macro (make-menu-command cmd)
  `(delay-command (lambda ()
		    (menu-before-action)
		    ,cmd
		    (menu-after-action))))

(define (kbd-system shortcut menu-flag?)
  (cond ((nstring? shortcut) "")
	((and (qt-gui?) menu-flag?) shortcut)
	(else (translate (kbd-system-rewrite shortcut)))))

(define (kbd-find-shortcut what menu-flag?)
  (with r (kbd-find-inv-binding what)
    (when (string-contains? r "accent:")
      (set! r (string-replace r "accent:deadhat" "^"))
      (set! r (string-replace r "accent:tilde" "~"))
      (set! r (string-replace r "accent:acute" "'"))
      (set! r (string-replace r "accent:grave" "`"))
      (set! r (string-replace r "accent:umlaut" "\""))
      (set! r (string-replace r "accent:abovedot" "."))
      (set! r (string-replace r "accent:breve" "U"))
      (set! r (string-replace r "accent:check" "C")))
    ;;(when (!= r "")
    ;;  (display* what " -> " r " -> " (kbd-system r menu-flag?) "\n"))
    (kbd-system r menu-flag?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu labels
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translatable? s)
  (or (string? s) (func? s 'concat) (func? s 'verbatim)))

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
    (cond ((translatable? p)		; "text"
	   (widget-text (translate p) col #t))
  	  ((tuple? p 'balloon 2)        ; (balloon <label> "balloon text")
  	   (make-menu-label (cadr p) e? tt?))
  	  ((tuple? p 'text 2)		; (text <font desc> "text")
	   (widget-box (cadr p) (caddr p) col #t #t))
  	  ((tuple? p 'icon 1)		; (icon "name.xpm")
  	   (widget-xpm (cadr p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elementary menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-hsep)
  "Make @--- menu item."
  (widget-separator #f))

(define (make-menu-vsep)
  "Make @| menu item."
  (widget-separator #t))

(define (make-menu-group s)
  "Make @(group :string?) menu item."
  (widget-menu-group s))

(define (make-menu-input p)
  "Make @(input :%1) menu item."
  (with (tag cmd type props) p
    (widget-input (object->command cmd) type (props))))
;;(widget-input (make-menu-command cmd) type (props))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-entry-button e? bar? check label short command)
  (if bar?
      (widget-menu-button (make-menu-label label e?) command "" "" e?)
      (widget-menu-button (make-menu-label label e?) command check short e?)))

(define (make-menu-entry-shortcut label action opt-key)
  (cond (opt-key (kbd-system opt-key #t))
	((pair? label) "")
	(else (with source (promise-source action)
		(if source (kbd-find-shortcut source #t) "")))))

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
  (let ((but (make-menu-entry-sub p e? bar?))
	(label (car p)))
    (if (tuple? label 'balloon 2)
	(let* ((text (caddr label))
	       (cmd (and (nnull? (cdr p)) (procedure? (cadr p)) (cadr p)))
	       (src (and cmd (promise-source cmd)))
	       (sh (and src (kbd-find-shortcut src #f)))
	       (txt (if (or (not sh) (== sh "")) text
			(string-append text " (" sh ")")))
	       (ftxt (translate txt))
	       (twid (widget-text ftxt (color "black") #t)))
	  (widget-balloon but twid))
	but)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-symbol-button e? sym opt-cmd)
  (with col (color (if e? "black" "dark grey"))
    (if opt-cmd
	(widget-menu-button (widget-box '() sym col #t #f)
			    (make-menu-command (apply opt-cmd '())) "" "" e?)
	(widget-menu-button (widget-box '() sym col #t #f)
			    (make-menu-command (insert sym)) "" "" e?))))

(define (make-menu-symbol p e?)
  "Make @(symbol :string? :*) menu item."
  ;; Possibilities for p:
  ;;   <menu-symbol> :: (symbol <symbol-string> [<cmd>])
  (with (tag symstring . opt) p
    (with opt-cmd (and (nnull? opt) (car opt))
      (if (and opt-cmd (not (procedure? opt-cmd)))
	  (make-menu-error "invalid symbol command in " p)
	  (let* ((source (and opt-cmd (promise-source opt-cmd)))
		 (sh (kbd-find-shortcut (if source source symstring) #f)))
	    (if (== sh "")
		(make-menu-symbol-button e? symstring opt-cmd)
		(widget-balloon
		 (make-menu-symbol-button e? symstring opt-cmd)
		 (make-menu-label (string-append "Keyboard equivalent: " sh)
				  e?))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite menus and submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-horizontal p e?)
  "Make @(horizontal :menu-item-list) menu item."
  (widget-hmenu (make-menu-items (cadr p) e? #t)))

(define (make-menu-vertical p e?)
  "Make @(vertical :menu-item-list) menu item."
  (widget-vmenu (make-menu-items (cadr p) e? #f)))

(define (make-menu-submenu p e?)
  "Make @((:or -> =>) :menu-label :menu-item-list) menu item."
  (with (tag label . items) p
    (let ((button
	   ((cond ((== tag '=>) widget-pulldown-button)
		  ((== tag '->) widget-pullright-button))
	    (make-menu-label label e?)
	    (object->promise-widget
	     (lambda () (make-menu-widget (list 'vertical items) e?))))))
      (if (tuple? label 'balloon 2)
	  (let* ((text (caddr label))
		 (ftxt (translate text))
		 (twid (widget-text ftxt (color "black") #t)))
	    (widget-balloon button twid))
	  button))))

(define (make-menu-tile p e?)
  "Make @(tile :integer? :menu-item-list) menu item."
  (with (tag width . items) p
    (widget-tmenu (make-menu-items items e? #f) width)))

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
      (cond ((match? p '(input :%1 :string? :%1))
	     (list (make-menu-input p)))
	    ((translatable? (car p))
	     (list (make-menu-entry p e? bar?)))
	    ((symbol? (car p))
	     (with result (ahash-ref make-menu-items-table (car p))
	       (if (or (not result) (not (match? (cdr p) (car result))))
		   (make-menu-items-list p e? bar?)
		   ((cadr result) p e? bar?))))
	    ((match? (car p) ':menu-wide-label)
	     (list (make-menu-entry p e? bar?)))
	    (else
	     (make-menu-items-list p e? bar?)))
      (cond ((== p '---) (list (make-menu-hsep)))
	    ((== p '|) (list (make-menu-vsep)))
	    ((== p '()) p)
	    (else (list (make-menu-bad-format p e?))))))

(define-table make-menu-items-table
  (group (:string?) ,(lambda (p e? bar?) (list (make-menu-group (cadr p)))))
  (symbol (:string? :*) ,(lambda (p e? bar?) (list (make-menu-symbol p e?))))
  (input (:%1 :string? :%1) ,(lambda (p e? bar?) (list (make-menu-input p))))
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

(define (replace-procedures x)
  (cond ((procedure? x) (procedure-source x))
	((list? x) (map replace-procedures x))
	(else x)))

(tm-define (menu-expand p)
  (:type (-> object object))
  (:synopsis "Expand links and conditional menus in menu @p.")
  (cond ((npair? p) (replace-procedures p))
	((string? (car p)) p)
	((symbol? (car p))
	 (with result (ahash-ref menu-expand-table (car p))
	   (if result ((car result) p) p)))
	((match? (car p) ':menu-wide-label) (replace-procedures p))
	(else (menu-expand-list p))))

(define-table menu-expand-table
  (--- ,(lambda (p) `(--- ,@(menu-expand-list (cdr p)))))
  (| ,(lambda (p) `(| ,@(menu-expand-list (cdr p)))))
  (group ,replace-procedures)
  (symbol ,replace-procedures)
  (input ,replace-procedures)
  (link ,menu-expand-link p)
  (horizontal ,(lambda (p) `(horizontal ,@(menu-expand-list (cdr p)))))
  (vertical ,(lambda (p) `(vertical ,@(menu-expand-list (cdr p)))))
  (-> ,replace-procedures)
  (=> ,replace-procedures)
  (tile ,replace-procedures)
  (if ,menu-expand-if)
  (when ,replace-procedures)
  (promise ,menu-expand-promise))
 
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
