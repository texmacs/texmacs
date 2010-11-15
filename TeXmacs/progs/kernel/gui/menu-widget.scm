
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
;; Menu grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-regexp-grammar
  (:menu-label (:or
    :string?
    (concat :*)
    (color :%5)
    (verbatim :%1)
    (text :tuple? :string?)
    (icon :string?)
    (balloon :menu-label :string?)))
  (:menu-wide-label (:or
    :menu-label
    (check :menu-wide-label :string? :%1)
    (shortcut :menu-wide-label :string?)))
  (:menu-item (:or
    ---
    |
    (group :string?)
    (glue :boolean? :boolean? :integer? :integer?)
    (color :%1 :boolean? :boolean? :integer? :integer?)
    (:menu-wide-label :%1)
    (symbol :string? :*)
    (input :%1 :string? :%1 :string?)
    (pick-color :%1)
    (pick-background :%1)
    (horizontal :menu-item-list)
    (vertical :menu-item-list)
    (hlist :menu-item-list)
    (vlist :menu-item-list)
    (minibar :menu-item-list)
    (-> :menu-label :menu-item-list)
    (=> :menu-label :menu-item-list)
    (tile :integer? :menu-item-list)
    (if :%1 :menu-item-list)
    (when :%1 :menu-item-list)
    (mini :%1 :menu-item-list)
    (link :%1)
    (promise :%1)
    (:menu-item-list)))
  (:menu-item-list (:repeat :menu-item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-error . args)
  (apply tm-display-error args)
  (widget-text "Error" 0 (color "black") #t))

(define (make-menu-bad-format p style)
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

(define (active? style)
  (== (logand style widget-style-inert) 0))

(define (make-menu-label p style . opt)
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
  ;;     Simple menu label, its display style is controlled by tt? and style
  ;;   <label> :: (icon <string>)
  ;;     Pixmap menu label, the <string> is the name of the pixmap.
  (let ((tt? (and (nnull? opt) (car opt)))
	(col (color (if (active? style) "black" "dark grey"))))
    (cond ((translatable? p)		; "text"
	   (widget-text (translate p) style col #t))
  	  ((tuple? p 'balloon 2)        ; (balloon <label> "balloon text")
  	   (make-menu-label (cadr p) style tt?))
  	  ((tuple? p 'text 2)		; (text <font desc> "text")
	   (widget-box (cadr p) (caddr p) col #t #t))
  	  ((tuple? p 'icon 1)		; (icon "name.xpm")
  	   (widget-xpm (cadr p)))
  	  ((tuple? p 'color 5)		; (color col hext? vext? minw minh)
  	   (widget-color (second p) (third p) (fourth p)
                         (* (fifth p) 256) (* (sixth p) 256))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Elementary menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-hsep)
  "Make @--- menu item."
  (widget-separator #f))

(define (make-menu-vsep)
  "Make @| menu item."
  (widget-separator #t))

(define (make-menu-glue hext? vext? minw minh)
  "Make @(glue :boolean? :boolean? :integer? :integer?) menu item."
  (widget-glue hext? vext? (* minw 256) (* minh 256)))

(define (make-menu-color col hext? vext? minw minh)
  "Make @(glue :1% :boolean? :boolean? :integer? :integer?) menu item."
  (widget-color col hext? vext? (* minw 256) (* minh 256)))

(define (make-menu-group s style)
  "Make @(group :string?) menu item."
  (widget-menu-group s style))

(define (make-menu-input p style)
  "Make @(input :%1 :string? :%1 :string?) menu item."
  (with (tag cmd type props width) p
    (widget-input (object->command cmd) type (props)
		  (logior style widget-style-mini) width)))

(define (make-menu-pick-color p)
  "Make @(pick-color :%1) menu item."
  (with (tag cmd bg?) p
    (widget-color-picker (object->command cmd) bg? '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-entry-button style bar? check label short command)
  (let* ((l (make-menu-label label style))
	 (pressed? (and bar? (!= check "")))
	 (new-style (logior style (if pressed? widget-style-pressed 0))))
    (if bar?
	(widget-menu-button l command "" "" new-style)
	(widget-menu-button l command check short style))))

(define-public (promise-source action)
  "Helper routines for menu-widget and kbd-define"
  (and (procedure? action)
       (with source (procedure-source action)
	 (and (== (car source) 'lambda)
	      (== (cadr source) '())
	      (null? (cdddr source))
	      (caddr source)))))

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

(define (menu-label-add-dots l)
  (cond ((match? l ':string?) (string-append l "..."))
	((match? l '(concat :*))
	 `(,@(cDr l) ,(menu-label-add-dots (cAr l))))
	((match? l '(verbatim :*))
	 `(,@(cDr l) ,(menu-label-add-dots (cAr l))))
	((match? l '(text :tuple? :string?))
	 `(text ,(cadr l) ,(string-append (caddr l) "...")))
	((match? l '(icon :string?)) l)
	(else `(,(car l) ,(menu-label-add-dots (cadr l)) ,(caddr l)))))

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

(define (make-menu-entry-sub p style bar?)
  (receive
      (label action opt-key opt-check)
      (make-menu-entry-attrs (car p) (cAr p) #f #f)
    (make-menu-entry-button
     style bar?
     (make-menu-entry-check opt-check action)
     (make-menu-entry-dots label action)
     (make-menu-entry-shortcut label action opt-key)
     (make-menu-command (if (active? style) (apply action '()))))))

(define (make-menu-entry p style bar?)
  "Make @:menu-wide-item menu item."
  (let ((but (make-menu-entry-sub p style bar?))
	(label (car p)))
    (if (tuple? label 'balloon 2)
	(let* ((text (caddr label))
	       (cmd (and (nnull? (cdr p)) (procedure? (cadr p)) (cadr p)))
	       (src (and cmd (promise-source cmd)))
	       (sh (and src (kbd-find-shortcut src #f)))
	       (txt (if (or (not sh) (== sh "")) text
			(string-append text " (" sh ")")))
	       (ftxt (translate txt))
	       (twid (widget-text ftxt style (color "black") #t)))
	  (widget-balloon but twid))
	but)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol fields
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-symbol-button style sym opt-cmd)
  (with col (color (if (active? style) "black" "dark grey"))
    (if opt-cmd
	(widget-menu-button (widget-box '() sym col #t #f)
			    (make-menu-command (apply opt-cmd '()))
			    "" "" style)
	(widget-menu-button (widget-box '() sym col #t #f)
			    (make-menu-command (insert sym))
			    "" "" style))))

(define (make-menu-symbol p style)
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
		(make-menu-symbol-button style symstring opt-cmd)
		(widget-balloon
		 (make-menu-symbol-button style symstring opt-cmd)
		 (make-menu-label (string-append "Keyboard equivalent: " sh)
				  style))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite menus and submenus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-horizontal p style)
  "Make @(horizontal :menu-item-list) menu item."
  (widget-hmenu (make-menu-items (cdr p) style #t)))

(define (make-menu-vertical p style)
  "Make @(vertical :menu-item-list) menu item."
  (widget-vmenu (make-menu-items (cdr p) style #f)))

(define (make-menu-hlist p style)
  "Make @(hlist :menu-item-list) menu item."
  (widget-hlist (make-menu-items (cdr p) style #t)))

(define (make-menu-vlist p style)
  "Make @(vertical :menu-item-list) menu item."
  (widget-vlist (make-menu-items (cdr p) style #f)))

(define (make-menu-minibar p style)
  "Make @(minibar :menu-item-list) menu items."
  (with new-style (logior style widget-style-mini)
    (widget-minibar-menu (make-menu-items (cdr p) new-style #t))))

(define (make-menu-submenu p style)
  "Make @((:or -> =>) :menu-label :menu-item-list) menu item."
  (with (tag label . items) p
    (let ((button
	   ((cond ((== tag '=>) widget-pulldown-button)
		  ((== tag '->) widget-pullright-button))
	    (make-menu-label label style)
	    (object->promise-widget
	     (lambda () (make-menu-widget (list 'vertical items) style))))))
      (if (tuple? label 'balloon 2)
	  (let* ((text (caddr label))
		 (ftxt (translate text))
		 (twid (widget-text ftxt style (color "black") #t)))
	    (widget-balloon button twid))
	  button))))

(define (make-menu-tile p style)
  "Make @(tile :integer? :menu-item-list) menu item."
  (with (tag width . items) p
    (widget-tmenu (make-menu-items items style #f) width)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-if p style bar?)
  "Make @(if :%1 :menu-item-list) menu items."
  (with (tag pred? . items) p
    (if (pred?) (make-menu-items-list items style bar?) '())))

(define (make-menu-when p style bar?)
  "Make @(when :%1 :menu-item-list) menu items."
  (with (tag pred? . items) p
    (let* ((old-active? (== (logand style widget-style-inert) 0))
	   (new-active? (and old-active? (pred?)))
	   (new-style (logior style (if new-active? 0 widget-style-inert))))
      (make-menu-items-list items new-style bar?))))

(define (make-menu-mini p style bar?)
  "Make @(mini :%1 :menu-item-list) menu items."
  (with (tag pred? . items) p
    (let* ((style-maxi (logand style (lognot widget-style-mini)))
	   (style-mini (logior style-maxi widget-style-mini))
	   (new-style (if (pred?) style-mini style-maxi)))
      (make-menu-items-list items new-style bar?))))

(define (make-menu-link p style bar?)
  "Make @(link :%1) menu items."
  (with linked ((eval (cadr p)))
    (if linked (make-menu-items linked style bar?)
	(make-menu-error "bad link: " (object->string (cadr p))))))

(define (make-menu-promise p style bar?)
  "Make @(promise :%1) menu items."
  (with value ((cadr p))
    ;;(make-menu-items value style bar?)
    (if (match? value ':menu-item) (make-menu-items value style bar?)
	(make-menu-error "promise did not yield a menu: " value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routines for making menu items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-items-list l style bar?)
  "Make menu items for each element in @l and append results."
  (append-map (lambda (p) (make-menu-items p style bar?)) l))

(define (make-menu-items p style bar?)
  "Make menu items @p. The items are on a bar if @bar? and of a given @style."
  ;;(display* "Make items " p ", " style "\n")
  (if (pair? p)
      (cond ((match? p '(input :%1 :string? :%1 :string?))
	     (list (make-menu-input p style)))
	    ((translatable? (car p))
	     (list (make-menu-entry p style bar?)))
	    ((symbol? (car p))
	     (with result (ahash-ref make-menu-items-table (car p))
	       (if (or (not result) (not (match? (cdr p) (car result))))
		   (make-menu-items-list p style bar?)
		   ((cadr result) p style bar?))))
	    ((match? (car p) ':menu-wide-label)
	     (list (make-menu-entry p style bar?)))
	    (else
	     (make-menu-items-list p style bar?)))
      (cond ((== p '---) (list (make-menu-hsep)))
	    ((== p '|) (list (make-menu-vsep)))
	    ((== p '()) p)
	    (else (list (make-menu-bad-format p style))))))

(define-table make-menu-items-table
  (glue (:boolean? :boolean? :integer? :integer?)
        ,(lambda (p style bar?)
           (list (make-menu-glue (second p) (third p)
                                 (fourth p) (fifth p)))))
  (color (:%1 :boolean? :boolean? :integer? :integer?)
         ,(lambda (p style bar?)
            (list (make-menu-color (second p) (third p)
                                   (fourth p) (fifth p) (sixth p)))))
  (group (:string?)
	 ,(lambda (p style bar?) (list (make-menu-group (cadr p) style))))
  (symbol (:string? :*)
	  ,(lambda (p style bar?) (list (make-menu-symbol p style))))
  (input (:%1 :string? :%1 :string?)
         ,(lambda (p style bar?) (list (make-menu-input p style))))
  (pick-color (:%1 :%1)
	      ,(lambda (p style bar?) (list (make-menu-pick-color p))))
  (link (:%1)
	,(lambda (p style bar?) (make-menu-link p style bar?)))
  (horizontal (:*)
	      ,(lambda (p style bar?) (list (make-menu-horizontal p style))))
  (vertical (:*)
	    ,(lambda (p style bar?) (list (make-menu-vertical p style))))
  (hlist (:*)
         ,(lambda (p style bar?) (list (make-menu-hlist p style))))
  (vlist (:*)
         ,(lambda (p style bar?) (list (make-menu-vlist p style))))
  (minibar (:*)
	    ,(lambda (p style bar?) (list (make-menu-minibar p style))))
  (-> (:%1 :*)
      ,(lambda (p style bar?) (list (make-menu-submenu p style))))
  (=> (:%1 :*)
      ,(lambda (p style bar?) (list (make-menu-submenu p style))))
  (tile (:integer? :*)
	,(lambda (p style bar?) (list (make-menu-tile p style))))
  (if (:%1 :*)
      ,(lambda (p style bar?) (make-menu-if p style bar?)))
  (when (:%1 :*)
        ,(lambda (p style bar?) (make-menu-when p style bar?)))
  (mini (:%1 :*)
        ,(lambda (p style bar?) (make-menu-mini p style bar?)))
  (promise (:%1)
	   ,(lambda (p style bar?) (make-menu-promise p style bar?))))

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

(define (menu-expand-when p)
  "Expand potentially greyed menu @p."
  (with (tag pred? . items) p
    (if (pred?)
	(cons* 'when #t (menu-expand-list items))
	(cons* 'when #f (replace-procedures items)))))

(define (menu-expand-mini p)
  "Expand mini menu @p."
  (with (tag pred? . items) p
    (cons* 'mini (pred?) (menu-expand-list items))))

(define (menu-expand-promise p)
  "Expand promised menu @p."
  (with value ((cadr p))
    ;;(menu-expand value)
    (if (match? value ':menu-item) (menu-expand value) p)))

(define (menu-expand-input p)
  "Expand input menu item @p."
  `(input ,(replace-procedures (cadr p))
          ,(caddr p)
	  ,(with r ((cadddr p))
	     (if (pair? r) (car r) (replace-procedures (cadddr p))))
	  ,(fifth p)))

(define (menu-expand-pick-color p)
  "Expand pick-color menu item @p."
  `(pick-color ,(replace-procedures (cadr p))
	       ,(caddr p)
	       ;; FIXME: add default proposals
	       ))

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
	((match? (car p) '(check :menu-wide-label :string? :%1))
	 (with a (cdar p)
	   (list (list 'check
		       (menu-expand (car a))
		       (cadr a)
		       ((caddr a)))
		 (replace-procedures (cadr p)))))
	((match? (car p) ':menu-wide-label)
	 (replace-procedures p))
	(else (menu-expand-list p))))

(define-table menu-expand-table
  (--- ,(lambda (p) `(--- ,@(menu-expand-list (cdr p)))))
  (| ,(lambda (p) `(| ,@(menu-expand-list (cdr p)))))
  (group ,replace-procedures)
  (glue ,replace-procedures)
  (color ,replace-procedures)
  (symbol ,replace-procedures)
  (input ,menu-expand-input)
  (pick-color ,menu-expand-pick-color)
  (link ,menu-expand-link p)
  (horizontal ,(lambda (p) `(horizontal ,@(menu-expand-list (cdr p)))))
  (vertical ,(lambda (p) `(vertical ,@(menu-expand-list (cdr p)))))
  (hlist ,(lambda (p) `(hlist ,@(menu-expand-list (cdr p)))))
  (vlist ,(lambda (p) `(vlist ,@(menu-expand-list (cdr p)))))
  (minibar ,(lambda (p) `(minibar ,@(menu-expand-list (cdr p)))))
  (-> ,replace-procedures)
  (=> ,replace-procedures)
  (tile ,replace-procedures)
  (if ,menu-expand-if)
  (when ,menu-expand-when)
  (mini ,menu-expand-mini)
  (promise ,menu-expand-promise))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-menu-main p style)
  "Transform the menu @p into a widget."
  (with l (make-menu-items p style #f)
    (cond ((null? l) (make-menu-empty))
	  ((and (list? l) (null? (cdr l))) (car l))
	  (else (make-menu-bad-format p style)))))

(tm-define (make-menu-widget p style)
  (:type (-> object widget))
  (:synopsis "Transform a menu into a widget.")
  (:argument p "a scheme object which represents the menu")
  (:argument style "menu style")
  ((wrap-catch make-menu-main) p style))
