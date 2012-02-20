
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
    (extend :menu-label :*)
    (style :integer? :menu-label)
    (balloon :menu-label :string?)))
  (:menu-wide-label (:or
    :menu-label
    (check :menu-wide-label :string? :%1)
    (shortcut :menu-wide-label :string?)))
  (:menu-item (:or
    ---
    |
    (group :string?)
    (text :string?)
    (glue :boolean? :boolean? :integer? :integer?)
    (color :%1 :boolean? :boolean? :integer? :integer?)
    (:menu-wide-label :%1)
    (symbol :string? :*)
    (texmacs-output :%1)
    (texmacs-input :%3)
    (input :%1 :string? :%1 :string?)
    (enum :%3 :string?)
    (choice :%3)
    (choices :%3)
    (toggle :%2)
    (horizontal :menu-item-list)
    (vertical :menu-item-list)
    (hlist :menu-item-list)
    (vlist :menu-item-list)
    (aligned :menu-item-list)
    (aligned-item :%2)
    (tabs :menu-item-list)
    (tab :menu-item-list)
    (minibar :menu-item-list)
    (extend :menu-item :menu-item-list)
    (style :integer? :menu-item-list)
    (-> :menu-label :menu-item-list)
    (=> :menu-label :menu-item-list)
    (tile :integer? :menu-item-list)
    (scrollable :menu-item-list)
    (resize :%2 :menu-item-list)
    (hsplit :menu-item :menu-item)
    (vsplit :menu-item :menu-item)
    (if :%1 :menu-item-list)
    (when :%1 :menu-item-list)
    (mini :%1 :menu-item-list)
    (link :%1)
    (promise :%1)
    (ink :%1)
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

(define (menu-protect cmd)
  (lambda x
    (exec-delayed
      (lambda ()
        (menu-before-action)
        (apply cmd x)
        (menu-after-action)))))

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

(define (greyed? style)
  (!= (logand style widget-style-grey) 0))

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
	(col (color (if (greyed? style) "dark grey" "black"))))
    (cond ((translatable? p)		; "text"
	   (widget-text (translate p) style col #t))
  	  ((tuple? p 'balloon 2)        ; (balloon <label> "balloon text")
  	   (make-menu-label (cadr p) style tt?))
  	  ((tuple? p 'extend)		; (extend <label> . ws)
           (with l (make-menu-items (cddr p) style tt?)
             (widget-extend (make-menu-label (cadr p) style tt?) l)))
  	  ((tuple? p 'style 2)		; (style st <label>)
           (let* ((x (cadr p))
                  (new-style (if (> x 0) (logior style x)
                                 (logand style (lognot (- x))))))
             (make-menu-label (caddr p) new-style tt?)))
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

(define (make-menu-text s style)
  "Make @(text :string?) menu item."
  ;;(widget-text s style (color "black") #t)
  (widget-text s style (color "black") #f))

(define (make-texmacs-output p style)
  "Make @(texmacs-output :%1) item."
  (with (tag t) p
    (widget-texmacs-output (t))))

(define (make-texmacs-input p style)
  "Make @(texmacs-input :%3) item."
  (with (tag t cmd cont?) p
    (widget-texmacs-input (t) (object->command (menu-protect cmd)) cont?)))

(define (make-menu-input p style)
  "Make @(input :%1 :string? :%1 :string?) menu item."
  (with (tag cmd type props width) p
    (widget-input (object->command (menu-protect cmd)) type (props)
		  (logior style widget-style-mini) width)))

(define (make-enum p style)
  "Make @(enum :%3 :string?) item."
  (with (tag cmd vals val width) p
    (widget-enum (object->command (menu-protect cmd))
                 (vals) (val) style width)))

(define (make-choice p style)
  "Make @(choice :%3) item."
  (with (tag cmd vals val) p
    (widget-choice (object->command (menu-protect cmd)) (vals) (val))))

(define (make-choices p style)
  "Make @(choices :%3) item."
  (with (tag cmd vals mc) p
    (widget-choices (object->command (menu-protect cmd)) (vals) (mc))))

(define (make-toggle p style)
  "Make @(toggle :%2) item."
  (with (tag cmd on) p
    (widget-toggle (object->command cmd) (on) style)))

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
  (with col (color (if (greyed? style) "dark grey" "black"))
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
  "Make @(vlist :menu-item-list) menu item."
  (widget-vlist (make-menu-items (cdr p) style #f)))

(define (make-aligned p style)
  "Make @(aligned :menu-item-list) item."
  (widget-aligned (make-menu-items (map cadr (cdr p)) style #f)
                  (make-menu-items (map caddr (cdr p)) style #f)))

(define (make-aligned-item p style)
  "Make @(aligned-item :2%) item."
  (display* "Error 'make-aligned-item', " p ", " style "\n")
  (list 'vlist))

(define (tab-key x)
  (cadr x))

(define (tab-value x)
  (list 'vlist (cddr x)))

(define (make-menu-tabs p style)
  "Make @(tabs :menu-item-list) menu item."
  (widget-tabs (make-menu-items (map tab-key (cdr p)) style #f)
               (make-menu-items (map tab-value (cdr p)) style #f)))

(define (make-menu-tab p style)
  "Make @(tab :menu-item-list) menu item."
  (display* "Error 'make-menu-tab', " p ", " style "\n")
  (list 'vlist))

(define (make-menu-extend p style bar?)
  "Make @(extend :menu-item :menu-item-list) menu item."
  (with l (make-menu-items (cdr p) style bar?)
    (widget-extend (car l) (cdr l))))

(define (make-menu-style p style bar?)
  "Make @(extend :integer? :menu-item-list) menu item."
  (let* ((x (cadr p))
         (new-style (if (> x 0) (logior style x)
                        (logand style (lognot (- x))))))
    (make-menu-items-list (cddr p) new-style bar?)))

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

(define (make-scrollable p style)
  "Make @(scrollable :menu-item-list) item."
  (with (tag . items) p
    (with inner (make-menu-items (list (cons 'vertical items)) style #f)
      (widget-scrollable (car inner) style))))

(define (decode-resize x)
  (cond ((string? x) (list x x x))
        ((list-3? x) x)
        (else (make-menu-error "bad length in " (object->string x)))))

(define (make-resize p style)
  "Make @(resize :%2 :menu-item-list) item."
  (with (tag w h . items) p
    (with inner (make-menu-items (list (cons 'vertical items)) style #f)
      (with (w1 w2 w3) (decode-resize w)
        (with (h1 h2 h3) (decode-resize h)
          (widget-resize (car inner) style w1 h1 w2 h2 w3 h3))))))

(define (make-hsplit p style)
  "Make @(hsplit :menu-item :menu-item) item."
  (with (tag . items) p
    (with l (make-menu-items items style #f)
      (widget-hsplit (car l) (cadr l)))))

(define (make-vsplit p style)
  "Make @(vsplit :menu-item :menu-item) item."
  (with (tag . items) p
    (with l (make-menu-items items style #f)
      (widget-vsplit (car l) (cadr l)))))

(define (make-ink p style)
  "Make @(ink) item."
  (with (tag cmd) p
    (widget-ink (object->command cmd))))

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
	   (new-style (logior style
                              (if new-active? 0
                                  (+ widget-style-inert widget-style-grey)))))
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

(define (make-refresh p style bar?)
  "Make @(refresh s) widget."
  (with (tag s) p
    (widget-refresh s)))

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
  (text (:string?)
	 ,(lambda (p style bar?) (list (make-menu-text (cadr p) style))))
  (symbol (:string? :*)
	  ,(lambda (p style bar?) (list (make-menu-symbol p style))))
  (texmacs-output (:%1)
    ,(lambda (p style bar?) (list (make-texmacs-output p style))))
  (texmacs-input (:%3)
    ,(lambda (p style bar?) (list (make-texmacs-input p style))))
  (input (:%1 :string? :%1 :string?)
         ,(lambda (p style bar?) (list (make-menu-input p style))))
  (enum (:%3 :string?)
        ,(lambda (p style bar?) (list (make-enum p style))))
  (choice (:%3)
          ,(lambda (p style bar?) (list (make-choice p style))))
  (choices (:%3)
           ,(lambda (p style bar?) (list (make-choices p style))))
  (toggle (:%2)
	  ,(lambda (p style bar?) (list (make-toggle p style))))
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
  (aligned (:*)
         ,(lambda (p style bar?) (list (make-aligned p style))))
  (aligned-item (:%2)
                ,(lambda (p style bar?) (list (make-aligned-item p style))))
  (tabs (:*)
        ,(lambda (p style bar?) (list (make-menu-tabs p style))))
  (tab (:*)
        ,(lambda (p style bar?) (list (make-menu-tab p style))))
  (minibar (:*)
	    ,(lambda (p style bar?) (list (make-menu-minibar p style))))
  (extend (:%1 :*)
	  ,(lambda (p style bar?) (list (make-menu-extend p style bar?))))
  (style (:%1 :*)
         ,(lambda (p style bar?) (make-menu-style p style bar?)))
  (-> (:%1 :*)
      ,(lambda (p style bar?) (list (make-menu-submenu p style))))
  (=> (:%1 :*)
      ,(lambda (p style bar?) (list (make-menu-submenu p style))))
  (tile (:integer? :*)
	,(lambda (p style bar?) (list (make-menu-tile p style))))
  (scrollable (:*)
	      ,(lambda (p style bar?) (list (make-scrollable p style))))
  (resize (:%2 :*)
      ,(lambda (p style bar?) (list (make-resize p style))))
  (hsplit (:%2)
          ,(lambda (p style bar?) (list (make-hsplit p style))))
  (vsplit (:%2)
          ,(lambda (p style bar?) (list (make-vsplit p style))))
  (ink (:%1)
       ,(lambda (p style bar?) (list (make-ink p style))))
  (if (:%1 :*)
      ,(lambda (p style bar?) (make-menu-if p style bar?)))
  (when (:%1 :*)
        ,(lambda (p style bar?) (make-menu-when p style bar?)))
  (mini (:%1 :*)
        ,(lambda (p style bar?) (make-menu-mini p style bar?)))
  (promise (:%1)
	   ,(lambda (p style bar?) (make-menu-promise p style bar?)))
  (refresh (:%1)
	   ,(lambda (p style bar?) (make-refresh p style bar?))))

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

(define (menu-expand-texmacs-input p)
  "Expand texmacs-input item @p."
  `(texmacs-input ,(replace-procedures (cadr p))
                  ,(replace-procedures (caddr p))
                  ,(cadddr p)))

(define (menu-expand-input p)
  "Expand input menu item @p."
  `(input ,(replace-procedures (cadr p))
          ,(caddr p)
	  ,(with r ((cadddr p))
	     (if (pair? r) (car r) (replace-procedures (cadddr p))))
	  ,(fifth p)))

(define (menu-expand-enum p)
  "Expand enum item @p."
  `(enum ,(replace-procedures (cadr p))
         ,(replace-procedures (caddr p))
         ,(replace-procedures (cadddr p))
         ,(fifth p)))

(define (menu-expand-choice p)
  "Expand choice item @p."
  `(,(car p) ,(replace-procedures (cadr p))
             ,(replace-procedures (caddr p))
             ,(replace-procedures (cadddr p))))

(define (menu-expand-toggle p)
  "Expand toggle item @p."
  `(toggle ,(replace-procedures (cadr p))
	   ,(replace-procedures (caddr p))))

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
  ;;(display* "Expand " p "\n")
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
  (text ,replace-procedures)
  (glue ,replace-procedures)
  (color ,replace-procedures)
  (symbol ,replace-procedures)
  (texmacs-output ,replace-procedures)
  (texmacs-input ,menu-expand-texmacs-input)
  (input ,menu-expand-input)
  (enum ,menu-expand-enum)
  (choice ,menu-expand-choice)
  (choices ,menu-expand-choice)
  (toggle ,menu-expand-toggle)
  (link ,menu-expand-link p)
  (horizontal ,(lambda (p) `(horizontal ,@(menu-expand-list (cdr p)))))
  (vertical ,(lambda (p) `(vertical ,@(menu-expand-list (cdr p)))))
  (hlist ,(lambda (p) `(hlist ,@(menu-expand-list (cdr p)))))
  (vlist ,(lambda (p) `(vlist ,@(menu-expand-list (cdr p)))))
  (aligned ,(lambda (p) `(aligned ,@(menu-expand-list (cdr p)))))
  (aligned-item ,(lambda (p) `(aligned-item ,@(menu-expand-list (cdr p)))))
  (tabs ,(lambda (p) `(tabs ,@(menu-expand-list (cdr p)))))
  (tab ,(lambda (p) `(tab ,@(menu-expand-list (cdr p)))))
  (minibar ,(lambda (p) `(minibar ,@(menu-expand-list (cdr p)))))
  (extend ,(lambda (p) `(extend ,@(menu-expand-list (cdr p)))))
  (style ,(lambda (p) `(extend ,@(menu-expand-list (cdr p)))))
  (-> ,replace-procedures)
  (=> ,replace-procedures)
  (tile ,replace-procedures)
  (scrollable ,(lambda (p) `(scrollable ,@(menu-expand-list (cdr p)))))
  (resize ,(lambda (p) `(resize ,@(menu-expand-list (cdr p)))))
  (hsplit ,(lambda (p) `(hsplit ,@(menu-expand-list (cdr p)))))
  (vsplit ,(lambda (p) `(vsplit ,@(menu-expand-list (cdr p)))))
  (ink ,replace-procedures)
  (if ,menu-expand-if)
  (when ,menu-expand-when)
  (mini ,menu-expand-mini)
  (promise ,menu-expand-promise)
  (refresh ,replace-procedures))

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
  (:synopsis "Transform a menu into a widget.")
  (:argument p "a scheme object which represents the menu")
  (:argument style "menu style")
  ((wrap-catch make-menu-main) p style))

(tm-define (top-window menu-promise name)
  (:interactive #t)
  (let* ((win (window-handle))
	 (qui (object->command (lambda () (window-delete win))))
	 (men (menu-promise))
	 (scm (list 'vertical men))
	 (wid (make-menu-widget scm 0)))
    (window-create-quit win wid name qui)
    (window-show win)))

(tm-define (dialogue-window menu-promise cmd name)
  (:interactive #t)
  (let* ((win (window-handle))
	 (qui (object->command (lambda () (window-delete win))))
	 (lbd (lambda x (apply cmd x) (window-delete win)))
	 (men (menu-promise lbd))
	 (scm (list 'vertical men))
	 (wid (make-menu-widget scm 0)))
    (window-create-quit win wid name qui)
    (window-show win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other top-level windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (interactive-window wid-promise cmd name)
  (:interactive #t)
  (let* ((win (window-handle))
	 (lbd (lambda x (apply cmd x) (window-delete win)))
	 (com (object->command (menu-protect lbd)))
	 (wid (wid-promise com)))
    (window-create win wid name #t)
    (window-show win)))

(tm-define (interactive-print done u)
  (:interactive #t)
  (with p (lambda (com) (widget-printer com u))
    (interactive-window p done "Print document")))

(tm-define (interactive-rgb-picker cmd l)
  (:interactive #t)
  (with cmd* (lambda (col) (when col (cmd col)))
    (dialogue-window rgb-color-picker cmd* "Choose color")))

(tm-define (interactive-color cmd proposals)
  (:interactive #t)
  (if (not (qt-gui?))
      (interactive-rgb-picker cmd proposals)
      (with p (lambda (com) (widget-color-picker com #f proposals))
        (with cmd* (lambda (t) (when t (cmd (tm->stree t))))
          (interactive-window p cmd* "Choose color")))))

(tm-define (interactive-background cmd proposals)
  (:interactive #t)
  (if (not (qt-gui?))
      (interactive-rgb-picker cmd proposals)
      ;;(with p (lambda (com) (widget-color-picker com #t proposals))
      (with p (lambda (com) (widget-color-picker com #f proposals))
        (with cmd* (lambda (t) (when t (cmd (tm->stree t))))
          (interactive-window p cmd* "Choose background")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some test widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (widget1)
  (centered
    (aligned
      (item (text "First:")
        (toggle (display* "First " answer "\n") #f))
      (item (text "Second:")
        (toggle (display* "Second " answer "\n") #f)))))

(tm-widget (widget2)
  (tabs
    (tab (text "General")
      (centered
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f)))))
    (tab (text "Extra")
      (centered
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f))))
      (bottom-buttons
        ("Cancel" (display "Cancel\n")) >> ("Ok" (display "Ok\n"))))
    (tab (text "Settings")
      (centered
        (aligned
          (item (text "First:")
            (enum (display* "First " answer "\n")
                  '("gnu" "gnat" "zebra")
                  "zebra" "10em"))
          (item (text "Second:")
            (enum (display* "Second " answer "\n")
                  '("fun" "foo" "bar")
                  "fun" "10em"))))
      (bottom-buttons
        >> ("Ok" (display "Ok\n"))))))

(tm-widget (widget3)
  (centered
    (resize "200px" "100px"
      (scrollable
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f))
          (item (text "Third:")
            (toggle (display* "Third " answer "\n") #f))
          (item (text "Fourth:")
            (toggle (display* "Fourth " answer "\n") #f))
          (item (text "Fifth:")
            (toggle (display* "Fifth " answer "\n") #f))
          (item (text "Sixth:")
            (toggle (display* "Sixth " answer "\n") #f))
          (item (text "Seventh:")
            (toggle (display* "Seventh " answer "\n") #f))
          (item (text "Eighth:")
            (toggle (display* "Eighth " answer "\n") #f)))))))

(tm-widget (widget4)
  (centered
    (resize "200px" "50px"
      (scrollable
        (choice (display* answer "\n")
                '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                "Third")))
    ======
    (resize "200px" "150px"
      (choices (display* answer "\n")
               '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
               '("Third" "Fifth")))))

(tm-widget (widget5)
  ===
  (hlist
    //
    (hsplit
      (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
        (scrollable
          (choice (display* answer "\n")
                  '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                  "Third")))
      (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
        (scrollable
          (choices (display* answer "\n")
                   '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                   '("Third" "Fifth")))))
    //)
  ===)

(tm-widget (widget6)
  (centered
    (resize "500px" "50px"
      (texmacs-output
        '(document (theorem (document "This is true.")))))
    ======
    (resize "500px" "300px"
      (texmacs-input
        '(with "bg-color" "#fcfcf8"
           (document (proof (document "Trivial."
                                      "But you may add more details."))))
        (noop) #f))))

(tm-widget (widget7)
  (padded
    ;;(ink (display* answer "\n"))))
    (ink (noop))))

(tm-define (show w)
  (top-window w "Simple widget"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Learning characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define last-glyph '())
(tm-define glyph-table (make-ahash-table))

(define glyphs-loaded? #f)

(define (load-glyphs)
  (when (not glyphs-loaded?)
    (with l (load-object "~/.TeXmacs/system/glyphs.scm")
      (set! glyph-table (list->ahash-table l))
      (for (x l)
        (let* ((key (car x))
               (im (cdr x)))
          (for (y im)
            (glyph-register key y)))))))

(define (save-glyphs)
  (save-object "~/.TeXmacs/system/glyphs.scm"
               (ahash-table->list glyph-table)))

(tm-define (learn-glyph name)
  (load-glyphs)
  (when (and (list? last-glyph) (nnull? last-glyph))
    (glyph-register name last-glyph)
    (with old (or (ahash-ref glyph-table name) '())
      (ahash-set! glyph-table name (cons last-glyph old))
      (save-glyphs))))

(tm-define (recognize-glyph)
  (load-glyphs)
  (when (and (list? last-glyph) (nnull? last-glyph))
    (display* "Recognized as " (glyph-recognize last-glyph) "\n")))

(tm-widget (glyph-widget)
  (padded
    (ink (set! last-glyph answer)))
  ---
  (hlist
    >>>
    (=> "Learn"
        (tile 8
          ("a" (learn-glyph "a"))
          ("b" (learn-glyph "b"))
          ("c" (learn-glyph "c"))
          ("d" (learn-glyph "d"))
          ("e" (learn-glyph "e"))
          ("f" (learn-glyph "f"))
          ("g" (learn-glyph "g"))
          ("h" (learn-glyph "h"))
          ("i" (learn-glyph "i"))
          ("j" (learn-glyph "j"))
          ("k" (learn-glyph "k"))
          ("l" (learn-glyph "l"))
          ("m" (learn-glyph "m"))
          ("n" (learn-glyph "n"))
          ("o" (learn-glyph "o"))
          ("p" (learn-glyph "p"))
          ("q" (learn-glyph "q"))
          ("r" (learn-glyph "r"))
          ("s" (learn-glyph "s"))
          ("t" (learn-glyph "t"))
          ("u" (learn-glyph "u"))
          ("v" (learn-glyph "v"))
          ("w" (learn-glyph "w"))
          ("x" (learn-glyph "x"))
          ("y" (learn-glyph "y"))
          ("z" (learn-glyph "z"))))
    ("Recognize" (recognize-glyph))
    >>>))

(tm-define (learn-glyphs)
  (show glyph-widget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some test forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (form1 cmd)
  (form "Test"
    (centered
      (aligned
        (item (text "First:")
          (form-input "First" "string" '("gnu") "1w"))
        (item (text "Second:")
          (form-input "Second" "string" '("gnat") "1w"))))
    (bottom-buttons
      ("Cancel" (cmd "Cancel")) >>
      ("Ok"
       (display* (form-fields) " -> " (form-values) "\n")
       (cmd "Ok")))))

(tm-widget (form2 cmd)
  (centered
    (aligned
      (item (text "First:")
        (toggle (display* "First " answer "\n") #f))
      (item (text "Second:")
        (toggle (display* "Second " answer "\n") #f))))
  (bottom-buttons >> ("Ok" (cmd "Ok"))))

(tm-define (show-form w)
  (dialogue-window w (lambda (x) (display* x "\n")) "Simple form"))
