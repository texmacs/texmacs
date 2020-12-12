
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-define.scm
;; DESCRIPTION : Definition of menus
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-define)
  (:use (kernel gui gui-markup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of dynamic menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (require-format x pattern)
  (if (not (match? x pattern))
    (texmacs-error "gui-make" "invalid menu item ~S" x)))

(define (gui-make-eval x)
  (require-format x '(eval :%1))
  (cadr x))

(define (gui-make-dynamic x)
  (require-format x '(dynamic :%1))
  `($dynamic ,(cadr x)))

(define (gui-make-former x)
  (require-format x '(former :*))
  `($dynamic ,x))

(define (gui-make-link x)
  (require-format x '(link :%1))
  `($menu-link ,(cadr x)))

(define (gui-make-let x)
  (require-format x '(:%1 :%1 :*))
  `(,(car x) ,(cadr x) (menu-dynamic ,@(cddr x))))

(define (gui-make-with x)
  (require-format x '(:%1 :%2 :*))
  `(,(car x) ,(cadr x) ,(caddr x) (menu-dynamic ,@(cdddr x))))

(define (gui-make-push-focus x)
  (require-format x '(push-focus :%1 :*))
  `(with pushed-tree ,(cadr x)
     (with pushed-focus (tree->fingerprint pushed-tree)
       (menu-dynamic
         (invisible (tree->path pushed-tree))
         ,@(cddr x)))))

(define (gui-make-cond x)
  (require-format x '(cond :*))
  (with fun (lambda (x)
              (with (pred? . body) x
                (list pred? (cons* 'menu-dynamic body))))
    `(cond ,@(map fun (cdr x)))))

(define (gui-make-loop x)
  (require-format x '(loop (:%1 :%1) :*))
  (with fun `(lambda (,(caadr x)) (menu-dynamic ,@(cddr x)))
    `($dynamic (append-map ,fun ,(cadadr x)))))

(define (gui-make-refresh x)
  (require-format x '(refresh :%1 :*))
  (with opts (cddr x)
    (when (and (null? opts) (symbol? (cadr x)))
      (set! opts (list (cadr x))))
    (when (not (symbol? (car opts)))
      (texmacs-error "gui-make-refresh" "invalid menu item ~S" x))
    `($refresh ,(cadr x) ,(symbol->string (car opts)))))

(define (gui-make-refreshable x)
  (require-format x '(refreshable :%1 :*))
  `($refreshable ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-group x)
  (require-format x '(group :%1))
  `($menu-group ,(cadr x)))

(define (gui-make-text x)
  (require-format x '(text :%1))
  `($menu-text ,(cadr x)))

(define (gui-make-invisible x)
  (require-format x '(invisible :%1))
  `($menu-invisible ,(cadr x)))

(define (gui-make-glue x)
  (require-format x '(glue :%4))
  `($glue ,(second x) ,(third x) ,(fourth x) ,(fifth x)))

(define (gui-make-color x)
  (require-format x '(color :%5))
  `($colored-glue ,(second x) ,(third x) ,(fourth x) ,(fifth x) ,(sixth x)))

(define (gui-make-texmacs-output x)
  (require-format x '(texmacs-output :%2))
  `($texmacs-output ,@(cdr x)))

(define (gui-make-texmacs-input x)
  (require-format x '(texmacs-input :%3))
  `($texmacs-input ,@(cdr x)))

(define (gui-make-input x)
  (require-format x '(input :%4))
  `($input ,@(cdr x)))

(define (gui-make-enum x)
  (require-format x '(enum :%4))
  `($enum ,@(cdr x)))

(define (gui-make-choice x)
  (require-format x '(choice :%3))
  `($choice ,@(cdr x)))

(define (gui-make-choices x)
  (require-format x '(choices :%3))
  `($choices ,@(cdr x)))

(define (gui-make-filtered-choice x)
  (require-format x '(filtered-choice :%4))
  `($filtered-choice ,@(cdr x)))

(define (gui-make-tree-view x)
  (require-format x '(tree-view :%3))
  `($tree-view ,@(cdr x)))

(define (gui-make-toggle x)
  (require-format x '(toggle :%2))
  `($toggle ,@(cdr x)))

(define (gui-make-icon x)
  (require-format x '(icon :%1))
  `($icon ,(cadr x)))

(define (gui-make-replace x)
  (require-format x '(replace :%1 :*))
  `($replace-text ,(cadr x) ,@(cddr x)))

(define (gui-make-concat x)
  (require-format x '(concat :*))
  `($concat-text ,@(cdr x)))

(define (gui-make-verbatim x)
  (require-format x '(verbatim :*))
  `($verbatim-text ,@(cdr x)))

(define (gui-make-check x)
  (require-format x '(check :%3))
  `($check ,(gui-make (cadr x)) ,(caddr x) ,(cadddr x)))

(define (gui-make-shortcut x)
  (require-format x '(shortcut :%2))
  `($shortcut* ,(gui-make (cadr x)) ,(caddr x)))

(define (gui-make-balloon x)
  (require-format x '(balloon :%2))
  `($balloon ,(gui-make (cadr x)) ,(gui-make (caddr x))))

(define (gui-make-submenu x)
  (require-format x '(-> :%1 :*))
  `($-> ,@(map gui-make (cdr x))))

(define (gui-make-top-submenu x)
  (require-format x '(=> :%1 :*))
  `($=> ,@(map gui-make (cdr x))))

(define (gui-make-horizontal x)
  (require-format x '(horizontal :*))
  `($horizontal ,@(map gui-make (cdr x))))

(define (gui-make-vertical x)
  (require-format x '(vertical :*))
  `($vertical ,@(map gui-make (cdr x))))

(define (gui-make-hlist x)
  (require-format x '(hlist :*))
  `($hlist ,@(map gui-make (cdr x))))

(define (gui-make-vlist x)
  (require-format x '(vlist :*))
  `($vlist ,@(map gui-make (cdr x))))

(define (gui-make-aligned x)
  (require-format x '(aligned :*))
  `($aligned ,@(map gui-make (cdr x))))

(define (gui-make-item x)
  (require-format x '(item :%2))
  `($aligned-item ,@(map gui-make (cdr x))))

(define (gui-make-meti x)
  (require-format x '(meti :%2))
  `($aligned-item ,@(map gui-make (reverse (cdr x)))))

(define (gui-make-tabs x)
  (require-format x '(tabs :*))
  `($tabs ,@(map gui-make (cdr x))))

(define (gui-make-tab x)
  (require-format x '(tab :%1 :*))
  `($tab ,@(map gui-make (cdr x))))

(define (gui-make-icon-tabs x)
  (require-format x '(icon-tabs :*))
  `($icon-tabs ,@(map gui-make (cdr x))))

(define (gui-make-icon-tab x)
  (require-format x '(icon-tab :%2 :*))
  `($icon-tab ,@(map gui-make (cdr x))))

(define (gui-make-inert x)
  (require-format x '(inert :*))
  `($widget-style ,widget-style-inert ,@(map gui-make (cdr x))))

(define (gui-make-explicit-buttons x)
  (require-format x '(explicit-buttons :*))
  `($widget-style ,widget-style-button ,@(map gui-make (cdr x))))

(define (gui-make-bold x)
  (require-format x '(bold :*))
  `($widget-style ,widget-style-bold ,@(map gui-make (cdr x))))

(define (gui-make-grey x)
  (require-format x '(grey :*))
  `($widget-style ,widget-style-grey ,@(map gui-make (cdr x))))

(define (gui-make-monospaced x)
  (require-format x '(mono :*))
  `($widget-style ,widget-style-monospaced ,@(map gui-make (cdr x))))

(define (gui-make-tile x)
  (require-format x '(tile :integer? :*))
  `($tile ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-scrollable x)
  (require-format x '(scrollable :*))
  `($scrollable ,@(map gui-make (cdr x))))

(define (gui-make-resize x)
  (require-format x '(resize :%2 :*))
  `($resize ,(cadr x) ,(caddr x) ,@(map gui-make (cdddr x))))

(define (gui-make-hsplit x)
  (require-format x '(hsplit :%2))
  `($hsplit ,@(map gui-make (cdr x))))

(define (gui-make-vsplit x)
  (require-format x '(vsplit :%2))
  `($vsplit ,@(map gui-make (cdr x))))

(define (gui-make-minibar x)
  (require-format x '(minibar :*))
  `(gui$minibar ,@(map gui-make (cdr x))))

(define (gui-make-extend x)
  (require-format x '(extend :%1 :*))
  `($widget-extend ,@(map gui-make (cdr x))))

(define (gui-make-padded x)
  (require-format x '(padded :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #f #f 25 0)
       ($vlist ,@(map gui-make (cdr x)))
       ($glue #f #f 25 0))
     ($glue #f #f 0 10)))

(define (gui-make-centered x)
  (require-format x '(centered :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #t #f 25 0)
       ($vlist ,@(map gui-make (cdr x)))
       ($glue #t #f 25 0))
     ($glue #f #f 0 10)))

(define (gui-make-bottom-buttons x)
  (require-format x '(bottom-buttons :*))
  `($vlist
     $---
     ($glue #f #f 0 5)
     ($hlist
       ($glue #f #f 5 0)
       ($widget-style ,widget-style-button
         ,@(map gui-make (cdr x)))
       ($glue #f #f 5 0))
     ($glue #f #f 0 5)))

(define (gui-make-assuming x)
  (require-format x '(assuming :%1 :*))
  `($when ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-if x)
  (require-format x '(if :%1 :*))
  `($delayed-when ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-when x)
  (require-format x '(when :%1 :*))
  `($assuming ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-for x)
  (require-format x '(for (:%1 :%1) :*))
  `($for* ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-mini x)
  (require-format x '(mini :%1 :*))
  `($mini ,(cadr x) ,@(map gui-make (cddr x))))

(define (gui-make-symbol x)
  (require-format x '(symbol :string? :*))
  `($symbol ,@(cdr x)))

(define (gui-make-promise x)
  (require-format x '(promise :%1))
  `($promise ,(cadr x)))

(define (gui-make-ink x)
  (require-format x '(ink :%1))
  `($ink ,(cadr x)))

(define (gui-make-form x)
  (require-format x '(form :%1 :*))
  `($form ,@(map gui-make (cdr x))))

(define (gui-make-form-input x)
  (require-format x '(form-input :%4))
  `($form-input ,@(cdr x)))

(define (gui-make-form-enum x)
  (require-format x '(form-enum :%4))
  `($form-enum ,@(cdr x)))

(define (gui-make-form-choice x)
  (require-format x '(form-choice :%3))
  `($form-choice ,@(cdr x)))

(define (gui-make-form-choices x)
  (require-format x '(form-choices :%3))
  `($form-choices ,@(cdr x)))

(define (gui-make-form-toggle x)
  (require-format x '(form-toggle :%2))
  `($form-toggle ,@(cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table with Gui primitives and dispatching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-table gui-make-table
  (eval ,gui-make-eval)
  (dynamic ,gui-make-dynamic)
  (former ,gui-make-former)
  (link ,gui-make-link)
  (let ,gui-make-let)
  (let* ,gui-make-let)
  (with ,gui-make-with)
  (push-focus ,gui-make-push-focus)
  (receive ,gui-make-with)
  (cond ,gui-make-cond)
  (loop ,gui-make-loop)
  (refresh ,gui-make-refresh)
  (refreshable ,gui-make-refreshable)
  (group ,gui-make-group)
  (text ,gui-make-text)
  (invisible ,gui-make-invisible)
  (glue ,gui-make-glue)
  (color ,gui-make-color)
  (texmacs-output ,gui-make-texmacs-output)
  (texmacs-input ,gui-make-texmacs-input)
  (input ,gui-make-input)
  (enum ,gui-make-enum)
  (choice ,gui-make-choice)
  (choices ,gui-make-choices)
  (tree-view ,gui-make-tree-view)
  (filtered-choice ,gui-make-filtered-choice)
  (toggle ,gui-make-toggle)
  (icon ,gui-make-icon)
  (replace ,gui-make-replace)
  (concat ,gui-make-concat)
  (verbatim ,gui-make-verbatim)
  (check ,gui-make-check)
  (shortcut ,gui-make-shortcut)
  (balloon ,gui-make-balloon)
  (-> ,gui-make-submenu)
  (=> ,gui-make-top-submenu)
  (horizontal ,gui-make-horizontal)
  (vertical ,gui-make-vertical)
  (hlist ,gui-make-hlist)
  (vlist ,gui-make-vlist)
  (aligned ,gui-make-aligned)
  (item ,gui-make-item)
  (meti ,gui-make-meti)
  (tabs ,gui-make-tabs)
  (tab ,gui-make-tab)
  (icon-tabs ,gui-make-icon-tabs)
  (icon-tab ,gui-make-icon-tab)
  (inert ,gui-make-inert)
  (explicit-buttons ,gui-make-explicit-buttons)
  (bold ,gui-make-bold)
  (grey ,gui-make-grey)
  (mono ,gui-make-monospaced)
  (tile ,gui-make-tile)
  (scrollable ,gui-make-scrollable)
  (resize ,gui-make-resize)
  (hsplit ,gui-make-hsplit)
  (vsplit ,gui-make-vsplit)
  (minibar ,gui-make-minibar)
  (extend ,gui-make-extend)
  (padded ,gui-make-padded)
  (centered ,gui-make-centered)
  (bottom-buttons ,gui-make-bottom-buttons)
  (assuming ,gui-make-assuming)
  (if ,gui-make-if)
  (when ,gui-make-when)
  (for ,gui-make-for)
  (mini ,gui-make-mini)
  (symbol ,gui-make-symbol)
  (promise ,gui-make-promise)
  (ink ,gui-make-ink)
  (form ,gui-make-form)
  (form-input ,gui-make-form-input)
  (form-enum ,gui-make-form-enum)
  (form-choice ,gui-make-form-choice)
  (form-choices ,gui-make-form-choices)
  (form-toggle ,gui-make-form-toggle))

(tm-define (gui-make x)
  ;;(display* "x= " x "\n")
  (cond ((symbol? x)
         (cond ((== x '---) '$---)
               ((== x '===) (gui-make '(glue #f #f 0 5)))
               ((== x '======) (gui-make '(glue #f #f 0 15)))
               ((== x '/) '$/)
               ((== x '//) (gui-make '(glue #f #f 5 0)))
               ((== x '///) (gui-make '(glue #f #f 15 0)))
               ((== x '>>) (gui-make '(glue #t #f 5 0)))
               ((== x '>>>) (gui-make '(glue #t #f 15 0)))
               ((== x (string->symbol "|")) '$/)
               (else
                 (texmacs-error "gui-make" "invalid menu item ~S" x))))
        ((string? x) x)
        ((and (pair? x) (ahash-ref gui-make-table (car x)))
         (apply (car (ahash-ref gui-make-table (car x))) (list x)))
        ((and (pair? x) (or (string? (car x)) (pair? (car x))))
         `($> ,(gui-make (car x)) ,@(cdr x)))
        (else
          (texmacs-error "gui-make" "invalid menu item ~S" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for dynamic menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (menu-dynamic . l)
  `($list ,@(map gui-make l)))

(tm-define-macro (define-menu head . body)
  `(define ,head (menu-dynamic ,@body)))

(tm-define-macro (define-widget head . body)
  `(define ,head (menu-dynamic ,@body)))

(tm-define-macro (tm-menu head . l)
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define ,head ,@opts (menu-dynamic ,@body))))

(tm-define-macro (tm-widget head . l)
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define ,head ,@opts (menu-dynamic ,@body))))

(tm-define-macro (menu-bind name . l)
  ;;(display* name " --> " l "\n")
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define (,name) ,@opts (menu-dynamic ,@body))))

(define-public-macro (lazy-menu module . menus)
  `(begin
     (lazy-define ,module ,@menus)
     (delayed
       (:idle 500)
       (module-provide ',module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic color pickers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (standard-color-list)
  '("dark red" "dark magenta" "dark blue" "dark cyan"
    "dark green" "dark yellow" "dark orange" "dark brown"
    "red" "magenta" "blue" "cyan"
    "green" "yellow" "orange" "brown"
    "#faa" "#faf" "#aaf" "#aff"
    "#afa" "#ffa" "#fa6" "#a66"
    "pastel red" "pastel magenta" "pastel blue" "pastel cyan"
    "pastel green" "pastel yellow" "pastel orange" "pastel brown"))

(define (standard-grey-list)
  '("black" "darker grey" "dark grey" "#a0a0a0"
    "light grey" "pastel grey" "#f0f0f0" "white"))

(tm-menu (standard-color-menu cmd)
  (tile 8
    (for (col (append (standard-color-list) (standard-grey-list)))
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col))))))

(define (gui-make-pick-color x)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cdr x))))))

(extend-table gui-make-table
  (pick-color ,gui-make-pick-color))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic pattern picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (get-preferred-list type nr)
  (with l (get-preference type)
    (when (string? l) (set! l (string->object l)))
    (cond ((nlist? l) (list))
          ((> (length l) nr) (sublist l 0 nr))
          (else l))))

(define-public (insert-preferred-list type what nr)
  (let* ((l (get-preferred-list type nr))
         (i (list-find-index l (cut == <> what)))
         (r l))
    (if i (set! r (append (sublist r 0 i)
                          (sublist r (+ i 1) (length r)))))
    (set! r (cons what r))
    (when (> (length r) nr)
      (set! r (sublist r 0 nr)))
    (when (!= r l)
      (set-preference type r))))

(define-public (tm-pattern name . args)
  (cond ((url-exists? (url-append "$TEXMACS_PATTERN_PATH" (url-tail name)))
         `(pattern ,(url->unix (url-tail name)) ,@args))
        ((string-starts? (url->unix (url->delta-unix name)) "../")
         (when (url? name) (set! name (url->system name)))
         `(pattern ,name ,@args))
        (else
         `(pattern ,(url->unix (url->delta-unix name)) ,@args))))

(tm-menu (my-pattern-menu cmd)
  (tile 8
    (for (col (get-preferred-list "my patterns" 32))
      (with args (cons* (cadr col) "100%" "100@" (cddddr col))
        (with col2 (apply tm-pattern args)
          (explicit-buttons
            ((color col2 #f #f 32 32)
             (cmd col))))))))

(define (standard-pattern-list dir scale)
  (let* ((l1 (url-read-directory dir "*.png"))
         (l2 (url-read-directory dir "*.jpg"))
         (l3 (url-read-directory dir "*.gif"))
         (l (append l1 l2 l3))
         (d (map (cut url-delta (string-append dir "/x") <>) l))
         (f (map (lambda (x) (string-append dir "/" (url->unix x))) d)))
    (map (lambda (x) (tm-pattern x scale "")) f)))

(tm-menu (standard-pattern-menu cmd dir scale)
  (tile 8
    (for (col (standard-pattern-list dir scale))
      (with col2 (tm-pattern (cadr col) "100%" "100@")
        (explicit-buttons
          ((color col2 #f #f 32 32)
           (cmd col)))))))

(tm-menu (big-pattern-menu cmd dir scale)
  (tile 6
    (for (col (standard-pattern-list dir scale))
      (with col2 (tm-pattern (cadr col) "100%" "100@")
        (explicit-buttons
          ((color col2 #f #f 90 90)
           (cmd col)))))))

(define-public (clipart-list)
  (list-filter
   (list (list "Dot hatches" "$TEXMACS_PATH/misc/patterns/dots-hatches")
         (list "Line hatches" "$TEXMACS_PATH/misc/patterns/lines-default")
         (list "Artistic hatches" "$TEXMACS_PATH/misc/patterns/lines-artistic")
         (list "Textile" "$TEXMACS_PATH/misc/patterns/textile")
         (list "Hatch" "/opt/local/share/openclipart/special/patterns")
         (list "Personal" "~/patterns")
         (list "Simple" "~/simple-tiles"))
   (lambda (p) (url-exists? (cadr p)))))

(tm-menu (clipart-pattern-menu cmd scale)
  (for (p (clipart-list))
    (-> (eval (car p))
        (dynamic (big-pattern-menu cmd (cadr p) scale)))))

(define (gui-make-pick-background x)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cddr x))))
     ---
     (dynamic (standard-pattern-menu (lambda (answer) ,@(cddr x))
                                     "$TEXMACS_PATH/misc/patterns/vintage"
                                     ,(cadr x)))
     (when (nnull? (get-preferred-list "my patterns" 32))
       ---
       (dynamic (my-pattern-menu (lambda (answer) ,@(cddr x)))))
     ;;(assuming (nnull? (clipart-list))
     ;;  ---
     ;;  (dynamic (clipart-pattern-menu (lambda (answer) ,@(cddr x))
     ;;                                 ,(cadr x))))
     ))

(extend-table gui-make-table
  (pick-background ,gui-make-pick-background))

(tm-define (allow-pattern-colors?)
  (qt-gui?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra RGB color picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rgb-color-name r g b)
  (string-append "#"
    (integer->padded-hexadecimal r 2)
    (integer->padded-hexadecimal g 2)
    (integer->padded-hexadecimal b 2)))

(tm-menu (rgb-palette cmd r1 r2 g1 g2 b1 b2 n)
  (for (rr (.. r1 r2))
    (for (gg (.. g1 g2))
      (for (bb (.. b1 b2))
        (let* ((r (/ (* 255 rr) (- n 1)))
               (g (/ (* 255 gg) (- n 1)))
               (b (/ (* 255 bb) (- n 1)))
               (col (rgb-color-name r g b)))
          (explicit-buttons
            ((color col #f #f 24 24)
             (cmd col))))))))

(tm-menu (rgb-color-picker cmd)
  (tile 18
    (dynamic (rgb-palette cmd 0 6 0 3 0 6 6)))
  (tile 18
    (dynamic (rgb-palette cmd 0 6 3 6 0 6 6)))
  ---
  (glue #f #f 0 3)
  (hlist
    (glue #t #f 0 17)
    (explicit-buttons
      ("Cancel" (cmd #f)))
    (glue #f #f 3 0))
  (glue #f #f 0 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deprecated functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (menu-extend name . l)
  (deprecated-function "menu-extend" "tm-menu" "former")
  (receive (opts body) (list-break l not-define-option?)
    `(tm-define (,name) ,@opts (menu-dynamic (former) ,@body))))
