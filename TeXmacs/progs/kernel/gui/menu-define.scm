
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
      (texmacs-error "gui-menu-item" "invalid menu item ~S" x)))

(tm-define (gui-menu-item x)
  (:case eval)
  (require-format x '(eval :%1))
  (cadr x))

(tm-define (gui-menu-item x)
  (:case dynamic)
  (require-format x '(dynamic :%1))
  `($dynamic ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case link)
  (require-format x '(link :%1))
  `($menu-link ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case let let*)
  (require-format x '(:%1 :%1 :*))
  `(,(car x) ,(cadr x) (menu-dynamic ,@(cddr x))))

(tm-define (gui-menu-item x)
  (:case with receive)
  (require-format x '(:%1 :%2 :*))
  `(,(car x) ,(cadr x) ,(caddr x) (menu-dynamic ,@(cdddr x))))

(tm-define (gui-menu-item x)
  (:case for)
  (require-format x '(for (:%1 :%1) :*))
  (with fun `(lambda (,(caadr x)) (menu-dynamic ,@(cddr x)))
    `($dynamic (append-map ,fun ,(cadadr x)))))

(tm-define (gui-menu-item x)
  (:case cond)
  (require-format x '(cond :*))
  (with fun (lambda (x)
              (with (pred? . body) x
                (list pred? (cons* 'menu-dynamic body))))
    `(cond ,@(map fun (cdr x)))))

(tm-define (gui-menu-item x)
  (:case group)
  (require-format x '(group :%1))
  `($menu-group ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case text)
  (require-format x '(text :%1))
  `($menu-text ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case glue)
  (require-format x '(glue :%4))
  `($glue ,(second x) ,(third x) ,(fourth x) ,(fifth x)))

(tm-define (gui-menu-item x)
  (:case color)
  (require-format x '(color :%5))
  `($colored-glue ,(second x) ,(third x) ,(fourth x) ,(fifth x) ,(sixth x)))

(tm-define (gui-menu-item x)
  (:case input)
  (require-format x '(input :%4))
  `($input ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case enum)
  (require-format x '(enum :%4))
  `($enum ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case toggle)
  (require-format x '(toggle :%2))
  `($toggle ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case icon)
  (require-format x '(icon :%1))
  `($icon ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case concat)
  (require-format x '(concat :*))
  `($concat-text ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case verbatim)
  (require-format x '(verbatim :*))
  `($concat-text ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case check)
  (require-format x '(check :%3))
  `($check ,(gui-menu-item (cadr x)) ,(caddr x) ,(cadddr x)))

(tm-define (gui-menu-item x)
  (:case balloon)
  (require-format x '(balloon :%2))
  `($balloon ,(gui-menu-item (cadr x)) ,(caddr x)))

(tm-define (gui-menu-item x)
  (:case ->)
  (require-format x '(-> :%1 :*))
  `($-> ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case =>)
  (require-format x '(=> :%1 :*))
  `($=> ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case horizontal)
  (require-format x '(horizontal :*))
  `($horizontal ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case vertical)
  (require-format x '(vertical :*))
  `($vertical ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case hlist)
  (require-format x '(hlist :*))
  `($hlist ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case vlist)
  (require-format x '(vlist :*))
  `($vlist ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case aligned)
  (require-format x '(aligned :*))
  `($aligned ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case tabs)
  (require-format x '(tabs :*))
  `($tabs ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case tab)
  (require-format x '(tab :%1 :*))
  `($tab ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case inert)
  (require-format x '(inert :*))
  `($widget-style ,widget-style-inert ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case explicit-buttons)
  (require-format x '(explicit-buttons :*))
  `($widget-style ,widget-style-button ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case bold)
  (require-format x '(bold :*))
  `($widget-style ,widget-style-bold ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case tile)
  (require-format x '(tile :integer? :*))
  `($tile ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case scrollable)
  (require-format x '(scrollable :string? :string? :*))
  `($scrollable ,(cadr x) ,(caddr x) ,@(map gui-menu-item (cdddr x))))

(tm-define (gui-menu-item x)
  (:case minibar)
  (require-format x '(minibar :*))
  `(gui$minibar ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case extend)
  (require-format x '(extend :%1 :*))
  `($widget-extend ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case padded)
  (require-format x '(padded :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #f #f 25 0)
       ($vlist ,@(map gui-menu-item (cdr x)))
       ($glue #f #f 25 0))
     ($glue #f #f 0 10)))

(tm-define (gui-menu-item x)
  (:case centered)
  (require-format x '(centered :*))
  `($vlist
     ($glue #f #f 0 10)
     ($hlist
       ($glue #t #f 25 0)
       ($vlist ,@(map gui-menu-item (cdr x)))
       ($glue #t #f 25 0))
     ($glue #f #f 0 10)))

(tm-define (gui-menu-item x)
  (:case bottom-buttons)
  (require-format x '(bottom-buttons :*))
  `($vlist
     $---
     ($glue #f #f 0 5)
     ($hlist
       ($glue #f #f 5 0)
       ($widget-style ,widget-style-button
         ,@(map gui-menu-item (cdr x)))
       ($glue #f #f 5 0))
     ($glue #f #f 0 5)))

(tm-define (gui-menu-item x)
  (:case assuming)
  (require-format x '(assuming :%1 :*))
  `($when ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case if)
  (require-format x '(if :%1 :*))
  `($delayed-when ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case when)
  (require-format x '(when :%1 :*))
  `($assuming ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case mini)
  (require-format x '(mini :%1 :*))
  `($mini ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case symbol)
  (require-format x '(symbol :string? :*))
  `($symbol ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case promise)
  (require-format x '(promise :%1))
  `($promise ,(cadr x)))

(tm-define (gui-menu-item x)
  ;;(display* "x= " x "\n")
  (cond ((symbol? x)
         (cond ((== x '---) '$---)
               ((== x '===) (gui-menu-item '(glue #f #f 0 5)))
               ((== x '======) (gui-menu-item '(glue #f #f 0 15)))
               ((== x '/) '$/)
               ((== x '//) (gui-menu-item '(glue #f #f 5 0)))
               ((== x '///) (gui-menu-item '(glue #f #f 15 0)))
               ((== x '>>) (gui-menu-item '(glue #t #f 5 0)))
               ((== x '>>>) (gui-menu-item '(glue #t #f 15 0)))
               ((== x (string->symbol "|")) '$/)
               (else
                 (texmacs-error "gui-menu-item" "invalid menu item ~S" x))))
	((string? x) x)
	((and (pair? x) (or (string? (car x)) (pair? (car x))))
	 `($> ,(gui-menu-item (car x)) ,@(cdr x)))
        (else
          (texmacs-error "gui-menu-item" "invalid menu item ~S" x))))

(tm-define (gui-menu-item x)
  (:case form)
  (require-format x '(form :%1 :*))
  `($form ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case form-input)
  (require-format x '(form-input :%4))
  `($form-input ,@(cdr x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for dynamic menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (menu-dynamic . l)
  `($list ,@(map gui-menu-item l)))

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

(define-public-macro (menu-extend name . l)
  (receive (opts body) (list-break l not-define-option?)
    `(tm-redefine ,name ,@opts
       (with old-menu (tm-definition ,name ,@opts)
	 (lambda () (append (old-menu) (menu-dynamic ,@body)))))))

(define-public-macro (lazy-menu module . menus)
  `(begin
     (lazy-define ,module ,@menus)
     (delayed
       (:idle 500)
       (import-from ,module))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic color pickers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (standard-color-list)
  '("dark red" "dark magenta" "dark blue" "dark cyan"
    "dark green" "dark yellow" "dark orange" "dark brown"
    "red" "magenta" "blue" "cyan"
    "green" "yellow" "orange" "brown"
    "pastel red" "pastel magenta" "pastel blue" "pastel cyan"
    "pastel green" "pastel yellow" "pastel orange" "pastel brown"))

(define (standard-grey-list)
  '("black" "darker grey" "dark grey" "light grey"
    "pastel grey" "white"))

(tm-menu (standard-color-menu cmd)
  (tile 8
    (for (col (standard-color-list))
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col)))))
  (glue #f #f 0 5)
  (tile 8
    (for (col (standard-grey-list))
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col))))))

(tm-define (gui-menu-item x)
  (:case pick-color)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cdr x))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic pattern picker
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (standard-pattern-list)
  (with d (url-read-directory "$TEXMACS_PATH/misc/patterns" "*.png")
    (map (lambda (x) `(pattern ,(url->string x) "" "")) d)))

(tm-menu (standard-pattern-menu cmd)
  (tile 8
    (for (col (standard-pattern-list))
      (explicit-buttons
        ((color col #f #f 32 24)
         (cmd col))))))

(tm-define (gui-menu-item x)
  (:case pick-background)
  `(menu-dynamic
     (dynamic (standard-color-menu (lambda (answer) ,@(cdr x))))
     (glue #f #f 0 5)
     (dynamic (standard-pattern-menu (lambda (answer) ,@(cdr x))))))

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
