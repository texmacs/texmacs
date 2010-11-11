
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-markup.scm
;; DESCRIPTION : Markup for the definition of GUI widgets
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-markup)
  (:use (kernel regexp regexp-match)
	(kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markup elements for the generation of widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-normalize l)
  (cond ((null? l) l)
	((func? (car l) 'list)
	 (append (gui-normalize (cdar l)) (gui-normalize (cdr l))))
	(else (cons (car l) (gui-normalize (cdr l))))))

(tm-define-macro (gui$list . l)
  (:synopsis "Make widgets")
  `(gui-normalize (list ,@l)))

(tm-define-macro (gui$promise cmd)
  (:synopsis "Promise widgets")
  `(list 'promise (lambda () ,cmd)))

(tm-define-macro (gui$dynamic w)
  (:synopsis "Make dynamic widgets")
  `(cons* 'list ,w))

(tm-define-macro (gui$assuming pred? . l)
  (:synopsis "Conditionally make widgets")
  `(cons* 'list (if ,pred? (gui$list ,@l) '())))

(tm-define-macro (gui$when pred? . l)
  (:synopsis "Make possibly inert (whence greyed) widgets")
  `(cons* 'when (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$icon name)
  (:synopsis "Make icon")
  `(list 'icon ,name))

(tm-define-macro (gui$balloon text balloon)
  (:synopsis "Make balloon")
  `(list 'balloon ,text ,balloon))

(tm-define-macro (gui$group text)
  (:synopsis "Make a menu group")
  `(list 'group ,text))

(tm-define-macro (gui$symbol sym . l)
  (:synopsis "Make a menu symbol")
  (if (null? l)
      `(list 'symbol ,sym)
      `(list 'symbol ,sym (lambda () ,(car l)))))

(tm-define-macro (gui$input cmd type proposals width)
  (:synopsis "Make input field")
  `(list 'input (lambda (answer) ,cmd) ,type (lambda () ,proposals) ,width))

(tm-define-macro (gui$pick-color cmd)
  (:synopsis "Make color picker")
  `(list 'pick-color (lambda (answer) ,cmd)))

(tm-define-macro (gui$pick-background cmd)
  (:synopsis "Make background picker")
  `(list 'pick-background (lambda (answer) ,cmd)))

(tm-define-macro (gui$button text . cmds)
  (:synopsis "Make button")
  `(list ,text (lambda () ,@cmds)))

(tm-define-macro (gui$pullright text . l)
  (:synopsis "Make pullright button")
  `(cons* '-> ,text (gui$list ,@l)))

(tm-define-macro (gui$pulldown text . l)
  (:synopsis "Make pulldown button")
  `(cons* '=> ,text (gui$list ,@l)))

(tm-define-macro (gui$horizontal . l)
  (:synopsis "Horizontal layout of widgets")
  `(cons* 'horizontal (gui$list ,@l)))

(tm-define-macro (gui$vertical . l)
  (:synopsis "Vertical layout of widgets")
  `(cons* 'vertical (gui$list ,@l)))

(tm-define-macro (gui$tile columns . l)
  (:synopsis "Tile layout of widgets")
  `(cons* 'tile ,columns (gui$list ,@l)))

(tm-define-macro (gui$minibar . l)
  (:synopsis "Make minibar")
  `(cons* 'minibar (gui$list ,@l)))

(tm-define-macro (gui$check text check pred?)
  (:synopsis "Make button")
  `(list 'check ,text ,check (lambda () ,pred?)))

(tm-define-macro (gui$mini pred? . l)
  (:synopsis "Make mini widgets")
  `(cons* 'mini (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$hsep)
  (:synopsis "Make horizontal separator")
  `(string->symbol "|"))

(tm-define-macro (gui$vsep)
  (:synopsis "Make vertical separator")
  `'---)

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
  `(gui$dynamic ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case link)
  (require-format x '(link :%1))
  `(gui$dynamic (,(cadr x))))

(tm-define (gui-menu-item x)
  (:case let let*)
  (require-format x '(:%1 :%1 :*))
  `(,(car x) ,(cadr x) (gui$menu ,@(cddr x))))

(tm-define (gui-menu-item x)
  (:case with receive)
  (require-format x '(:%1 :%2 :*))
  `(,(car x) ,(cadr x) ,(caddr x) (gui$menu ,@(cdddr x))))

(tm-define (gui-menu-item x)
  (:case for)
  (require-format x '(for (:%1 :%1) :*))
  (with fun `(lambda (,(caadr x)) (gui$menu ,@(cddr x)))
    `(gui$dynamic (append-map ,fun ,(cadadr x)))))

(tm-define (gui-menu-item x)
  (:case cond)
  (require-format x '(cond :*))
  (with fun (lambda (x)
              (with (pred? . body) x
                (list pred? (cons* 'gui$menu body))))
    `(cond ,@(map fun (cdr x)))))

(tm-define (gui-menu-item x)
  (:case group)
  (require-format x '(group :%1))
  `(gui$group ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case input)
  (require-format x '(input :%4))
  `(gui$input ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case icon)
  (require-format x '(icon :%1))
  `(gui$icon ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case check)
  (require-format x '(check :%3))
  `(gui$check ,(gui-menu-item (cadr x)) ,(caddr x) ,(cadddr x)))

(tm-define (gui-menu-item x)
  (:case balloon)
  (require-format x '(balloon :%2))
  `(gui$balloon ,(gui-menu-item (cadr x)) ,(caddr x)))

(tm-define (gui-menu-item x)
  (:case ->)
  (require-format x '(-> :%1 :*))
  `(gui$pullright ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case =>)
  (require-format x '(=> :%1 :*))
  `(gui$pulldown ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case horizontal)
  (require-format x '(horizontal :*))
  `(gui$horizontal ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case vertical)
  (require-format x '(vertical :*))
  `(gui$vertical ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case tile)
  (require-format x '(tile :integer? :*))
  `(gui$tile ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case minibar)
  (require-format x '(minibar :*))
  `(gui$minibar ,@(map gui-menu-item (cdr x))))

(tm-define (gui-menu-item x)
  (:case assuming)
  (require-format x '(assuming :%1 :*))
  `(gui$assuming ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case when)
  (require-format x '(when :%1 :*))
  `(gui$when ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case mini)
  (require-format x '(mini :%1 :*))
  `(gui$mini ,(cadr x) ,@(map gui-menu-item (cddr x))))

(tm-define (gui-menu-item x)
  (:case pick-color)
  (require-format x '(pick-color :%1))
  `(gui$pick-color ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case pick-background)
  (require-format x '(pick-background :%1))
  `(gui$pick-background ,(cadr x)))

(tm-define (gui-menu-item x)
  (:case symbol)
  (require-format x '(symbol :string? :*))
  `(gui$symbol ,@(cdr x)))

(tm-define (gui-menu-item x)
  (:case promise)
  (require-format x '(promise :%1))
  `(gui$promise ,(cadr x)))

(tm-define (gui-menu-item x)
  (cond ((== x '---) `(gui$vsep))
	((== x (string->symbol "|")) `(gui$hsep))
	((string? x) x)
	((and (pair? x) (or (string? (car x)) (pair? (car x))))
	 `(gui$button ,(gui-menu-item (car x)) ,@(cdr x)))
        (else (texmacs-error "gui-menu-item" "invalid menu item ~S" x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for dynamic menu definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (gui$menu . l)
  `(gui$list ,@(map gui-menu-item l)))

(tm-define-macro (define-menu head . body)
  `(define ,head (gui$menu ,@body)))

(tm-define-macro (tm-menu head . l)
  (let* ((body? (lambda (x) (or (npair? x) (not (keyword? (car x))))))
	 (i (list-find-index l body?))
	 (prop (if i (sublist l 0 i) l))
	 (body (if i (sublist l i (length l)) '())))
    `(tm-define ,head ,@prop (gui$menu ,@body))))
