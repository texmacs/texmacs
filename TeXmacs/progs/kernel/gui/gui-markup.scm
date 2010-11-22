
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-markup.scm
;; DESCRIPTION : Macros and functions for content generation
;; COPYRIGHT   : (C) 2010  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-markup)
  (:use (kernel regexp regexp-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public widget-style-mini 1)
(define-public widget-style-monospaced 2)
(define-public widget-style-grey 4)
(define-public widget-style-pressed 8)
(define-public widget-style-inert 16)
(define-public widget-style-button 32)
(define-public widget-style-centered 64)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Control structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (gui-normalize l)
  (cond ((null? l) l)
	((func? (car l) 'list)
	 (append (gui-normalize (cdar l)) (gui-normalize (cdr l))))
	(else (cons (car l) (gui-normalize (cdr l))))))

(tm-define-macro ($list . l)
  (:synopsis "Make widgets")
  `(gui-normalize (list ,@l)))

(tm-define-macro ($begin . l)
  (:synopsis "Begin primitive for content generation")
  `(cons* 'list ($list ,@l)))

(tm-define-macro ($if pred? . l)
  (:synopsis "When primitive for content generation")
  (cond ((== (length l) 1)
         `(cons* 'list (if ,pred? ($list ,(car l)) '())))
        ((== (length l) 2)
         `(cons* 'list (if ,pred? ($list ,(car l)) ($list ,(cadr l)))))
        (else
          (texmacs-error "$if" "invalid number of arguments"))))

(tm-define-macro ($when pred? . l)
  (:synopsis "When primitive for content generation")
  `(cons* 'list (if ,pred? ($list ,@l) '())))

(tm-define (cond$sub l)
  (cond ((null? l)
         (list `(else '())))
        ((npair? (car l))
         (texmacs-error "cond$sub" "syntax error ~S" l))
        ((== (caar l) 'else)
         (list `(else ($list ,@(cdar l)))))
        (else (cons `(,(caar l) ($list ,@(cdar l)))
                    (cond$sub (cdr l))))))

(tm-define-macro ($cond . l)
  (:synopsis "Cond primitive for content generation")
  `(cons* 'list (cond ,@(cond$sub l))))

(tm-define-macro ($let decls . l)
  (:synopsis "Let* primitive for content generation")
  `(let ,decls
     (cons* 'list ($list ,@l))))

(tm-define-macro ($let* decls . l)
  (:synopsis "Let* primitive for content generation")
  `(let* ,decls
     (cons* 'list ($list ,@l))))

(tm-define-macro ($with var val . l)
  (:synopsis "With primitive for content generation")
  `(with ,var ,val
     (cons* 'list ($list ,@l))))

(tm-define-macro ($for var-val . l)
  (:synopsis "For primitive for content generation")
  (when (nlist-2? var-val)
    (texmacs-error "$for" "syntax error in ~S" var-val))
  (with fun `(lambda (,(car var-val)) ($list ,@l))
    `(cons* 'list (append-map ,fun ,(cadr var-val)))))

(tm-define-macro ($dynamic w)
  (:synopsis "Make dynamic widgets")
  `(cons* 'list ,w))

(tm-define-macro ($promise cmd)
  (:synopsis "Promise widgets")
  `(list 'promise (lambda () ,cmd)))

(tm-define-macro ($menu-link w)
  (:synopsis "Make dynamic link to another widget")
  `(list 'link ',w))

(tm-define-macro ($delayed-when pred? . l)
  (:synopsis "Delayed when primitive for content generation")
  `(cons* 'if (lambda () ,pred?) ($list ,@l)))

(tm-define-macro ($assuming pred? . l)
  (:synopsis "Make possibly inert (whence greyed) widgets")
  `(cons* 'when (lambda () ,pred?) ($list ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General layout widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro ($glue hext? vext? minw minh)
  (:synopsis "Make extensible glue")
  `(list 'glue ,hext? ,vext? ,minw ,minh))

(tm-define-macro ($colored-glue col hext? vext? minw minh)
  (:synopsis "Make extensible colored glue")
  `(list 'color ,col ,hext? ,vext? ,minw ,minh))

(tm-define-macro ($hlist . l)
  (:synopsis "Horizontal layout of widgets")
  `(cons* 'hlist ($list ,@l)))

(tm-define-macro ($vlist . l)
  (:synopsis "Vertical layout of widgets")
  `(cons* 'vlist ($list ,@l)))

(tm-define-macro ($horizontal . l)
  (:synopsis "Horizontal layout of widgets")
  `(cons* 'horizontal ($list ,@l)))

(tm-define-macro ($vertical . l)
  (:synopsis "Vertical layout of widgets")
  `(cons* 'vertical ($list ,@l)))

(tm-define-macro ($tile columns . l)
  (:synopsis "Tile layout of widgets")
  `(cons* 'tile ,columns ($list ,@l)))

(tm-define $/
  (:synopsis "Horizontal separator")
  (string->symbol "|"))

(tm-define $---
  (:synopsis "Vertical separator")
  '---)

(tm-define-macro ($mini pred? . l)
  (:synopsis "Make mini widgets")
  `(cons* 'mini (lambda () ,pred?) ($list ,@l)))

(tm-define-macro (gui$minibar . l)
  (:synopsis "Make minibar")
  `(cons* 'minibar ($list ,@l)))

(tm-define-macro ($widget-style st . l)
  (:synopsis "Change the style of a widget")
  `(cons* 'style ,st ($list ,@l)))

(tm-define-macro ($widget-extend w . l)
  (:synopsis "Extend the size of a widget")
  `(cons* 'extend ,w ($list ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu and widget elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro ($-> text . l)
  (:synopsis "Make pullright button")
  `(cons* '-> ,text ($list ,@l)))

(tm-define-macro ($=> text . l)
  (:synopsis "Make pulldown button")
  `(cons* '=> ,text ($list ,@l)))

(tm-define-macro ($> text . cmds)
  (:synopsis "Make button")
  `(list ,text (lambda () ,@cmds)))

(tm-define-macro ($check text check pred?)
  (:synopsis "Make button")
  `(list 'check ,text ,check (lambda () ,pred?)))

(tm-define-macro ($balloon text balloon)
  (:synopsis "Make balloon")
  `(list 'balloon ,text ,balloon))

(tm-define-macro ($concat-text . l)
  (:synopsis "Make text concatenation")
  `(quote (concat ,@l)))

(tm-define-macro ($verbatim-text . l)
  (:synopsis "Make verbatim text")
  `(quote (verbatim ,@l)))

(tm-define-macro ($icon name)
  (:synopsis "Make icon")
  `(list 'icon ,name))

(tm-define-macro ($symbol sym . l)
  (:synopsis "Make a menu symbol")
  (if (null? l)
      `(list 'symbol ,sym)
      `(list 'symbol ,sym (lambda () ,(car l)))))

(tm-define-macro ($menu-group text)
  (:synopsis "Make a menu group")
  `(list 'group ,text))

(tm-define-macro ($input cmd type proposals width)
  (:synopsis "Make input field")
  `(list 'input (lambda (answer) ,cmd) ,type (lambda () ,proposals) ,width))
