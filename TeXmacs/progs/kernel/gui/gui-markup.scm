
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

(tm-define-macro (gui$list . l)
  (:synopsis "Make widgets")
  `(gui-normalize (list ,@l)))

(tm-define-macro (gui$promise cmd)
  (:synopsis "Promise widgets")
  `(list 'promise (lambda () ,cmd)))

(tm-define-macro (gui$dynamic w)
  (:synopsis "Make dynamic widgets")
  `(cons* 'list ,w))

(tm-define-macro (gui$link w)
  (:synopsis "Make dynamic link to another widget")
  `(list 'link ',w))

(tm-define-macro (gui$assuming pred? . l)
  (:synopsis "Conditionally make widgets")
  `(cons* 'list (if ,pred? (gui$list ,@l) '())))

(tm-define-macro (gui$if pred? . l)
  (:synopsis "Conditionally make widgets, a posteriori")
  `(cons* 'if (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$when pred? . l)
  (:synopsis "Make possibly inert (whence greyed) widgets")
  `(cons* 'when (lambda () ,pred?) (gui$list ,@l)))

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
  `(cons* 'hlist (gui$list ,@l)))

(tm-define-macro ($vlist . l)
  (:synopsis "Vertical layout of widgets")
  `(cons* 'vlist (gui$list ,@l)))

(tm-define-macro ($horizontal . l)
  (:synopsis "Horizontal layout of widgets")
  `(cons* 'horizontal (gui$list ,@l)))

(tm-define-macro ($vertical . l)
  (:synopsis "Vertical layout of widgets")
  `(cons* 'vertical (gui$list ,@l)))

(tm-define-macro (gui$tile columns . l)
  (:synopsis "Tile layout of widgets")
  `(cons* 'tile ,columns (gui$list ,@l)))

(tm-define-macro (gui$hsep)
  (:synopsis "Make horizontal separator")
  `(string->symbol "|"))

(tm-define-macro (gui$vsep)
  (:synopsis "Make vertical separator")
  `'---)

(tm-define-macro (gui$mini pred? . l)
  (:synopsis "Make mini widgets")
  `(cons* 'mini (lambda () ,pred?) (gui$list ,@l)))

(tm-define-macro (gui$minibar . l)
  (:synopsis "Make minibar")
  `(cons* 'minibar (gui$list ,@l)))

(tm-define-macro (gui$style st . l)
  (:synopsis "Change the style of a widget")
  `(cons* 'style ,st (gui$list ,@l)))

(tm-define-macro (gui$extend w . l)
  (:synopsis "Extend the size of a widget")
  `(cons* 'extend ,w (gui$list ,@l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Menu and widget elements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (gui$pullright text . l)
  (:synopsis "Make pullright button")
  `(cons* '-> ,text (gui$list ,@l)))

(tm-define-macro (gui$pulldown text . l)
  (:synopsis "Make pulldown button")
  `(cons* '=> ,text (gui$list ,@l)))

(tm-define-macro (gui$button text . cmds)
  (:synopsis "Make button")
  `(list ,text (lambda () ,@cmds)))

(tm-define-macro (gui$check text check pred?)
  (:synopsis "Make button")
  `(list 'check ,text ,check (lambda () ,pred?)))

(tm-define-macro (gui$balloon text balloon)
  (:synopsis "Make balloon")
  `(list 'balloon ,text ,balloon))

(tm-define-macro (gui$concat . l)
  (:synopsis "Make text concatenation")
  `(quote (concat ,@l)))

(tm-define-macro (gui$verbatim . l)
  (:synopsis "Make verbatim text")
  `(quote (verbatim ,@l)))

(tm-define-macro (gui$icon name)
  (:synopsis "Make icon")
  `(list 'icon ,name))

(tm-define-macro (gui$symbol sym . l)
  (:synopsis "Make a menu symbol")
  (if (null? l)
      `(list 'symbol ,sym)
      `(list 'symbol ,sym (lambda () ,(car l)))))

(tm-define-macro (gui$group text)
  (:synopsis "Make a menu group")
  `(list 'group ,text))

(tm-define-macro (gui$input cmd type proposals width)
  (:synopsis "Make input field")
  `(list 'input (lambda (answer) ,cmd) ,type (lambda () ,proposals) ,width))
