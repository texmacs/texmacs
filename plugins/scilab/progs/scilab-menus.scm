
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scilab-menus.scm
;; DESCRIPTION : Menus for the pari plugin
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (scilab-menus)
  (:use (dynamic scripts-edit)
        (dynamic session-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Several subroutines for the evaluation of Scilab expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (scilab-clean-insert-menu)
  (tm-define (scilab-insert-menu) `((-> "Insert"))))

(tm-define (scilab-add-to-insert-menu libname funcs)
  (:secure #t)
  (with menu `(,@(if (defined-menu? 'scilab-insert-menu)
                     (caddar (scilab-insert-menu)) '())
               (-> ,libname ,@(map (lambda (x)
                                   `(,x ,(lambda () (insert x)))) funcs)))
    (tm-define (scilab-insert-menu) `((-> "Insert" ,menu)))))

(tm-define (scilab-rm-from-insert-menu libname)
  (:secure #t)
  (with oldmenu (if (defined-menu? 'scilab-insert-menu)
                    (caddar (scilab-insert-menu)) '())
    (with menu (filter (lambda (x) (!= (second x) libname)) oldmenu)
      (tm-define (scilab-insert-menu) `((-> "Insert" ,menu))))))

(define (defined-menu? s)
  (and (defined? s) (list>1? (cdar ((eval s))))))

(tm-define (scilab-clean-demo-menu)
  (tm-define (scilab-demo-menu) `((-> "Demo"))))

(define (lists->menu t)
  (let ((name (car t))
        (tail (cdr t)))
    (if (and (list-1? tail) (string? (car tail))) (set! tail (car tail)))
    (cond
      ((nstring? name) "")
      ((string? tail) `(,name ,(lambda ()
                                 (insert
                                   (string-append "demo_run ('" tail "')")))))
      ((list?   tail) `((-> ,name ,@(map lists->menu tail))))
      (else ""))))

(tm-define (scilab-add-to-demo-menu t)
  (:secure #t)
  (with t (lists->menu t)
    (with menu `(,@(if (defined-menu? 'scilab-demo-menu)
                       (caddar (scilab-demo-menu)) '()) ,t)
    (tm-define (scilab-demo-menu) `((-> "Demo" ,menu))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Scilab menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind scilab-insert-menu
  (-> "Insert"))

(menu-bind scilab-demo-menu
  (-> "Demo"))

;(menu-bind scilab-help-menu
;  (-> "Help"))

(menu-bind scilab-menu
      (if (defined-menu? 'scilab-demo-menu)
        (link scilab-demo-menu))
      (if (defined-menu? 'scilab-insert-menu)
        (link scilab-insert-menu))
      ---
      (if (defined? 'scilab-help-menu)
        (link scilab-help-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Activate menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind texmacs-extra-menu
  (former)
  (if (in-scilab?)
      (=> "Scilab"
	  (link scilab-menu))))
