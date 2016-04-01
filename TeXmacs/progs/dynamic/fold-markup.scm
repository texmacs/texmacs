
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-markup.scm
;; DESCRIPTION : extra markup for laptop presentations
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-markup)
  (:use (utils library tree)
        (utils library cursor)
        (dynamic fold-edit)))

(define (screens-parent? t)
  (with p (tree-up t)
    (or (tree-is? p 'screens)
	(and (tree-is? p 'document)
	     (tree-is? (tree-up p) 'slideshow)))))

(tm-define (screens-index body)
  (:secure #t)
  (with t (tree-search-upwards body screens-parent?)
    (if t (number->string (tree-index t)) "-1")))

(tm-define (screens-arity body)
  (:secure #t)
  (with t (tree-search-upwards body screens-parent?)
    (if t (number->string (tree-arity (tree-up t))) "-1")))

(define (screen-link i strong?)
  (if (== i "...")
      "<cdots> "
      (let* ((nr* (number->string (+ i 1)))
             (nr (if strong? `(screens-emphasize ,nr*) nr*))
             (s (number->string i))
             (cmd (string-append "(screens-switch-to " s ")"))
             (act `(action ,nr ,cmd)))
        `(concat ,act " "))))

(define (page-accept? p c n)
  (or (<= n 40)
      (and (<= c 12) (<= p 24))
      (and (>= c (- n 12)) (>= p (- n 24)))
      (and (>= p (- c 12)) (<= p (+ c 12)))
      (or (== p 0) (== p (- n 1)))
      (== (remainder p 10) 9)))

(define (pages-list c b e)
  (cond ((>= b e) (list))
        ((page-accept? b c e) (cons b (pages-list c (+ b 1) e)))
        ((page-accept? (- b 1) c e) (cons "..." (pages-list c (+ b 1) e)))
        (else (pages-list c (+ b 1) e))))

(tm-define (screens-bar body)
  (:secure #t)
  (with t (tree-search-upwards body screens-parent?)
    (if (not t) ""
        (let* ((n (tree-arity (tree-up t)))
               (c (tree-index t))
               (l (pages-list c 0 n)))
          `(concat 
             ,@(map (lambda (x) (screen-link x (== x c))) l))))))

