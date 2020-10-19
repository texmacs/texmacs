
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : css.scm
;; DESCRIPTION : tools for manipulation of CSS attributes
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools css)
  (:use (convert tools sxml)))

(define (css-style-item->pair s)
  (with l (string-tokenize-by-char s #\:)
    (and (== (length l) 2)
         (cons (tm-string-trim-both (car l))
               (tm-string-trim-both (cadr l))))))

(define (css-style->assoc s)
  (let* ((l1 (string-tokenize-by-char s #\;))
         (l2 (map css-style-item->pair l1)))
    (list-filter l2 (lambda (x) x))))

(define (pair->css-style-item p)
  (string-append (car p) ": " (cdr p)))

(define (assoc->css-style a)
  (with l (map pair->css-style-item a)
    (string-recompose l "; ")))

(define (css-max l1 l2)
  (if (and (length? l1) (length? l2))
      (length-max l1 l2)
      l2))

(define (css-add l1 l2)
  (if (and (length? l1) (length? l2))
      (length-add l1 l2)
      l2))

(define (assoc-change a k v)
  (cond ((null? a) a)
        ((== (caar a) k) (cons (cons k v) (cdr a)))
        (else (cons (car a) (assoc-change (cdr a) k v)))))

(define (css-merge-attrs a1 a2)
  (cond ((null? a1) a2)
        ((not (assoc-ref a2 (caar a1)))
         (cons (car a1) (css-merge-attrs (cdr a1) a2)))
        ((in? (caar a1) '("margin-top" "margin-bottom"
                          "padding-top" "padding-bottom"))
         (let* ((k (caar a1))
                (v (css-max (assoc-ref a1 k) (assoc-ref a2 k)))
                (a2* (assoc-change a2 k v)))
           (css-merge-attrs (cdr a1) a2*)))
        ((in? (caar a1) '("margin-left" "margin-right"
                          "padding-left" "padding-right"))
         (let* ((k (caar a1))
                (v (css-add (assoc-ref a1 k) (assoc-ref a2 k)))
                (a2* (assoc-change a2 k v)))
           (css-merge-attrs (cdr a1) a2*)))
        (else (cons (car a1) (css-merge-attrs (cdr a1) a2)))))

(tm-define (css-merge-styles s1 s2)
  (let* ((a1 (css-style->assoc s1))
         (a2 (css-style->assoc s2))
         (a  (css-merge-attrs a1 a2)))
    (assoc->css-style a)))
