
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ref-markup.scm
;; DESCRIPTION : rendering macros for smart references
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (link ref-markup)
  (:use (link ref-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smart-has? tp)
  (style-has? (string-append tp "-ref")))

(define (smart-ref-type s)
  (let* ((i (string-find-non-alpha s 0 #t))
         (j (string-find-non-alpha s (string-length s) #f)))
    (cond ((and (>= i 0) (smart-has? (substring s 0 i)))
           (substring s 0 i))
          ((and (>= j 0) (smart-has? (substring s (+ j 1) (string-length s))))
           (substring s (+ j 1) (string-length s)))
          (else "unknown"))))

(define (smart-extract* tp c tps)
  (if (null? c) (list)
      (with r (smart-extract* tp (cdr c) (cdr tps))
        (if (== (car tps) tp) (cons (car c) r) r))))

(define (smart-extract tp c tps)
  `(,(string->symbol (string-append tp "-ref"))
    ,@(smart-extract* tp c tps)))

(define (smart-plural s)
  (cond ((== s "") s)
        ((string-ends? s "y")
         (string-append (substring s 0 (- (string-length s) 1)) "ies"))
        (else
         (string-append s "s"))))

(define (smart-list* l)
  (if (null? (cdr l))
      (list `(localize "and") `(nbsp) (car l))
      (cons* (car l) ", " (smart-list* (cdr l)))))

(define (smart-list l)
  (cond ((<= (length l) 1) l)
        ((== (length l) 2)
         (list (car l) " " `(localize "and") `(nbsp) (cadr l)))
        (else (smart-list* l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main smart reference rendering macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-typed-ref* name macro t)
  (:secure #t)
  (let* ((s (or (tm->string name) ""))
         (l (string->symbol (or (tm->string macro) "identity")))
         (v (lambda (x) `(reference ,x)))
         (w (lambda (x) (if (== l 'identity) x `(,l ,x))))
         (c (map w (map v (tm-children t))))
         (r (smart-list c)))
    (when (> (length c) 1) (set! s (smart-plural s)))
    (cond ((null? r) "")
          ((and (== s "") (null? (cdr r))) (car r))
          ((== s "") `(concat ,@r))
          ((null? (cdr r)) `(concat (localize ,s) (nbsp) ,(car r)))
          (else `(concat (localize ,s) (nbsp) ,@r)))))

(tm-define (ext-typed-ref name t)
  (:secure #t)
  (ext-typed-ref* name "identity" t))

(tm-define (ext-smart-ref t)
  (:secure #t)
  (let* ((c (tm-children t))
         (l (map tm->string c))
         (tps (map smart-ref-type (map locase-all l)))
         (u (list-remove-duplicates tps))
         (x (map (lambda (tp) (smart-extract tp c tps)) u))
         (r (smart-list x)))
    (cond ((null? r) "")
          ((null? (cdr r)) (car r))
          (else `(concat ,@r)))))
