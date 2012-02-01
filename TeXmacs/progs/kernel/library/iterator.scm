
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : iterator.scm
;; DESCRIPTION : abstract iterators
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library iterator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of iterators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (iterator val next)
  "Primitive iterator constructor from value @val and next iterator @next."
  `(lambda () (cons ,val ,next)))

(define-public (range start end)
  "Range iterator from @start to @end (not included)."
  (and (< start end)
       (iterator start (range (+ start 1) end))))

(define-public (list->iterator l)
  "Convert the list @l to an iterator."
  (and (nnull? l)
       (iterator (car l) (list->iterator (cdr l)))))

(define-public (iterator-append . its)
  "Append the iterators @its."
  (cond ((null? its) #f)
	((not (car its)) (apply iterator-append (cdr its)))
	(else (let* ((next ((car its)))
		     (cont (cons (cdr next) (cdr its))))
		(iterator (car next) (apply iterator-append cont))))))

(define-public (iterator-filter it pred?)
  "Get elements in iterator @it which match the predicate @pred?."
  (with next #f
    (while (and it (begin (set! next (it)) (not (pred? (car next)))))
      (set! it (cdr next)))
    (and it (lambda () next))))

(define-public-macro (extract var it prop?)
  "Extract all values @var from iterator @it which match the property @prop?."
  `(iterator-filter ,it (lambda (,var) ,prop?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Traversal of iterators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (iterator-value it)
  "Get current value of iterator @it."
  (and it (car (it))))

(define-public (iterator-next it)
  "Get next iterator after @it."
  (and it (cdr (it))))

(define-public-macro (iterator-read! it)
  "Read one value from iterator @it."
  `(and ,it
	(with next (,it)
	  (set! ,it (cdr next))
	  (car next))))

(define-public (iterator-apply it fun)
  (while it
    (with next (it)
      (fun (car next))
      (set! it (cdr next)))))

(define-public-macro (for-in var-it . body)
  "Execute @body for values in an iterator."
  (let* ((var (car var-it))
	 (it (cadr var-it))
	 (fun `(lambda (,var) ,@body)))
    `(iterator-apply ,it ,fun)))

(define-public (iterator->list it)
  "Convert iterator @it into a list."
  (if (not it) '()
      (with next (it)
	(cons (car next) (iterator->list (cdr next))))))
