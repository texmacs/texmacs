
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : iterator.scm
;; DESCRIPTION : abstract iterators
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nice but slow iterators using call/cc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public yield-handlers '())

(define (iterate-proceed back)
  (with-cc cont
    (set! yield-handlers (cons cont yield-handlers))
    (back (noop))))

(define-public (yield val)
  "Yield the value @val inside an 'iterate' construct."
  (when (nnull? yield-handlers)
    (with yield-handler (car yield-handlers)
      (set! yield-handlers (cdr yield-handlers))
      (and (!= val :iterate-end)
	   (with-cc back
	     (yield-handler (iterator val (iterate-proceed back))))))))

(define-public-macro (iterate . body)
  "Construct iterator from @body with 'yield' constructs."
  `(with-cc cont
     (set! yield-handlers (cons cont yield-handlers))
     ,@body
     (with end (car yield-handlers)
       (set! yield-handlers (cdr yield-handlers))
       (end #f))))

;;(define-public (iterator-test)
;;  (iterate
;;    (display "Hallo\n")
;;    (yield 1)
;;    (display "Hop\n")
;;    (yield 2)
;;    (display "Done\n")))
