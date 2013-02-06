;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : ptrees.scm
;; DESCRIPTION : (Simple) Prefix trees for autocompletion
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This simple implementation of prefix trees would make more sense in the C++
;; side for better speed and availability.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library ptrees))

(define (pt-marker . l)
  (if (null? l) (acons "" '() '()) (acons "" '() (car l))))

(define (pt-terminal? pt)
  (== pt (pt-marker)))

(define (pt-word-mark? pt)
  (!= #f (assoc-ref pt "")))

; Planning for the future...
(tm-define (make-ptree)
  '())

; FIXME: fix this in the (future) c++ impl.
(tm-define (ptree? pt)
  (and (list? pt) (pair? (car pt))))
  
(tm-define (pt-add pt str)
  (if (== str "")  ; We are done with the input
    (cond ((== pt #f) (pt-marker))  ; The last letter was a new node
          ((pt-word-mark? pt) pt)   ; The word was already there
          (else (pt-marker pt)))    ; This was a new word
    (let ((char (string-take str 1))
          (rest (string-drop str 1))
          (npt (if (== pt #f) '() pt)))
      (assoc-set! npt char (pt-add (assoc-ref npt char) rest)) )))

(tm-define (pt-find pt str)
  (if (== str "") pt
    (let* ((char (string-take str 1))
           (rest (string-drop str 1))
           (val  (assoc-ref pt char)))
      (if (== val #f) #f (pt-find val rest)))))

(tm-define (pt-add-list pt l)
  (if (null? l) pt
      (pt-add-list (pt-add pt (car l)) (cdr l))))

(tm-define (pt-has? pt str)
  (!= #f (pt-find pt str)))

(tm-define (pt-has-list? pt l) 
  (:synopsis "Check whether a given ptree @pt contains all items in the list @l")
  (list-fold (lambda (val prior) (and (pt-has? pt val) prior)) #t l))

(define (pt-words-below-sub pt step)
  (cond ((or (null? pt) (== #f pt)) '())
        ((pt-terminal? pt) (list step))
        ((== "" (caar pt))
         (append (list step) (pt-words-below-sub (cdr pt) step)))
        (else (append (pt-words-below-sub (cdar pt)
                                          (string-append step (caar pt)))
                      (pt-words-below-sub (cdr pt) step)))))

(tm-define (pt-words-below pt)
  (:synopsis "Return the list of words below the given p-tree node @pt")
  (pt-words-below-sub pt ""))

