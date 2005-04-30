
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrevs.scm
;; DESCRIPTION : useful abbreviations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot abbrevs)
  (:export
    == != with
    nstring? nnull? npair? nlist?
    list-1? nlist-1? list-2? nlist-2?
    keyword->number number->keyword
    always? root? true?))

(define == equal?)
(define (!= x y) (not (equal? x y)))

(define-macro (with var val . body)
  (if (pair? var)
      `(apply (lambda ,var ,@body) ,val)
      `(let ((,var ,val)) ,@body)))

(define (nstring? x) (not (string? x)))
(define (nnull? x) (not (null? x)))
(define (npair? x) (not (pair? x)))
(define (nlist? x) (not (list? x)))

(define (list-1? x) (and (pair? x) (null? (cdr x))))
(define (nlist-1? x) (not (list-1? x)))
(define (list-2? x) (and (list? x) (= (length x) 2)))
(define (nlist-2? x) (not (list-2? x)))
(define (list-3? x) (and (list? x) (= (length x) 3)))
(define (nlist-3? x) (not (list-3? x)))

(define (keyword->number x)
  (string->number (symbol->string (keyword->symbol x))))

(define (number->keyword x)
  (symbol->keyword (string->symbol (number->string x))))

(define (always? . l) #t)
(define (root? t) (== (reverse (tree-ip t)) (the-buffer-path)))
(define (true? . l) #t)
