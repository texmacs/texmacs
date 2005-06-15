
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

(texmacs-module (kernel boot abbrevs))

(define-public == equal?)
(define-public (!= x y) (not (equal? x y)))

(define-public call/cc call-with-current-continuation)
(define-public-macro (with-cc cont . body)
  `(call/cc (lambda (,cont) ,@body)))

(define-public-macro (with var val . body)
  (if (pair? var)
      `(apply (lambda ,var ,@body) ,val)
      `(let ((,var ,val)) ,@body)))

(define-public (nstring? x) (not (string? x)))
(define-public (nnull? x) (not (null? x)))
(define-public (npair? x) (not (pair? x)))
(define-public (nlist? x) (not (list? x)))

(define-public (list-1? x) (and (pair? x) (null? (cdr x))))
(define-public (nlist-1? x) (not (list-1? x)))
(define-public (list-2? x) (and (list? x) (= (length x) 2)))
(define-public (nlist-2? x) (not (list-2? x)))
(define-public (list-3? x) (and (list? x) (= (length x) 3)))
(define-public (nlist-3? x) (not (list-3? x)))

(define-public (in? x l) (not (not (member x l))))
(define-public (nin? x l) (not (member x l)))

(define-public (keyword->number x)
  (string->number (symbol->string (keyword->symbol x))))

(define-public (number->keyword x)
  (symbol->keyword (string->symbol (number->string x))))

(define-public (always? . l) #t)
(define-public (root? t) (== (reverse (tree-ip t)) (buffer-path)))
(define-public (true? . l) #t)

(define-public (save-object file value)
  (write value (open-file (url-materialize file "") OPEN_WRITE))
  (flush-all-ports))

(define-public (load-object file)
  (read (open-file (url-materialize file "r") OPEN_READ)))
