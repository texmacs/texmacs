
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : compat-s7.scm
;; DESCRIPTION : compatability layer for S7
;; COPYRIGHT   : (C) 2021 Massimiliano Gubinelli
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot compat-s7))

;;; certain Scheme versions do not define 'filter'
(if (not (defined? 'filter))
    (define-public (filter pred? l)
      (apply append (map (lambda (x) (if (pred? x) (list x) (list))) l))))

;; curried define
(define base-define define)
(define-public-macro (curried-define head . body)
    (if (pair? head)
      `(,curried-define ,(car head) (lambda ,(cdr head) ,@body))
      `(,base-define ,head ,@body)))
(varlet *texmacs-user-module* 'define curried-define)


;(define primitive-string->symbol string->symbol)
;(define-public (string->symbol s) (if (string-null? s) '() (primitive-string->symbol s)))

(define-public-macro (1+ n) `(+ ,n 1))
(define-public-macro (1- n) `(- ,n 1))
(define-public (noop . args) (and (pair? args) (car args)))

(define-public (delq x l)
  (if (pair? l) (if (eq? x (car l)) (delq x (cdr l)) (cons (car l) (delq x (cdr l)))) ()))

(define-public (acons key datum alist) (cons (cons key datum) alist))

(define-public (symbol-append . l)
   (string->symbol (apply string-append (map symbol->string l))))

(define-public (map-in-order . l) (apply map l))

(define-public lazy-catch catch)

(define-public (last-pair lis)
;;  (check-arg pair? lis last-pair)
  (let lp ((lis lis))
    (let ((tail (cdr lis)))
      (if (pair? tail) (lp tail) lis))))


(define-public (seed->random-state seed) (random-state seed))

;; Guile's *random-state* is the default state used by 'random';
;; setting it reseeds s7's default random state
(varlet (rootlet) '*random-state* (*s7* 'default-random-state))
(set! (setter '*random-state* (rootlet))
      (lambda (sym val) (set! (*s7* 'default-random-state) val) val))

(define-public (list-copy lst)
  (copy lst)) ;; S7 has generic functions. copy do a shallow copy
  
(define-public (copy-tree tree)
  (let loop ((tree tree))
    (if (pair? tree)
        (cons (loop (car tree)) (loop (cdr tree)))
        tree)))


(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (set! (cdr b) val) (set! l (cons (cons what val) l)))
    l))

;;FIXME: assoc-set! is tricky to use, maybe just get rid in the code
(define-public (assoc-set! l what val)
  (let ((b (assoc what l)))
    (if b (set! (cdr b) val) (set! l (cons (cons what val) l)))
    l))

(define-public (assoc-ref l what)
  (let ((b (assoc what l)))
    (if b (cdr b) #f)))

(define-public (sort l op) (sort! (copy l) op))

(define-public (force-output) (flush-output-port *stdout*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (string-null? s) (equal? (length s) 0))

(define-public (append! . ls) (apply append ls))

(define-public (string-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
            ((char=? ch (string-ref str b))
             (cond
               ((!= a b)
                (cons (substring str a b) (split b b)))
               ((and (= a b) (or (= b 0) (= b (- len 1))))
                (cons "" (split (+ 1 b) (+ 1 b))))
               (else
                (split (+ 1 b) (+ 1 b)))))
            (else
             (split a (+ 1 b)))))))
      (split 0 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;guile-style records

;(define tmtable-type (make-record-type "tmtable" '(nrows ncols cells formats)))
;(define tmtable-record (record-constructor tmtable-type))
;(tm-define tmtable? (record-predicate tmtable-type))
;(tm-define tmtable-nrows (record-accessor tmtable-type 'nrows))
;(tm-define tmtable-ncols (record-accessor tmtable-type 'ncols))
;(tm-define tmtable-cells (record-accessor tmtable-type 'cells))
;(define tmtable-formats (record-accessor tmtable-type 'formats))

(define-public (make-record-type type fields)
  (inlet 'type type 'fields fields))

(define-public (record-constructor rec-type)
  (eval `(lambda ,(rec-type 'fields)
     (inlet 'type ,(rec-type 'type) ,@(map (lambda (f) (values (list 'quote f) f)) (rec-type 'fields))))))
 
(define-public-macro (record-accessor rec-type field)
  `(lambda (rec) (rec ,field)))

(define-public (record-predicate rec-type)
  (lambda (rec) (eq? (rec 'type) (rec-type 'type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From S7/r7rs.scm

;; delay and force: ugh
;;   this implementation is based on the r7rs spec

(define-public (make-promise done? proc)
  (list (cons done? proc)))

(define-public-macro (delay-force expr)
  `(make-promise #f (lambda () ,expr)))

(define-public-macro (delay expr) ; "delay" is taken damn it
  (list 'delay-force (list 'make-promise #t (list 'lambda () expr))))

(define-public (force promise)
  (if (caar promise)
      ((cdar promise))
      (let ((promise* ((cdar promise))))
        (if (not (caar promise))
            (begin
              (set-car! (car promise) (caar promise*))
              (set-cdr! (car promise) (cdar promise*))))
        (force promise))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hashing (use S7 internal hash)

(define *default-bound* (- (expt 2 29) 3))

(define-public (hash obj . maybe-bound)
  (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
    (modulo (hash-code obj) bound))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (while test . body)      ; while loop with predefined break and continue
  `(call-with-exit
    (lambda (break)
      (let continue ()
    (if (let () ,test)
        (begin
          (let () ,@body)
          (continue))
        (break))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; string search and charsets

; Char-sets are hash tables from characters to #t.  Hash tables are
; applicable, so (cs ch) works as a membership test, like for the predicates
; which are also accepted wherever a char-set is expected.  We avoid closures
; on purpose: s7 (at least up to 11.9) can mis-apply a closure called from a
; loop after that loop was run with a closure of a different shape.
; s7 characters are bytes, so every char-set is a subset of 256 characters.

(define (char-set-from-predicate pred)
  (let ((cs (make-hash-table 256)))
    (do ((i 0 (+ i 1))) ((= i 256) cs)
      (let ((ch (integer->char i)))
        (if (pred ch) (hash-table-set! cs ch #t))))))

(define (->char-set cs)
  (if (hash-table? cs) cs (char-set-from-predicate cs)))

(define-public (char-set . l)
  (let ((cs (make-hash-table 256)))
    (for-each (lambda (ch) (hash-table-set! cs ch #t)) l)
    cs))

(define-public (string->char-set s)
  (apply char-set (string->list s)))

(define-public (char-set-adjoin cs . l)
  (let ((r (copy (->char-set cs))))
    (for-each (lambda (ch) (hash-table-set! r ch #t)) l)
    r))

(define-public (char-set-complement cs)
  (let ((cs (->char-set cs)) (r (make-hash-table 256)))
    (do ((i 0 (+ i 1))) ((= i 256) r)
      (let ((ch (integer->char i)))
        (if (not (hash-table-ref cs ch)) (hash-table-set! r ch #t))))))

(define-public (char-set-intersection cs . l)
  (let ((r (copy (->char-set cs))) (l (map ->char-set l)))
    (for-each (lambda (entry)
                (let ((ch (car entry)))
                  (if (not (let loop ((l l))
                             (or (null? l)
                                 (and (hash-table-ref (car l) ch)
                                      (loop (cdr l))))))
                      (hash-table-set! r ch #f))))
              (copy r))
    r))

(define-public (char-set-union . l)
  (let ((r (make-hash-table 256)))
    (for-each (lambda (cs)
                (for-each (lambda (entry) (hash-table-set! r (car entry) #t))
                          (->char-set cs)))
              l)
    r))

(define-public (char-set-contains? cs ch)
  (if (hash-table? cs) (hash-table-ref cs ch) (and (cs ch) #t)))

(define-public (char-set-size cs)
  (hash-table-entries (->char-set cs)))

(define-public char-set:whitespace (char-set #\space #\tab #\newline))
(define-public char-set:lower-case (char-set-from-predicate char-lower-case?))
(define-public char-set:upper-case (char-set-from-predicate char-upper-case?))
(define-public char-set:digit (char-set-from-predicate char-numeric?))

; string-index and string-rindex accepts char-sets

(define-public (string-index str cs)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs)))
  (define len (string-length str))
  (do ((pos 0 (+ 1 pos)))
      ((or (>= pos len) (chr (string-ref str pos)))
       (and (< pos len) pos)))))

(define-public (string-rindex str cs)
 (let ((chr (if (char? cs) (lambda (c) (char=? c cs)) cs)))
  (do ((pos (+ -1 (string-length str)) (+ -1 pos)))
      ((or (negative? pos) (chr (string-ref str pos)))
       (and (not (negative? pos)) pos)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; s7 does not have iota, let's provide it

(define-public (iota n)
   (let loop ((count (1- n)) (result '()))
     (if (< count 0) result
         (loop (1- count) (cons count result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO/FIXME

; redefine (error ...) to match guile usage
; https://www.gnu.org/software/guile/manual/html_node/Error-Reporting.html

