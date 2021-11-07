
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

; we implement char-sets via predicates

(define-public (char-set-adjoin cs . l)
   (lambda (ch) (or (cs ch) (memq ch l))))
   
(define-public (char-set-complement cs)
  (lambda (ch) (not (cs ch))))

(define-public (char-set:whitespace ch)
  (memq ch '(#\space #\tab #\newline)))
  

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO/FIXME

; redefine (error ...) to match guile usage
; https://www.gnu.org/software/guile/manual/html_node/Error-Reporting.html

