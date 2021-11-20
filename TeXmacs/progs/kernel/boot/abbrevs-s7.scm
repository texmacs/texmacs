
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : abbrevs.scm
;; DESCRIPTION : useful abbreviations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel boot abbrevs-s7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common notations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public == equal?)
(define-public (!= x y) (not (equal? x y)))

(define-public (nsymbol? x) (not (symbol? x)))
(define-public (nstring? x) (not (string? x)))
(define-public (nnull? x) (not (null? x)))
(define-public (npair? x) (not (pair? x)))
(define-public (nlist? x) (not (list? x)))
(define-public (nnot x) (not (not x)))
(define-public-macro (toggle! x) `(set! ,x (not ,x)))
(define-public (safe-car l) (and (pair? l) (car l)))
(define-public (safe-cdr l) (and (pair? l) (cdr l)))

(define-public (list-1? x) (and (pair? x) (null? (cdr x))))
(define-public (nlist-1? x) (not (list-1? x)))
(define-public (list-2? x) (and (list? x) (= (length x) 2)))
(define-public (nlist-2? x) (not (list-2? x)))
(define-public (list-3? x) (and (list? x) (= (length x) 3)))
(define-public (nlist-3? x) (not (list-3? x)))
(define-public (list-4? x) (and (list? x) (= (length x) 4)))
(define-public (nlist-4? x) (not (list-4? x)))
(define-public (list>0? x) (and (pair? x) (list? x)))
(define-public (nlist>0? x) (not (list>0? x)))
(define-public (list>1? x) (and (list? x) (> (length x) 1)))
(define-public (nlist>1? x) (not (list>1? x)))

(define-public (in? x l) (not (not (member x l))))
(define-public (nin? x l) (not (member x l)))
(define-public (cons-new x l) (if (in? x l) l (cons x l)))

(define-public (always? . l) #t)
(define-public (never? . l) #f)
(define-public (root? t) (== (reverse (tree-ip t)) (buffer-path)))
(define-public (nroot? t) (!= (reverse (tree-ip t)) (buffer-path)))
(define-public (leaf? t) (== (tree-ip t) (cdr (reverse (cursor-path)))))
(define-public (nleaf? t) (!= (tree-ip t) (cdr (reverse (cursor-path)))))
(define-public (true? . l) #t)
(define-public (false? . l) #f)

(provide-public (identity x) x)
(provide-public (ignore . l) (noop))
(provide-public (negate pred?) (lambda x (not (apply pred? x))))

(define-public (keyword->string x)
  (symbol->string (keyword->symbol x)))
(define-public (string->keyword x)
  (symbol->keyword (string->symbol x)))
(define-public (keyword->number x)
  (string->number (string-tail (symbol->string (keyword->symbol x)) 1)))
(define-public (number->keyword x)
  (symbol->keyword (string->symbol (string-append "%" (number->string x)))))

(define-public (save-object file value)
  (call-with-output-file (url-materialize file "") (lambda (port)
    (let-temporarily (((*s7* 'print-length) 9223372036854775807)) (write value port)))))

(define-public (load-object file)
  (let ((r (call-with-input-file (url-materialize file "r") (lambda (port) (read port)))))
        (if (eof-object? r) '() r)))

(define-public (persistent-ref dir key)
  (and (persistent-has? dir key)
       (persistent-get dir key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common programming constructs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (with var val . body)
  (if (pair? var)
      `(apply (lambda ,var ,@body) ,val)
      `(let ((,var ,val)) ,@body)))

(define-public-macro (with-define fun fun-body . body)
  `(let ((,(car fun) (lambda ,(cdr fun) ,fun-body)))
     ,@body))

;; https://ccrma.stanford.edu/software/snd/snd/s7.html#multiplevalues: One problem with this way of handling multiple values involves cases where you can't tell whether an expression will return multiple values. Then you have, for example, (let ((val (expr)))...) and need to accept either a normal single value from expr, or one member of the possible set of multiple values. In lint.scm, I'm currently handling this with lambda: (let ((val ((lambda args (car args)) (expr))))...), but this feels kludgey. CL has nth-value which appears to do "the right thing" in this context; perhaps s7 needs it too.
;; The example the author gave isn't what this macro is trying to achieve, which is annoying

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; old code below ;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define-public-macro (with-global var val . body)
;;   (let ((old (gensym)) (new (gensym)))
;;     `(let ((,old ,var))
;;        (set! ,var ,val)
;;        (let ((,new (begin ,@body)))
;;          (set! ,var ,old)
;;          ,new))))

(define-public-macro (with-global var val . body)
  (let ((old (gensym)) (new (gensym)))
    `(let ((,old ,var))
       (set! ,var ,val)
       (let ((,new (list (begin ,@body))))
         (set! ,var ,old)
         (apply-values ,new)))))


(define-public-macro (and-with var val . body)
  `(with ,var ,val
     (and ,var (begin ,@body))))

(define-public-macro (with-result result . body)
  `(let* ((return ,result)
          (dummy (begin ,@body)))
     return))

(define (range-list start end delta)
  (if (< start end)
      (cons start (range-list (+ start delta) end delta))
      '()))

(define (range-list* start end delta)
  (if (<= start end)
      (cons start (range-list* (+ start delta) end delta))
      '()))

(define-public (.. start end . delta)
  (if (null? delta)
      (range-list start end 1)
      (range-list start end (car delta))))

(define-public (... start end . delta)
  (if (null? delta)
      (range-list* start end 1)
      (range-list* start end (car delta))))

(define-public-macro (for what . body)
  (let ((n (length what)))
    (cond ((== n 2)
           ;; range over values of a list
           `(for-each (lambda (,(car what)) ,@body)
                      ,(cadr what)))
          ((== n 3)
           ;; range over values from start to end with step 1
           `(do ((,(car what) ,(cadr what) (+ ,(car what) 1)))
                ((>= ,(car what) ,(caddr what)) (noop))
              ,@body))
          ((== n 4)
           ;; range over values from start to end with step
           `(if (> ,(cadddr what) 0)
                (do ((,(car what) ,(cadr what) (+ ,(car what) ,(cadddr what))))
                    ((>= ,(car what) ,(caddr what)) (noop))
                  ,@body)
                (do ((,(car what) ,(cadr what) (+ ,(car what) ,(cadddr what))))
                    ((<= ,(car what) ,(caddr what)) (noop))
                  ,@body)))
          ((== n 5)
           ;; range over values from start to end with step and comparison
           `(do ((,(car what) ,(cadr what) (+ ,(car what) ,(cadddr what))))
                ((not (,(car (cddddr what)) ,(car what) ,(caddr what))) (noop))
              ,@body))
          (else '(noop)))))

(define-public-macro (repeat n . body)
  (let ((x (gensym)))
    `(for (,x 0 ,n) ,@body)))

(define-public-macro (twice . body)
  `(begin ,@body ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Small rewritings on top of C++ interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (path->tree p)
  (and (path-exists? p) (cpp-path->tree p)))

(define-public selection-active? selection-active-any?)

(define-public (selection-active-non-small?)
  (and (selection-active?)
       (not (selection-active-small?))))

(define-public (selection-active-large?)
  (and (selection-active?)
       (not (selection-active-small?))
       (not (selection-active-table?))))

(define-public (go-to p)
  (let* ((r (buffer-path))
         (lp (length p))
         (lr (length r)))
    (and (or (and (<= lr lp) (== (sublist p 0 lr) r))
             (and-with buf (path->buffer p)
               (switch-to-buffer buf) #t))
         (go-to-path p))))

(define-public (choose-file fun title type . opts)
  (when (null? opts)
    (with prompt (cond ((string-starts? title "Save") "Save as:")
		       ((string-starts? title "Export") "Export as:")
		       ((== title "Select database") "Selected database:")
		       (else ""))
      (set! opts (list prompt))))
  (when (null? (cdr opts))
    (with u (buffer-get-master (current-buffer))
      (set! opts (list (car opts) u))))
  (cpp-choose-file fun title type (car opts) (cadr opts)))

(define-public (alt-windows-delete l)
  (for-each alt-window-delete l))

(define-public (qt4-gui?) (== (gui-version) "qt4"))
(define-public (qt4-or-later-gui?) (in? (gui-version) (list "qt4" "qt5" "qt6")))
(define-public (qt5-gui?) (== (gui-version) "qt5"))
(define-public (qt5-or-later-gui?) (in? (gui-version) (list "qt5" "qt6")))
(define-public (qt6-gui?) (== (gui-version) "qt6"))
(define-public (qt6-or-later-gui?) (in? (gui-version) (list "qt6")))
