
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-dialogue.scm
;; DESCRIPTION : Interactive dialogues between Scheme and C++
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-dialogue)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Questions with user interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (user-ask prompt cont)
  (tm-interactive cont
    (if (string? prompt)
        (list (build-interactive-arg prompt))
        (list prompt))))

(define-public (user-confirm prompt default cont)
  (let ((k (lambda (answ) (cont (yes? answ)))))
    (if default
        (user-ask (list prompt "question" (translate "yes") (translate "no")) k)
        (user-ask (list prompt "question" (translate "no") (translate "yes")) k))))

(define-public (user-url prompt type cont)
  (user-delayed (lambda () (choose-file cont prompt type))))

(define-public (user-delayed cont)
  (exec-delayed cont))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delayed execution of commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (delayed-sub body)
  (cond ((or (npair? body) (nlist? (car body)) (not (keyword? (caar body))))
         `(lambda () ,@body #t))
        ((== (caar body) :pause)
         `(let* ((start (texmacs-time))
                 (proc ,(delayed-sub (cdr body))))
            (lambda ()
              (with left (- (+ start ,(cadar body)) (texmacs-time))
                (if (> left 0) left
                    (begin
                      (set! start (texmacs-time))
                      (proc)))))))
        ((== (caar body) :every)
         `(let* ((time (+ (texmacs-time) ,(cadar body)))
                 (proc ,(delayed-sub (cdr body))))
            (lambda ()
              (with left (- time (texmacs-time))
                (if (> left 0) left
                    (begin
                      (set! time (+ (texmacs-time) ,(cadar body)))
                      (proc)))))))
        ((== (caar body) :idle)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              (with left (- ,(cadar body) (idle-time))
                (if (> left 0) left
                    (proc))))))
        ((== (caar body) :refresh)
         (with sym (gensym)
           `(let* ((,sym #f)
                   (proc ,(delayed-sub (cdr body))))
              (lambda ()
                (if (!= ,sym (change-time)) 0
                    (with left (- ,(cadar body) (idle-time))
                      (if (> left 0) left
                          (begin
                            (set! ,sym (change-time))
                            (proc)))))))))
        ((== (caar body) :require)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              (if (not ,(cadar body)) 0
                  (proc)))))
        ((== (caar body) :while)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              (if (not ,(cadar body)) #t
                  (with left (proc)
                    (if (== left #t) 0 left))))))
        ((== (caar body) :clean)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              (with left (proc)
                (if (!= left #t) left
                    (begin ,(cadar body) #t))))))
        ((== (caar body) :permanent)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              (with left (proc)
                (if (!= left #t) left
                    (with next ,(cadar body)
                      (if (!= next #t) #t
                          0)))))))
        ((== (caar body) :do)
         `(with proc ,(delayed-sub (cdr body))
            (lambda ()
              ,(cadar body)
              (proc))))
        (else (delayed-sub (cdr body)))))

(define-public-macro (delayed . body)
  `(exec-delayed-pause ,(delayed-sub body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Messages and feedback on the status bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public message-serial 0)

(define-public (set-message-notify)
  (set! message-serial (+ message-serial 1)))

(define-public (recall-message-after len)
  (with current message-serial
    (delayed
      (:idle len)
      (when (== message-serial current)
        (recall-message)))))

(define-public (set-temporary-message left right len)
  (set-message-temp left right #t)
  (recall-message-after len))

(define-public (texmacs-banner)
  (with tmv (string-append "GNU TeXmacs " (texmacs-version))
    (delayed
     (set-message "Welcome to GNU TeXmacs" tmv)
     (delayed
     (:pause 5000)
     (set-message "GNU TeXmacs falls under the GNU general public license" tmv)
     (delayed
     (:pause 2500)
     (set-message "GNU TeXmacs comes without any form of legal warranty" tmv)
     (delayed
     (:pause 2500)
     (set-message
      "More information about GNU TeXmacs can be found in the Help->About menu"
      tmv)
     (delayed
     (:pause 2500)
     (set-message "" ""))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define interactive-arg-table (make-ahash-table))

(define (list-but l1 l2)
  (cond ((null? l1) l1)
        ((in? (car l1) l2) (list-but (cdr l1) l2))
        (else (cons (car l1) (list-but (cdr l1) l2)))))

(define (as-stree x)
  (cond ((tree? x) (tree->stree x))
        ((== x #f) "false")
        ((== x #t) "true")
        (else x)))

(define-public (procedure-symbol-name fun)
  (cond ((symbol? fun) fun)
        ((string? fun) (string->symbol fun))
        ((and (procedure? fun) (procedure-name fun)) => identity)
        (else #f)))

(define-public (procedure-string-name fun)
  (and-with name (procedure-symbol-name fun)
    (symbol->string name)))

(define-public (learn-interactive fun assoc-t)
  "Learn interactive values for @fun"
  (set! assoc-t (map (lambda (x) (cons (car x) (as-stree (cdr x)))) assoc-t))
  (set! fun (procedure-symbol-name fun))
  (when (symbol? fun)
    (let* ((l1 (or (ahash-ref interactive-arg-table fun) '()))
           (l2 (cons assoc-t (list-but l1 (list assoc-t)))))
      (ahash-set! interactive-arg-table fun l2))))

(define-public (learned-interactive fun)
  "Return learned list of interactive values for @fun"
  (set! fun (procedure-symbol-name fun))
  (or (ahash-ref interactive-arg-table fun) '()))

(define-public (forget-interactive fun)
  "Forget interactive values for @fun"
  (set! fun (procedure-symbol-name fun))
  (when (symbol? fun)
    (ahash-remove! interactive-arg-table fun)))

(define (learned-interactive-arg fun nr)
  (let* ((l (learned-interactive fun))
         (arg (number->string nr))
         (extract (lambda (assoc-l) (assoc-ref assoc-l arg))))
    (map extract l)))

(define (compute-interactive-arg-text fun which)
  (with arg (property fun (list :argument which))
    (cond ((npair? arg) (upcase-first (symbol->string which)))
          ((and (string? (car arg)) (null? (cdr arg))) (car arg))
          ((string? (cadr arg)) (cadr arg))
          (else (upcase-first (symbol->string which))))))

(define (compute-interactive-arg-type fun which)
  (with arg (property fun (list :argument which))
    (cond ((or (npair? arg) (npair? (cdr arg))) "string")
          ((string? (car arg)) (car arg))
          ((symbol? (car arg)) (symbol->string (car arg)))
          (else "string"))))

(define (compute-interactive-arg-proposals fun which)
  (let* ((default (property fun (list :default which)))
         (proposals (property fun (list :proposals which)))
         (learned '()))
    (cond ((procedure? default) (list (default)))
          ((procedure? proposals) (proposals))
          (else '()))))

(define (compute-interactive-arg fun which)
  (cons (compute-interactive-arg-text fun which)
        (cons (compute-interactive-arg-type fun which)
              (compute-interactive-arg-proposals fun which))))

(define (compute-interactive-args-try-hard fun)
  (with src (procedure-source fun)
    (if (and (pair? src) (== (car src) 'lambda)
             (pair? (cdr src)) (list? (cadr src)))
        (map upcase-first (map symbol->string (cadr src)))
        '())))

(define (compute-interactive-arg-list fun l)
  (if (npair? l) (list)
      (cons (compute-interactive-arg fun (car l))
            (compute-interactive-arg-list fun (cdr l)))))

(tm-define (compute-interactive-args fun)
  (with args (property fun :arguments)
    (if (not args)
        (compute-interactive-args-try-hard fun)
        (compute-interactive-arg-list fun args))))

(define (build-interactive-arg s)
  (cond ((string-ends? s ":") s)
        ((string-ends? s "?") s)
        (else (string-append s ":"))))

(tm-define (build-interactive-args fun l nr learned?)
  (cond ((null? l) l)
        ((string? (car l))
         (build-interactive-args
          fun (cons (list (car l) "string") (cdr l)) nr learned?))
        (else
         (let* ((name (build-interactive-arg (caar l)))
                (type (cadar l))
                (pl (cddar l))
                (ql pl)
                ;;(ql (if (null? pl) '("") pl))
                (ll (if learned? (learned-interactive-arg fun nr) '()))
                (rl (append ql (list-but ll ql)))
                (props (if (<= (length ql) 1) rl ql)))
           (cons (cons name (cons type props))
                 (build-interactive-args fun (cdr l) (+ nr 1) learned?))))))

(tm-define (interactive fun . args)
  (:synopsis "Call @fun with interactively specified arguments @args")
  (:interactive #t)
  (lazy-define-force fun)
  (if (null? args) (set! args (compute-interactive-args fun)))
  (with fun-args (build-interactive-args fun args 0 #t)
    (tm-interactive fun fun-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Store learned arguments from one session to another
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-learned)
  (with l (ahash-table->list interactive-arg-table)
    (save-object "$TEXMACS_HOME_PATH/system/interactive.scm" l)))

(define (ahash-set-2! t x)
  (with (key . l) x
    (with (form arg) key
      (with a (or (ahash-ref t form) '())
        (set! a (assoc-set! a arg l))
        (ahash-set! t form a)))))      

(define (rearrange-old x)
  (with (form . l) x
    (let* ((len (apply min (map length l)))
           (truncl (map (cut sublist <> 0 len) l))
           (sl (sort truncl (lambda (l1 l2) (< (car l1) (car l2)))))
           (nl (map (lambda (x) (cons (number->string (car x)) (cdr x))) sl))
           (build (lambda args (map cons (map car nl) args)))
           (r (apply map (cons build (map cdr nl)))))
      (cons form r))))

(define (decode-old l)
  (let* ((t (make-ahash-table))
         (setter (cut ahash-set-2! t <>)))
    (for-each setter l)
    (let* ((r (ahash-table->list t))
           (m (map rearrange-old r)))
      (list->ahash-table m))))

(define (retrieve-learned)
  (if (url-exists? "$TEXMACS_HOME_PATH/system/interactive.scm")
      (let* ((l (load-object "$TEXMACS_HOME_PATH/system/interactive.scm"))
             (old? (and (pair? l) (pair? (car l)) (list-2? (caar l))))
             (decode (if old? decode-old list->ahash-table)))
        (set! interactive-arg-table (decode l)))))

(on-entry (retrieve-learned))
(on-exit (save-learned))
