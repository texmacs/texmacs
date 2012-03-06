
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmconcat.scm
;; DESCRIPTION : manipulation of concatenations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmconcat))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor for concatenations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tmconcat* . l)
  (:synopsis "Non-correcting constructor of horizontal concatenations.")
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (cons 'concat l))))

(tm-define (tmconcat-simplify l)
  (cond ((null? l) l)
        ((tm-atomic? (car l))
         (let* ((head (tm->string (car l)))
                (tail (tmconcat-simplify (cdr l))))
           (cond ((== head "") head)
                 ((and (nnull? tail) (string? (car tail)))
                  (cons (string-append head (car tail)) (cdr tail)))
                 (else (cons head tail)))))
	((tm-func? (car l) 'concat)
         (tmconcat-simplify (append (tm-cdr (car l)) (cdr l))))
        (else (cons (car l) (tmconcat-simplify (cdr l))))))

;; (tm-define (tmconcat . in)
;;   (:synopsis "Constructor of horizontal concatenations with corrections.")
;;   (let* ((l (tmconcat-simplify in))
;; 	 (o (length (list-filter l (lambda (x) (tm-func? x 'left)))))
;; 	 (c (length (list-filter l (lambda (x) (tm-func? x 'right))))))
;;     (if (> o c) (set! l (append l (make-list (- o c) '(right ".")))))
;;     (if (< o c) (set! l (append l '(right ".") (make-list (- o c)))))
;;     (apply tmconcat* l)))

(tm-define (tmconcat . in)
  (:synopsis "Constructor of horizontal concatenations with corrections.")
  (apply tmconcat* (tmconcat-simplify in)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacing mathematical string by list of tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-eat s pos plus pred?)
  (let eat ((end pos))
    (cond ((>= end (string-length s))
	   (cons (substring s pos end)
		 (tmconcat-math-sub s end)))
	  ((pred? (string-ref s end))
	   (cons (substring s pos (+ end plus))
		 (tmconcat-math-sub s (+ end plus))))
	  (else (eat (+ end 1))))))

(define (tmconcat-math-sub s pos)
  (if (>= pos (string-length s)) '()
      (with c (string-ref s pos)
	(cond ((== c #\<)
	       (tmconcat-eat s pos 1 (lambda (c) (== c #\>))))
	      ((char-numeric? c)
	       (tmconcat-eat s pos 0 (lambda (c) (not (char-numeric? c)))))
	      ((and (char-alphabetic? c)
		    (< (+ pos 1) (string-length s))
		    (char-alphabetic? (string-ref s (+ pos 1))))
	       (tmconcat-eat s pos 0 (lambda (c) (not (char-alphabetic? c)))))
	      (else (cons (substring s pos (+ pos 1))
			  (tmconcat-math-sub s (+ pos 1))))))))

(tm-define (tmconcat-tokenize-math s)
  (:type (-> string (list string)))
  (:synopsis "Decompose mathematical string @s into list of tokens.")
  (tmconcat-math-sub s 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replacing tab information by alignment information
;; The alignment is expressed using the !left, !middle and !right tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tab-left? tab) (== (cddr tab) 'last))
(define (tab-right? tab) (== (cddr tab) 'first))

(define (tmconcat-tabs-make head tail where next)
  (with (first . others) (tmconcat-tabs-sub (cdar tail) (cdr tail) next)
    (if (== where next)
	`((,where ,@head ,@(cdr first)) ,@others)
	`((,where ,@head) ,first ,@others))))

(define (tmconcat-tabs-filter l first?)
  (cond ((null? l) l)
	(first? (cons (car l) (tmconcat-tabs-filter (cdr l) #f)))
	((and (pair? (car l)) (pair? (caar l)) (== (cAr (caar l)) "first"))
	 (tmconcat-tabs-filter (cdr l) #f))
	(else (cons (car l) (tmconcat-tabs-filter (cdr l) #f)))))

(define (tmconcat-tabs-sub head tail where)
  ;; FIXME: should better take into account the different types of tabs
  (cond ((null? tail) `((,where ,@head)))
	((and (tab-left? (caar tail))
	      (or (and (== where '!middle) (> (length tail) 1))
		  (> (length tail) 2)))
	 (tmconcat-tabs-make head tail where where))
	((tab-right? (caar tail))
	 (tmconcat-tabs-make head tail where '!right))
	((and (== where '!left) (> (length (tmconcat-tabs-filter tail #t)) 1))
	 (tmconcat-tabs-make head tail where '!middle))
	(else (tmconcat-tabs-make head tail where '!right))))

(tm-define (tmconcat-structure-tabs l)
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Structure tabs in concatenation @l.")
  (with r (list-scatter l (lambda (x) (func? x 'htab)) #t)
    (if (null? (cdr r)) l
	(tmconcat-tabs-sub (car r) (cdr r) '!left))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-opening? x)
  (or (func? x 'left)
      (and (string? x) (== (math-symbol-type x) "opening-bracket"))))

(define (tmconcat-closing? x)
  (or (func? x 'right)
      (and (string? x) (== (math-symbol-type x) "closing-bracket"))))

(define (tmconcat-brackets-sub l)
  ;; used for instance in MathML generation
  (cond ((null? l) (values l l))
	((tmconcat-opening? (car l))
	 (receive (r tail) (tmconcat-brackets-sub (cdr l))
	   (receive (r2 tail2) (tmconcat-brackets-sub tail)
	     (values (cons (cons* 'concat! (car l) r) r2) tail2))))
	((tmconcat-closing? (car l))
	 (values (list (car l)) (cdr l)))
	(else
	 (receive (r tail) (tmconcat-brackets-sub (cdr l))
	   (values (cons (car l) r) tail)))))

(tm-define (tmconcat-structure-brackets l)
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Recursively group matching brackets in concatenation @l.")
  (receive (r tail) (tmconcat-brackets-sub l)
    (if (null? tail) r
	(append r (tmconcat-structure-brackets tail)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Grouping scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmconcat-structure-scripts-sub l)
  (cond ((null? l) l)
	((func? (car l) 'rsub)
	 (cons (list 'rsub! "" (cadar l))
	       (tmconcat-structure-scripts-sub (cdr l))))
	((func? (car l) 'rsup)
	 (cons (list 'rsup! "" (cadar l))
	       (tmconcat-structure-scripts-sub (cdr l))))
	((match? l '(:%1 (rsub :%1) (rsup :%1) :*))
	 (cons (list 'rsubsup! (car l) (cadadr l) (cadr (caddr l)))
	       (tmconcat-structure-scripts-sub (cdddr l))))
	((match? l '(:%1 (rsup :%1) (rsub :%1) :*))
	 (cons (list 'rsubsup! (car l) (cadr (caddr l)) (cadadr l))
	       (tmconcat-structure-scripts-sub (cdddr l))))
	((match? l '(:%1 (rsub :%1) :*))
	 (cons (list 'rsub! (car l) (cadadr l))
	       (tmconcat-structure-scripts-sub (cddr l))))
	((match? l '(:%1 (rsup :%1) :*))
	 (cons (list 'rsup! (car l) (cadadr l))
	       (tmconcat-structure-scripts-sub (cddr l))))
	((or (func? (car l) 'lsub) (func? (car l) 'lsup))
	 (let* ((r (tmconcat-structure-scripts-sub (cdr l)))
		(s (if (== (caar l) 'lsub) 'lsub! 'lsup!)))
	   (cond ((null? r) (list (list s "" (cadar l))))
		 ((and (== s 'lsub!) (func? (car r) 'lsup!))
		  (cons (list 'lsubsup! (cadar r) (cadar l) (caddar r))
			(cdr r)))
		 ((and (== s 'lsup!) (func? (car r) 'lsub!))
		  (cons (list 'lsubsup! (cadar r) (caddar r) (cadar l))
			(cdr r)))
		 (else (cons (list s (car r) (cadar l)) (cdr r))))))
	(else (cons (car l) (tmconcat-structure-scripts-sub (cdr l))))))

(define (tmconcat-simplify-scripts l)
  (cond ((null? l) l)
	((func? (car l) 'lprime)
	 (tmconcat-simplify-scripts (cons (cons 'lsup (cdar l)) (cdr l))))
	((func? (car l) 'rprime)
	 (tmconcat-simplify-scripts (cons (cons 'rsup (cdar l)) (cdr l))))
	((null? (cdr l)) l)
	((and (pair? (car l)) (pair? (cadr l))
	      (in? (caar l) '(lsub lsup rsub rsup))
	      (== (caar l) (caadr l)))
	 (tmconcat-simplify-scripts
	  `((,(caar l) (concat ,(cadar l) ,(cadadr l))) ,@(cddr l))))
	(else (cons (car l) (tmconcat-simplify-scripts (cdr l))))))

(tm-define (tmconcat-structure-scripts l)
  ;; used for instance in MathML generation
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Group scripts in @l.")
  (tmconcat-structure-scripts-sub (tmconcat-simplify-scripts l)))
