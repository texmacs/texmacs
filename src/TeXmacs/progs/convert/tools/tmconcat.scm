
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmconcat.scm
;; DESCRIPTION : manipulation of concatenations
;; COPYRIGHT   : (C) 2003  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools tmconcat)
  (:export
    tmconcat-tokenize-math
    tmconcat-structure-tabs
    tmconcat-structure-brackets
    tmconcat-structure-scripts))

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

(define (tmconcat-tabs-sub head tail where)
  ;; FIXME: should better take into account the different types of tabs
  (cond ((null? tail) `((,where ,@head)))
	((and (tab-left? (caar tail))
	      (or (and (== where '!middle) (> (length tail) 1))
		  (> (length tail) 2)))
	 (tmconcat-tabs-make head tail where where))
	((tab-right? (caar tail))
	 (tmconcat-tabs-make head tail where '!right))
	((and (== where '!left) (> (length tail) 1))
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
      (and (string? x) (== (math-symbol-type x) "opening bracket"))))

(define (tmconcat-closing? x)
  (or (func? x 'right)
      (and (string? x) (== (math-symbol-type x) "closing bracket"))))

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

(tm-define (tmconcat-structure-scripts l)
  ;; used for instance in MathML generation
  (:type (forall T (-> (list T) (list T))))
  (:synopsis "Group scripts in @l.")
  (cond ((null? l) l)
	((match? l '(:1 (rsub :1) (rsup :1) :*))
	 (cons (list 'rsubsup! (car l) (cadadr l) (cadr (caddr l)))
	       (tmconcat-structure-scripts (cdddr l))))
	((match? l '(:1 (rsup :1) (rsub :1) :*))
	 (cons (list 'rsubsup! (car l) (cadr (caddr l)) (cadadr l))
	       (tmconcat-structure-scripts (cdddr l))))
	((match? l '(:1 (rsub :1) :*))
	 (cons (list 'rsub! (car l) (cadadr l))
	       (tmconcat-structure-scripts (cddr l))))
	((match? l '(:1 (rsup :1) :*))
	 (cons (list 'rsup! (car l) (cadadr l))
	       (tmconcat-structure-scripts (cddr l))))
	((func? (car l) 'rsub)
	 (cons (list 'rsub! "" (cadar l))
	       (tmconcat-structure-scripts (cdr l))))
	((func? (car l) 'rsup)
	 (cons (list 'rsup! "" (cadar l))
	       (tmconcat-structure-scripts (cdr l))))
	(else (cons (car l) (tmconcat-structure-scripts (cdr l))))))
