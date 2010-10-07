
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-brackets.scm
;; DESCRIPTION : add missing brackets
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform into old-style brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (large-bracket l s)
  (cond ((nstring? s) `(,l "."))
	((<= (string-length s) 1) `(,l ,s))
	((== s "<nomid>") `(,l "."))
	((and (string-starts? s "<") (string-ends? s ">"))
	 `(,l ,(substring s 1 (- (string-length s) 1))))
	(else `(,l "."))))

(tm-define (small-bracket s)
  (cond ((or (func? s 'left) (func? s 'mid) (func? s 'right) (func? s 'big))
	 (small-bracket (cadr s)))
	((nstring? s) "<nomid>")
	((== s ".") "<nomid>")
	((<= (string-length s) 1) s)
	(else (string-append "<" s ">"))))

(tm-define (downgrade-brackets t)
  (with cc (lambda (x) (if (func? x 'concat) (cdr x) (list x)))
    (cond ((func? t 'around 3)
	   `(concat ,(cadr t) ,@(cc (caddr t)) ,(cadddr t)))
	  ((func? t 'around* 3)
	   `(concat ,(large-bracket 'left (cadr t))
		    ,@(cc (caddr t))
		    ,(large-bracket 'right (cadddr t))))
	  ((func? t 'big-around 2)
	   `(concat ,(large-bracket 'big (cadr t)) ,@(cc (caddr t))))
	  (else t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket matching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tmtm-match-brackets-sub l level)
  ;; Returns (l' level') with
  ;;   l     : list to complete
  ;;   level : unmatched number of left brackets
  ;;   l'    : completed list
  ;;   level': unmatched number of right brackets
  (cond ((null? l) (list (make-list level '(right ".")) 0))
	((func? (car l) 'left 1)
	 (let ((result (tmtm-match-brackets-sub (cdr l) (+ level 1))))
	   (list (cons (car l) (car result)) (cadr result))))
	((and (func? (car l) 'right 1) (> level 0))
	 (let ((result (tmtm-match-brackets-sub (cdr l) (- level 1))))
	   (list (cons (car l) (car result)) (cadr result))))
	((func? (car l) 'right 1)
	 (let ((result (tmtm-match-brackets-sub (cdr l) 0)))
	   (list (cons (car l) (car result)) (+ (cadr result) 1))))
	(else
	 (let ((result (tmtm-match-brackets-sub (cdr l) level)))
	   (list (cons (car l) (car result)) (cadr result))))))

(define (tmtm-match-brackets-concat l)
  (let ((result (tmtm-match-brackets-sub l 0)))
    (append (make-list (cadr result) '(left ".")) (car result))))

(define (tmtm-match-brackets-bis l)
  (cond ((npair? l) l)
	((== (car l) 'concat)
	 (let ((complete (tmtm-match-brackets-concat (cdr l))))
	   (cons 'concat (map tmtm-match-brackets-bis complete))))
	((or (func? l 'around 3) (func? l 'around* 3) (func? l 'big-around 2))
	 (tmtm-match-brackets (downgrade-brackets l)))
	(else (cons (car l) (map tmtm-match-brackets (cdr l))))))

(tm-define (tmtm-match-brackets l)
  "Add missing brackets to TeXmacs stree @l."
  (cond ((npair? l) l)
	((func? l 'left 1) `(concat ,l (right ".")))
	((func? l 'right 1) `(concat (left ".") ,l))
	(else (tmtm-match-brackets-bis l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions for big operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (big-name b)
  (and (func? b 'big-around 2)
       (cadr (large-bracket 'big (cadr b)))))

(tm-define (big-subscript b)
  (and (func? b 'big-around 2)
       (with cc (caddr b)
	 (and (func? cc 'concat)
	      (>= (length cc) 3)
	      (or (and (func? (cadr cc) 'rsub 1)
		       (cadr (cadr cc)))
		  (and (func? (cadr cc) 'rsup 1)
		       (func? (caddr cc) 'rsub 1)
		       (cadr (caddr cc))))))))

(tm-define (big-supscript b)
  (and (func? b 'big-around 2)
       (with cc (caddr b)
	 (and (func? cc 'concat)
	      (>= (length cc) 3)
	      (or (and (func? (cadr cc) 'rsup 1)
		       (cadr (cadr cc)))
		  (and (func? (cadr cc) 'rsub 1)
		       (func? (caddr cc) 'rsup 1)
		       (cadr (caddr cc))))))))

(define (remove-scripts l)
  (cond ((null? l) l)
	((func? (car l) 'rsub 1) (remove-scripts (cdr l)))
	((func? (car l) 'rsup 1) (remove-scripts (cdr l)))
	(else l)))

(tm-define (big-body b)
  (and (func? b 'big-around 2)
       (with cc (caddr b)
	 (if (not (func? cc 'concat)) cc
	     (with l (remove-scripts (cdr cc))
	       (cond ((null? l) "")
		     ((list-1? l) (car l))
		     (else `(concat ,@l))))))))
