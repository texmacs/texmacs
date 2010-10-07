
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

(define (make-large l s)
  (cond ((nstring? s) `(,l "."))
	((<= (string-length s) 1) `(,l ,s))
	((== s "<nomid>") `(,l "."))
	((and (string-starts? s "<") (string-ends? s ">"))
	 `(,l ,(substring s 1 (- (string-length s) 1))))
	(else `(,l "."))))

(tm-define (downgrade-brackets t)
  (with cc (lambda (x) (if (func? x 'concat) (cdr x) (list x)))
    (cond ((func? t 'around 3)
	   `(concat ,(cadr t) ,@(cc (caddr t)) ,(cadddr t)))
	  ((func? t 'around* 3)
	   `(concat ,(make-large 'left (cadr t))
		    ,@(cc (caddr t))
		    ,(make-large 'right (cadddr t))))
	  ((func? t 'big-around 2)
	   `(concat ,(make-large 'big (cadr t)) ,@(cc (caddr t))))
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
