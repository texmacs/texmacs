
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
	((func? l 'around 3)
	 `(concat ,(cadr l) ,(tmtm-match-brackets (caddr l)) ,(cadddr l)))
	(else (cons (car l) (map tmtm-match-brackets (cdr l))))))

(tm-define (tmtm-match-brackets l)
  "Add missing brackets to TeXmacs stree @l."
  (cond ((npair? l) l)
	((func? l 'left 1) `(concat ,l (right ".")))
	((func? l 'right 1) `(concat (left ".") ,l))
	(else (tmtm-match-brackets-bis l))))
