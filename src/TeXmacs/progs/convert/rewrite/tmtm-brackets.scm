
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tmtm-brackets.scm
;; DESCRIPTION : add missing brackets
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rewrite tmtm-brackets)
  (:export tmtm-match-brackets))

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
  (cond ((not (pair? l)) l)
	((== (car l) 'concat)
	 (let ((complete (tmtm-match-brackets-concat (cdr l))))
	   (cons 'concat (map tmtm-match-brackets-bis complete))))
	(else (cons (car l) (map tmtm-match-brackets (cdr l))))))

(define (tmtm-match-brackets l)
  "Add missing brackets to TeXmacs object @l."
  (cond ((not (pair? l)) l)
	((func? l 'left 1) `(concat ,l (right ".")))
	((func? l 'right 1) `(concat (left ".") ,l))
	(else (tmtm-match-brackets-bis l))))
