
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-brackets.scm
;; DESCRIPTION : quotes and auto-close brackets
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs tools tm-bracket)
  (:export
    insert-quote
    make-bracket-open make-separator make-bracket-close make-big-operator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User preferences for quoting and bracket behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define quoting-style "default")
(define auto-close-brackets? #f)

(define (notify-quoting-style var val)
  (set! quoting-style val))

(define (notify-auto-close-brackets var val)
  (set! auto-close-brackets? (== val "on")))

(define-preferences
  ("automatic quotes" "default" notify-quoting-style)
  ("automatically close brackets" "off" notify-auto-close-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (close-quotes?)
  (let* ((p (tm-where))
	 (t (tree->object (subtree (the-buffer) (cDr p)))))
    (if (string? t)
	(not (or (== t "") (string-ends? t " ")))
	(> (cAr p) 0))))

(define (open-quotes lan)
  (cond ((== lan "none") (insert-object "\""))
	((== lan "french") (insert-object " "))
	((== lan "german") (insert-object ""))
	((== lan "dutch") (insert-object ""))
	((== lan "swiss") (insert-object ""))
	(else (insert-object "``"))))

(define (close-quotes lan)
  (cond ((== lan "none") (insert-object "\""))
	((== lan "french") (insert-object " "))
	((== lan "german") (insert-object "``"))
	((== lan "dutch") (insert-object "''"))
	((== lan "swiss") (insert-object ""))
	(else (insert-object "''"))))

(define (insert-quote-both lan)
  (cond ((== lan "none") (insert-object "\""))
	((== lan "french") (insert-object-go-to "  " '(2)))
	((== lan "german") (insert-object-go-to "``" '(1)))
	((== lan "dutch") (insert-object-go-to "''" '(1)))
	((== lan "swiss") (insert-object-go-to "" '(1)))
	(else (insert-object-go-to "``''" '(2)))))

(define (insert-quote-sub lan)
  (cond (auto-close-brackets? (insert-quote-both lan))
	((close-quotes?) (close-quotes lan))
	(else (open-quotes lan))))

(define (insert-quote)
  (if (== quoting-style "default")
      (insert-quote-sub (get-env "language"))
      (insert-quote-sub quoting-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-bracket-open l r . opt)
  (let ((large? (== opt '(#t)))
	(sel? (selection-active-normal?)))
    (if auto-close-brackets?
	(begin
	  (if sel? (clipboard-cut "temp"))
	  (if large?
	      (insert-object-go-to `(concat (left ,l) (right ,r)) '(1 0))
	      (insert-object-go-to (string-append l r)
				   (list (string-length l))))
	  (if sel? (clipboard-paste "temp")))
	(if large?
	    (insert-object `(left ,l))
	    (insert-object l)))))

(define (make-separator sep . opt)
  (let ((large? (== opt '(#t))))
    (if large?
	(insert-object `(mid ,sep))
	(insert-object sep))))

(define (make-bracket-close r l . opt)
  (let ((large? (== opt '(#t))))
    (if large?
	(insert-object `(right ,r))
	(insert-object r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-big-operator op)
  (let ((sel? (selection-active-normal?)))
    (if (and auto-close-brackets? (not (== op ".")))
	(begin
	  (if sel? (clipboard-cut "temp"))
	  (insert-object-go-to `(concat (big ,op) (big ".")) '(1 0))
	  (if sel? (clipboard-paste "temp")))
	(insert-object `(big ,op)))))
