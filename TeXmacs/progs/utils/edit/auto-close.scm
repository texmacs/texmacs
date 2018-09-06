
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-close.scm
;; DESCRIPTION : quotes and auto-close brackets
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils edit auto-close))

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
  ("text spacebar" "default" ignore)
  ("math spacebar" "default" ignore)
  ("automatic quotes" "default" notify-quoting-style)
  ("automatic brackets" "mathematics" notify-auto-close-brackets)
  ("use large brackets" "on" (lambda args (noop))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (close-quotes?)
  (let* ((p (cursor-path))
	 (t (tree->stree (path->tree (cDr p)))))
    (if (string? t)
	(and (!= t "")
	     (!= (cAr p) 0)
	     (nin? (string-ref t (- (cAr p) 1))
                   '(#\space #\( #\[ #\{ #\' #\- #\/ #\\)))
	(> (cAr p) 0))))

(define (open-quotes lan)
  (cond ((== lan "none") (insert "\""))
	((== lan "danish") (insert ""))
	((== lan "dutch") (insert ""))
	((== lan "french") (insert " "))
	((== lan "german") (insert ""))
	((== lan "spanish") (insert ""))
	((== lan "swiss") (insert ""))
	((== lan "polish") (insert ""))
	(else (insert ""))))

(define (close-quotes lan)
  (cond ((== lan "none") (insert "\""))
	((== lan "danish") (insert ""))
	((== lan "dutch") (insert ""))
	((== lan "french") (insert " "))
	((== lan "german") (insert ""))
	((== lan "spanish") (insert ""))
	((== lan "swiss") (insert ""))
	((== lan "polish") (insert ""))
	(else (insert ""))))

(define (insert-quote-both lan)
  (cond ((== lan "none") (insert "\""))
	((== lan "danish") (insert-go-to "" '(1)))
	((== lan "dutch") (insert-go-to "" '(1)))
	((== lan "french") (insert-go-to "  " '(2)))
	((== lan "german") (insert-go-to "" '(1)))
	((== lan "spanish") (insert-go-to "" '(1)))
	((== lan "swiss") (insert-go-to "" '(1)))
	((== lan "polish") (insert-go-to "" '(1)))
	(else (insert-go-to "" '(1)))))

(define (insert-quote-sub lan)
  (cond (auto-close-brackets? (insert-quote-both lan))
	((close-quotes?) (close-quotes lan))
	(else (open-quotes lan))))

(tm-define (insert-quote)
  (if (== quoting-style "default")
      (insert-quote-sub (get-env "language"))
      (insert-quote-sub quoting-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart apostrophes (or closing single quotes when appropriate)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-opening-quote* s i)
  (let* ((open  (string-search-backwards "<#2018>" i s))
         (close (string-search-backwards "<#2019>" i s)))
    (and (>= open 0) (or (< close 0) (< close open)) open)))

(define (find-opening-quote t i j)
  (cond ((and (string? (tm-ref t i))
              (find-opening-quote* (tm-ref t i) j)) #t)
        ((and (string? (tm-ref t i))
              (>= (string-search-forwards "<#2019>" 0 (tm-ref t i)) 0)) #f)
        ((and (> i 0)
              (let* ((s (tm-ref t (- i 1)))
                     (n (if (string? s) (string-length s) 0)))
                (find-opening-quote t (- i 1) n))) #t)
        (else #f)))

(define (search-opening-quote)
  (cond ((and (tree-up (cursor-tree))
              (tm-func? (tree-up (cursor-tree)) 'concat))
         (find-opening-quote (tm->stree (tree-up (cursor-tree)))
                             (tree-index (cursor-tree))
                             (cAr (cursor-path))))
        ((tm-atomic? (cursor-tree))
         (find-opening-quote* (tm->stree (cursor-tree))
                              (cAr (cursor-path))))
        (else #f)))

(tm-define (insert-apostrophe flag?)
  (if (xor flag? (nnot (search-opening-quote)))
      (insert "<#2019>")
      (insert "'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-bracket-open l r . opt)
  (let ((large? (== opt '(#t)))
	(sel? (selection-active-normal?)))
    (if auto-close-brackets?
	(begin
	  (if sel? (clipboard-cut "temp"))
	  (if large?
	      (insert-go-to `(concat (left ,l) (right ,r)) '(1 0))
	      (insert-go-to (string-append l r) (list (string-length l))))
	  (if sel? (clipboard-paste "temp")))
	(if large?
	    (insert `(left ,l))
	    (insert l)))))

(tm-define (make-separator sep . opt)
  (let ((large? (== opt '(#t))))
    (if large?
	(insert `(mid ,sep))
	(insert sep))))

(tm-define (make-bracket-close r l . opt)
  (let ((large? (== opt '(#t))))
    (if large?
	(insert `(right ,r))
	(insert r))))

(tm-define (test-matching-brackets?)
  (!= (get-preference "automatic brackets") "off"))

(tm-define (toggle-matching-brackets)
  (:check-mark "v" test-matching-brackets?)
  (set-preference "automatic brackets"
                  (if (== (get-preference "automatic brackets") "off")
                      "mathematics"
                      "off")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-big-operator op)
  (let ((sel? (selection-active-normal?)))
    (if (and auto-close-brackets? (!= op "."))
	(begin
	  (if sel? (clipboard-cut "temp"))
	  (insert-go-to `(concat (big ,op) (big ".")) '(1 0))
	  (if sel? (clipboard-paste "temp")))
	(insert `(big ,op)))))
