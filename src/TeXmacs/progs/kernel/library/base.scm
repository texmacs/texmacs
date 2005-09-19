
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : base.scm
;; DESCRIPTION : frequently used Scheme subroutines
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel library base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xor-sub l)
  (cond ((null? l) #f)
	((car l) (not (xor-sub (cdr l))))
	(else (xor-sub (cdr l)))))

(define-public (xor . l)
  "Exclusive or of all elements in @l."
  (xor-sub l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: guile-1.6.0 implements SRFI-13 (string library) in C.

(define-public (char->string c)
  "Convert @c to a string"
  (list->string (list c)))

(define-public (string-tail s n)
  "Return all but the first @n chars of @s."
  (substring s n (string-length s)))

(define-public (char-in-string? c s)
  "Test whether @c occurs in @s"
  (!= (string-index s c) #f))

(define-public (string-starts? s what)
  "Test whether @s starts with @what."
  (let ((n (string-length s))
	(k (string-length what)))
    (and (>= n k) (== (substring s 0 k) what))))

(define-public (string-ends? s what)
  "Test whether @s ends with @what."
  (let ((n (string-length s))
	(k (string-length what)))
    (and (>= n k) (== (substring s (- n k) n) what))))

(define-public (string-contains? s what)
  "Test whether @s contains @what as a substring."
  (>= (string-search-forwards what 0 s) 0))

(define-public (force-string s)
  "Return @s if @s is a string and the empty string otherwise"
  (if (string? s) s ""))

(define-public (reverse-list->string cs)	; srfi-13
  "Efficient implementation of (compose list->string reverse)."
  ;; Not yet any more efficient, but this may be fixed in the future.
  (list->string (reverse cs)))

(define-public (string-join	ss . opt)	; srfi-13 (subset)
  "Concatenate elements of @ss inserting separators."
  (if (null? opt) (string-join ss " ")
      (string-concatenate (list-intersperse ss (car opt)))))

(define-public (string-drop-right s n)	; srfi-13
  "Return all but the last @n chars of @s."
  (substring s 0 (- (string-length s) n)))

(define-public string-drop string-tail)	; srfi-13

(define-public (string-take s n)		; srfi-13
  "Return the first @n chars of @s."
  (substring s 0 n))

(define-public (string-trim s)		; srfi-13 (subset)
  "Remove whitespace at start of @s."
  (list->string (list-drop-while (string->list s) char-whitespace?)))

(define-public (list-drop-right-while l pred)
  (reverse! (list-drop-while (reverse l) pred)))

(define-public (string-trim-right s)	; srfi-13 (subset)
  "Remove whitespace at end of @s."
  (list->string (list-drop-right-while (string->list s) char-whitespace?)))

(define-public (string-trim-both s)		; srfi-13 (subset)
  "Remove whitespace at start and end of @s."
  (list->string
   (list-drop-right-while
    (list-drop-while (string->list s) char-whitespace?)
    char-whitespace?)))

(define-public (string-concatenate ss)	; srfi-13
  "Append the elements of @ss toghether."
  ;; WARNING: not portable for long lists
  (apply string-append ss))

(define-public (string-map proc s) 		; srfi-13 (subset)
  "Map @proc on every char of @s."
  (list->string (map proc (string->list s))))

(define-public (string-fold kons knil s) 	; srfi-13 (subset))
  "Fundamental string iterator."
  (list-fold kons knil (string->list s)))

(define-public (string-fold-right kons knil s) ; srfi-13 (subset)
  "Right to left fundamental string iterator."
  (list-fold-right kons knil (string->list s)))

(define (string-split-lines/kons c cs+lines)
  (if (== c #\newline)
      (cons '() cs+lines)
      (cons (cons c (car cs+lines)) (cdr cs+lines))))

(define-public (string-split-lines s)
  "List of substrings of @s separated by newlines."
  (map list->string
       (list-fold-right string-split-lines/kons '(()) (string->list s))))

(define-public (string-tokenize s c)
  "Cut string @s into pieces using @c as a separator."
  (with d (string-index s c)
    (if d
	(cons (substring s 0 d)
	      (string-tokenize (substring s (+ 1 d) (string-length s)) c))
	(list s))))

(define-public (string-tokenize-n s c n)
  "As @string-tokenize, but only cut first @n pieces"
  (with d (string-index s c)
    (if (or (= n 0) (not d))
	(list s)
	(cons (substring s 0 d)
	      (string-tokenize-n (substring s (+ 1 d) (string-length s))
				 c
				 (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some string-like functions on symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (symbol-starts? s1 s2)
  (string-starts? (symbol->string s1) (symbol->string s2)))

(define-public (symbol-ends? s1 s2)
  (string-ends? (symbol->string s1) (symbol->string s2)))

(define-public (symbol-drop-right s n)
  (string->symbol (string-drop-right (symbol->string s) n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (compose g f)
  "Compose the functions @f and @g"
  (lambda x (g (apply f x))))

(define-public (non pred?)
  "Return the negation of @pred?."
  (lambda args (not (apply pred? args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (string->object s)
  "Parse @s and build scheme object"
  (call-with-input-string s read))

(define-public (func? x f . opts)
  "Is @x a list with first stree @f? Optionally test the length of @x."
  (let ((n (length opts)))
    (cond ((= n 0) (and (list? x) (nnull? x) (== (car x) f)))
	  ((= n 1)
	   (let ((nn (car opts)))
             (and (list? x) (nnull? x)
                  (== (car x) f) (= (length x) (+ nn 1)))))
	  (else (error "Too many arguments.")))))

(define-public (tuple? x . opts)
  "Equivalent to @list? without options or to @func? otherwise"
  (if (null? opts)
      (list? x)
      (apply func? (cons x opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (position-new . opts)
  (position-new-path (if (null? opts) (cursor-path) (car opts))))
