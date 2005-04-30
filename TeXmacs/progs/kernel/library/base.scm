
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

(texmacs-module (kernel library base)
  (:use (kernel texmacs tm-define))
  (:export ;; booleans
    xor-sub xor
    ;; strings
    char->string string-tail char-in-string?
    string-starts? string-ends? string-contains?
    force-string reverse-list->string string-join
    string-drop-right string-drop string-take
    string-trim string-trim-right string-trim-both
    string-concatenate string-map string-fold string-fold-right
    string-split-lines string-tokenize string-tokenize-n
    ;; functions
    compose negate
    ;; dictionaries
    fill-dictionary-entry fill-dictionary
    ;; objects
    string->object func? tuple?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Booleans
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (xor-sub l)
  (cond ((null? l) #f)
	((car l) (not (xor-sub (cdr l))))
	(else (xor-sub (cdr l)))))

(tm-define (xor . l)
  (:type (-> (tuple bool) bool))
  (:synopsis "Exclusive or of all elements in @l.")
  (xor-sub l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: guile-1.6.0 implements SRFI-13 (string library) in C.

(tm-define (char->string c)
  (:type (-> char string))
  (:synopsis "Convert @c to a string")
  (list->string (list c)))

(tm-define (string-tail s n)
  (:type (-> string int string))
  (:synopsis "Return all but the first @n chars of @s.")
  (substring s n (string-length s)))

(tm-define (char-in-string? c s)
  (:type (-> char string bool))
  (:synopsis "Test whether @c occurs in @s")
  (!= (string-index s c) #f))

(tm-define (string-starts? s what)
  (:type (-> string string bool))
  (:synopsis "Test whether @s starts with @what.")
  (let ((n (string-length s))
	(k (string-length what)))
    (and (>= n k) (== (substring s 0 k) what))))

(tm-define (string-ends? s what)
  (:type (-> string string bool))
  (:synopsis "Test whether @s ends with @what.")
  (let ((n (string-length s))
	(k (string-length what)))
    (and (>= n k) (== (substring s (- n k) n) what))))

(tm-define (string-contains? s what)
  (:type (-> string string bool))
  (:synopsis "Test whether @s contains @what as a substring.")
  (>= (string-search-forwards what 0 s) 0))

(tm-define (force-string s)
  (:type (-> object string))
  (:synopsis "Return @s if @s is a string and the empty string otherwise")
  (if (string? s) s ""))

(tm-define (reverse-list->string cs)	; srfi-13
  (:type (-> (list char) string))
  (:synopsis "Efficient implementation of (compose list->string reverse).")
  ;; Not yet any more efficient, but this may be fixed in the future.
  (list->string (reverse cs)))

(tm-define (string-join	ss . opt)	; srfi-13 (subset)
  ;; (:type ... How to write that?
  (:synopsis "Concatenate elements of @ss inserting separators.")
  (if (null? opt) (string-join ss " ")
      (string-concatenate (list-intersperse ss (car opt)))))

(tm-define (string-drop-right s n)	; srfi-13
  (:type (-> string int string))
  (:synopsis "Return all but the last @n chars of @s.")
  (substring s 0 (- (string-length s) n)))

(define string-drop string-tail)	; srfi-13

(tm-define (string-take s n)		; srfi-13
  (:type (-> string int string))
  (:synopsis "Return the first @n chars of @s.")
  (substring s 0 n))

(tm-define (string-trim s)		; srfi-13 (subset)
  (:type (-> string string))
  (:synopsis "Remove whitespace at start of @s.")
  (list->string (list-drop-while (string->list s) char-whitespace?)))

(define (list-drop-right-while l pred)
  (reverse! (list-drop-while (reverse l) pred)))

(tm-define (string-trim-right s)	; srfi-13 (subset)
  (:type (-> string string))
  (:synopsis "Remove whitespace at end of @s.")
  (list->string (list-drop-right-while (string->list s) char-whitespace?)))

(tm-define (string-trim-both s)		; srfi-13 (subset)
  (:type (-> string string))
  (:synopsis "Remove whitespace at start and end of @s.")
  (list->string
   (list-drop-right-while
    (list-drop-while (string->list s) char-whitespace?)
    char-whitespace?)))

(tm-define (string-concatenate ss)	; srfi-13
  (:type (-> (list string) string))
  (:synopsis "Append the elements of @ss toghether.")
  ;; WARNING: not portable for long lists
  (apply string-append ss))

(tm-define (string-map proc s) 		; srfi-13 (subset)
  (:type (-> (-> char char) string string))
  (:synopsis "Map @proc on every char of @s.")
  (list->string (map proc (string->list s))))

(tm-define (string-fold kons knil s) 	; srfi-13 (subset))
  (:synopsis "Fundamental string iterator.")
  (list-fold kons knil (string->list s)))

(tm-define (string-fold-right kons knil s) ; srfi-13 (subset)
  (:synopsis "Right to left fundamental string iterator.")
  (list-fold-right kons knil (string->list s)))

(tm-define (string-split-lines s)
  (:type (-> string (list string)))
  (:synopsis "List of substrings of @s separated by newlines.")
  (map list->string
       (list-fold-right string-split-lines/kons '(()) (string->list s))))

(define (string-split-lines/kons c cs+lines)
  (if (== c #\newline)
      (cons '() cs+lines)
      (cons (cons c (car cs+lines)) (cdr cs+lines))))
		
(tm-define (string-tokenize s c)
  (:type (-> string char (list string)))
  (:synopsis "Cut string @s into pieces using @c as a separator.")
  (with d (string-index s c)
    (if d
	(cons (substring s 0 d)
	      (string-tokenize (substring s (+ 1 d) (string-length s)) c))
	(list s))))

(tm-define (string-tokenize-n s c n)
  (:type (-> string char int (list string)))
  (:synopsis "As @string-tokenize, but only cut first @n pieces")
  (with d (string-index s c)
    (if (or (= n 0) (not d))
	(list s)
	(cons (substring s 0 d)
	      (string-tokenize-n (substring s (+ 1 d) (string-length s))
				 c
				 (- n 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (compose g f)
  (:type (forall A B C (-> (-> B C) (-> A B) (-> A C))))
  (:synopsis "Compose the functions @f and @g")
  (lambda x (g (apply f x))))

(tm-define (negate pred?)
  (:type (forall T (-> (-> T bool) (-> T bool))))
  (:synopsis "Return the negation of @pred?.")
  (lambda args (not (apply pred? args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fill-dictionary-entry d key im)
  (if (nnull? key)
      (begin
	(ahash-set! d (car key) im)
	(fill-dictionary-entry d (cdr key) im))))

(tm-define (fill-dictionary d l)
  (:type (forall Key Im
		 (-> (alias (ahash-table Key Im)) (list (cross Key Im)) void)))
  (:synopsis "Fill hash table @d with list of entries @l")
  (:note "Depreciated")
  (if (nnull? l)
      (begin
	(let* ((r (reverse (car l))))
	  (fill-dictionary-entry d (cdr r) (car r)))
	(fill-dictionary d (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string->object s)
  (:type (-> string object))
  (:synopsis "Parse @s and build scheme object")
  (call-with-input-string s read))

(tm-define (func? x f . opts)
  (:type (-> stree symbol bool)
	 (-> stree symbol int bool))
  (:synopsis "Is @x a list with first stree @f?"
	     "Optionally test the length of @x.")
  (let ((n (length opts)))
    (cond ((= n 0) (and (list? x) (nnull? x) (== (car x) f)))
	  ((= n 1)
	   (let ((nn (car opts)))
             (and (list? x) (nnull? x)
                  (== (car x) f) (= (length x) (+ nn 1)))))
	  (else (error "Too many arguments.")))))

(tm-define (tuple? x . opts)
  (:type (-> stree bool)
	 (-> stree symbol bool)
	 (-> stree symbol int bool))
  (:synopsis "Equivalent to @list? without options"
	     "Equivalent to @func? otherwise")
  (if (null? opts)
      (list? x)
      (apply func? (cons x opts))))
