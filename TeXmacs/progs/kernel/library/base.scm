
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : base.scm
;; DESCRIPTION : frequently used Scheme subroutines
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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
;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (float->string s)
  (number->string s))

(define-public (string->float s)
  (exact->inexact (string->number s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: guile-1.6.0 implements SRFI-13 (string library) in C.

(define-public (char->string c)
  "Convert @c to a string"
  (list->string (list c)))

(define-public (tm-char-whitespace? c)
  "Is @c a whitespace character?"
  ;; NOTE: this routine is implemented in an incorrect way in certain
  ;; versions of Guile.  These erroneous versions incorrectly recognize
  ;; characters such as A0 (hexadecimal), which breaks certain routines
  ;; involving unicode or cork.
  (in? c '(#\space #\ht #\newline #\return)))

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

(provide-public (reverse-list->string cs)	; srfi-13
  "Efficient implementation of (compose list->string reverse)."
  ;; Not yet any more efficient, but this may be fixed in the future.
  (list->string (reverse cs)))

(provide-public (string-join	ss . opt)	; srfi-13 (subset)
  "Concatenate elements of @ss inserting separators."
  (if (null? opt) (string-join ss " ")
      (string-concatenate (list-intersperse ss (car opt)))))

(provide-public (string-drop-right s n)	; srfi-13
  "Return all but the last @n chars of @s."
  (substring s 0 (- (string-length s) n)))

(provide-public string-drop string-tail)	; srfi-13

(provide-public (string-take s n)		; srfi-13
  "Return the first @n chars of @s."
  (substring s 0 n))

(provide-public (string-take-right s n)
  "Return the first @n chars of @s."
  (let ((l (string-length s)))
    (substring s (- l n) l)))

(define-public (tm-string-trim s)		; srfi-13 (subset)
  "Remove whitespace at start of @s."
  (list->string (list-drop-while (string->list s) tm-char-whitespace?)))

(define-public (list-drop-right-while l pred)
  (reverse! (list-drop-while (reverse l) pred)))

(define-public (tm-string-trim-right s)	; srfi-13 (subset)
  "Remove whitespace at end of @s."
  (list->string (list-drop-right-while (string->list s) tm-char-whitespace?)))

(define-public (tm-string-trim-both s)		; srfi-13 (subset)
  "Remove whitespace at start and end of @s."
  (list->string
   (list-drop-right-while
    (list-drop-while (string->list s) tm-char-whitespace?)
    tm-char-whitespace?)))

(provide-public (string-concatenate ss)	; srfi-13
  "Append the elements of @ss toghether."
  ;; WARNING: not portable for long lists
  (apply string-append ss))

(provide-public (string-map proc s) 		; srfi-13 (subset)
  "Map @proc on every char of @s."
  (list->string (map proc (string->list s))))

(provide-public (string-fold kons knil s) 	; srfi-13 (subset))
  "Fundamental string iterator."
  (list-fold kons knil (string->list s)))

(provide-public (string-fold-right kons knil s) ; srfi-13 (subset)
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

(provide-public (string-tokenize-by-char s sep)
  "Cut string @s into pieces using @sep as a separator."
  (with d (string-index s sep)
    (if d
	(cons (substring s 0 d)
	      (string-tokenize-by-char (substring s (+ 1 d) (string-length s)) sep))
	(list s))))

(define-public (string-tokenize-by-char-n s sep n)
  "As @string-tokenize-by-char, but only cut first @n pieces"
  (with d (string-index s sep)
    (if (or (= n 0) (not d))
	(list s)
	(cons (substring s 0 d)
	      (string-tokenize-by-char-n
               (substring s (+ 1 d) (string-length s))
               sep
               (- n 1))))))

(define-public (string-decompose s sep)
  (with d (string-search-forwards sep 0 s)
    (if (< d 0)
        (list s)
        (cons (substring s 0 d)
              (string-decompose (substring s (+ d (string-length sep))
                                           (string-length s)) sep)))))

(define-public (string-recompose l sep)
  "Turn list @l of strings into one string using @sep as separator."
  (if (char? sep) (set! sep (list->string (list sep))))
  (cond ((null? l) "")
	((null? (cdr l)) (car l))
	(else (string-append (car l) sep (string-recompose (cdr l) sep)))))

(define-public (string-tokenize-comma s)
  "Cut string @s into pieces using comma as a separator and remove whitespace."
  (map tm-string-trim-both (string-tokenize-by-char s #\,)))

(define-public (string-recompose-comma l)
  "Turn list @l of strings into comma separated string."
  (string-recompose l ", "))

(define (property-pair->string p)
  (string-append (car p) "=" (cdr p)))

(define (string->property-pair s)
  (with pos (string-index s #\=)
    (if pos
	(cons (string-take s pos) (string-drop s (+ pos 1)))
	(cons s "true"))))

(define-public (string->alist s)
  "Parse @s of the form \"var1=val1/.../varn=valn\" as an association list."
  (map string->property-pair (string-tokenize-by-char s #\/)))

(define-public (alist->string l)
  "Pretty print the association list @l as a string."
  (string-recompose (map property-pair->string l) "/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some string-like functions on symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (symbol<=? x y)
  (string<=? (symbol->string x) (symbol->string y)))

(define-public (symbol-starts? s1 s2)
  (string-starts? (symbol->string s1) (symbol->string s2)))

(define-public (symbol-ends? s1 s2)
  (string-ends? (symbol->string s1) (symbol->string s2)))

(define-public (symbol-drop s n)
  (string->symbol (string-drop (symbol->string s) n)))

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

(define-public (object->string* obj)
  (cond ((null? obj) (object->string obj))
	((pair? obj) (object->string obj))
	((number? obj) (object->string obj))
	((string? obj) (object->string obj))
	((symbol? obj) (object->string obj))
	((tree? obj) (object->string (tree->stree obj)))
	(else (object->string #f))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Urls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (url->list u)
  (cond ((url-none? u) '())
	((url-or? u) (append (url->list (url-ref u 1))
			     (url->list (url-ref u 2))))
	(else (list u))))

(define-public (list->url l)
  (cond ((null? l) (url-none))
	((null? (cdr l)) (car l))
	(else (url-or (car l) (list->url (cdr l))))))

(define-public (url-read-directory u wc)
  (with d (url-expand (url-complete (url-append u (url-wildcard wc)) "r"))
    (url->list d)))

(define-public (url-remove u)
  (system-remove u))

(define-public (url-autosave u suf)
  (and (not (url-rooted-web? name))
       (not (url-rooted-tmfs? name))
       (url-glue u suf)))

(define-public (url-wrap u)
  #f)

(define-public (url->delta-unix u)
  (with base (buffer-get-master (current-buffer))
    (when (and (url-rooted? u) (not (url-none? base)))
      (set! u (url-delta base u))))
  (url->unix u))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (current-buffer)
  (with u (current-buffer-url)
    (and (not (url-none? u)) u)))

(define-public (path->buffer p)
  (with u (path-to-buffer p)
    (and (not (url-none? u)) u)))

(define-public (buffer->tree u)
  (with t (buffer-get-body u)
    (and (tree-active? t) t)))

(define-public (tree->buffer t)
  (and-with p (tree->path t)
    (path->buffer p)))

(define-public (buffer->path u)
  (with t (buffer->tree u)
    (and t (tree->path t))))

(define-public (buffer-exists? name)
  (in? (url->url name) (buffer-list)))

(define-public (buffer-master)
  (buffer-get-master (current-buffer)))

(define-public (buffer-in-recent-menu? u)
  (or (not (url-rooted-tmfs? u))
      (string-starts? (url->unix u) "tmfs://part/")))

(define-public (buffer-in-menu? u)
  (or (buffer-in-recent-menu? u)
      (string-starts? (url->unix u) "tmfs://help/")
      (string-starts? (url->unix u) "tmfs://remote-file/")
      (string-starts? (url->unix u) "tmfs://apidoc/")))

(define-public (window->buffer win)
  (with u (window-to-buffer win)
    (and (not (url-none? u)) u)))

(define-public (buffer->window buf)
  (with l (buffer->windows buf)
    (and (nnull? l) (car l))))

(define-public (current-view)
  (with u (current-view-url)
    (and (not (url-none? u)) u)))

(define-public (view->window vw)
  (with win (view->window-url vw)
    (and (not (url-none? win)) win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Redirections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (tm-with-output-to-string p)
  (cout-buffer)
  (p)
  (cout-unbuffer))
