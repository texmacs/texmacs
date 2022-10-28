
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : speech-define.scm
;; DESCRIPTION : Definition of speech commands
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui speech-define)
  (:use (kernel gui kbd-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-map-table (make-ahash-table))

(tm-define (speech-map-set lan mode key im)
  (ahash-set! speech-map-table (list lan mode key) im))

(tm-define (speech-map-ref lan mode key)
  (ahash-ref speech-map-table (list lan mode key)))

(define (speech-map-line lan mode line)
  (if (and (pair? line) (string? (car line)))
      (with (key . l) line
        (set! key (speech-pre-sanitize lan (utf8->cork key)))
        (speech-map-set lan mode key l)
        (speech-accepts lan mode key)
        (with key* (string-append (symbol->string lan) ":" (locase-all key))
          (list (cons* key* '(noop) l))))
      (list line)))

(tm-define-macro (speech-map lan mode . l)
  (:synopsis "Add entries in @l to the keyboard speech mapping")
  `(kbd-map
     ,@(if (== mode 'any) (list) `((:mode ,(symbol-append 'in- mode '?))))
     ,@(append-map (cut speech-map-line lan mode <>) l)))

(define (speech-subst-wildcard x w)
  (cond ((string? x) (string-replace x "*" w))
        ((list? x) (map (cut speech-subst-wildcard <> w) x))
        (else x)))

(define (speech-expand-wildcards l)
  (if (null? l) l
      (append (map (cut speech-subst-wildcard (car l) <>) roman-letters)
              (speech-expand-wildcards (cdr l)))))

(tm-define-macro (speech-map-wildcard lan mode . l)
  `(speech-map ,lan ,mode
     ,@(speech-expand-wildcards l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of speech ajustments
;; - speech-adjust corresponds to words that are badly recognized
;; - speech-reduce is used for synonyms that are mapped to a single command
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-adjust-table (make-ahash-table))

(tm-define (speech-adjust-set lan mode key im)
  (ahash-set! speech-adjust-table (list lan mode key) im))

(tm-define (speech-adjust-ref lan mode key)
  (ahash-ref speech-adjust-table (list lan mode key)))

(define (speech-adjust-line lan mode line)
  (with *lan (if (symbol-ends? lan '*) (symbol-drop-right lan 1) lan)
    (if (and (list-2? line) (string? (car line)))
	(with (key im) line
	  (set! key (speech-pre-sanitize *lan (utf8->cork key)))
	  (set! im  (speech-pre-sanitize *lan (utf8->cork im )))
	  (speech-accepts lan mode key)
	  `(speech-adjust-set ',lan ',mode ,key ,im))
	`(noop))))

(tm-define-macro (speech-adjust lan mode . l)
  (:synopsis "Add entries in @l to the keyboard speech adjustment table")
  `(begin
     ,@(map (cut speech-adjust-line lan mode <>) l)))

(tm-define-macro (speech-reduce lan mode . l)
  (:synopsis "Add entries in @l to the keyboard speech reduction table")
  `(speech-adjust ,(symbol-append lan '*) ,mode ,@l))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collections of language specific words
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-collection-table (make-ahash-table))

(tm-define (speech-collection-set name lan elem)
  (set! elem (speech-pre-sanitize lan (utf8->cork elem)))
  (ahash-set! speech-collection-table (list name lan elem) #t))

(tm-define (speech-has? lan name elem)
  (ahash-ref speech-collection-table (list name lan elem)))

(define (speech-collection-add name lan elem)
  `(speech-collection-set ',name ',lan ,elem))

(tm-define-macro (speech-collection name lan . l)
  (:synopsis "Add entries in @l to the speech collection tables")
  `(begin
     ,@(map (cut speech-collection-add name lan <>) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mathematical symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-list-2? line)
  (and (list-2? line) (string? (car line)) (string? (cadr line))))

(tm-define roman-letters
  (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
        "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))

(tm-define greek-letters
  (list "<alpha>" "<beta>" "<gamma>" "<delta>" "<epsilon>" "<zeta>" "<eta>"
        "<theta>" "<iota>" "<kappa>" "<lambda>" "<mu>" "<nu>" "<xi>"
        "<omicron>" "<pi>" "<rho>" "<sigma>" "<tau>" "<upsilon>"
        "<phi>" "<psi>" "<chi>" "<omega>"))

(tm-define punctuation-symbols
  (list "." "," ":" ";" "!" "?"))

(tm-define standard-operators
  (list "arc" "arc cos" "arc sin" "arc tan" "arccos" "arcsin" "arctan"
        "arg" "cos" "cosh" "cot" "coth" "csc" "deg" "det" "dim" "exp" "gcd"
        "hom" "inf" "ker" "lg" "lim" "liminf" "limsup" "lim inf" "lim sup"
        "ln" "log" "max" "min" "Pr" "sec" "sin" "sinh" "sup" "tan" "tanh"))

(tm-define lowercase-letters
  (append roman-letters greek-letters))

(define (number-cadr? x) (string-number? (cadr x)))
(define (roman-cadr? x) (in? (cadr x) roman-letters))
(define (greek-cadr? x) (in? (cadr x) greek-letters))

(define (ordinary-cadr? x)
  (and (== (math-symbol-type (cadr x)) "symbol")
       (nin? (cadr x) roman-letters)
       (nin? (cadr x) greek-letters)))

(define (infix-cadr? x) (== (math-symbol-type (cadr x)) "infix"))
(define (prefix-cadr? x) (== (math-symbol-type (cadr x)) "prefix"))
(define (postfix-cadr? x) (== (math-symbol-type (cadr x)) "postfix"))
(define (prefix-infix-cadr? x) (== (math-symbol-type (cadr x)) "prefix-infix"))
(define (separator-cadr? x) (== (math-symbol-type (cadr x)) "separator"))

(tm-define (speech-insert-symbol sym)
  (insert sym))

(define (speech-map-symbol line)
  (with (key im) line
    `(,key (speech-insert-symbol ,im))))

(tm-define-macro (speech-symbols lan . l)
  (:synopsis "Add entries in @l to the keyboard speech adjustment table")
  (set! l (filter string-list-2? l))
  (let* ((number (filter number-cadr? l))
         (roman (filter roman-cadr? l))
         (greek (filter greek-cadr? l))
         (ordinary (filter ordinary-cadr? l))
         (infix (filter infix-cadr? l))
         (prefix (filter infix-cadr? l))
         (postfix (filter infix-cadr? l))
         (prefix-infix (filter prefix-infix-cadr? l))
         (separator (filter separator-cadr? l)))
    `(begin
       (speech-map ,lan math
         ,@(map speech-map-symbol l))
       (speech-collection number ,lan ,@(map car number))
       (speech-collection roman ,lan ,@(map car roman))
       (speech-collection greek ,lan ,@(map car greek))
       (speech-collection ordinary-symbol ,lan ,@(map car ordinary))
       (speech-collection prefix ,lan ,@(map car ordinary))
       (speech-collection infix ,lan ,@(map car infix))
       (speech-collection postfix ,lan ,@(map car postfix))
       (speech-collection prefix-infix ,lan ,@(map car prefix-infix))
       (speech-collection separator ,lan ,@(map car separator)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sanitizing speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string-locase? s)
  (and (string? s) (string-alpha? s) (== s (locase-all s))))

(tm-define (string-upcase? s)
  (and (string? s) (string-alpha? s) (== s (upcase-all s))))

(tm-define (clean-quotes s)
  (cond ((string-starts? s "'")
         (string-append "'" (clean-quotes (string-drop s 1))))
        ((string-ends? s "'")
         (string-append (clean-quotes (string-drop-right s 1)) "'"))
        (else (string-replace (string-replace s "'" "' ") "'  " "' "))))

(tm-define (clean-letter-digit l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (string-alpha? (car l)) (string-number? (cadr l)))
         (cons* (car l) " " (clean-letter-digit (cdr l))))
        ((and (string-number? (car l)) (string-alpha? (cadr l)))
         (cons* (car l) " " (clean-letter-digit (cdr l))))
        ((and (string-locase? (car l)) (string-upcase? (cadr l)))
         (cons* (car l) " " (clean-letter-digit (cdr l))))
        (else (cons (car l) (clean-letter-digit (cdr l))))))

(define (string-replace-trailing-one s what by)
  (let* ((l (string-length what))
         (n (string-length s)))
    (if (and (>= n l) (>= n 2)
             (not (string-occurs? what (substring s 1 (- n 1)))))
        (string-replace s what by)
        s)))

(tm-define (string-replace-trailing s what by)
  (let* ((l (string-decompose s " "))
         (r (map (cut string-replace-trailing-one <> what by) l)))
    (string-recompose r " ")))

(tm-define (speech-pre-sanitize lan s) s)
(tm-define (speech-sanitize lan mode s) s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rewriting speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-current-mode) 'any)

(define (speech-rewrite*** lan mode h t)
  (with key (locase-all (string-recompose h " "))
    (if (null? h)
        (if (null? t) t
            (cons (car t) (speech-rewrite*** lan mode (cdr t) (list))))
        (or (and-with im (speech-adjust-ref lan mode key)
              ;;(display* "  Rewrote " key " ~> " im "\n")
              (with im* (string-decompose im " ")
                (append im* t)))
            (speech-rewrite*** lan mode (cDr h) (cons (cAr h) t))))))

(define (speech-rewrite** lan mode l depth)
  ;;(display* "Rewrite " l ", " lan ", " mode ", " depth "\n")
  (with r (speech-rewrite*** lan mode l (list))
    (if (or (<= depth 0) (== r l)) r
        (speech-rewrite** lan mode r (- depth 1)))))

(define (speech-rewrite* lan mode l)
  (with r (speech-rewrite** lan mode l 100)
    (speech-rewrite** (symbol-append lan '*) mode r 100)))

(tm-define (speech-rewrite lan mode s)
  (set! s (speech-sanitize lan mode s))
  ;;(display* "Sanitized " (cork->utf8 s) "\n")
  (let* ((l (string-decompose s " "))
         (r (speech-rewrite* lan mode l)))
    (string-recompose r " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check whether commands are recognized in a given mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-accepts-table (make-ahash-table))
(define speech-recognizes-table (make-ahash-table))

(tm-define (speech-accepts lan mode s)
  (with ls (locase-all s)
    (ahash-set! speech-recognizes-table (list lan mode ls) #t))
  (with l (string-decompose s " ")
    (for (ss l)
      (ahash-set! speech-accepts-table (list lan mode ss) #t))))

(tm-define (raw-speech-accepts? lan mode s)
  (with lan* (symbol-append lan '*)
    (or (nnot (string->number s))
        (ahash-ref speech-accepts-table (list lan mode s))
        (ahash-ref speech-accepts-table (list lan* mode s)))))

(tm-define (speech-accepts? lan mode s)
  (set! s (speech-sanitize lan mode s))
  (forall? (cut raw-speech-accepts? lan mode <>)
           (string-decompose s " ")))

(tm-define (speech-border-accepts? lan mode s)
  (set! s (speech-sanitize lan mode s))
  (with lan* (symbol-append lan '*)
    (or (nnot (string->number s))
        (ahash-ref speech-recognizes-table (list lan mode s))
        (ahash-ref speech-recognizes-table (list lan* mode s)))))

(define (speech-recognizes-list? lan mode h t)
  ;;(display* "    checking " h ", " t "\n")
  (with key (locase-all (string-recompose h " "))
    (if (null? h) (null? t)
        (or (and (or (string->number key)
                     (ahash-ref speech-recognizes-table (list lan mode key))
                     (ahash-ref speech-recognizes-table (list lan 'any key)))
                 (speech-recognizes-list? lan mode t (list)))
            (speech-recognizes-list? lan mode (cDr h) (cons (cAr h) t))))))

(tm-define (speech-recognizes? lan mode s)
  ;;(display* "    checking " (speech-sanitize lan mode s) "\n")
  (and (speech-accepts? lan mode (speech-sanitize lan mode s))
       (let* ((r (speech-rewrite lan mode s))
              (l (string-decompose r " ")))
         (speech-recognizes-list? lan mode l (list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Execution of speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-language)
  (with lan (string->symbol (get-preference "speech"))
    (if (== lan 'off) 'english lan)))

(tm-define (speech-insert-number nr)
  (insert nr))

(tm-define (speech-exec-hook l) #f)

(define (speech-exec-list lan h t)
  (let* ((key* (locase-all (string-recompose h " ")))
         (key (string-append (symbol->string lan) ":" key*)))
    ;;(display* "  Try " key* " -> " (kbd-find-key-binding key) "\n")
    (cond ((and (null? h) (null? t)) (noop))
	  ((null? h) (speech-exec-list lan (cdr t) (list)))
	  ((speech-exec-hook (apply string-append h))
	   (speech-exec-list lan t (list)))
	  ((string->number key*)
	   (speech-insert-number key*)
	   (speech-exec-list lan t (list)))
	  ((kbd-find-key-binding key)
	   (with cmd (kbd-find-key-binding key)
	     (when (pair? cmd) (set! cmd (car cmd)))
	     (when (procedure? cmd)
	       ;;(display* "  Execute " key*
	       ;;          " -> " (procedure-source cmd) "\n")
	       (cmd))
	     (speech-exec-list lan t (list))))
	  (else (speech-exec-list lan (cDr h) (cons (cAr h) t))))))

(tm-define (speech-done)
  (noop))

(tm-define (speech-exec s)
  ;;(display* "Execute " (cork->utf8 s) "\n")
  (let* ((lan (speech-language))
         (mode (speech-current-mode))
         (r (speech-rewrite lan mode s))
         (l (string-decompose r " ")))
    ;;(display* "Rewritten " (cork->utf8 r) "\n")
    (speech-exec-list lan l (list))
    (speech-done)))

(tm-define (speech-make S)
  (with lan (get-preference "language")
    (when (!= lan "english")
      (set! S (translate-from-to S lan "english"))))
  (with s (string-replace (locase-all S) " " "-")
    (and (style-has? s)
         (>= (string-length s) 3)
         (begin
           (make (string->symbol s))
           #t))))

(define (speech-command* key)
  (and-with cmd (kbd-find-key-binding key)
    (when (pair? cmd) (set! cmd (car cmd)))
    (cond ((procedure? cmd)
           ;;(display* "  Command " key* " -> " (procedure-source cmd) "\n")
           (cmd))
          ((string? cmd)
           ;;(display* "  Insert " cmd "\n")
           (insert cmd)))
    #t))

(tm-define (speech-command key)
  (let* ((lan (symbol->string (speech-language)))
         (key* (string-append lan  ":" (locase-all key))))
    (or (speech-command* key)
        (speech-command* key*))))

(tm-define (kbd-speech s)
  ;;(display* "Speech " (cork->utf8 s) "\n")
  (cond ((speech-command s) (noop))
        ((speech-make s) (noop))
        (else (kbd-insert s))))
