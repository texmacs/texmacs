
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-edit.scm
;; DESCRIPTION : editing mathematics
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils edit auto-close)
	(math math-drd)
        (generic format-geometry-edit)
        (convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some drd properties, which should go into table-drd.scm later on
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-group variant-tag (math-table-tag))
(define-group similar-tag (math-table-tag))

(define-group math-table-tag
  matrix det bmatrix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (allow-space-after? b)
  (and b (not (tm-func? b 'big))
       (or (not (tm-atomic? b))
           (let* ((s (tm->string b))
                  (last (and (!= s "") (tmstring-reverse-ref s 0)))
                  (type (and last (math-symbol-type last))))
	     (nin? type (list "prefix" "infix" "separator"
			      "opening-bracket" "middle-bracket"))))))

(tm-define (skip-decorations-leftwards t)
  (if (and (tree? t)
           (tree-in? t '(rsub rsup rprime suppressed))
           (tree-ref t :previous))
      (skip-decorations-leftwards (tree-ref t :previous))
      t))

(tm-define (kbd-space-bar t shift?)
  (:require (and (tree-is-buffer? t) (in-math?)))
  (let* ((b (skip-decorations-leftwards (before-cursor)))
	 (p (get-preference "math spacebar")))
    (cond ((== p "allow spurious spaces")
	   (insert " "))
	  ((and (== b " ") (== p "no spurious spaces"))
	   (noop))
	  ((== b " ")
	   (remove-text #f)
	   (make-space "1em"))
	  ((and (tree? b) (tree-func? b 'space 1))
	   (if (and (tree-atomic? (tree-ref b 0))
		    (string-ends? (tree->string (tree-ref b 0)) "em"))
	       (make-space "1em")
	       (geometry-horizontal b #t)))
	  ((not (allow-space-after? b))
	   (noop))
	  (else
	   (insert " ")))))

(tm-define (kbd-insert s)
  (:require (in-math?))
  (when (== (before-cursor) " ")
    (let* ((p (get-preference "math spacebar"))
	   (type (if (string? s) (math-symbol-type s) "symbol")))
      (when (in? type (list "postfix" "infix" "separator"
			    "middle-bracket" "closing-bracket"))
	(remove-text #f))))
  (former s))

(tm-define (math-insert t)
  (insert t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Special customizations inside equation environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'equation))
  (go-end-of 'equation)
  (insert-return))

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'equation*))
  (go-end-of 'equation*)
  (insert-return))

(tm-define (label-insert t)
  (:require (tree-is? t 'eqnarray*))
  (go-end-line)
  (make 'label))

(tm-define (math-make-math)
  (if (inside? 'math)
      (go-end-of 'math)
      (set-message (string-append "Warning: already inside mathematics")
		   (string-append "make 'math'"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for moving punctuation symbols around
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-ref-nspace? s i)
  (!= (string-ref s i) #\space))

(define (string-ref-npunct? s i)
  (nin? (string-ref s i) '(#\space #\. #\, #\: #\;)))

(define (string-search-forwards s i n pred?)
  (cond ((>= i n) i)
	((pred? s i) i)
	(else (string-search-forwards s (+ i 1) n pred?))))

(define (string-search-backwards s i b pred?)
  (cond ((<= i b) i)
	((pred? s (- i 1)) i)
	(else (string-search-backwards s (- i 1) b pred?))))

(define (atomic-cut-left-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-forwards s 0 n pred?)))
	(if (> i 0)
	    (with ss (substring s 0 i)
	      (tree-remove! t 0 i)
	      (tree-correct-old t)
	      ss)
	    ""))
      ""))

(define (atomic-cut-right-until t pred?)
  (if (atomic-tree? t)
      (let* ((s (tree->string t))
	     (n (string-length s))
	     (i (string-search-backwards s n 0 pred?)))
	(if (< i n)
	    (with ss (substring s i n)
	      (tree-remove! t i (- n i))
	      (tree-correct-old t)
	      ss)
	    ""))
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switching between inlined and displayed equations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (concat-isolate! t)
  `(cond ((not (tree-is? t :up 'concat)) (noop))
	 ((not (tree-is? t :up :up 'document)) (noop))
	 ((= (tree-arity (tree-up t)) 1) (tree-set! t :up t))
	 ((< (tree-index t) (- (tree-arity (tree-up t)) 1))
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (+ (tree-index t) 1))
	  (concat-isolate! t))
	 (else
	  (tree-split (tree-up t 2)
		      (tree-index (tree-up t))
		      (tree-index t))
	  (concat-isolate! t))))

(define (math->equation* t)
  (let* ((c (and (tree-is? t :up 'concat) (tree-is? t :up :up 'document)))
         (r (and c (atomic-cut-left-until (tree-ref t :next)
                                          string-ref-npunct?)))
         (l (and c (atomic-cut-right-until (tree-ref t :previous)
                                           string-ref-nspace?))))
    (concat-isolate! t)
    (if (tree-is? t :up 'document)
        (begin
          (if (not r) (set! r ""))
          (if (not l) (set! l ""))
          (tree-set! t `(equation* (document ,(tree-ref t 0))))
          (while (string-ends? r " ")
            (set! r (string-drop-right r 1)))
          (with-cursor (tree->path t 0 0 :end)
            (insert r))))))

(define (equation*->math t)
  (if (or (not (tree-is? t 0 'document))
          (= (tree-arity (tree-ref t 0)) 1))
      (begin
        (if (tree-is? t 0 'document)
            (tree-set! t 0 (tree-ref t 0 0)))
        (tree-set! t `(math ,(tree-ref t 0)))
        (with r (atomic-cut-right-until (tree-end (tree-ref t 0))
                                        string-ref-npunct?)
          (with-cursor (tree->path t :start)
            (kbd-backspace)
            (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
                     (!= (cursor-path) (cursor-after (go-end-paragraph))))
                (insert " ")))
          (with-cursor (tree->path t :end)
            (insert r)
            (kbd-delete)
            (if (and (!= (cursor-path) (cursor-after (go-start-paragraph)))
                     (!= (cursor-path) (cursor-after (go-end-paragraph))))
                (insert " ")))))))

(define (with-math-context? t)
  (match? t '(with "mode" "math" :%1)))

(tm-define (variant-circulate t forward?)
  (:require (with-math-context? t))
  (tree-set! t `(math ,(tree-ref t 2)))
  (math->equation* t))

(tm-define (variant-circulate t forward?)
  (:require (tree-is? t 'math))
  (math->equation* t))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(equation equation*)))
  (equation*->math t))

(tm-define (variant-formula t)
  (with-innermost t '(math equation equation*)
    (when (tree-in? t '(equation equation*))
      (equation*->math t))))

(tm-define (variant-equation t)
  (with-innermost t '(math equation equation*)
    (when (tree-in? t '(math))
      (math->equation* t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Switching between displayed equations and equation arrays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (search-labels t)
  (tree-search t (lambda (l) (tree-in? l (label-tag-list)))))

(define (cut-all t w)
  (cond ((tree-atomic? t) (noop))
        ((tree-is? t w) (tree-cut t))
        (else (for-each (cut cut-all <> w) (tree-children t)))))

(define (check-border? l empty?)
  (cond ((null? l) #t)
        ((and (not empty?) (nnull? (car l)) (!= (caar l) "")) #f)
        (else (check-border? (cdr l) (== (cAr (car l)) "")))))

(define (convertible-eqnarray? t)
  (let* ((rs (select (tm->stree t) '(:* row)))
         (rcs (map (lambda (r) (select r '(:* cell 0))) rs)))
    (check-border? rcs #t)))

(tm-define (eqnarray->equation t)
  (with labs (map tree-copy (search-labels t))
    (when (and (<= (length labs) 1) (convertible-eqnarray? t))
      (cut-all t 'label)
      (cut-all t 'eq-number)
      (let* ((l* (select t '(:* cell 0)))
             (l (if (null? labs) l* (cons (car labs) l*)))
             (c (apply tmconcat (map tree->stree l)))
             (n (if (null? labs) `(equation* ,c) `(equation ,c))))
        (tree-set! t n)
        (tree-go-to t 0 :end)))))

(define (binary-relations)
  (or (get-packrat-definition "std-symbols" "Relation-nolim-symbol") (list)))

(define (binary-relation? t)
  (in? (tm->stree t) (binary-relations)))

(define (atom-decompose t)
  (if (tree-atomic? t)
      (tmstring->list (tree->string t))
      (list t)))

(define (concat-decompose t)
  (cond ((tree-atomic? t) (atom-decompose t))
        ((tree-is? t 'concat)
         (apply append (map atom-decompose (tree-children t))))
        (else (list t))))

(define (make-eqn-row-sub l)
  (list "" (car l) (apply tmconcat (cdr l))))

(define (finalize-row l)
  `(row (cell ,(car l)) (cell ,(cadr l)) (cell ,(caddr l))))

(tm-define (equation->eqnarray t)
  (let* ((labs (map tm->stree (search-labels t)))
         (c* (tree-ref t 0))
         (c  (if (tree-func? c* 'document 1) (tree-ref c* 0) c*))
         (l0 (concat-decompose c))
         (l1 (list-filter l0 (lambda (x) (not (tm-is? x 'label)))))
         (l2 (list-scatter l1 binary-relation? #t)))
    (when (>= (length l2) 2)
      (cut-all t 'label)
      (let* ((l3 (cons (list (apply tmconcat (car l2))
                             (caadr l2)
                             (apply tmconcat (cdadr l2)))
                       (map make-eqn-row-sub (cddr l2))))
             (l4 (if (null? labs) l3
                     (rcons (cDr l3)
                            (rcons (cDr (cAr l3))
                                   (tmconcat (cAr (cAr l3))
                                             '(eq-number) (car labs))))))
             (l5 (map finalize-row l4))
             (r `(eqnarray* (document (tformat (table ,@l5))))))
        (tree-set! t r)
        (tree-go-to t 0 0 0 :last :last 0 :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of roots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (sqrt-toggle t)
  (when (tree-is? t 'sqrt)
    (cond ((== (tree-arity t) 1)
           (tree-insert! t 1 '(""))
           (tree-go-to t 1 0))
          ((== (tree-arity t) 2)
           (tree-remove! t 1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (script-context? t)
  (tree-in? t '(lsub lsup rsub rsup)))

(tm-define (script-only-script? t)
  (and (tree-is? (tree-up t) 'concat)
       (not (or (and (tree-ref t :previous)
                     (script-context? (tree-ref t :previous)))
                (and (tree-ref t :next)
                     (script-context? (tree-ref t :next)))))))

(tm-define (variant-circulate t forward?)
  (:require (script-context? t))
  (when (script-only-script? t)
    (cond ((tree-is? t 'lsub) (variant-set t 'lsup))
          ((tree-is? t 'lsup) (variant-set t 'lsub))
          ((tree-is? t 'rsub) (variant-set t 'rsup))
          ((tree-is? t 'rsup) (variant-set t 'rsub)))))

(tm-define (structured-insert-vertical t downwards?)
  (:require (tree-in? t '(lsub rsub)))
  (when (and (not downwards?) (script-only-script? t))
    (tree-go-to t :end)
    (cond ((tree-is? t 'lsub) (make 'lsup))
          ((tree-is? t 'rsub) (make 'rsup)))))

(tm-define (structured-insert-vertical t downwards?)
  (:require (tree-in? t '(lsup rsup)))
  (when (and downwards? (script-only-script? t))
    (tree-go-to t :end)
    (cond ((tree-is? t 'lsup) (make 'lsub))
          ((tree-is? t 'rsup) (make 'rsub)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured editing of wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define wide-list-1
  '("~" "^" "<bar>" "<vect>" "<check>" "<breve>" "<invbreve>"))

(define wide-list-2
  '("<acute>" "<grave>" "<dot>" "<ddot>" "<dddot>" "<ddddot>" "<abovering>"))

(define wide-list-3
  '("<wide-overbrace>" "<wide-underbrace*>"
    "<wide-poverbrace>" "<wide-punderbrace*>"
    "<wide-sqoverbrace>" "<wide-squnderbrace*>"))

(define wide-list-4
  '("<wide-underbrace>" "<wide-overbrace*>"
    "<wide-punderbrace>" "<wide-poverbrace*>"
    "<wide-squnderbrace>" "<wide-sqoverbrace*>"))

(define wide-list-5
  '("<wide-varrightarrow>" "<wide-varleftarrow>"
    "<wide-varleftrightarrow>" "<wide-bar>"))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(wide wide*)))
  (when (tree-atomic? (tree-ref t 1))
    (with s (tree->string (tree-ref t 1))
      (and-with i (list-find-index wide-list-1 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-1))
          (tree-set t 1 (list-ref wide-list-1 j))))
      (and-with i (list-find-index wide-list-2 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-2))
          (tree-set t 1 (list-ref wide-list-2 j))))
      (and-with i (list-find-index wide-list-3 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-3))
          (tree-set t 1 (list-ref wide-list-3 j))))
      (and-with i (list-find-index wide-list-4 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-4))
          (tree-set t 1 (list-ref wide-list-4 j))))
      (and-with i (list-find-index wide-list-5 (lambda (x) (== x s)))
        (with j (modulo (+ i (if forward? 1 -1)) (length wide-list-5))
          (tree-set t 1 (list-ref wide-list-5 j)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wide arrows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-long-arrow s)
  (when (and (string? s) (string-starts? s "<") (string-ends? s ">"))
    (with rs (string-append "<rubber-" (substring s 1 (string-length s)))
      (insert-go-to `(long-arrow ,rs "") '(1 0)))))

(tm-define (make-long-arrow* s)
  (when (and (string? s) (string-starts? s "<") (string-ends? s ">"))
    (with rs (string-append "<rubber-" (substring s 1 (string-length s)))
      (insert-go-to `(long-arrow ,rs "" "") '(2 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the shape of brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lbrackets
  '("(" "[" "{" "<langle>" "|" "<||>" "<lfloor>" "<lceil>" "<llbracket>"))

(define mbrackets
  '("|" "<||>" "/" "\\"))

(define rbrackets
  '(")" "]" "}" "<rangle>" "|" "<||>" "<rfloor>" "<rceil>" "<rrbracket>"))

(define (bracket-circulate t forward? brackets)
  (cond ((and (tree-in? t '(around around*))
              (== (tree-arity t) 3))
         (bracket-circulate (tree-ref t 0) forward? lbrackets)
         (bracket-circulate (tree-ref t 2) forward? rbrackets))
        ((and (tree-is? t 'left) (> (tree-arity t) 1))
         (bracket-circulate (tree-ref t 0) forward? lbrackets))
        ((and (tree-is? t 'mid) (> (tree-arity t) 1))
         (bracket-circulate (tree-ref t 0) forward? mbrackets))
        ((and (tree-is? t 'right) (> (tree-arity t) 1))
         (bracket-circulate (tree-ref t 0) forward? rbrackets))
        ((and (tree-atomic? t)
              (in? (tree->string t) brackets))
         (let* ((s (tree->string t))
                (i (list-find-index brackets (lambda (x) (== x s))))
                (j (modulo (+ i (if forward? 1 -1)) (length brackets)))
                (r (list-ref brackets j)))
           (tree-assign t r)))))

(tm-define (variant-circulate t forward?)
  (:require (tree-in? t '(left mid right around around*)))
  (bracket-circulate t forward? mbrackets))

(define bigops
  '("<int>" "<intlim>" "<oint>" "<ointlim>"
    "<sum>" "<prod>" "<amalg>"
    "<cap>" "<cup>" "<sqcap>" "<sqcup>"
    "<vee>" "<wedge>" "<curlyvee>" "<curlywedge>"
    "<odot>" "<otimes>" "<oplus>"
    "<triangleup>" "<triangledown>"
    "<box>" "<parallel>" "<interleave>"))

(tm-define (variant-circulate t forward?)
  (:require (tree-is? t 'big-around))
  (when (and (== (tree-arity t) 2)
             (tree-atomic? (tree-ref t 0)))
    (with s (tree->string (tree-ref t 0))
      (when (in? s bigops)
        (let* ((i (list-find-index bigops (lambda (x) (== x s))))
               (j (modulo (+ i (if forward? 1 -1)) (length bigops)))
               (ns (list-ref bigops j)))
          (tree-assign (tree-ref t 0) ns))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the dimension of brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bracket-size-increase t by)
  (cond ((and (tree-in? t '(left mid right))
              (>= (tree-arity t) 2))
         (bracket-size-increase (tree-ref t 1) by))
        ((and (tree-in? t '(around around*))
              (== (tree-arity t) 3))
         (when (tree-atomic? (tree-ref t 0))
           (tree-set t 0 `(left ,(tree-ref t 0) "0")))
         (when (tree-atomic? (tree-ref t 2))
           (tree-set t 2 `(right ,(tree-ref t 2) "0")))
         (bracket-size-increase (tree-ref t 0) by)
         (bracket-size-increase (tree-ref t 2) by)
         (when (tm-equal? (tree-ref t 0 1) "0")
           (tree-set t 0 (tree-ref t 0 0)))
         (when (tm-equal? (tree-ref t 2 1) "0")
           (tree-set t 2 (tree-ref t 2 0))))
        ((tree-integer? t)
         (let* ((old (tree->number t))
                (new (+ old by)))
           (tree-set t (number->string new))))
        ((tm-length? t)
         (length-increase t by))))

(tm-define (geometry-vertical t down?)
  (:require (tree-in? t '(left mid right around around*)))
  (with inc (if down? -1 1)
    (bracket-size-increase t inc)))

(define (bracket-size-reset t)
  (when (and (tree-in? t '(left mid right)) (>= (tree-arity t) 1))
    (tree-set t (tree-ref t 0))))

(tm-define (geometry-default t)
  (:require (tree-in? t '(around around*)))
  (when (== (tree-arity t) 3)
    (bracket-size-reset (tree-ref t 0))
    (bracket-size-reset (tree-ref t 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Not necessarily matching brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-non-bracket t)
  (cond ((not (tree-up t)) t)
        ((tm-in? t '(around around* big-around temp-slot))
         (find-non-bracket (tree-up t)))
        ((tm-in? (tree-up t) '(concat around around* big-around temp-slot))
         (find-non-bracket (tree-up t)))
        (else t)))

(define (find-and-remove-temp-slot x)
  ;;(display* "x= " x "\n")
  (cond ((tree-atomic? x) #f)
        ((tree-is? x 'temp-slot)
         (tree-go-to x 0 0)
         (remove-structure-upwards))
        ((tree? x)
         (list-or (map find-and-remove-temp-slot (tree-children x))))
        (else #f)))

(tm-define (brackets-refresh)
  (insert-go-to '(temp-slot "") '(0 0))
  (let* ((t (find-non-bracket (cursor-tree)))
	 (u (tree-downgrade-brackets t #t #f))
	 (v (tree-upgrade-brackets u "math"))
         (w (tree-downgrade-big v)))
    ;;(tree-set! t v)
    (tree-set! t w)
    (find-and-remove-temp-slot t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Match bracket with missing bracket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (count-missing t open?)
  (cond ((tree-atomic? t) 0)
	((tree-in? t '(around around*))
	 (+ (count-missing (tree-ref t 1) open?)
	    (if (deleted? t (if open? 0 2)) 1 0)))
	((tree-is? t (if open? 'right 'left)) 1)
	((tree-is? t 'concat)
	 (with l (tree-children t)
	   (apply + (map (lambda (x) (count-missing x open?)) l))))
	(else 0)))

(define (try-matching-insert open? which large?)
  (try-modification
    (let* ((nr (count-missing (find-non-bracket (cursor-tree)) open?))
	   (tag (if large? 'around* 'around)))
      ;;(display* nr ", " (find-non-bracket (cursor-tree)) "\n")
      (if open?
	  (insert-go-to (list tag which "" "<nobracket>") '(1 0))
	  (insert-go-to (list tag "<nobracket>" "" which) '(1)))
      (brackets-refresh)
      ;;(display* (count-missing (find-non-bracket (cursor-tree)) open?) ", "
      ;;(find-non-bracket (cursor-tree)) "\n")
      (> nr (count-missing (find-non-bracket (cursor-tree)) open?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (deleted? t i)
  (== (tm->stree (tree-ref t i)) "<nobracket>"))

;;(define (make-small s)
;;  (cond ((nstring? s) "<nobracket>")
;;	  ((== s ".") "<nobracket>")
;;	  ((<= (string-length s) 1) s)
;;	  (else (string-append "<" s ">"))))

(define (find-adjacent-around del?)
  (let* ((ret #f)
	 (p (cursor-path))
	 (p* (cursor-path*)))
    (with t (tree-innermost '(around around*))
      (when t
	(when (== p (tree->path t 1 :start))
	  (when (deleted? t 0)
	    (set! ret t)))
	(when (== p (tree->path t 1 :end))
	  (when (or (not del?) (deleted? t 2))
	    (set! ret t)))))
    (when (not ret)
      (with t (path->tree (cDr p))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p) 0)
	    (when (deleted? t 0)
	      (set! ret t)))
	  (when (== (cAr p) 1)
	    (when (or (not del?) (deleted? t 2))
	      (set! ret t))))))
    (when (and (not ret) (!= p p*))
      (with t (path->tree (cDr p*))
	(when (tree-in? t '(around around*))
	  (when (== (cAr p*) 0)
	    (when (deleted? t 0)
	      (set! ret t))))))
    ret))

(tm-define (math-bracket-open lb rb large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (== (get-preference "automatic brackets") "off")
    (make-bracket-open lb rb large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (if (selection-active-normal?)
	(begin
	  (clipboard-cut "temp")
          (insert-go-to `(,(if large? 'around* 'around) ,lb "" ,rb) '(1 0))
	  (clipboard-paste "temp"))
        (let* ((t (find-adjacent-around #t))
               (u (find-adjacent-around #f)))
          (cond ((and t (deleted? t 2)
                      (or (not (deleted? t 0))
                          (tree-at-end? t)))
                 (tree-set t 2 lb)
                 (tree-go-to t :end))
                ((and t (deleted? t 0))
                 (tree-set t 0 lb)
                 (tree-go-to t 1 :start))
                ((and u (== lb rb)
                      (== (tree->stree (tree-ref u 2)) rb))
                 (tree-go-to u :end))
                ((and u (== rb "|")
                      (== (tree->stree (tree-ref u 0)) "<langle>"))
                 (tree-set u 2 rb)
                 (tree-go-to u :end))
                ((try-matching-insert #t lb large?)
                 (noop))
                (else
                  (insert-go-to `(,(if large? 'around* 'around) ,lb "" ,rb)
                                '(1 0))))))))

(tm-define (math-separator sep large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (and (string? sep) (string-starts? sep "<") (string-ends? sep ">"))
    (set! sep (substring sep 1 (- (string-length sep) 1))))
  (when (== (get-preference "automatic brackets") "off")
    (make-separator sep large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (make-separator sep large?)))

(tm-define (math-bracket-close rb lb large?)
  (when (== large? 'default)
    (set! large? (!= (get-preference "use large brackets") "off")))
  (when (== (get-preference "automatic brackets") "off")
    (make-bracket-close rb lb large?)
    (brackets-refresh))
  (when (!= (get-preference "automatic brackets") "off")
    (let* ((t (find-adjacent-around #t))
	   (u (find-adjacent-around #f)))
      (cond ((and t (deleted? t 0)
                  (or (not (deleted? t 2))
                      (not (tree-at-end? t))))
	     (tree-set t 0 rb)
	     (tree-go-to t 1 :start))
	    ((and t (deleted? t 2))
	     (tree-set t 2 rb)
	     (tree-go-to t :end))
	    (u
	     (tree-set u 2 rb)
	     (tree-go-to u :end))
	    ((try-matching-insert #f rb large?)
	     (noop))
	    (else
	      (set-message "Error: bracket does not match"
			   (force-string rb)))))))

(tm-define (math-big-operator op)
  ;;(when (== (get-preference "automatic brackets") "off")
  ;;  (make-big-operator op)
  ;;  (brackets-refresh))
  ;;(when (!= (get-preference "automatic brackets") "off")
  ;;  (insert-go-to `(big-around ,(make-small op) "") '(1 0)))
  (insert `(big ,op)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Correction of mathematical formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (manual-correct-math t)
  (with mode (tree->stree (get-env-tree-at "mode" (tree->path t :start)))
    (if (!= mode "math")
	(manual-correct t)
	(with r (manual-correct `(math ,t))
	  (if (tm-func? r 'math 1) (tm-ref r 0) r)))))

(define (math-correct-tree t)
  (with r (manual-correct-math t)
    (when (!= r t)
      (tree-set! t r))))

(define (math-manually-correct-tree t)
  (with r (manual-correct-math t)
    (when (!= r t)
      (import-from (version version-compare))
      (let* ((t1 (tree->stree t))
	     (t2 (tree->stree r))
	     (tb (compare-versions t1 t2))
	     (rt (stree->tree tb)))
	(tree-set! t rt)
	(tree-go-to t :start)
	(version-next-difference)))))

(tm-define (math-correct-all)
  (:synopsis "Correct selected formula or all formulas in document")
  (if (selection-active-any?)
      (if (== (selection-tree) (path->tree (selection-path)))
	  (math-correct-tree (path->tree (selection-path)))
	  (set-message "Only implemented for complete subtrees"
		       "correct formula"))
      (math-correct-tree (buffer-tree))))

(tm-define (math-correct-manually)
  (:synopsis "Manually correct selected formula or all formulas in document")
  (if (selection-active-any?)
      (if (== (selection-tree) (path->tree (selection-path)))
	  (math-manually-correct-tree (path->tree (selection-path)))
	  (set-message "Only implemented for complete subtrees"
		       "correct formula"))
      (math-manually-correct-tree (buffer-tree))))
