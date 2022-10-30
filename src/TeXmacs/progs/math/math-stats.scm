
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-speech.scm
;; DESCRIPTION : analyze statistical properties of mathematical documents
;;               as needed for speech recognition, for instance
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-stats)
  (:use (convert tools tmconcat)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface to C++ statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stats-url #f)
(define stats-lines -1)
(define stats-cursor #f)

(define (stats-update)
  (let* ((buf (buffer-tree))
         (n (tree-arity buf))
         (down (tree-ref buf :down)))
    (when (or (!= stats-url (current-buffer))
              (!= stats-lines n))
      ;;(display* "update  buffer stats\n")
      (math-stats-compile "buffer" buf "text")
      ;;(display* "updated buffer stats\n")
      (set! stats-url (current-buffer))
      (set! stats-lines n))
    (when (and down (!= (tm->stree down) stats-cursor))
      ;;(display* "update  cursor stats\n")
      (math-stats-compile "cursor" (tree-ref buf :down) "text")
      ;;(display* "updated cursor stats\n")
      (set! stats-cursor (tm->stree down)))))

(define (stats-occurrences* s)
  (+ (* 10 (math-stats-occurrences "cursor" s))
     (math-stats-occurrences "buffer" s)))

(tm-define (stats-occurrences s)
  (stats-update)
  (stats-occurrences* s))

(define (stats-in-role* s)
  (if (not s) 0
      (+ (* 10 (math-stats-number-in-role "cursor" s))
         (math-stats-number-in-role "buffer" s))))

(tm-define (stats-in-role s)
  (stats-update)
  (stats-in-role* s))

(tm-define (stats-has? s)
  (> (stats-occurrences s) 0))

(tm-define (stats-filter l)
  (stats-update)
  (list-filter l (lambda (s) (> (stats-occurrences* s) 0))))

(tm-define (stats-role? s)
  (> (stats-in-role s) 0))

(tm-define (stats-better? alt best)
  (> (stats-occurrences alt) (stats-occurrences best)))

(define (stats-best* l0 c0 l c)
  (cond ((null? l) l0)
        ((> (car c) c0) (stats-best* (car l) (car c) (cdr l) (cdr c)))
        (else (stats-best* l0 c0 (cdr l) (cdr c)))))

(tm-define (stats-best . l)
  (stats-update)
  (with c (map stats-occurrences l)
    (stats-best* (car l) (car c) (cdr l) (cdr c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol categories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (frac-context? t) (tree-in? t '(frac frac*)))
(tm-define (around-context? t) (tree-in? t '(around around*)))
(tm-define (wide-context? t) (tree-in? t '(wide wide*)))

(tm-define (math-symbol? s)
  (and (string? s) (== (math-symbol-type s) "symbol")))

(tm-define (math-operator? s)
  (and (string? s) (>= (string-length s) 2) (string-alpha? s)))

(tm-define (math-relation? s)
  (and (string? s) (== (math-symbol-group s) "Relation-nolim-symbol")))

(define weak-infix-types
  (list "Assign-symbol"
        "Models-symbol" "Modeled-symbol"
        "Imply-nolim-symbol" "Or-symbol" "And-symbol"
        "Relation-nolim-symbol" "Arrow-nolim-symbol"))

(tm-define (math-weak-infix? s)
  (and (string? s) (in? (math-symbol-group s) weak-infix-types)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis of content before cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-previous* s k)
  (with j (string-previous s k)
    (if (== j 0) j
        (with i (string-previous s j)
          (cond ((and (string-alpha? (substring s j k))
                      (or (string-alpha? (substring s i j))
                          (== (substring s i j) "<mathd>")))
                 (string-previous* s j))
                ((and (string-number? (substring s j k))
                      (string-number? (substring s i j)))
                 (string-previous* s j))
                (else j))))))

(tm-define (expr-before-cursor)
  (let* ((t (cursor-tree))
	 (i (cAr (cursor-path))))
    (cond ((and (tree-atomic? t) (> i 0))
	   (with s (tree->string t)
	     (with j (string-previous* s i)
	       (substring s j i))))
	  ((tree-atomic? t) #f)
	  ((> i 0) t)
	  (else #f))))

(tm-define (root-before-cursor)
  (with t (expr-before-cursor)
    (cond ((string? t) t)
          ((not (tree? t)) t)
          (else
            (while (and (tree? t) (tree-in? t '(rsub rsup)))
              (set! t (tree-ref t :previous)))
            (while (and (tree? t) (tree-in? t '(wide wide* neg)))
              (set! t (tree-ref t 0)))
            (cond ((not t) t)
                  ((tree-atomic? t)
                   (let* ((s (tree->string t))
                          (n (string-length s))
                          (j (if (> n 0) (string-previous* s n) n)))
                     (substring s j n)))
                  (else t))))))

(define (before-cursor-path)
  (let* ((t (cursor-tree))
         (p (cDr (cursor-path)))
	 (i (cAr (cursor-path))))
    (cond ((and (tree-atomic? t) (> i 0))
	   (with s (tree->string t)
	     (with j (string-previous* s i)
               (cond ((> j 0) (rcons p j))
                     ((tree-ref t :previous) (tree->path t :previous :end))
                     (else (rcons p j))))))
	  ((tree-atomic? t) #f)
	  ((> i 0) (rcons p 0))
	  (else #f))))

(tm-define (expr-before-before-cursor)
  (and-with p (before-cursor-path)
    (with-cursor p
      (expr-before-cursor))))

(tm-define (root-before-before-cursor)
  (and-with p (before-cursor-path)
    (with-cursor p
      (root-before-cursor))))

(tm-define (root-before-start)
  (and-with p (rcons (cDr (cursor-path)) 0)
    (with-cursor p
      (root-before-cursor))))

(tm-define (select-before-cursor)
  (let* ((t (cursor-tree))
         (p (cursor-path))
	 (i (cAr p)))
    (cond ((and (tree-atomic? t) (> i 0))
	   (with s (tree->string t)
	     (with j (string-previous* s i)
               (selection-set (rcons (cDr p) j) p))))
	  ((or (tree-atomic? t) (<= i 0)) (selection-cancel))
          ((tree-in? t '(rsub rsup around around*))
           (let* ((j (cAr (cDr p)))
                  (q (cDr (cDr p))))
             (while (and (> j 0)
                         (tree-in? (path->tree (rcons q j))
                                   '(rsub rsup around around*)))
               (set! j (- j 1)))
             (with u (path->tree (rcons q j))
               (if (or (not (tree-atomic? u)) (tree-empty? u))
                   (selection-set (append q (list j 0)) p)
                   (let* ((s (tree->string u))
                          (n (string-length s))
                          (k (string-previous* s n)))
                     (if (and (tree-in? (path->tree (rcons q (+ j 1)))
                                        '(around around*))
                              (not (math-symbol? (substring s k n)))
                              (not (math-operator? (substring s k n))))
                         (selection-set (append q (list (+ j 1) 0)) p)
                         (selection-set (append q (list j k)) p)))))))
	  (else (selection-set (rcons (cDr p) 0) p)))))

(tm-define (cut-before-cursor)
  (select-before-cursor)
  (if (selection-active-any?)
      (with t (selection-tree)
        (cpp-clipboard-cut "dummy")
        t)
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statistical determination of implicit content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stats-best-implicit* l r p)
  (define (count ok? x) (if ok? (stats-in-role x) 0))
  (let* ((mul (count (in? :multiply    p) (tmconcat l "*" r)))
         (spc (count (in? :space       p) (tmconcat l " " r)))
         (com (count (in? :comma       p) (tmconcat l "," r)))
         (app (count (in? :apply       p) (tmconcat l `(around "(" ,r ")"))))
         (bra (count (in? :brackets    p) (tmconcat l `(around "[" ,r "]"))))
         (sub (count (in? :subscript   p) (tmconcat l `(rsub ,r))))
         (sup (count (in? :superscript p) (tmconcat l `(rsup ,r))))
         (m (max mul spc com app bra sub sup)))
    (when (== r "") (set! sup 0))
    (cond ((== m 0) :none)
          ((== m mul) :multiply)
          ((== m spc) :space)
          ((== m com) :comma)
          ((== m app) :apply)
          ((== m bra) :brackets)
          ((== m sub) :subscript)
          ((== m sup) :superscript)
          (else :none))))

(tm-define (stats-best-implicit l r p)
  (let* ((impl1 (stats-best-implicit* l r p))
         (impl2 (stats-best-implicit* l "" p)))
    (cond ((!= impl1 :none) impl1)
          ((!= impl2 :none) impl2)
          (else :none))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default fall back implicit behaviour
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define basic-letters (list "a" "b" "c" "d" "e" "o" "u" "v" "w" "x" "y" "z"
                            "<alpha>" "<beta>" "<gamma>" "<delta>"
                            "<varepsilon>" "<eta>" "<theta>"
                            "<lambda>" "<mu>" "<xi>" "<omicron>" "<pi>" "<rho>"
                            "<sigma>" "<tau>" "<upsilon>" "<chi>" "<omega>"
                            "<mathe>" "<mathi>" "<mathpi>" "<mathgamma>"
                            "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
                            "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
                            "U" "V" "W" "X" "Y" "Z" "<Delta>" "<Theta>"
                            "<Lambda>" "<Xi>" "<Pi>" "<Sigma>" "<Omega>"))
(define function-letters (list "f" "g" "h" "<phi>" "<psi>" "<zeta>"
                               "<Gamma>" "<Phi>" "<Psi>"))
(define index-letters (list "1" "i" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t"
                            "<iota>" "<kappa>" "<nu>"))

(define (best-implicit* l r p)
  (with impl1 (stats-best-implicit l r p)
    (cond ((!= impl1 :none) impl1)
          ((and (in? :multiply p)
                (in? l (list "<pi>" "<mathpi>"))
                (== r "i"))
           :multiply)
          ((and (in? :space p)
                (in? l standard-operators))
           :space)
          ((and (in? :subscript p)
                (math-symbol? l)
                (not (string-number? l))
                (in? r index-letters))
           :subscript)
          ((and (in? :apply p)
                (in? l function-letters)
                (math-symbol? r)
                (nin? r function-letters))
           :apply)
          ((and (in? :multiply p)
                (or (string-number? l)
                    (math-symbol? l)
                    (tm-in? l '(sqrt frac frac* around around*))))
           :multiply)
          (else :none))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for determination and insertion of implicit content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (permitted-implicit)
  (let* ((l (list :multiply :space :comma :apply :brackets
                  :subscript :superscript))
         (x (expr-before-cursor))
         (a (tree-innermost around-context?)))
    (when (not a)
      ;; TODO: also disallow commas inside brackets that
      ;; enclose infix operators or relations
      (set! l (list-remove l :comma)))
    (when (== x " ")
      (set! l (list)))
    (while (tm-in? x '(rsub rsup))
      (when (tree-is? x 'rsub)
        (set! l (list-remove l :subscript)))
      (when (tree-is? x 'rsup)
        (set! l (list-remove l :superscript)))
      (set! x (tm-ref x :previous)))
    (when (and (tree? x) (tree-atomic? x))
      (with-cursor (tree->path x :end)
        (set! x (expr-before-cursor))))
    (when (tm-atomic? x)
      (set! x (tm->string x)))
    (when (if (string? x)
              (not (or (string-number? x) (math-symbol? x) (math-operator? x)))
              (not (tree-in? x '(math-ss math-tt rsub rsup wide wide*
                                 frac frac* sqrt around around*))))
      (set! l (list)))
    l))

(define (best-permitted-implicit l r p)
  (when (tree? l) (set! l (tm->stree l)))
  (when (tree? r) (set! r (tm->stree r)))
  (when (string-number? r)  (set! r "1"))
  (when (tm-func? r 'sqrt)  (set! r '(sqrt "1")))
  (when (tm-func? r 'frac)  (set! r '(frac "1" "1")))
  (when (tm-func? r 'frac*) (set! r '(frac "1" "1")))
  (best-implicit* l r p))

(tm-define (best-implicit l r)
  (if (not l) :none (best-permitted-implicit l r (permitted-implicit))))

(tm-define (insert-implicit impl x)
  (cond ((== impl :multiply)    (insert "*") (insert x))
        ((== impl :space)       (insert " ") (insert x))
        ((== impl :comma)       (insert ",") (insert x))
        ((== impl :apply)       (insert `(around "(" ,x ")")))
        ((== impl :brackets)    (insert `(around "[" ,x "]")))
        ((== impl :subscript)   (insert `(rsub ,x)))
        ((== impl :superscript) (insert `(rsup ,x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fine-grained contextual preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (infix-like? x)
  (and (string? x)
       (in? (math-symbol-type x)
            (list "infix" "prefix-infix" "separator"))))

(define (get-rich-contextual x)
  (let* ((prev  (expr-before-cursor))
         (prev2 (root-before-before-cursor))
         (up    (tm-ref (cursor-tree) :up))
         (prev* (root-before-start)))
    (when (== prev "-") (set! prev "+"))
    (cond ((and prev*
                (infix-like? prev))
           (tmconcat prev2 prev x))
          ((and prev* up
                (tm-in? up '(rsub rsup))
                (tree-empty? (tm-ref up 0)))
           (tmconcat prev* `(,(tm-label up) ,x)))
          ((and prev* up
                (tm-in? up '(around around*))
                (tree-empty? (tm-ref up 1)))
           (tmconcat prev* `(,(tm-label up) ,(tm-ref 0) ,x ,(tm-ref 2))))
          (else #f))))

(define (get-combine x impl)
  (and-with prev (expr-before-cursor)
    (cond ((== impl :multiply   ) (tmconcat prev "*" x))
          ((== impl :space      ) (tmconcat prev " " x))
          ((== impl :comma      ) (tmconcat prev "," x))
          ((== impl :apply      ) (tmconcat prev `(around "(" ,x ")")))
          ((== impl :brackets   ) (tmconcat prev `(around "[" ,x "]")))
          ((== impl :subscript  ) (tmconcat prev `(rsub ,x)))
          ((== impl :superscript) (tmconcat prev `(rsub ,x)))
          (else #f))))

(tm-define (stats-rich-contextual x)
  (max (stats-in-role (get-rich-contextual x))
       (stats-in-role (get-combine x :multiply))
       (stats-in-role (get-combine x :space))
       (stats-in-role (get-combine x :comma))
       (stats-in-role (get-combine x :apply))
       (stats-in-role (get-combine x :brackets))
       (stats-in-role (get-combine x :subscript))
       (stats-in-role (get-combine x :superscript))
       0))

(define (get-medium-contextual x)
  (let* ((prev  (expr-before-cursor))
         (up    (tm-ref (cursor-tree) :up))
         (prev* (root-before-start)))
    (when (== prev "-") (set! prev "+"))
    (cond ((infix-like? prev)
           (tmconcat prev x))
          ((and (tm-in? up '(rsub rsup))
                (tree-empty? (tm-ref up 0)))
           `(,(tm-label up) ,x))
          ((and (tm-in? up '(around around*))
                (tree-empty? (tm-ref up 1)))
           `(,(tm-label up) ,(tm-ref 0) ,x ,(tm-ref 2)))
          (else #f))))

(tm-define (stats-medium-contextual x)
  (with c (get-medium-contextual x)
    (if (not c) 0 (stats-in-role c))))

(tm-define (stats-prefer-weak-contextual? what over prefer?)
  (let* ((what3 (stats-occurrences what))
         (over3 (stats-occurrences over)))
    ;;(display* "  weak  : " what ", " what3
    ;;          "; " over ", " over3 "\n")
    (cond ((prefer? what3 over3) #t)
          ((> over3 0) #f)
          (else #f))))

(tm-define (stats-prefer-medium-contextual? what over prefer?)
  (let* ((what2 (stats-medium-contextual what))
         (over2 (stats-medium-contextual over)))
    ;;(display* "  medium: " what ", " what2
    ;;          "; " over ", " over2 "\n")
    (cond ((> what2 over2) #t)
          ((> over2 0) #f)
          (else (stats-prefer-weak-contextual? what over prefer?)))))

(tm-define (stats-prefer-contextual? what over prefer?)
  (let* ((what1 (stats-rich-contextual what))
         (over1 (stats-rich-contextual over)))
    ;;(display* "  rich  : " what ", " what1
    ;;          "; " over ", " over1 "\n")
    (cond ((> what1 over1) #t)
          ((> over1 0) #f)
          (else (stats-prefer-medium-contextual? what over prefer?)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Contextual preferences for letter combinations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stats-combination x y)
  (max (stats-in-role (tmconcat x "*" y))
       (stats-in-role (tmconcat x " " y))
       (stats-in-role (tmconcat x "," y))
       (stats-in-role (tmconcat x `(around "(" ,y ")")))
       (stats-in-role (tmconcat x `(around "[" ,y "]")))
       (stats-in-role (tmconcat x `(rsub ,y)))
       (stats-in-role (tmconcat x `(rsup ,y)))
       0))

(tm-define (stats-prefer-combination? p1 p2)
  (with (x1 y1) p1
    (with (x2 y2) p2
      (let* ((occ1 (stats-combination x1 y1))
             (occ2 (stats-combination x2 y2)))
        ;;(display* "  compare: " p1 ", " occ1 "; " p2 ", " occ2 "\n")
        (cond ((> occ1 occ2) #t)
              ((> occ2 0) #f)
              (else #f))))))

(define (stats-preferred-combination* best l)
  (cond ((null? l) best)
        ((stats-prefer-combination? (car l) best)
         (stats-preferred-combination* (car l) (cdr l)))
        (else (stats-preferred-combination* best (cdr l)))))

(define (list-pairs l1 l2)
  (if (null? l1) l1
      (append (map (cut list (car l1) <>) l2)
              (list-pairs (cdr l1) l2))))

(tm-define (stats-preferred-combination l1 l2)
  (and (nnull? l1) (nnull? l2)
       (let* ((prod (list-pairs l1 l2))
              (comb (stats-preferred-combination* (car prod) (cdr prod))))
         (with (x y) comb
           (and (> (stats-combination x y) 0) comb)))))
