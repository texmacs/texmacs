
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : math-speech.scm
;; DESCRIPTION : control mathematical editing via speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (math math-speech)
  (:use (math math-kbd)
        (utils library cursor)
        (convert tools tmconcat)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of main speech hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-letter-mode (list))
(define speech-letter-mode* (list))
(define speech-operator-mode :off)
(define speech-state (list))

(tm-define (speech-done)
  (set! speech-letter-mode (list))
  (set! speech-operator-mode :off)
  (while (nnull? speech-state)
    (speech-exit-innermost))
  (former))

(tm-define (speech-current-mode)
  (:mode in-math?)
  'math)

(tm-define (speech-exec-hook s)
  (:mode in-math?)
  ;;(display* "Hook " s "\n")
  (and (nin? s standard-operators)
       (>= (string-length s) 2)
       (<= (string-length s) 10)
       (string-alpha? s)
       (let* ((ss (locase-all s))
              (Ss (upcase-first ss))
              (SS (upcase-all s)))
         (stats-update)
         (cond ((stats-has? ss) (speech-insert-operator ss) #t)
               ((stats-has? Ss) (speech-insert-operator Ss) #t)
               ((stats-has? SS) (speech-insert-operator SS) #t)
               (else #f)))))

(tm-define (kbd-speech s)
  (:mode in-math?)
  ;;(display* "Math speech " s "\n")
  (cond ((speech-make s) (noop))
        (else (speech-exec (letterize (locase-all s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Separate letters that where agglutinated by the speech recognition software
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-collection consonants
  "b" "c" "d" "f" "g" "h" "j" "k" "l" "m"
  "n" "p" "q" "r" "s" "t" "v" "w" "x" "z"
  "y")

(define (letterize-one lan s)
  (with l (tmstring->list s)
    (cond ((< (length l) 2) s)
          ((or (speech-has? lan 'dont-break (substring s 0 2))
               (speech-has? lan 'dont-break (string-take-right s 2)))
           s)
          ((or (forall? (cut ahash-ref consonants <>) (cdr l))
               (forall? (cut ahash-ref consonants <>) (cDr l)))
           (list->tmstring (list-intersperse l " ")))
          (else s))))

(tm-define (letterize s)
  (let* ((lan (speech-language))
         (l (string-decompose s " "))
         (r (map (cut letterize-one lan <>) l)))
    (string-recompose r " ")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech state related routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (speech-exit-from op)
  (and-with t (tree-innermost op)
    (tree-go-to t :end))
  (set! speech-state (cdr speech-state)))

(define (around-context? t) (tree-in? t '(around around*)))
(define (wide-context? t) (tree-in? t '(wide wide*)))

(define (speech-exit-innermost)
  (with type (and (nnull? speech-state) (car speech-state))
    (cond ((not type) (noop))
          ((== type :subscript) (speech-exit-from 'rsub))
          ((== type :superscript) (speech-exit-from 'rsup))
          ((== type :over) (speech-exit-from 'frac))
          ((== type :sqrt) (speech-exit-from 'sqrt))
          ((== type :wide) (speech-exit-from wide-context?))
          ((== type :apply) (speech-exit-from around-context?))
          ((== type :factor) (speech-exit-from around-context?))
          ((== type :brackets) (speech-exit-from around-context?))
          ((== type :braces) (speech-exit-from around-context?))
          (else (set! speech-state (cdr speech-state))))))

(define (speech-enter type)
  (set! speech-state (cons type speech-state)))

(tm-define (speech-leave)
  (:mode in-math?)
  (if (nnull? speech-state)
      (speech-exit-innermost)
      (structured-exit-right)))

(tm-define (speech-exit-scripts)
  (when (and (nnull? speech-state)
             (in? (car speech-state) (list :subscript :superscript)))
    (speech-exit-innermost)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exploiting statistics
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

(tm-define (stats-occurrences s)
  (stats-update)
  (+ (* 1000 (math-stats-occurrences "cursor" s))
     (math-stats-occurrences "buffer" s)))

(tm-define (stats-in-role s)
  (if (not s) 0
      (begin
        (stats-update)
        (+ (* 1000 (math-stats-number-in-role "cursor" s))
           (math-stats-number-in-role "buffer" s)))))

(tm-define (stats-has? s)
  (> (stats-occurrences s) 0))

(tm-define (stats-better? alt best)
  (> (stats-occurrences alt) (stats-occurrences best)))

(tm-define (stats-best . l)
  (stats-update)
  (with best (car l)
    (for (alt (cdr l))
      (when (stats-better? alt best)
        (set! best alt)))
    best))

(define (stats-best-implicit* l r p)
  (define (count ok? x) (if ok? (stats-in-role x) 0))
  (let* ((mul (count (in? :multiply    p) (tmconcat l "*" r)))
         (spc (count (in? :space       p) (tmconcat l " " r)))
         (com (count (in? :comma       p) (tmconcat l "," r)))
         (app (count (in? :apply       p) (tmconcat l `(around "(" ,r ")"))))
         (sub (count (in? :subscript   p) (tmconcat l `(rsub ,r))))
         (sup (count (in? :superscript p) (tmconcat l `(rsup ,r))))
         (m (max mul spc com app sub sup)))
    (when (== r "") (set! sup 0))
    (cond ((== m 0) :none)
          ((== m mul) :multiply)
          ((== m spc) :space)
          ((== m com) :comma)
          ((== m app) :apply)
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
;; Default letter roles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define basic-letters (list "a" "b" "c" "d" "u" "v" "w" "x" "y" "z"
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
          ((and (in? :space p)
                (in? l standard-operators))
           :space)
          ((and (in? :multiply p)
                (or (string-number? l)
                    (and (in? l basic-letters) (in? r basic-letters))
                    (and (in? l index-letters) (in? r index-letters))
                    (and (in? l (list "<pi>" "<mathpi>")) (== r "i"))
                    (tm-in? l '(sqrt frac around around*))))
           :multiply)
          ((and (in? :subscript p)
                (letter-symbol? l)
                (in? r index-letters))
           :subscript)
          ((and (in? :superscript p)
                (in? l function-letters)
                (in? r basic-letters))
           :apply)
          (else :none))))

(tm-define (best-implicit l r p)
  (if (not l) :none
      (begin
        (when (tree? l) (set! l (tm->stree l)))
        (when (tree? r) (set! r (tm->stree r)))
        (when (string-number? r) (set! r "1"))
        (when (tm-func? r 'sqrt) (set! r '(sqrt "1")))
        (when (tm-func? r 'frac) (set! r '(frac "1" "1")))
        (best-implicit* l r p))))

(define (get-permitted)
  (let* ((l (list :multiply :space :comma :apply :subscript :superscript))
         (x (expr-before-cursor))
         (a (tree-innermost around-context?)))
    (when (not a)
      ;; TODO: also disallow commas inside brackets that
      ;; enclose infix operators or relations
      (set! l (list-remove l :comma)))
    (when (tree-is? x 'rsub)
      (set! l (list-remove l :subscript)))
    (when (tree-is? x 'rsup)
      (set! l (list-remove l :superscript)))
    (when (and (nstring? x)
               (not (tree-in? x '(math-ss math-tt frac sqrt around around*))))
      (set! l (list)))
    (when (and (string? x) (!= (math-symbol-type x) "symbol"))
      (set! l (list)))
    l))

(define (insert-implicit impl x)
  (cond ((== impl :multiply)    (insert "*") (insert x))
        ((== impl :space)       (insert " ") (insert x))
        ((== impl :comma)       (insert ",") (insert x))
        ((== impl :apply)       (insert `(around "(" ,x ")")))
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
    (cond ((infix-like? prev)
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
          ((== impl :subscript  ) (tmconcat prev `(rsub ,x)))
          ((== impl :superscript) (tmconcat prev `(rsub ,x)))
          (else #f))))

(tm-define (stats-rich-contextual x)
  (max (stats-in-role (get-rich-contextual x))
       (stats-in-role (get-combine x :multiply))
       (stats-in-role (get-combine x :space))
       (stats-in-role (get-combine x :comma))
       (stats-in-role (get-combine x :apply))
       (stats-in-role (get-combine x :subscript))
       (stats-in-role (get-combine x :superscript))
       0))

(define (get-contextual x)
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

(tm-define (stats-contextual x)
  (with c (get-contextual x)
    (if (not c) 0 (stats-in-role c))))

(tm-define (stats-prefer-contextual? what over prefer?)
  (let* ((what1 (stats-rich-contextual what))
         (over1 (stats-rich-contextual over)))
    ;;(display* "  rich  : " what ", " what1
    ;;          "; " over ", " over1 "\n")
    (cond ((> what1 over1) #t)
          ((> over1 0) #f)
          (else (let* ((what2 (stats-contextual what))
                       (over2 (stats-contextual over)))
                  ;;(display* "  medium: " what ", " what2
                  ;;          "; " over ", " over2 "\n")
                  (cond ((> what2 over2) #t)
                        ((> over2 0) #f)
                        (else (let* ((what3 (stats-occurrences what))
                                     (over3 (stats-occurrences over)))
                                ;;(display* "  weak  : " what ", " what3
                                ;;          "; " over ", " over3 "\n")
                                (cond ((prefer? what3 over3) #t)
                                      ((> over3 0) #f)
                                      (else #f))))))))))

(define (stats-prefer-predicate mode)
  (cond ((== mode :normal) >)
        ((== mode :strong) (lambda (w o) (> w (* 5 (+ o 1)))))
        (else >)))

(tm-define (stats-prefer? what over mode)
  (let* ((prefer? (stats-prefer-predicate mode))
         (what* (best-letter-variant what))
         (over* (best-letter-variant over)))
    (stats-prefer-contextual? what* over* prefer?)))

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
	       (rcons p j))))
	  ((tree-atomic? t) #f)
	  ((> i 0) (rcons p 0))
	  (else #f))))

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
          ((tree-in? t '(rsub rsup))
           (let* ((j (cAr (cDr p)))
                  (q (cDr (cDr p))))
             (while (and (> j 0)
                         (tree-in? (path->tree (rcons q j))
                                   '(rsub rsup)))
               (set! j (- j 1)))
             (with u (path->tree (rcons q j))
               (if (or (not (tree-atomic? u)) (tree-empty? u))
                   (selection-set (append q (list j 0)) p)
                   (let* ((s (tree->string u))
                          (n (string-length s))
                          (k (string-previous* s n)))
                     (selection-set (append q (list j k)) p))))))
	  (else (selection-set (rcons (cDr p) 0) p)))))

(tm-define (cut-before-cursor)
  (select-before-cursor)
  (if (selection-active-any?)
      (with t (selection-tree)
        (cpp-clipboard-cut "dummy")
        t)
      ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers, letters, and other symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (letter-symbol? s)
  (and (string? s)
       (or (== (math-symbol-group s) "Letter-symbol")
           (string-alpha? s))))

(tm-define (speech-insert-number x)
  (:mode in-math?)
  (let* ((permit (get-permitted))
         (prev (root-before-cursor))
         (impl (best-implicit prev x permit)))
    (cond ((string-number? prev) (insert x))
          ((!= impl :none) (insert-implicit impl x))
          (else (insert x)))
    (speech-exit-scripts)))

(define (update-mode mods type)
  (cond ((in? type mods) mods)
        ((and (in? type (list :cal :bbb))
              (nin? :small mods)
              (nin? :big mods))
         (update-mode (update-mode mods :big) type))
        ((in? type (list :big :small))
         (cons type (list-difference mods (list :big :small))))
        ((in? type (list :bold :medium))
         (cons type (list-difference mods (list :bold :medium))))
        ((in? type (list :up :it :cal :frak :bbb))
         (cons type (list-difference mods (list :up :it :cal :frak :bbb))))
        ((in? type (list :normal :ss :tt))
         (cons type (list-difference mods (list :normal :ss :tt))))
        (else mods)))

(tm-define (speech-alter-letter type)
  ;; Alteration spoken before letter (e.g. 'bold x')
  (with prev (expr-before-cursor)
    (when (not (and (string? prev) (string-alpha? prev)))
      (set! speech-operator-mode :off)))
  (set! speech-letter-mode (update-mode speech-letter-mode type)))

(define (root-letter s)
  (cond ((not s) #f)
        ((tree? s) (root-letter (tm->stree s)))
        ((tm-in? s '(math-ss math-tt)) (root-letter (tm-ref s 0)))
        ((and (string? s) (string-occurs? "-" s) (string-ends? s ">"))
         (let* ((n (string-length s))
                (i (string-search-backwards "-" n s)))
           (string-append "<" (substring s (+ i 1) n))))
        ((string? s) s)
        (else #f)))

(define (best-letter-variant* x)
  (best-variant x speech-letter-mode*))

(tm-define (speech-alter-letter* type)
  ;; Alteration spoken after letter (e.g. 'X calligraphique')
  (set! speech-letter-mode* (update-mode speech-letter-mode* type))
  (and-with prev (root-letter (expr-before-cursor))
    (cut-before-cursor)
    (insert (best-letter-variant* prev))))

(define (modified-letter x mods)
  (with x* x
    (when (> (string-length x) 1)
      (set! x (substring x 1 (- (string-length x) 1))))
    (when (in? :big mods)   (set! x (upcase-first x)))
    (when (in? :small mods) (set! x (locase-first x)))
    (cond ((in? :up mods)   (set! x (string-append "up-" x)))
          ((in? :cal mods)  (set! x (string-append "cal-" x)))
          ((in? :frak mods) (set! x (string-append "frak-" x)))
          ((in? :bbb mods)  (set! x (string-append "bbb-" x))))
    (when (in? :bold mods)  (set! x (string-append "b-" x)))
    (when (> (string-length x) 1)
      (set! x (string-append "<" x ">")))
    (cond ((in? :ss mods) (set! x `(math-ss ,x)))
          ((in? :tt mods) (set! x `(math-tt ,x))))
    x))

(define (improve-letter best x mods)
  (when (and (nin? :big mods) (nin? :small mods))
    (set! best (improve-letter best x (cons* :big mods))))
  (when (and (nin? :bold mods) (nin? :medium mods))
    (set! best (improve-letter best x (cons* :bold mods))))
  (when (and (nin? :up mods) (nin? :it mods)
             (nin? :cal mods) (nin? :frak mods) (nin? :bbb mods))
    (set! best (improve-letter best x (cons* :up mods)))
    (set! best (improve-letter best x (cons* :cal mods)))
    (set! best (improve-letter best x (cons* :frak mods)))
    (set! best (improve-letter best x (cons* :bbb mods))))
  (when (and (nin? :ss mods) (nin? :tt mods) (nin? :normal mods))
    (set! best (improve-letter best x (cons* :ss mods)))
    (set! best (improve-letter best x (cons* :tt mods))))
  (stats-best best (modified-letter x mods)))

(define (best-variant x mods)
  ;;(display* "    best variant " x ", " mods "\n")
  (with best (modified-letter x mods)
    (set! best (improve-letter best x mods))
    (when (== mods (list))
      (when (and (== x "e") (stats-better? "<mathe>" best))
        (set! best "<mathe>"))
      (when (and (== x "i") (stats-better? "<mathi>" best))
        (set! best "<mathi>"))
      (when (and (== x "<pi>") (stats-better? "<mathpi>" best))
        (set! best "<mathpi>")))
    (when (and (in? x (list "<epsilon>" "<theta>" "<kappa>" "<pi>"
                            "<rho>" "<sigma>" "<phi>"))
               (nin? :big mods) (nin? :up mods))
      (with y (string-append "<var" (substring x 1 (string-length x)))
        (with var-best (best-variant y mods)
          (when (in? x (list "<epsilon>" "<phi>"))
            (with aux best
              (set! best var-best)
              (set! var-best aux)))
          (when (stats-better? var-best best)
            (set! best var-best)))))
    ;;(display* "    best variant " x ", " mods " -> " best "\n")
    best))

(tm-define (best-letter-variant x)
  (best-variant x speech-letter-mode))

(tm-define (speech-insert-letter x*)
  ;;(display* "insert letter " x* "\n")
  (with prev* (expr-before-cursor)
    (set! speech-letter-mode* (list))
    (when (and (== speech-operator-mode :on)
               (not (and (string? prev*) (string-alpha? prev*))))
      (set! speech-letter-mode (list))
      (set! speech-operator-mode :off))
    (let* ((prev (root-before-cursor))
           (x (best-letter-variant x*))
           (permit (get-permitted))
           (impl (best-implicit prev x permit)))
      ;;(display* "  inserting " x* " as " x "\n")
      (cond ((!= speech-operator-mode :off) (insert x))
            ((!= impl :none) (insert-implicit impl x))
            (else (insert x) (speech-exit-scripts)))
      ;;(display* "  inserted  " x* " as " x "\n")
      (set! speech-letter-mode* speech-letter-mode)
      (when (== speech-operator-mode :start)
        (set! speech-operator-mode :on))
      (when (== speech-operator-mode :off)
        (set! speech-letter-mode (list)))))
  ;;(display* "inserted letter " x* "\n")
  )

(tm-define (speech-insert-symbol x)
  (insert x)
  (speech-exit-scripts))

(tm-define (speech-insert-symbol x)
  (:require (string-number? x))
  (speech-insert-number x))

(tm-define (speech-insert-symbol x)
  (:require (or (in? x roman-letters) (in? x greek-letters)))
  (speech-insert-letter x))

(define (speech-relation-exit)
  (when (nnull? speech-state)
    (when (in? (car speech-state)
               (list :over :sqrt :wide :apply :factor :brackets))
      (speech-leave)
      (speech-relation-exit))))

(tm-define (speech-insert-symbol x)
  (:require (== (math-symbol-group x) "Relation-nolim-symbol"))
  (speech-relation-exit)
  (insert x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textual operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-insert-operator x)
  (set! speech-operator-mode :off)
  (let* ((permit (get-permitted))
         (prev (root-before-cursor))
         (impl (best-implicit prev x permit)))
    (if (!= impl :none)
        (insert-implicit impl x)
        (insert x))))

(tm-define (speech-insert-d x)
  (speech-insert-operator "<mathd>")
  (insert x))

(tm-define (speech-operator)
  (set! speech-operator-mode :start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subscripts, superscripts, and wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-subscript)
  (make 'rsub)
  (speech-enter :subscript))

(tm-define (speech-superscript)
  (make 'rsup)
  (speech-enter :superscript))

(tm-define (speech-insert-superscript s)
  (if (== (expr-before-cursor) " ")
      (with p (cursor-path)
        (with-cursor (append (cDr p) (list (- (cAr p) 1)))
          (math-insert `(rsup ,s))))
      (math-insert `(rsup ,s))))

(tm-define (speech-accent acc)
  (with sel (tm->stree (cut-before-cursor))
    (when (== sel "i") (set! sel "<imath>"))
    (when (== sel "j") (set! sel "<jmath>"))
    (insert `(wide ,sel ,acc))))

(tm-define (speech-accent-under acc)
  (with sel (cut-before-cursor)
    (insert `(wide* ,sel ,acc))))

(tm-define (speech-wide acc)
  (make-wide acc)
  (speech-enter :wide))

(tm-define (speech-wide-under acc)
  (make-wide-under acc)
  (speech-enter :wide))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (speech-start-2d)
  (when (not (selection-active-any?))
    (with prev (root-before-cursor)
      (cond ((or (string-number? prev)
                 (in? prev basic-letters)
                 (in? prev index-letters)
                 (tree-in? prev '(sqrt frac around around*)))
             (insert "*"))
            ((and (in? prev function-letters) (in? x basic-letters))
             (speech-apply-brackets))))))

(tm-define (speech-sqrt)
  (speech-start-2d)
  (make 'sqrt))

(tm-define (speech-sqrt-of)
  (speech-sqrt)
  (speech-enter :sqrt))

(tm-define (speech-fraction)
  (speech-start-2d)
  (make 'frac))

(tm-define (speech-over)
  (with prev (expr-before-cursor)
    (if (tm-is? prev 'big) (make 'rsub)
        (with sel (cut-before-cursor)
          (insert-go-to `(frac ,sel "") (list 1 0))
          (speech-enter :over)))))

(tm-define (go-to-fraction where)
  (with-innermost t 'frac
    (when t
      (cond ((== where :numerator) (tree-go-to t 0 :end))
            ((== where :denominator) (tree-go-to t 1 :end))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-apply-brackets)
  (math-bracket-open "(" ")" 'default)
  (speech-enter :apply))

(tm-define (speech-factor)
  (when (and (nnull? speech-state) (== (car speech-state) :factor))
    (speech-leave))
  (insert "*")
  (math-bracket-open "(" ")" 'default)
  (speech-enter :factor))

(tm-define (speech-brackets open close)
  (math-bracket-open open close 'default)
  (speech-enter (if (== open "{") :braces :brackets)))

(tm-define (speech-open open close)
  (math-bracket-open open close 'default))

(tm-define (speech-close)
  (and-with t (tree-innermost around-context?)
    (tree-go-to t :end)))

(tm-define (speech-of)
  (with prev (expr-before-cursor)
    (cond ((tm-is? prev 'big) (make 'rsub))
          ((editing-big-operator?)
           (with-innermost t script-context?
             (tree-go-to t :end)))
          ((or (letter-symbol? prev)
               (tm-in? prev '(with math-ss math-tt rsub rsup around)))
           (speech-apply-brackets)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big operators and dots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (editing-big-operator?)
  (and-with t (tree-innermost script-context?)
    (while (and t (script-context? t))
      (set! t (tree-ref t :previous)))
    (and t (tree-is? t 'big))))

(tm-define (speech-for)
  (make 'rsub))

(tm-define (speech-dots sym dots)
  (speech-insert-symbol sym)
  (insert dots)
  (speech-insert-symbol sym))

(define (big->dots prev t sym dots)
  (with arg (tm->stree (tm-ref t 0))
    (selection-set (rcons (tree->path prev) 0) (rcons (tree->path t) 1))
    (cpp-clipboard-cut "dummy")
    (insert arg)
    (speech-dots sym dots)))

(define (relation-symbol? x)
  (and (string? x)
       (== (math-symbol-group x) "Relation-nolim-symbol")))

(tm-define (speech-until)
  (if (inside? 'rsub)
      (with-innermost t 'rsub
        (let* ((prev (tree-ref t :previous))
               (big? (and (tree? prev) (tree-is? prev 'big)))
               (op (and big? (tm->stree (tm-ref prev 0))))
               (l (map tm->stree (concat-tokenize-math (tm-ref t 0)))))
          (cond ((not big?) (noop))
                ((exists? relation-symbol? l)
                 (tree-go-to t :end)
                 (make 'rsup))
                ((== op "sum") (big->dots prev t "+" "<cdots>"))
                ((== op "prod") (big->dots prev t "*" "<cdots>"))
                (else
                  (tree-go-to t :end)
                  (make 'rsup)))))
      (speech-dots "," "<ldots>")))

(tm-define (speech-to)
  (cond ((inside? 'rsub) (speech-until))
        (else (speech-insert-number "2"))))
