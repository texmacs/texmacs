
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
        (utils library cursor)))

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
    (structured-exit-right)
    (set! speech-state (cdr speech-state)))
  (former))

(tm-define (speech-current-mode)
  (:mode in-math?)
  'math)

(tm-define (speech-exec-hook s)
  (:mode in-math?)
  ;;(display* "Hook " s "\n")
  (and (>= (string-length s) 2)
       (<= (string-length s) 10)
       (string-alpha? s)
       (let* ((ss (locase-all s))
              (Ss (upcase-first ss))
              (SS (upcase-all s)))
         (stats-update)
         (cond ((stats-has? ss) (speech-insert-op ss) #t)
               ((stats-has? Ss) (speech-insert-op Ss) #t)
               ((stats-has? SS) (speech-insert-op SS) #t)
               (else #f)))))

(tm-define (kbd-speech s)
  (:mode in-math?)
  ;;(display* "Math speech " s "\n")
  ;;(display* "  Letterized " (letterize (locase-all s)) "\n")
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

(tm-define (speech-leave)
  (:mode in-math?)
  (when (nnull? speech-state)
    (set! speech-state (cdr speech-state)))
  (structured-exit-right))

(tm-define (speech-exit-scripts)
  (when (and (nnull? speech-state)
             (in? (car speech-state) (list :subscript :superscript)))
    (structured-exit-right)
    (set! speech-state (cdr speech-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exploiting statistics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stats-url #f)
(define stats-lines -1)

(define (stats-update)
  (let* ((buf (buffer-tree))
         (n (tree-arity buf)))
    (when (or (!= stats-url (current-buffer))
              (!= stats-lines n))
      (math-stats-compile "buffer" buf "text")
      (set! stats-url (current-buffer))
      (set! stats-lines n))
    (when (tree-ref buf :down)
      (math-stats-compile "cursor" (tree-ref buf :down) "text"))))

(tm-define (stats-has? s)
  (or (> (math-stats-occurrences "cursor" s) 0)
      (> (math-stats-occurrences "buffer" s) 0)))

(tm-define (stats-better? alt best)
  (let* ((alt1  (math-stats-occurrences "cursor" alt))
         (best1 (math-stats-occurrences "cursor" best))
         (alt2  (math-stats-occurrences "buffer" alt))
         (best2 (math-stats-occurrences "buffer" best)))
    (or (> alt1 best1) (and (== alt1 best1) (> alt2 best2)))))

(tm-define (stats-better*? alt best)
  (> (math-stats-occurrences "cursor" alt)
     (math-stats-occurrences "cursor" best)))

(tm-define (stats-best . l)
  (stats-update)
  (with best (car l)
    (for (alt (cdr l))
      (when (stats-better? alt best)
        (set! best alt)))
    best))

(tm-define (stats-best* . l)
  (stats-update)
  (with best (car l)
    (for (alt (cdr l))
      (when (stats-better*? alt best)
        (set! best alt)))
    best))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default letter roles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define basic-letters (list "a" "b" "c" "d" "u" "v" "w" "x" "y" "z"
                            "<alpha>" "<beta>" "<gamma>" "<delta>"
                            "<varepsilon>" "<zeta>" "<eta>" "<theta>"
                            "<lambda>" "<mu>" "<xi>" "<omicron>" "<pi>" "<rho>"
                            "<sigma>" "<tau>" "<upsilon>" "<chi>" "<omega>"
                            "<mathe>" "<mathi>" "<mathpi>" "<mathgamma>"
                            "A" "B" "C" "D" "E" "F" "G" "H" "I" "J"
                            "K" "L" "M" "N" "O" "P" "Q" "R" "S" "T"
                            "U" "V" "W" "X" "Y" "Z" "<Delta>" "<Theta>"
                            "<Lambda>" "<Xi>" "<Pi>" "<Sigma>" "<Omega>"))
(define function-letters (list "f" "g" "h" "<phi>" "<psi>"
                               "<Gamma>" "<Phi>" "<Psi>"))
(define index-letters (list "i" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t"
                            "<iota>" "<kappa>" "<nu>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analysis of content before cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (root-before-cursor)
  (with t (before-cursor)
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
                          (j (if (> n 0) (string-previous s n) n)))
                     (substring s j n)))
                  (else t))))))

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

(tm-define (speech-insert-number nr)
  (:mode in-math?)
  (with s (before-cursor)
    (if (letter-symbol? s)
        (math-insert `(rsub ,nr))
        (former nr)))
  (speech-exit-scripts))

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
  (with prev (before-cursor)
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
  (with mx (modified-letter x speech-letter-mode*)
    (improve-letter mx x speech-letter-mode*)))

(tm-define (speech-alter-letter* type)
  ;; Alteration spoken after letter (e.g. 'X calligraphique')
  (set! speech-letter-mode* (update-mode speech-letter-mode* type))
  (and-with prev (root-letter (before-cursor))
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

(define (improve-letter* best x mods)
  (when (and (nin? :big mods) (nin? :small mods))
    (set! best (improve-letter* best x (cons* :big mods))))
  (when (and (nin? :bold mods) (nin? :medium mods))
    (set! best (improve-letter* best x (cons* :bold mods))))
  (when (and (nin? :up mods) (nin? :it mods)
             (nin? :cal mods) (nin? :frak mods) (nin? :bbb mods))
    (set! best (improve-letter* best x (cons* :up mods)))
    (set! best (improve-letter* best x (cons* :cal mods)))
    (set! best (improve-letter* best x (cons* :frak mods)))
    (set! best (improve-letter* best x (cons* :bbb mods))))
  (when (and (nin? :ss mods) (nin? :tt mods) (nin? :normal mods))
    (set! best (improve-letter* best x (cons* :ss mods)))
    (set! best (improve-letter* best x (cons* :tt mods))))
  (stats-best best (modified-letter x mods)))

(define (improve-letter best x mods)
  (set! best (improve-letter* best x mods))
  (when (== mods (list))
    (when (and (== x "e") (stats-better? "<mathe>" best))
      (set! best "<mathe>"))
    (when (and (== x "i") (stats-better? "<mathi>" best))
      (set! best "<mathi>"))
    (when (and (== x "<pi>") (stats-better? "<mathpi>" best))
      (set! best "<mathpi>")))
  best)

(tm-define (best-letter-variant x)
  (with mx (modified-letter x speech-letter-mode)
    (improve-letter mx x speech-letter-mode)))

(tm-define (speech-insert-letter x*)
  (with prev* (before-cursor)
    (set! speech-letter-mode* (list))
    (when (and (== speech-operator-mode :on)
               (not (and (string? prev*) (string-alpha? prev*))))
      (set! speech-letter-mode (list))
      (set! speech-operator-mode :off))
    (let* ((prev (root-before-cursor))
           (x (best-letter-variant x*)))
      ;;(display* x* ", " speech-letter-mode " ~> " x "\n")
      (cond ((!= speech-operator-mode :off)
             (insert x))
            ((or (and (string? prev) (string-number? prev))
                 (and (in? prev basic-letters) (in? x basic-letters))
                 (and (in? prev index-letters) (in? x index-letters))
                 (and (in? prev (list "<pi>" "<mathpi>")) (== x "i")))
             (insert "*")
             (insert x))
            ((and (not (and (tree? prev*) (tree-in? prev* '(rsub rsup))))
                  (letter-symbol? prev) (in? x index-letters))
             (insert `(rsub ,x)))
            ((and (in? prev function-letters) (in? x basic-letters))
             (insert `(around "(" ,x ")")))
            (else
              (insert x)
              (speech-exit-scripts)))
      (set! speech-letter-mode* speech-letter-mode)
      (when (== speech-operator-mode :start)
        (set! speech-operator-mode :on))
      (when (== speech-operator-mode :off)
        (set! speech-letter-mode (list))))))

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
    (when (in? (car speech-state) (list :over :apply))
      (speech-leave)
      (speech-relation-exit))))

(tm-define (speech-insert-symbol x)
  (:require (== (math-symbol-group x) "Relation-nolim-symbol"))
  (speech-relation-exit)
  (insert x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textual operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-insert-op x)
  (set! speech-operator-mode :off)
  (with prev (root-before-cursor)
    (cond ((or (and (string? prev) (string-number? prev))
               (in? prev basic-letters)
               (in? prev index-letters))
           (insert "*")
           (insert x))
          ((and (in? prev function-letters) (in? x basic-letters))
           (insert `(around "(" ,x ")")))
          (else
           (insert x)))))

(tm-define (speech-insert-operator op)
  (speech-insert-op op)
  (insert " "))

(tm-define (speech-insert-function op)
  (speech-insert-op op)
  (speech-apply-brackets))

(tm-define (speech-insert-d x)
  (speech-insert-op "<mathd>")
  (insert x))

(tm-define (speech-operator)
  (set! speech-operator-mode :start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-subscript)
  (make 'rsub)
  (set! speech-state (cons :subscript speech-state)))

(tm-define (speech-superscript)
  (make 'rsup)
  (set! speech-state (cons :superscript speech-state)))

(tm-define (speech-insert-superscript s)
  (if (== (before-cursor) " ")
      (with p (cursor-path)
        (with-cursor (append (cDr p) (list (- (cAr p) 1)))
          (math-insert `(rsup ,s))))
      (math-insert `(rsup ,s))))

(tm-define (speech-apply-brackets)
  (math-bracket-open "(" ")" 'default)
  (set! speech-state (cons :apply speech-state)))

(tm-define (speech-sqrt)
  (make 'sqrt)
  (set! speech-state (cons :sqrt speech-state)))

(tm-define (speech-over)
  (with prev (before-cursor)
    (if (tm-is? prev 'big) (make 'rsub)
        (with sel (cut-before-cursor)
          (insert-go-to `(frac ,sel "") (list 1 0))
          (set! speech-state (cons :over speech-state))))))

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
  (set! speech-state (cons :wide speech-state)))

(tm-define (speech-wide-under acc)
  (make-wide-under acc)
  (set! speech-state (cons :wide speech-state)))

(tm-define (go-to-fraction where)
  (with-innermost t 'frac
    (when t
      (cond ((== where :numerator) (tree-go-to t 0 :end))
            ((== where :denominator) (tree-go-to t 1 :end))))))

(define (editing-big-operator?)
  (and-with t (tree-innermost script-context?)
    (while (and t (script-context? t))
      (set! t (tree-ref t :previous)))
    (and t (tree-is? t 'big))))

(tm-define (speech-of)
  (with prev (before-cursor)
    (cond ((tm-is? prev 'big) (make 'rsub))
          ((editing-big-operator?)
           (with-innermost t script-context?
             (tree-go-to t :end)))
          ((or (letter-symbol? prev)
               (tm-in? prev '(with math-ss math-tt rsub rsup around)))
           (speech-apply-brackets)))))

(tm-define (speech-factor)
  (when (and (nnull? speech-state) (== (car speech-state) :factor))
    (speech-leave))
  (insert "*")
  (math-bracket-open "(" ")" 'default)
  (set! speech-state (cons :factor speech-state)))

(tm-define (speech-for)
  (make 'rsub))

(tm-define (speech-dots sym dots)
  (speech-insert-symbol sym)
  (insert dots)
  (speech-insert-symbol sym))

(define (relation-symbol? x)
  (and (string? x)
       (== (math-symbol-group x) "Relation-nolim-symbol")))

(define (big->dots prev t sym dots)
  (with arg (tm->stree (tm-ref t 0))
    (selection-set (rcons (tree->path prev) 0) (rcons (tree->path t) 1))
    (cpp-clipboard-cut "dummy")
    (insert arg)
    (speech-dots sym dots)))

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
