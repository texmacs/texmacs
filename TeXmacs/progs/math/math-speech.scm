
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
        (math math-stats)))

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
         (cond ((stats-has? ss) (speech-insert-operator ss) #t)
               ((stats-has? Ss) (speech-insert-operator Ss) #t)
               ((stats-has? SS) (speech-insert-operator SS) #t)
               (else #f)))))

(tm-define (kbd-speech s)
  (:mode in-math?)
  ;;(display* "Math speech " (cork->utf8 s) "\n")
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
          ((and (== lan 'french) (in? (string-ref s 1) (list #\d #\t)))
           (with de (if (== (string-ref s 1) #\d) " de " " t/de ")
             (when (== (string-length s) 2)
               (set! de (string-drop-right de 1)))
             (string-append (string-take s 1) de
                            (letterize-one lan (string-drop s 2)))))
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
;; Letter alteration mode (bold, calligraphic, sans serif, etc.)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(tm-define (speech-alter-letter type)
  ;; Alteration spoken before letter (e.g. 'bold x')
  (with prev (expr-before-cursor)
    (when (not (and (string? prev) (string-alpha? prev)))
      (set! speech-operator-mode :off)))
  (set! speech-letter-mode (update-mode speech-letter-mode type)))

(tm-define (speech-alter-letter* type)
  ;; Alteration spoken after letter (e.g. 'X calligraphique')
  (set! speech-letter-mode* (update-mode speech-letter-mode* type))
  (and-with prev (root-letter (expr-before-cursor))
    (cut-before-cursor)
    (insert (best-letter-variant* prev))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile list of letter variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define letter-variants-table (make-ahash-table))
(define current-variants (list))

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

(define (add-variant x mods)
  (set! current-variants
        (cons (modified-letter x mods)
              current-variants)))

(define (add-variants* x mods)
  (when (and (nin? :up mods) (nin? :it mods)
             (nin? :cal mods) (nin? :frak mods) (nin? :bbb mods)
             (nin? :ss mods) (nin? :tt mods) (nin? :normal mods))
    (add-variants* x (cons* :tt mods))
    (add-variants* x (cons* :ss mods))
    (add-variants* x (cons* :up mods))
    (add-variants* x (cons* :cal mods))
    (add-variants* x (cons* :frak mods))
    (when (nin? :bold mods)
      (add-variants* x (cons* :bbb mods))))
  (when (and (nin? :bold mods) (nin? :medium mods))
    (when (nin? :bbb mods)
      (add-variants* x (cons* :bold mods))))
  (when (and (nin? :big mods) (nin? :small mods))
    (add-variants* x (cons* :big mods)))
  (add-variant x mods))

(define (add-greek-variants x mods)
  (add-variants* x (cons* :tt :up mods))
  (add-variants* x (cons* :tt :it mods))
  (add-variants* x (cons* :ss :up mods))
  (add-variants* x (cons* :ss :it mods))
  (add-variants* x (cons* :up mods))
  (add-variants* x (cons* :it mods)))

(define (add-variants x mods)
  (when (== mods (list))
    (when (== x "e"   ) (add-variant "<mathe>"  mods))
    (when (== x "i"   ) (add-variant "<mathi>"  mods))
    (when (== x "<pi>") (add-variant "<mathpi>" mods)))
  (when (and (in? x (list "<theta>" "<kappa>" "<pi>" "<rho>" "<sigma>"))
             (nin? :big mods) (nin? :up mods))
    (with y (string-append "<var" (substring x 1 (string-length x)))
      (add-greek-variants y (cons :small mods))))
  (if (in? x greek-letters)
      (add-greek-variants x mods)
      (add-variants* x mods))
  (when (and (in? x (list "<epsilon>" "<phi>"))
             (nin? :big mods) (nin? :up mods))
    (with y (string-append "<var" (substring x 1 (string-length x)))
      (add-greek-variants y (cons :small mods)))))

(tm-define (letter-variants x mods)
  (let* ((key (list x mods))
         (im (ahash-ref letter-variants-table key)))
    (when (not im)
      (set! current-variants (list))
      (add-variants x mods)
      (set! im current-variants)
      (set! im (reverse (list-remove-duplicates (reverse im))))
      (ahash-set! letter-variants-table key im))
    im))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Best and statistically preferred letter variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (best-variant x mods)
  ;;(display* "    best variant " x ", " mods "\n")
  (with l (letter-variants x mods)
    (apply stats-best l)))

(tm-define (best-letter-variant x)
  (best-variant x speech-letter-mode))

(define (best-letter-variant* x)
  (best-variant x speech-letter-mode*))

(define (stats-prefer-predicate mode)
  (cond ((== mode :normal) >)
        ((== mode :strong) (lambda (w o) (> w (* 5 (+ o 1)))))
        (else >)))

(tm-define (stats-prefer? what over mode)
  (let* ((prefer? (stats-prefer-predicate mode))
         (what* (best-letter-variant what))
         (over* (best-letter-variant over)))
    (stats-prefer-contextual? what* over* prefer?)))

(tm-define (stats-preferred . l)
  (and (nnull? l)
       (with best (apply stats-preferred (cdr l))
         (if (and best (stats-prefer? best (car l) :normal)) best
             (and (stats-has? (best-letter-variant (car l))) (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General symbols and numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-insert-number x)
  (:mode in-math?)
  (let* ((prev (root-before-cursor))
         (impl (best-implicit prev x)))
    (cond ((string-number? prev) (insert x))
          ((!= impl :none) (insert-implicit impl x))
          (else (insert x)))
    (speech-exit-scripts)))

(tm-define (speech-insert-symbol x)
  (insert x)
  (speech-exit-scripts))

(tm-define (speech-insert-symbol x)
  (:require (string-number? x))
  (speech-insert-number x))

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
  (let* ((prev (root-before-cursor))
         (impl (best-implicit prev x)))
    (if (!= impl :none)
        (insert-implicit impl x)
        (insert x))))

(tm-define (speech-insert-d x)
  (speech-insert-operator "<mathd>")
  (insert x))

(tm-define (speech-operator)
  (set! speech-operator-mode :start))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Letters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
           (impl (best-implicit prev x)))
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
  (:require (or (in? x roman-letters) (in? x greek-letters)))
  (speech-insert-letter x))

(tm-define (speech-best-letter . l)
  (with sym (apply stats-preferred l)
    (speech-insert-symbol (or sym (cAr l)))))

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

(tm-define (speech-best-accent acc . l)
  (let* ((v (append-map (cut letter-variants <> speech-letter-mode) l))
         (w (map (lambda (x) `(wide ,x ,acc)) v))
         (b (apply stats-best w)))
    (if (> (stats-occurrences b) 0)
        (insert b)
        (begin
          (apply speech-best-letter (rcons l (car l)))
          (speech-accent acc)))))

(tm-define (speech-wide acc)
  (make-wide acc)
  (speech-enter :wide))

(tm-define (speech-under)
  (cond ((inside? 'wide)
         (with t (tree-innermost 'wide)
           (tree-assign-node! t 'wide*)))
        ((tm-is? (before-cursor) 'wide)
         (with t (before-cursor)
           (tree-assign-node! t 'wide*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (speech-start-2d)
  (when (not (selection-active-any?))
    (let* ((prev (root-before-cursor))
           (impl (best-implicit prev "<nosymbol>")))
      (cond ((== impl :multiply) (insert "*"))
            ((== impl :space)    (insert " "))
            ((== impl :apply)    (math-bracket-open "(" ")" 'default))
            ((== impl :brackets) (math-bracket-open "[" "]" 'default))))))

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

(tm-define (speech-apply)
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
          ((or (math-symbol? prev)
               (tm-in? prev '(with math-ss math-tt rsub rsup around)))
           (speech-apply)))))

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
