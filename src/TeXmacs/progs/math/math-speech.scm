
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
        (math math-stats)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of main speech hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define speech-state (list))
(define speech-letter-mode (list))
(define speech-letter-mode* (list))
(define speech-operator-mode :off)
(define speech-can-extend-script? #f)
(define speech-can-extend-brackets? #f)

(tm-define (speech-cleanup)
  ;;(display* "Cleanup\n")
  (speech-exit-all)
  (set! speech-letter-mode (list))
  (set! speech-operator-mode :off)
  (set! speech-can-extend-script? #f)
  (set! speech-can-extend-brackets? #f)
  (former))

(tm-define (speech-pause)
  ;;(display* "Pause\n")
  (speech-exit-all)
  (set! speech-letter-mode (list))
  (set! speech-operator-mode :off)
  (set! speech-can-extend-script? #f)
  (set! speech-can-extend-brackets? #f)
  (former))

(tm-define (speech-current-mode)
  (:mode in-math?)
  'math)

(define (stats-has-operator? s)
  (and (<= (string-length s) 10)
       (>= (string-length s) 2)
       (string-alpha? s)
       (let* ((ss (locase-all s))
              (Ss (upcase-first ss))
              (SS (upcase-all s)))
         (cond ((stats-has? ss) ss)
               ((stats-has? Ss) Ss)
               ((stats-has? SS) SS)
               (else #f)))))

(tm-define (speech-exec-hook s)
  (:mode in-math?)
  ;;(display* "Hook " s "\n")
  (and (nin? s standard-operators)
       (and-with op (stats-has-operator? s)
         (speech-insert-operator op)
         #t)))

(tm-define (kbd-speech s)
  (:mode in-math?)
  ;;(display* "\nMath speech " (cork->utf8 s) "\n")
  (cond ((speech-make s) (noop))
        (else (speech-exec s))))

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
          ((stats-has-operator? s) s)
          ((or (speech-has? lan 'dont-break (substring s 0 2))
               (speech-has? lan 'dont-break (string-take-right s 2)))
           s)
          ((and (== lan 'french)
                (<= (string-length s) 3)
                (in? (string-ref s 1) (list #\d #\t)))
           (with de (if (== (string-ref s 1) #\d) " d/de " " t/de ")
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

(tm-define (letterized? s*)
  (with s (locase-all s*)
    (!= (letterize s) s)))

(tm-define (letterized-list s*)
  (with s (locase-all s*)
    (string-decompose (letterize (locase-all s)) " ")))

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
          ((== type :short-subscript) (speech-exit-from 'rsub))
          ((== type :superscript) (speech-exit-from 'rsup))
          ((== type :short-superscript) (speech-exit-from 'rsup))
          ((== type :over) (speech-exit-from 'frac))
          ((== type :short-over) (speech-exit-from frac-context?))
          ((== type :sqrt) (speech-exit-from 'sqrt))
          ((== type :wide) (speech-exit-from wide-context?))
          ((== type :apply) (speech-exit-from around-context?))
          ((== type :factor) (speech-exit-from around-context?))
          ((== type :brackets) (speech-exit-from around-context?))
          ((== type :braces) (speech-exit-from around-context?))
          (else (set! speech-state (cdr speech-state))))))

(define (speech-exit-all)
  (while (nnull? speech-state)
    (speech-exit-innermost)))

(define (speech-enter type)
  (set! speech-state (cons type speech-state)))

(tm-define (speech-leave)
  (:mode in-math?)
  (if (nnull? speech-state)
      (speech-exit-innermost)
      (structured-exit-right)))

(define short-list
  (list :short-over :short-subscript :short-superscript))

(tm-define (speech-exit-short)
  (when (and (nnull? speech-state) (in? (car speech-state) short-list))
    (when (in? (car speech-state) (list :short-subscript :short-superscript))
      (set! speech-can-extend-script? #t))
    (speech-exit-innermost)
    (speech-exit-short)))

(define 2d-list
  (list :subscript :short-subscript :superscript :short-superscript
        :over :short-over :sqrt :wide))

(tm-define (speech-exit-2d)
  (when (and (nnull? speech-state) (in? (car speech-state) 2d-list))
    (speech-exit-innermost)
    (speech-exit-2d)))

(tm-define (speech-end tag)
  (while (and (inside? tag) (nnull? speech-state))
    (speech-exit-innermost))
  (and-with t (tree-innermost tag)
    (tree-go-to t :end)))

(tm-define (insert-implicit impl x)
  (cond ((== impl :multiply)    (insert "*") (insert x))
        ((== impl :space)       (insert " ") (insert x))
        ((== impl :comma)       (insert ",") (insert x))
        ((== impl :apply)       (insert `(around "(" ,x ")")))
        ((== impl :brackets)    (insert `(around "[" ,x "]")))
        ((== impl :subscript)   (insert `(rsub ,x)))
        ((== impl :superscript) (insert `(rsup ,x)))))

(tm-define (speech-insert-implicit impl x)
  (insert-implicit impl x)
  (when (in? impl (list :subscript :superscript))
    (set! speech-can-extend-script? #t))
  (when (in? impl (list :apply :brackets))
    (set! speech-can-extend-brackets? #t)))

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
      (when (string-number? x)
        (set! mods (cons* :it :normal mods)))
      (set! current-variants (list))
      (add-variants x mods)
      (set! im current-variants)
      (set! im (reverse (list-remove-duplicates (reverse im))))
      (ahash-set! letter-variants-table key im))
    im))

(tm-define (stats-has-variant? x)
  (with l (letter-variants x speech-letter-mode)
    (nnull? (stats-filter l))))

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

(define (combine-implicit prev impl next)
  (cond ((not prev) next)
        ((== impl :multiply)    (tmconcat prev "*" next))
        ((== impl :space)       (tmconcat prev " " next))
        ((== impl :comma)       (tmconcat prev "," next))
        ((== impl :apply)       (tmconcat prev `(around "(" ,next ")")))
        ((== impl :brackets)    (tmconcat prev `(around "[" ,next "]")))
        ((== impl :subscript)   (tmconcat prev `(rsub ,next)))
        ((== impl :superscript) (tmconcat prev `(rsup ,next)))
        (else (tmconcat prev next))))

(define (find-best i v o)
  (if (null? (cdr i)) (list (car i) (car v) (car o))
      (with (bi bv bo) (find-best (cdr i) (cdr v) (cdr o))
        (if (> bo (car o)) (list bi bv bo)
            (list (car i) (car v) (car o))))))

(tm-define (best-implicit-variant prev x)
  (if (not prev)
      (list :none (best-letter-variant x))
      (let* ((a (letter-variants x speech-letter-mode))
             (l (list-filter a stats-has?))
             (i (map (cut best-implicit prev <>) l))
             (c (map (cut combine-implicit prev <> <>) i l))
             (o (map stats-in-role c)))
        (if (or (null? l) (== (apply max o) 0))
            (let* ((v (best-letter-variant x))
                   (impl (best-implicit prev v)))
              (list impl v))
            (with (bi bv bo) (find-best i l o)
              (list bi bv))))))

(define (stats-prefer-predicate mode)
  (cond ((== mode :normal) >)
        ((== mode :strong) (lambda (w o) (> w (* 5 (+ o 1)))))
        (else >)))

(tm-define (stats-prefer? what over mode)
  (let* ((prefer? (stats-prefer-predicate mode))
         (what* (best-letter-variant what))
         (over* (best-letter-variant over)))
    (stats-prefer-contextual? what* over* prefer?)))

(tm-define (stats-preferred l mode)
  (and (nnull? l)
       (with best (stats-preferred (cdr l) mode)
         (if (and best (stats-prefer? best (car l) mode)) best
             (and (stats-has? (best-letter-variant (car l))) (car l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General symbols and numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-insert-number x)
  (:mode in-math?)
  (let* ((prev (root-before-cursor))
         (impl (best-implicit prev x)))
    (cond ((string-number? prev) (insert x))
          ((!= impl :none) (speech-insert-implicit impl x))
          (else (insert x)))
    (speech-exit-short)))

(tm-define (speech-insert-symbol x)
  (let* ((prev (root-before-cursor))
         (impl (best-implicit prev x)))
    (cond ((in? x (list "," "." ";" ":" "?" "!")) (insert x))
          ((== impl :none) (insert x))
          ((tm-in? x '(math-ss math-tt)) (speech-insert-implicit impl x))
          ((math-symbol? x) (speech-insert-implicit impl x))
          (else (insert x))))
  (speech-exit-short))

(tm-define (speech-insert-symbol x)
  (:require (string-number? x))
  (speech-insert-number x))

(define weak-quit-list
  (list :over :short-over :sqrt :wide :apply :factor :brackets))

(define script-list
  (list :subscript :short-subscript :superscript :short-superscript))

(define (speech-weak-exit)
  (when (nnull? speech-state)
    (with mode (car speech-state)
      (when (or (in? mode weak-quit-list)
                (and (not (editing-big-operator?)) (in? mode script-list)))
        (speech-leave)
        (speech-weak-exit)))))

(tm-define (speech-insert-symbol x)
  (:require (math-weak-infix? x))
  (speech-weak-exit)
  (insert x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Textual operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-insert-operator x)
  (set! speech-operator-mode :off)
  (let* ((prev (root-before-cursor))
         (impl (best-implicit prev x)))
    (if (!= impl :none)
        (speech-insert-implicit impl x)
        (insert x))))

(tm-define (speech-insert-infix-operator x)
  (set! speech-operator-mode :off)
  (with prev (root-before-cursor)
    (when (and prev (!= prev " "))
      (insert " "))
    (insert x)
    (insert " ")))

(tm-define (speech-insert-d x)
  (speech-insert-operator "<mathd>")
  (insert x))

(tm-define (speech-insert-symbol x)
  (:require (== x "<mathd>"))
  (speech-insert-operator x))

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
    (with prev (root-before-cursor)
      (with (impl x) (best-implicit-variant prev x*)
        ;;(display* "  inserting " x* " as " x "\n")
        (cond ((!= speech-operator-mode :off) (insert x))
              ((!= impl :none) (speech-insert-implicit impl x))
              (else (insert x) (speech-exit-short)))
        ;;(display* "  inserted  " x* " as " x "\n")
        (set! speech-letter-mode* speech-letter-mode)
        (when (== speech-operator-mode :start)
          (set! speech-operator-mode :on))
        (when (== speech-operator-mode :off)
          (set! speech-letter-mode (list))))))
  ;;(display* "inserted letter " x* "\n")
  )

(tm-define (speech-insert-symbol x)
  (:require (or (in? x roman-letters) (in? x greek-letters)))
  (speech-insert-letter x))

(tm-define (speech-best-letter . l)
  (with sym (stats-preferred l :normal)
    (speech-insert-symbol (or sym (cAr l)))))

(tm-define (speech-insert-best . l)
  (with sym (stats-preferred l :strong)
    (speech-insert-symbol (or sym (car l)))))

(define (complete-letter-variants l mode)
  (if (null? l) l
      (with v (letter-variants (car l) mode)
        (append (stats-filter v)
                (complete-letter-variants (cdr l) mode)))))

(tm-define (speech-best-combination l1 l2)
  ;;(display* "best combination? " l1 ", " l2 "\n")
  (let* ((l1* (list-remove-duplicates
               (complete-letter-variants l1 speech-letter-mode)))
         (l2* (list-remove-duplicates
               (complete-letter-variants l2 (list))))
         (p   (stats-preferred-combination l1* l2*)))
    ;;(display* "best combination " l1* ", " l2* " ~> " p "\n")
    (if p
        (with (x y) p
          (speech-insert-symbol x)
          (speech-insert-symbol y))
        (begin
          (apply speech-insert-best (rcons l1 (car l1)))
          (apply speech-insert-best (rcons l2 (car l2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subscripts, superscripts, and wide accents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (re-enter-script type mode)
  (with prev (expr-before-cursor)
    (when (tm-is? prev type)
      (tree-go-to prev 0 :end)
      (speech-enter mode)
      (re-enter-script type mode))))

(tm-define (speech-subscript)
  (re-enter-script 'rsub :subscript)
  (make 'rsub)
  (speech-enter :subscript))

(tm-define (speech-short-subscript)
  (re-enter-script 'rsub :short-subscript)
  (make 'rsub)
  (speech-enter :short-subscript))

(tm-define (speech-superscript)
  (re-enter-script 'rsup :superscript)
  (make 'rsup)
  (speech-enter :superscript))

(tm-define (speech-short-superscript)
  (re-enter-script 'rsup :short-superscript)
  (make 'rsup)
  (speech-enter :short-superscript))

(tm-define (speech-insert-superscript s)
  (if (== (expr-before-cursor) " ")
      (with p (cursor-path)
        (with-cursor (append (cDr p) (list (- (cAr p) 1)))
          (math-insert `(rsup ,s))))
      (begin
        (re-enter-script 'rsup :short-superscript)
        (math-insert `(rsup ,s)))))

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
;; Extend previously finished scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dont-extend-script? x)
  (and-let* ((root (root-before-cursor))
             (expr (expr-before-cursor))
             (ok?  (tm-in? expr '(rsub rsup))))
    (or (stats-role? (tmconcat root "*" x))
        (stats-role? (tmconcat root " " x))
        (stats-role? (tmconcat root "," x))
        (stats-role? (tmconcat root ";" x))
        (stats-role? (tmconcat root `(around "(" ,x ")")))
        (stats-role? (tmconcat root `(around "[" ,x "]"))))))

(define (do-extend-script? x)
  (and-let* ((root (root-before-cursor))
             (expr (expr-before-cursor))
             (ok?  (tm-in? expr '(rsub rsup)))
             (c1   (tmconcat root `(,(tm-label expr) ,(tmconcat "," x))))
             (c2   (tmconcat root `(,(tm-label expr) ,(tmconcat ";" x)))))
    (or (and (stats-role? c1) ",")
        (and (stats-role? c2) ";"))))

(define (extend-script? x)
  (with v (letter-variants x speech-letter-mode)
    (and speech-can-extend-script?
         (not (exists? dont-extend-script? v))
         (exists? do-extend-script? v))))

(define (extend-script op x)
  (with expr (expr-before-cursor)
    (and (tm-in? expr '(rsub rsup))
         (with-cursor (tree->path expr 0 :end)
           (insert op)
           (speech-insert-symbol x)))))

(define (must-continue-script? x)
  (and-let* ((prev (expr-before-cursor))
             (ok1? (in? prev (list "+" "-" "," ";")))
             (expr (expr-before-before-cursor))
             (ok2? (tm-in? expr '(rsub rsup)))
             (op   (if (== prev "-") "+" prev))
             (x*   (if (string-number? x) "1" x))
             (pref (tm->stree (tm-ref expr 0))))
    (or (stats-has?  (tmconcat pref prev x))
        (stats-role? (tmconcat pref op x*)))))

(define (dont-continue-script? x)
  (and-let* ((prev (expr-before-cursor))
             (ok?  (in? prev (list "+" "-" "," ";")))
             (op   (if (== prev "-") "+" prev))
             (x*   (if (string-number? x) "1" x))
             (root (root-before-before-cursor)))
    (stats-role? (tmconcat root op x*))))

(define (do-continue-script? x)
  (and-let* ((prev (expr-before-cursor))
             (ok?  (in? prev (list "+" "-" "," ";")))
             (op   (if (== prev "-") "+" prev))
             (x*   (if (string-number? x) "1" x))
             (app  (tmconcat op x*))
             (root (root-before-before-cursor))
             (expr (expr-before-before-cursor))
             (ok2? (tm-in? expr '(rsub rsup))))
    (stats-role? (tmconcat root `(,(tm-label expr) ,app)))))

(define (continue-script? x)
  (with v (letter-variants x speech-letter-mode)
    (and speech-can-extend-script?
         (or (exists? must-continue-script? v)
             (and (not (exists? dont-continue-script? v))
                  (exists? do-continue-script? v))))))

(define (continue-script x)
  (with op (cut-before-cursor)
    (extend-script op x)))

(tm-define (speech-insert-symbol x)
  (:require (or (string-number? x)
                (in? x roman-letters)
                (in? x greek-letters)))
  (with extend? (extend-script? x)
    (cond (extend? (extend-script extend? x))
          ((continue-script? x) (continue-script x))
          (else (former x)))))

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
    (cond ((tm-is? prev 'big) (make 'rsub))
          ((inside? 'frac)
           (go-to-fraction :denominator)
           (speech-enter :over))
          (else (with sel (cut-before-cursor)
                  (insert-go-to `(frac ,sel "") (list 1 0))
                  (speech-enter :over))))))

(tm-define (speech-short-over)
  (with sel (cut-before-cursor)
    (if (inside? 'math)
        (insert-go-to `(frac* ,sel "") (list 1 0))
        (insert-go-to `(frac ,sel "") (list 1 0)))
    (speech-enter :short-over)))

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
    (cond ((tm-in? prev '(frac frac*)) (insert "*"))
          ((tm-is? prev 'big) (make 'rsub))
          ((editing-big-operator?)
           (with-innermost t script-context?
             (tree-go-to t :end)))
          ((or (math-symbol? prev)
               (tm-in? prev '(with math-ss math-tt rsub rsup around)))
           (speech-apply)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extend previously finished brackets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (brackets-before-cursor)
  (let* ((expr (expr-before-cursor))
         (prev (expr-before-before-cursor)))
    (cond ((tm-in? expr '(around around*)) expr)
          ((nin? expr (list "," ";")) #f)
          ((tm-in? prev '(around around*)) prev)
          (else #f))))

(define (root-before-brackets)
  (and-let* ((expr (brackets-before-cursor))
             (prev (tree-ref expr :previous))
             (ok2? (tree? prev)))
    (with-cursor (tree->path prev :end)
      (root-before-cursor))))

(define (dont-extend-brackets? x)
  (and-with root (root-before-brackets)
    (or (stats-role? (tmconcat root "*" x))
        (stats-role? (tmconcat root " " x))
        (stats-role? (tmconcat root "," x))
        (stats-role? (tmconcat root ";" x))
        (stats-role? (tmconcat root `(rsub ,x)))
        (stats-role? (tmconcat root `(rsup ,x))))))

(define (do-extend-brackets? x)
  (and-let* ((expr (expr-before-cursor))
             (ok?  (tm-in? expr '(around around*)))
             (root (root-before-brackets))
             (l    (tm->stree (tm-ref expr 0)))
             (r    (tm->stree (tm-ref expr 2)))
             (c1   (tmconcat root `(around ,l ,(tmconcat "," x) ,r)))
             (c2   (tmconcat root `(around ,l ,(tmconcat ";" x) ,r)))
             (c3   (tmconcat root `(around* ,l ,(tmconcat "," x) ,r)))
             (c4   (tmconcat root `(around* ,l ,(tmconcat ";" x) ,r))))
    (or (and (stats-role? c1) ",")
        (and (stats-role? c2) ";"))))

(define (extend-brackets? x)
  (with v (letter-variants x speech-letter-mode)
    (and speech-can-extend-brackets?
         (not (exists? dont-extend-brackets? v))
         (exists? do-extend-brackets? v))))

(define (extend-brackets op x)
  (with expr (expr-before-cursor)
    (and (tm-in? expr '(around around*))
         (with-cursor (tree->path expr 1 :end)
           (insert op)
           (speech-insert-symbol x)))))

(define (dont-continue-brackets? x)
  (and-let* ((prev (expr-before-cursor))
             (ok?  (in? prev (list "," ";")))
             (root (root-before-brackets)))
    (stats-role? (tmconcat root prev x))))

(define (do-continue-brackets? x)
  (and-let* ((prev (expr-before-cursor))
             (ok?  (in? prev (list "," ";")))
             (expr (brackets-before-cursor))
             (root (root-before-brackets))
             (l    (tm->stree (tm-ref expr 0)))
             (c    (tmconcat prev x))
             (r    (tm->stree (tm-ref expr 2))))
    (stats-role? (tmconcat root `(,(tm-label expr) ,l ,c ,r)))))

(define (continue-brackets? x)
  (with v (letter-variants x speech-letter-mode)
    (and speech-can-extend-brackets?
         (not (exists? dont-continue-brackets? v))
         (exists? do-continue-brackets? v))))

(define (continue-brackets x)
  (with op (cut-before-cursor)
    (extend-brackets op x)))

(tm-define (speech-insert-symbol x)
  (:require (or (string-number? x)
                (in? x roman-letters)
                (in? x greek-letters)))
  (with extend? (extend-brackets? x)
    (cond (extend? (extend-brackets extend? x))
          ((continue-brackets? x) (continue-brackets x))
          (else (former x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big operators and dots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-big-operator sym)
  (speech-exit-2d)
  (speech-start-2d)
  (math-big-operator sym))

(tm-define (editing-big-operator?)
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

(tm-define (speech-until)
  (if (inside? 'rsub)
      (with-innermost t 'rsub
        (let* ((prev (tree-ref t :previous))
               (big? (and (tree? prev) (tree-is? prev 'big)))
               (op (and big? (tm->stree (tm-ref prev 0))))
               (l (map tm->stree (concat-tokenize-math (tm-ref t 0)))))
          (cond ((not big?) (noop))
                ((exists? math-weak-infix? l)
                 (tree-go-to t :end)
                 (make 'rsup))
                ((== op "sum") (big->dots prev t "+" "<cdots>"))
                ((== op "prod") (big->dots prev t "*" "<cdots>"))
                (else
                  (tree-go-to t :end)
                  (make 'rsup)))))
      (speech-dots "," "<ldots>")))

(tm-define (speech-to)
  (when (inside? 'rsub)
    (speech-until)))
