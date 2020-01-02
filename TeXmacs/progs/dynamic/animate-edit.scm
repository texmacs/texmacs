
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : animate-edit.scm
;; DESCRIPTION : routines for editing animations
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic animate-edit)
  (:use (utils library tree)
        (utils library cursor)
        (dynamic dynamic-drd)
        (generic generic-edit)
        (generic format-geometry-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Useful subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (reset-players t)
  (players-set-elapsed t 0.0)
  (update-players (tree->path t) #t))

(tm-define (anim-play t)
  (when (tree? t)
    (if (tree-is? t :up 'anim-accelerate)
        (set! t (tree-up t)))
    (reset-players t)))

(tm-define (current-anim-play)
  (and-with t (tree-innermost user-anim-context? #t)
    (anim-play t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parameters for various animation tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-choice-list var)
  (:require (and (!= var "")
                 (with s (string-drop-right var 1)
                   (or (string-ends? s "-start-")
                       (string-ends? s "-end-")))))
  (list "-2" "-1" "-0.5" "0" "0.5" "1" "2" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'translate-in))
  (list (list "translate-start-x" "Start x")
        (list "translate-start-y" "Start y")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'translate-out))
  (list (list "translate-end-x" "End x")
        (list "translate-end-y" "End y")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'translate-smooth))
  (list (list "translate-start-x" "Start x")
        (list "translate-start-y" "Start y")
        (list "translate-end-x" "End x")
        (list "translate-end-y" "End y")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'progressive-in))
  (list (list "progressive-start-l" "Start left")
        (list "progressive-start-b" "Start bottom")
        (list "progressive-start-r" "Start right")
        (list "progressive-start-t" "Start top")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'progressive-out))
  (list (list "progressive-end-l" "End left")
        (list "progressive-end-b" "End bottom")
        (list "progressive-end-r" "End right")
        (list "progressive-end-t" "End top")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'progressive-smooth))
  (list (list "progressive-start-l" "Start left")
        (list "progressive-start-b" "Start bottom")
        (list "progressive-start-r" "Start right")
        (list "progressive-start-t" "Start top")
        (list "progressive-end-l" "End left")
        (list "progressive-end-b" "End bottom")
        (list "progressive-end-r" "End right")
        (list "progressive-end-t" "End top")))

(tm-define (parameter-choice-list var)
  (:require (in? var (list "fade-start" "fade-end" "zoom-start" "zoom-end")))
  (list "0" "0.2" "0.5" "0.8" "1" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'fade-in))
  (list (list "fade-start" "Start intensity")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'fade-out))
  (list (list "fade-end" "End intensity")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'fade-smooth))
  (list (list "fade-start" "Start intensity")
        (list "fade-end" "End intensity")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'zoom-in))
  (list (list "zoom-start" "Start magnification")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'zoom-out))
  (list (list "zoom-end" "End magnification")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'zoom-smooth))
  (list (list "zoom-start" "Start magnification")
        (list "zoom-end" "End magnification")))

(tm-define (parameter-choice-list var)
  (:require (or (string-starts? var "emboss-start-")
                (string-starts? var "emboss-end-")))
  (list "-5ln" "-4ln" "-3ln" "-2ln" "-1ln" "0ln"
        "1ln" "2ln" "3ln" "4ln" "5ln" :other))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(shadowed-smooth emboss-smooth
                          outlined-emboss-smooth)))
  (list (list "emboss-start-dx" "Start dx")
        (list "emboss-start-dy" "Start dy")
        (list "emboss-end-dx" "End dx")
        (list "emboss-end-dy" "End dy")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-context? t)
  (tree-in? t (append (anim-tag-list)
                      '(anim-constant anim-translate anim-progressive))))

(tm-define (make-anim-constant duration)
  (:argument duration "Duration")
  (insert-go-to `(anim-constant "" ,duration) '(0 0)))

(define (make-anim-translate duration start)
  (insert-go-to `(anim-translate "" ,duration ,start "") '(0 0)))

(tm-define (make-anim-translate-right duration)
  (:argument duration "Duration")
  (make-anim-translate duration '(tuple "-1.0" "0.0")))

(tm-define (make-anim-translate-left duration)
  (:argument duration "Duration")
  (make-anim-translate duration '(tuple "1.0" "0.0")))

(tm-define (make-anim-translate-up duration)
  (:argument duration "Duration")
  (make-anim-translate duration '(tuple "0.0" "-1.0")))

(tm-define (make-anim-translate-down duration)
  (:argument duration "Duration")
  (make-anim-translate duration '(tuple "0.0" "1.0")))

(define (make-anim-progressive duration start)
  (insert-go-to `(anim-progressive "" ,duration ,start "") '(0 0)))

(tm-define (make-anim-progressive-right duration)
  (:argument duration "Duration")
  (make-anim-progressive duration '(tuple "0.0" "0.0" "0.0" "1.0")))

(tm-define (make-anim-progressive-left duration)
  (:argument duration "Duration")
  (make-anim-progressive duration '(tuple "1.0" "0.0" "1.0" "1.0")))

(tm-define (make-anim-progressive-up duration)
  (:argument duration "Duration")
  (make-anim-progressive duration '(tuple "0.0" "0.0" "1.0" "0.0")))

(tm-define (make-anim-progressive-down duration)
  (:argument duration "Duration")
  (make-anim-progressive duration '(tuple "0.0" "1.0" "1.0" "1.0")))

(tm-define (make-anim-progressive-center duration)
  (:argument duration "Duration")
  (make-anim-progressive duration '(tuple "0.5" "0.5" "0.5" "0.5")))

(tm-define (geometry-speed t inc?)
  (:require (anim-context? t))
  (with inc (if inc? 1 -1)
    (with-focus-after t
      (length-increase-step (tree-ref t 1) inc))))

(tm-define (geometry-horizontal t forward?)
  (:require (anim-context? t))
  (with inc (if forward? 1 -1)
    (with-focus-after t
      (replace-empty t 1 "1s")
      (length-increase (tree-ref t 1) inc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time bending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (animation-tag-list*)
  (append '(anim-static anim-dynamic)
          (animation-tag-list)))

(tm-define (anim-get-accelerate t)
  (cond ((not (tree? t)) #f)
        ((tree-is? t 'anim-accelerate) t)
        ((tree-is? t :up 'anim-accelerate) (tree-up t))
        ((and (tree-is? t :up 'with) (tree-is? t :up :up 'anim-accelerate))
         (tree-up (tree-up t)))
        ((and (tree-is? t :up 'with) (tree-in? t (animation-tag-list*)))
         (tree-up t))
        ((tree-in? t (animation-tag-list*)) t)
        (else #f)))

(tm-define (accelerate-get-type t)
  (with a (anim-get-accelerate t)
    (or (and a
             (tree-func? a 'anim-accelerate 2)
             (tree->stree (tree-ref a 1)))
        "normal")))

(tm-define (accelerate-test-type? dummy new-type)
  (with t (tree-innermost 'anim-accelerate #t)
    (tm-equal? (accelerate-get-type t) new-type)))

(tm-define (accelerate-set-type t new-type)
  (:check-mark "*" accelerate-test-type?)
  (and-with a (anim-get-accelerate t)
    (if (tree-func? a 'anim-accelerate 2)
        (if (== new-type "normal")
            (tree-remove-node! a 0)
            (tree-set (tree-ref a 1) new-type))
        (if (!= new-type "normal")
            (tree-set! a `(anim-accelerate ,a ,new-type))))
    (reset-players a)))

(tm-define (accelerate-get-reverse? t)
  (and-with s (accelerate-get-type t)
    (and (string? s) (string-starts? s "reverse"))))

(tm-define (accelerate-test-reverse? dummy)
  (with t (tree-innermost 'anim-accelerate #t)
    (accelerate-get-reverse? t)))

(tm-define (accelerate-toggle-reverse? t)
  (:check-mark "*" accelerate-test-reverse?)
  (accelerate-set-type**
   t (accelerate-get-type* t) (not (accelerate-get-reverse? t))))

(tm-define (accelerate-get-type* t)
  (with s (accelerate-get-type t)
    (cond ((not (string? s)) s)
          ((== s "reverse") "normal")
          ((string-starts? s "reverse-") (string-drop s 8))
          (else s))))

(tm-define (accelerate-test-type*? dummy new-type)
  (with t (tree-innermost 'anim-accelerate #t)
    (tm-equal? (accelerate-get-type* t) new-type)))

(define (accelerate-set-type** t new-type reverse?)
  (if (and (string? new-type) reverse?)
      (if (== new-type "normal")
          (accelerate-set-type t "reverse")
          (accelerate-set-type t (string-append "reverse-" new-type)))
      (accelerate-set-type t new-type)))

(tm-define (accelerate-set-type* t new-type)
  (:check-mark "*" accelerate-test-type*?)
  (accelerate-set-type** t new-type (accelerate-get-reverse? t)))

(tm-define (retime-selection type)
  (:argument len "Duration")
  (with sel (selection-tree)
    (clipboard-cut "primary")
    (insert-go-to `(anim-accelerate ,sel ,type)
                  (cons 0 (path-end sel (list))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start and end editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (user-anim-context? t)
  (tree-in? t '(anim-static anim-dynamic anim-edit)))

(define (checkout-animation t len)
  (cond ((tm-func? t 'gr-screen 1)
         (with (r p) (checkout-animation (tm-ref t 0) len)
           (list `(gr-screen ,r) (cons 0 p))))
        ((not (tm-func? t 'morph))
         (checkout-animation
          `(morph (tuple "0" ,(tm->stree t))
                  (tuple "1" ,(tm->stree t))) len))
        (else
          (with r (animate-checkout `(anim-static ,t ,len "0.1s" "0s"))
            (list r (cons 1 (path-start (tm-ref r 1) '())))))))

(tm-define (make-animate t len)
  (with (r p) (checkout-animation t len)
    (insert-go-to r p)))

(tm-define (animate-selection len)
  (:argument len "Duration")
  (with sel (selection-tree)
    (clipboard-cut "primary")
    (make-animate sel len)
    (set-bottom-bar "animate" #t)))

(tm-define (commit-animation t)
  (animate-commit t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global duration and step length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-duration t)
  (cond ((tree-in? t '(anim-static anim-dynamic))
         (tree-ref t 1))
        ((tree-in? t '(anim-edit))
         (tree-ref t 2))
        (else #f)))

(tm-define (anim-set-duration t d)
  (and-with x (anim-portion t)
    (cond ((tree-in? t '(anim-static anim-dynamic))
           (tree-set! t 1 d))
          ((tree-in? t '(anim-edit))
           (tree-set! t 2 d)))
    (anim-set-portion t x)
    (anim-play t)))

(tm-define (current-anim-set-duration d)
  (and-with t (tree-innermost user-anim-context? #t)
    (when d (anim-set-duration t d))))

(tm-define (anim-step t)
  (cond ((tree-in? t '(anim-static anim-dynamic))
         (tree-ref t 2))
        ((tree-in? t '(anim-edit))
         (tree-ref t 3))
        (else #f)))

(tm-define (anim-set-step t d)
  (cond ((tree-in? t '(anim-static anim-dynamic))
         (tree-set! t 2 d))
        ((tree-in? t '(anim-edit))
         (tree-set! t 3 d)))
  (anim-play t))

(tm-define (current-anim-set-step d)
  (and-with t (tree-innermost user-anim-context? #t)
    (when d (anim-set-step t d))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Current animation frame
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-now t)
  (cond ((tree-in? t '(anim-static anim-dynamic))
         (tree-ref t 3))
        ((tree-in? t '(anim-edit))
         (tree-ref t 4))
        (else #f)))

(define (cursor-path-in t)
  (let* ((p (cursor-path))
         (r (tree->path t)))
    (and (list-starts? p r)
         (sublist p (length r) (length p)))))

(define (label-list t p)
  (cond ((or (not p) (not (tree? t)) (null? p)) #f)
        ((tree-atomic? t) (and (null? (cdr p)) (list (tree->string t))))
        ((null? (cdr p)) (list (tree-label t)))
        (else (and-with l (label-list (tree-ref t (car p)) (cdr p))
                (cons (tree-label t) l)))))

(tm-define (anim-set-now t now)
  (cond ((tree-in? t '(anim-static anim-dynamic))
         (tree-set! t 3 now))
        ((tree-in? t '(anim-edit))
         (let* ((p (cursor-path-in (tree-ref t 1)))
                (l (label-list (tree-ref t 1) p)))
           (with r (commit-animation t)
             (tree-set! t 0 (tree-ref r 0))
             (tree-set! t 4 now))
           (with r (animate-checkout `(anim-static ,(tree-ref t 0)
                                                   ,@(cddr (tm-children t))))
             (tree-set! t 0 (tree-ref r 0))
             (tree-set! t 1 (tree-ref r 1))
             (with l* (label-list (tree-ref t 1) p)
               (if (and l (== l* l))
                   (apply tree-go-to (cons* t 1 p))
                   (tree-go-to t 1 :start))))))))

(tm-define (current-anim-set-now x)
  (and-with t (tree-innermost user-anim-context? #t)
    (when x (anim-set-now t x))))

(define (get-ms t)
  (cond ((and (tree? t) (tree-atomic? t)) (get-ms (tree->string t)))
        ((not (string? t)) #f)
        ((string-ends? t "sec") (get-ms (string-drop-right t 2)))
        ((string-ends? t "ms") (string->number (string-drop-right t 2)))
        (else (and-with d (string->number (string-drop-right t 1))
                (* 1000 d)))))

(tm-define (anim-portion t)
  (let* ((n (get-ms (anim-now t)))
         (d (get-ms (anim-duration t))))
    (and n d (/ (* 1.0 n) (* 1.0 d)))))

(tm-define (anim-set-portion t x)
  (and-with d (get-ms (anim-duration t))
    (when (number? x)
      (with n (inexact->exact (floor (+ (* x d) 0.5)))
        (anim-set-now t (string-append (number->string (* 0.001 n)) "s"))))))

(tm-define (current-anim-set-portion x)
  (and-with t (tree-innermost user-anim-context? #t)
    (when x (anim-set-portion t x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removal of entire animations and specific frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-var l var)
  (cond ((or (null? l) (null? (cdr l))) (list))
        ((tm-equal? (car l) var) (remove-var (cddr l) var))
        (else (cons* (car l) (cadr l) (remove-var (cddr l) var)))))

(tm-define (anim-principal t)
  (cond ((tm-in? t '(anim-static anim-dynamic))
         (anim-principal (tm-ref t 0)))
        ((and (tm-is? t 'morph)
              (tm-func? (tm-ref t :last) 'tuple 2))
         (anim-principal (tm-ref t :last 1)))
        ((tm-is? t 'anim-edit)
         (anim-principal (tm-ref t 1)))
        ((tm-is? t 'with)
         (let* ((c (tm-children t))
                (l (cDr c))
                (r (anim-principal (cAr c))))
           (set! l (remove-var l "line-portion"))
           (set! l (remove-var l "opacity"))
           (if (null? l) r `(with ,@l ,r))))
        (else t)))

(tm-define (current-anim-remove)
  (and-with t (tree-innermost user-anim-context? #t)
    (tree-set! t (anim-principal t))))

(tm-define (anim-can-remove-frame? t)
  (and (tree-is? t 'anim-edit)
       (in? (anim-portion t) (anim-control-times t))
       (> (length (anim-control-times t)) 1)))

(define (morph-remove l x)
  (cond ((null? l) l)
        ((and (tree-func? (car l) 'tuple 2)
              (tree-atomic? (tree-ref (car l) 0))
              (= (tree->number (tree-ref (car l) 0)) x))
         (morph-remove (cdr l) x))
        (else (cons (car l) (morph-remove (cdr l) x)))))

(define (anim-remove-frame-sub t x)
  (cond ((tree-atomic? t) t)
        ((user-anim-context? t) t)
        ((tree-is? t 'morph)
         `(morph ,@(morph-remove (tree-children t) x)))
        (else `(,(tree-label t)
                ,@(map (cut anim-remove-frame-sub <> x)
                       (tree-children t))))))

(tm-define (anim-current-remove-frame)
  (and-with t (tree-innermost 'anim-edit #t)
    (let* ((n (anim-remove-frame-sub (tree-ref t 0) (anim-portion t)))
           (r (cddr (tree-children t)))
           (a `(anim-static ,n ,@r))
           (b (animate-checkout a)))
      (tree-set! t 0 (tree-ref b 0))
      (tree-set! t 1 (tree-ref b 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Managing objects with respect to frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (current-anim-can-copy?) #f)
(tm-define (current-anim-copy-before) (noop))
(tm-define (current-anim-copy-after) (noop))
(tm-define (current-anim-copy-all) (noop))
(tm-define (current-anim-delete-before) (noop))
(tm-define (current-anim-delete-after) (noop))

(define anim-new-mode "after")
(tm-define (anim-show-new-mode?) #f)
(tm-define (anim-get-new-mode) anim-new-mode)
(tm-define (anim-test-new-mode? s) (== anim-new-mode s))
(tm-define (anim-set-new-mode s)
  (:check-mark "*" anim-test-new-mode?)
  (set! anim-new-mode s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Start and end editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-checkout t)
  (with r (animate-checkout t)
    (tree-assign-node! t (tree-label r))
    (tree-set! t 0 (tree-ref r 0))
    (tree-insert! t 1 (list (tree-ref r 1)))
    (tree-go-to t 1 :start)
    (set-bottom-bar "animate" #t)))

(tm-define (current-anim-checkout)
  (and-with t (tree-innermost user-anim-context? #t)
    (anim-checkout t)))

(tm-define (anim-commit t)
  (with r (commit-animation t)
    (tree-set! t 0 (tree-ref r 0))
    (tree-remove! t 1 1)
    (tree-assign-node! t (tree-label r))
    (tree-go-to t :end)
    (anim-play t)))

(tm-define (current-anim-commit)
  (and-with t (tree-innermost user-anim-context? #t)
    (anim-commit t)))
