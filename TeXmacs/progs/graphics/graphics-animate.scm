
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : graphics-animate.scm
;; DESCRIPTION : editing routines for graphical animations group mode
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (graphics graphics-animate)
  (:use (graphics graphics-group)
        (generic format-edit)
        (dynamic animate-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animating individual objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (anim-type-get t)
  (cond ((tm-in? t '(anim-static anim-dynamic anim-edit))
         (anim-type-get (tm-ref t 0)))
        ((and (tm-func? t 'morph 2)
              (tm-func? (tm-ref t 0) 'tuple 2)
              (tm-func? (tm-ref t 1) 'tuple 2)
              (tm-func? (tm-ref t 0 1) 'with)
              (tm-func? (tm-ref t 1 1) 'with))
         (let* ((c1 (tm-children (tm-ref t 0 1)))
                (c2 (tm-children (tm-ref t 1 1))))
           (if (and (>= (length c1) 3)
                    (>= (length c2) 3)
                    (tm-equal? `(concat ,@(cDDr c1)) `(concat ,@(cDDr c2)))
                    (tm-equal? (cAr c1) (cAr c2)))
               (let* ((var  (tm->stree (cAr (cDDr c1))))
                      (val1 (tm->stree (cAr (cDr c1))))
                      (val2 (tm->stree (cAr (cDr c2))))
                      (trip (list var val1 val2)))
                 (cond ((== trip '("line-portion" "0" "1")) "ink in")
                       ((== trip '("line-portion" "1" "0")) "ink out")
                       ((== trip '("opacity" "0" "1")) "fade in")
                       ((== trip '("opacity" "1" "0")) "fade out")
                       (else "animated")))
               "animated")))
        ((tm-func? t 'morph) "animated")
        (else "inanimated")))

(define (decode-anim-type val)
  (cond ((== val "ink in") '("line-portion" "0" "1"))
        ((== val "ink out") '("line-portion" "1" "0"))
        ((== val "fade in") '("opacity" "0" "1"))
        ((== val "fade out") '("opacity" "1" "0"))
        (else '("opacity" "0" "1"))))

(define (add-with var val t)
  (if (tm-func? t 'with)
      `(with ,@(cDr (tm-children t)) ,var ,val ,(tm-ref t :last))
      `(with ,var ,val ,t)))

(define (anim-parameters t)
  (cond ((tm-in? t '(anim-static anim-dynamic)) (cdr (tm-children t)))
        ((tm-is? t 'anim-edit) (cddr (tm-children t)))
        (else (list "1s" "0.1s" "0s"))))

(define (anim-type-set t val)
  (cond ((and (tm-in? t '(anim-static anim-dynamic)) (== val "inanimated"))
         (tree-set! t (anim-principal t))
         t)
        ((and (tm-in? t '(anim-edit)) (== val "inanimated"))
         (tree-remove-node! t 1)
         t)
        ((and (not (tm-in? t '(anim-static anim-dynamic anim-edit)))
              (== val "animated"))
         (with t* (tm->stree t)
           (tree-set! t `(anim-edit (morph (tuple "0" ,t*) (tuple "1" ,t*))
                                    ,t* "1s" "0.1s" "0s"))
           t))
        ((== val "animated")
         t)
        (else
         (with t* (tm->stree (anim-principal t))
           (with (var val1 val2) (decode-anim-type val)
             (let* ((t1 (add-with var val1 t*))
                    (t2 (add-with var val2 t*)))
               (tree-set! t `(anim-static (morph (tuple "0" ,t1)
                                                 (tuple "1" ,t2))
                                          ,@(anim-parameters (tm->stree t))))
               t))))))

(tm-define (graphics-get-anim-type)
  (:require (and (== (graphics-mode) '(group-edit animate))
                 (graphics-selection-active?)))
  (with l (map anim-type-get (sketch-get))
    (properties-and l)))

(tm-define (graphics-set-anim-type val)
  (:require (and (== (graphics-mode) '(group-edit animate))
                 (graphics-selection-active?)))
  (with r (map (cut anim-type-set <> val) (sketch-get))
    (sketch-set! r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing global animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (graphics-group-selection?)
  (and (== (car (graphics-mode)) 'group-edit)
       (graphics-selection-active?)))

(define (sketch-map! fun)
  (with r (map (lambda (t) (tree-set! t (fun t)) t) (sketch-get))
    (sketch-set! r)))

(tm-define (current-anim-remove)
  (:require (graphics-group-selection?))
  (sketch-map! anim-principal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copying and removing objects to and from other frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (anim-id t)
  (and-with val (with-ref t "anim-id")
    (and (tree-atomic? val)
         (tree->string val))))

(define (anim-attach-id t)
  (if (anim-id t) t
      (with-set t "anim-id" (create-unique-id))))

(define (copy-to* t g i)
  (cond ((>= i (tree-arity g))
         (tree-insert! g i (list t)))
        ((== (anim-id (tree-ref g i)) (anim-id t))
         (tree-set g i t))
        (else (copy-to* t g (+ i 1)))))

(define (copy-to t g)
  (when (anim-id t)
    (copy-to* t g 0)))

(define (delete-from* t g i)
  (cond ((>= i (tree-arity g)) (noop))
        ((== (anim-id (tree-ref g i)) (anim-id t))
         (tree-remove g i 1))
        (else (delete-from* t g (+ i 1)))))

(define (delete-from t g)
  (when (anim-id t)
    (delete-from* t g 0)))

(define (anim-operate*** t a op)
  (cond ((tree-atomic? a) (noop))
        ((tree-is? a 'graphics) (op t a))
        (else
         (for-each (cut anim-operate*** t <> op)
                   (tree-children a)))))

(define (anim-operate** t a x op pred?)
  (when (tree-func? a 'tuple 2)
    (and-with y (tree->number (tree-ref a 0))
      (when (pred? y x)
        (anim-operate*** t (tree-ref a 1) op)))))

(define (anim-operate* t a x op pred?)
  (cond ((tree-atomic? a) (noop))
        ((tree-is? a 'morph)
         (for-each (cut anim-operate** t <> x op pred?)
                   (tree-children a)))
        (else
         (for-each (cut anim-operate* t <> x op pred?)
                   (tree-children a)))))

(define (anim-operate t op pred?)
  (and-with a (tree-innermost 'anim-edit)
    (when (tree-inside? t a)
      (anim-operate* t (tree-ref a 0) (anim-portion a) op pred?))))

(define (anim-copy-to t pred?)
  (with u (anim-attach-id t)
    (anim-operate u copy-to pred?)
    u))

(define (anim-delete-from t pred?)
  (when (anim-id t)
    (anim-operate t delete-from pred?)
    t))

(tm-define (current-anim-can-copy?)
  (:require (graphics-group-selection?))
  #t)

(tm-define (current-anim-copy-after)
  (:require (graphics-group-selection?))
  (sketch-map! (lambda (t) (anim-copy-to t >=))))

(tm-define (current-anim-copy-before)
  (:require (graphics-group-selection?))
  (sketch-map! (lambda (t) (anim-copy-to t <=))))

(tm-define (current-anim-copy-all)
  (:require (graphics-group-selection?))
  (sketch-map! (lambda (t) (anim-copy-to t always?))))

(tm-define (current-anim-delete-after)
  (:require (graphics-group-selection?))
  (sketch-map! (lambda (t) (anim-delete-from t >))))

(tm-define (current-anim-delete-before)
  (:require (graphics-group-selection?))
  (sketch-map! (lambda (t) (anim-delete-from t <))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic copying of new objects to other frames
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (anim-show-new-mode?)
  (:require (and (inside? 'graphics)
                 (!= (car (graphics-mode)) 'group-edit)))
  #t)

(define (tree-get-graphics t)
  (cond ((tree-is? t 'graphics) t)
        ((tree-is? t 'with) (tree-get-graphics (tm-ref t :last)))
        (else #f)))

(define (get-mode-pred)
  (with mode (anim-get-new-mode)
    (cond ((== mode "after") >=)
          ((== mode "before") <=)
          ((== mode "all") always?)
          (else ==))))

(define (commit-prepare a obj)
  (when (and (tree-compound? obj) (not (anim-id obj)))
    (with t (anim-attach-id obj)
      (anim-operate* t (tree-ref a 0) (anim-portion a)
                     copy-to (get-mode-pred)))))

(tm-define (commit-animation t)
  (:require (and (tree-is? t 'anim-edit)
                 (tree-get-graphics (tree-ref t 1))))
  (with g (tree-get-graphics (tree-ref t 1))
    (for-each (cut commit-prepare t <>) (tree-children g))
    (former t)))
