
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fold-edit.scm
;; DESCRIPTION : routines for switching, folding and layers
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic fold-edit)
  (:use (utils library tree)
        (utils library cursor)
        (dynamic dynamic-drd)
        (generic generic-edit)
        (generic document-edit)
        (text text-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Style package rules for beamer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (beamer-themes)
  (list "bluish" "boring-white" "dark-vador" "granite"
        "ice" "manila-paper" "metal" "pale-blue"
        "pine" "reddish" "ridged-paper" "rough-paper"
        "xperiment"))

(tm-define (current-beamer-theme)
  (with l (get-style-list)
    (or (list-find l (cut in? <> (beamer-themes)))
        (if (!= (car l) "old-beamer") "bluish" "ridged-paper"))))

(tm-define (style-category p)
  (:require (in? p (beamer-themes)))
  :beamer-theme)

(tm-define (style-category p)
  (:require (in? p (list "title-bar" "framed-title")))
  :beamer-title-theme)

(tm-define (style-category-precedes? x y)
  (:require (and (== x :beamer-theme)
                 (in? y (list :beamer-title-theme
                              :theorem-decorations))))
  #t)

(tm-define (style-includes? x y)
  (:require (and (== x "beamer")
                 (in? y (list "title-bar" "bluish" "framed-session"))))
  #t)

(tm-define (style-includes? x y)
  (:require (and (== x "old-beamer")
                 (in? y (list "framed-title" "ridged-paper" "framed-session"))))
  #t)

(tm-define (screens-buffer?)
  (with t (buffer-tree)
    (and (tree-is? t 'document)
         (tree-is? t :last 'screens))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dynamic movements for fold tags and switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (dynamic-context? t)
  (or (toggle-tag? (tree-label t))
      (switch-tag? (tree-label t))
      (overlays-tag? (tree-label t))))

(tm-define (dynamic-extremal t forwards?)
  (and-with p (tree-outer t)
    (dynamic-extremal p forwards?)))

(tm-define (dynamic-incremental t forwards?)
  (and-with p (tree-outer t)
    (dynamic-incremental p forwards?)))

(tm-define (dynamic-first)
  (dynamic-extremal (focus-tree) #f))
(tm-define (dynamic-last)
  (dynamic-extremal (focus-tree) #t))
(tm-define (dynamic-previous)
  (dynamic-incremental (focus-tree) #f))
(tm-define (dynamic-next)
  (dynamic-incremental (focus-tree) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract stuff for fold tags and switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-horizontal t forwards?)
  (:require (dynamic-context? t))
  (dynamic-incremental t forwards?))

(tm-define (structured-vertical t downwards?)
  (:require (dynamic-context? t))
  (dynamic-incremental t downwards?))

(tm-define (structured-extremal t forwards?)
  (:require (dynamic-context? t))
  (dynamic-extremal t forwards?))

(tm-define (structured-incremental t downwards?)
  (:require (dynamic-context? t))
  (dynamic-extremal t downwards?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Folding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (toggle-context? t)
  (toggle-tag? (tree-label t)))

(tm-define (toggle-first-context? t)
  (toggle-first-tag? (tree-label t)))

(tm-define (toggle-second-context? t)
  (toggle-second-tag? (tree-label t)))

(tm-define (fold-context? t)
  (or (folded-tag? (tree-label t)) (unfolded-tag? (tree-label t))))

(tm-define (make-toggle tag)
  (:type (-> void))
  (:synopsis "Insert a 'fold' environment")
  (insert-go-to `(,tag (document "") (document "")) (list 0 0)))

(tm-define (alternate-toggle t)
  (:require (toggle-context? t))
  (with i (if (toggle-first-context? t) 1 0)
    (variant-set t (ahash-ref alternate-table (tree-label t)))
    (tree-go-to t i :start)
    (when (> i 0)
      (players-set-elapsed (tree-ref t i) 0.0))))

(tm-define (dynamic-extremal t forwards?)
  (:require (toggle-context? t))
  (with action (if forwards? alternate-unfold alternate-fold)
    (action t)))

(tm-define (dynamic-incremental t forwards?)
  (:require (toggle-context? t))
  (with action (if forwards? alternate-unfold alternate-fold)
    (action t)))

(tm-define (tree-show-hidden t)
  (:require (toggle-context? t))
  (alternate-toggle t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operations on switch trees
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-hidden-tag t)
  'hidden)

(tm-define (get-hidden-tag t)
  (:require (unroll-context? t))
  'hidden*)

(define (switch-ref t i)
  (and t (>= i 0) (< i (tree-arity t))
       (not (hidden-context? (tree-ref t i)))))

(define (switch-set t i on?)
  (if (== i :last) (set! i (- (tree-arity t) 1)))
  (when (and (>= i 0) (< i (tree-arity t)))
    (cond ((and on? (hidden-context? (tree-ref t i)))
           (tree-assign-node (tree-ref t i) 'shown))
          ((and (not on?) (tree-is? t i 'shown))
           (tree-assign-node (tree-ref t i) (get-hidden-tag t)))
          ((and on? (not (tree-is? t i 'shown)))
           (tree-insert-node (tree-ref t i) 0 '(shown)))
          ((and (not on?) (not (hidden-context? (tree-ref t i))))
           (tree-insert-node (tree-ref t i) 0 (list (get-hidden-tag t)))))))

(define (switch-set-range t first last on?)
  (if (== last :last) (set! last (tree-arity t)))
  (for (i first last) (switch-set t i on?)))

(define (switch-last-visible t)
  (with v (- (tree-arity t) 1)
    (while (and (>= v 0) (not (switch-ref t v)))
      (set! v (- v 1)))
    v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines on switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (switch-context? t)
  (switch-tag? (tree-label t)))

(tm-define (switch-valid-child? t i)
  (and t i (>= i 0) (< i (tree-arity t))))

(tm-define (switch-index t . args)
  (and-let* ((o (switch-context? t))
             (i (if (null? args) :current (car args)))
             (c (tree-down-index t))
             (l (- (tree-arity t) 1))
             (v (switch-last-visible t)))
    (cond ((< v 0) #f)
          ((== i :visible) v)
          ((== i :current) c)
          ((== i :previous) (max 0 (- c 1)))
          ((== i :next) (min l (+ c 1)))
          ((== i :var-previous) (- c 1))
          ((== i :var-next) (+ c 1))
          ((== i :rotate-backward) (if (= c 0) l (- c 1)))
          ((== i :rotate-forward) (if (= c l) 0 (+ c 1)))
          ((== i :first) 0)
          ((== i :last) l)
          (else i))))

(tm-define (switch-to t i . args)
  (set! i (switch-index t i))
  (if (null? args) (set! args '(:start)))
  (when (switch-valid-child? t i)
    (with old (switch-index t)
      (switch-select t i)
      (apply tree-go-to (cons* t i 0 args))
      (when (> i old)
	(players-set-elapsed (tree-ref t i) 0.0)))))

(tm-define (switch-insert-at t i what)
  (set! i (if (== i :end) (tree-arity t) (switch-index t i)))
  (when (and (>= i 0) (<= i (tree-arity t)))
    (let* ((empty (if (tree-in? t (big-switch-tag-list)) '(document "") ""))
           (v (switch-index t :visible)))
      (tree-insert! t i `((shown ,(or what empty))))
      (if (tree-in? t (alternative-tag-list))
          (switch-select t i)
          (switch-select t (+ v 1)))
      (tree-go-to t i :start))))

(tm-define (switch-remove-at t i)
  (set! i (switch-index t i))
  (when (and (>= i 0) (< i (tree-arity t)) (> (tree-arity t) 1))
    (let* ((v (switch-index t :visible))
           (l (- (tree-arity t) 2)))
      (switch-set-range t (max 0 (- i 1)) (min l (+ i 1)) #t)
      (tree-remove! t i 1)
      (if (tree-in? t (alternative-tag-list))
          (switch-select t (min i l))
          (switch-select t (max 0 (- v 1))))
      (tree-go-to t (min i l) :start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific types of switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternative-context? t)
  (alternative-tag? (tree-label t)))

(tm-define (switch-select t i)
  (texmacs-error "switch-select" "invalid context"))

(tm-define (switch-select t i)
  (:require (alternative-context? t))
  (switch-set-range t 0 :last #f)
  (switch-set t i #t))

(define (unroll-context? t)
  (unroll-tag? (tree-label t)))

(tm-define (switch-select t i)
  (:require (unroll-context? t))
  (switch-set-range t 0 (+ i 1) #t)
  (switch-set-range t (+ i 1) :last #f))

(define (expanded-context? t)
  (expanded-tag? (tree-label t)))

(tm-define (switch-select t i)
  (:require (expanded-context? t))
  (switch-set-range t 0 :last #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface to switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-switch tag)
  (if (in? tag (big-switch-tag-list))
      (insert-go-to `(,tag (shown (document ""))) '(0 0 0 0))
      (insert-go-to `(,tag (shown "")) '(0 0 0))))

(tm-define (dynamic-extremal t forwards?)
  (:require (switch-context? t))
  (if forwards?
      (switch-to t :last :end)
      (switch-to t :first :start)))

(tm-define (dynamic-incremental t forwards?)
  (:require (switch-context? t))
  (if forwards?
      (switch-to t :next :start)
      (switch-to t :previous :end)))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (switch-context? t))
  (switch-insert-at t (if forwards? :var-next :current) #f))

(tm-define (structured-insert-vertical t downwards?)
  (:require (switch-context? t))
  (structured-insert-horizontal t downwards?))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (switch-context? t))
  (with i (if forwards? :current :var-previous)
    (set! i (switch-index t i))
    (cond ((< i 0) (tree-go-to t :start))
          ((>= i (tree-arity t)) (tree-go-to t :end))
          (else (switch-remove-at t i)))))

(tm-define (structured-remove-vertical t downwards?)
  (:require (switch-context? t))
  (structured-remove-horizontal t downwards?))

(tm-define (alternate-toggle t)
  (:require (switch-context? t))
  (switch-to t :rotate-forward))

(tm-define (switch-normalize-hidden t)
  (with l (get-hidden-tag t)
    (for (i (.. 0 (tree-arity t)))
      (when (and (hidden-context? (tree-ref t i)) (not (tree-is? t i l)))
        (tree-assign-node (tree-ref t i) l)))))

(tm-define (variant-circulate t forward?)
  (:require (switch-context? t))
  (with i (switch-index t)
    (variant-circulate-in t (big-switch-tag-list) forward?)
    (switch-normalize-hidden t)
    (switch-select t i)))

(tm-define (tree-show-hidden t)
  (:require (switch-context? t))
  (with i (tree-down-index t)
    (if (hidden-context? (tree-ref t i))
        (switch-select t i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface to overlays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (overlays-context? t)
  (and (overlays-tag? (tree-label t))
       (== (tree-arity t) 3)))

(tm-define (overlay-context? t)
  (overlay-tag? (tree-label t)))

(tm-define (make-overlays l)
  (if (== l 'overlays)
      (insert-go-to `(,l "1" "1" (document "")) '(2 0 0))
      (insert-go-to `(,l "1" "1" "") '(2 0))))

(tm-define (make-overlay l)
  (with-innermost t overlays-context?
    (if (unary-overlay-tag? (tree-label t))
        (insert-go-to `(,l ,(tree-copy (tree-ref t 0)) "") '(1 0))
        (insert-go-to `(,l ,(tree-copy (tree-ref t 0)) "" "") '(1 0)))))

(tm-define (make-alter-colors)
  (insert-go-to `(alter-colors "" "#0000" "black") '(0 0)))

(tm-define (overlays-current t)
  (and (overlays-context? t)
       (tree-atomic? (tree-ref t 0))
       (tree->number (tree-ref t 0))))

(tm-define (overlays-arity t)
  (and (overlays-context? t)
       (tree-atomic? (tree-ref t 1))
       (tree->number (tree-ref t 1))))

(define (overlays-animate t old new)
  (when (tree-compound? t)
    (when (and (overlay-visible? t new)
               (not (overlay-visible? t old))
	       (> new old))
      (players-set-elapsed t 0.0))
    (for-each (cut overlays-animate <> old new)
              (tree-children t))))

(define (overlays-set t new)
  (with old (tree->number (tree-ref t 0))
    (when (!= new old)
      (tree-set t 0 (number->string new))
      (graphics-update-proviso (tree-ref t 2) (number->string new))
      (overlays-animate t old new))))

(tm-define (overlays-switch-to t i)
  (when (overlays-context? t)
    (let* ((tot (tree->number (tree-ref t 1)))
           (nxt (min (max i 1) tot)))
      (overlays-set t nxt))))

(tm-define (overlay-current t)
  (and-with p (tree-search-upwards t overlays-context?)
    (overlays-current p)))

(tm-define (overlay-arity t)
  (and-with p (tree-search-upwards t overlays-context?)
    (overlays-arity p)))

(define (overlay-satisfies-proviso? t i pos)
  (cond ((>= pos (- (tree-arity t) 1)) #f)
        ((tm-equal? (tree-ref t pos) "proviso")
         (and-with c (tree-ref t (+ pos 1))
           (and (tree-in? c (nullary-overlay-tag-list))
                (== (tree-arity c) 1)
                (and-with ref (tree->number (tree-ref c 0))
                  (and (integer? ref)
                       (cond ((tree-is? c 'show-always) #t)
                             ((tree-is? c 'show-from)   (>= i ref))
                             ((tree-is? c 'show-until)  (<= i ref))
                             ((tree-is? c 'show-this)   (== i ref))
                             ((tree-is? c 'show-other)  (!= i ref))
                             (else #f)))))))
        (else (overlay-satisfies-proviso? t i (+ pos 2)))))

(define (overlay-visible-sub? t i)
  (cond ((tree-atomic? t) #t)
	((or (overlay-context? t)
	     (tree-in? t '(with anim-static anim-morph)))
	 (overlay-visible? t i))
	(else (forall? (cut overlay-visible-sub? <> i)
		       (tree-children t)))))

(define (overlay-frame-visible? f i)
  (and (tree-func? f 'tuple 2)
       (overlay-visible-sub? (tree-ref f 1) i)))

(tm-define (overlay-visible? t i)
  (or (and (overlay-context? t)
           (with ref (tree->number (tree-ref t 0))
             (cond ((not (integer? ref)) #f)
                   ((tree-is? t 'show-always)     #t)
                   ((tree-is? t 'show-from)       (>= i ref))
                   ((tree-is? t 'show-until)      (<= i ref))
                   ((tree-is? t 'show-this)       (== i ref))
                   ((tree-is? t 'show-other)      (!= i ref))
                   ((tree-is? t 'overlay-from)    (>= i ref))
                   ((tree-is? t 'overlay-until)   (<= i ref))
                   ((tree-is? t 'overlay-this)    (== i ref))
                   ((tree-is? t 'overlay-other)   (!= i ref))
                   ((tree-is? t 'alternate-from)  (>= i ref))
                   ((tree-is? t 'alternate-until) (<= i ref))
                   ((tree-is? t 'alternate-this)  (== i ref))
                   ((tree-is? t 'alternate-other) (!= i ref))
                   (else #f))))
      (and (tree-is? t 'with)
           (overlay-satisfies-proviso? t i 0))
      (and (tree-in? t '(anim-static anim-dynamic))
	   (tree-is? t 0 'morph)
	   (list-and (map (cut overlay-frame-visible? <> i)
			  (tree-children (tree-ref t 0)))))))

(tm-define (dynamic-extremal t forwards?)
  (:require (overlays-context? t))
  (let* ((tot (tree->number (tree-ref t 1)))
         (nxt (if forwards? tot 1)))
    (overlays-set t nxt)
    (when (not (tree-innermost overlay-context?))
      (tree-go-to t 2 :start))))

(tm-define (dynamic-incremental t forwards?)
  (:require (overlays-context? t))
  (let* ((cur (tree->number (tree-ref t 0)))
         (tot (tree->number (tree-ref t 1)))
         (inc (if forwards? 1 -1))
         (nxt (min (max (+ cur inc) 1) tot)))
    (overlays-set t nxt)
    (when (not (tree-innermost overlay-context?))
      (tree-go-to t 2 :start))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Additional routines for inserting and removing overlays
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (overlay-renumber-sub t p cur tot delta)
  (let* ((old (tree->number t))
         (new (if (>= old cur) (+ old delta) old))
         (ari (+ tot delta)))
    (cond ((or (>= new cur) (> delta 0))
           (tree-set t (number->string new)))
          ((tree-in? p '(overlay-from alternate-from))
           (tree-set t (number->string cur)))
          ((tree-in? p '(overlay-until alternate-until))
           (tree-set t (number->string new)))
          ((tree-in? p '(overlay-this alternate-this))
           (tree-set t "0"))
          ((tree-in? p '(overlay-other alternate-other))
           (tree-set t "0"))
          (else ;; NOTE: should never occur
           (tree-set t (number->string (min (max cur 1) ari)))))))

(define (overlay-renumber t cur tot delta)
  (cond ((tree-atomic? t) (noop))
        ((overlays-context? t) (noop))
        ((overlay-context? t)
         (for (c (cdr (tree-children t)))
           (overlay-renumber c cur tot delta))
         (overlay-renumber-sub (tree-ref t 0) t cur tot delta))
        (else
         (for-each (cut overlay-renumber <> cur tot delta)
                   (tree-children t)))))

(tm-define (structured-horizontal? t)
  (:require (overlays-context? t))
  #t)

(tm-define (structured-insert-horizontal t forwards?)
  (:require (overlays-context? t))
  (let* ((cur (tree->number (tree-ref t 0)))
         (tot (tree->number (tree-ref t 1)))
         (ins (if forwards? (+ cur 1) cur)))
    (overlay-renumber (tree-ref t 2) ins tot 1)
    (when forwards?
      (tree-set t 0 (number->string ins))
      (graphics-update-proviso (tree-ref t 2) (number->string ins)))
    (tree-set t 1 (number->string (+ tot 1)))))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (overlays-context? t))
  (let* ((cur (tree->number (tree-ref t 0)))
         (tot (tree->number (tree-ref t 1)))
         (del (if forwards? cur (- cur 1))))
    (when (and (> del 0) (> tot 1))
      (overlay-renumber (tree-ref t 2) del tot -1)
      (when (not forwards?)
        (tree-set t 0 (number->string del))
        (graphics-update-proviso (tree-ref t 2) (number->string del)))
      (tree-set t 1 (number->string (- tot 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Analyzing the environments occurring in folds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fold-environments (make-ahash-table))
(define fold-environments-first (make-ahash-table))
(define fold-environments-second (make-ahash-table))

(define (fold-add-environment t first?)
  (cond ((and (toggle-first-context? t)
              (not (tree-is? t 'summarized-algorithm)))
         (fold-add-environment (tree-ref t 1) #t))
        ((and (toggle-second-context? t)
              (not (tree-is? t 'detailed-algorithm)))
         (fold-add-environment (tree-ref t 1) #f))
        ((tm-func? t 'document 1)
         (fold-add-environment (tree-ref t 0) first?))
        (else
         (with tag 'text
           (if (tree-compound? t) (set! tag (tree-label t)))
           (if (== tag 'concat) (set! tag 'text))
           (if (== tag 'document) (set! tag 'text))
           (if (== tag 'render-proof) (set! tag 'proof))
           (if (in? tag '(summarized-algorithm detailed-algorithm))
               (set! tag 'algorithm))
           (ahash-set! fold-environments tag #t)
           (ahash-set! (if first?
                           fold-environments-first
                           fold-environments-second)
                       tag #t)))))

(define (fold-get-environments-sub t)
  (if (tree-compound? t)
      (for-each fold-get-environments-sub (tree-children t)))
  (if (toggle-context? t)
      (fold-add-environment t (toggle-first-context? t))))

(tm-define (fold-get-environments-in-buffer)
  (set! fold-environments (make-ahash-table))
  (set! fold-environments-first (make-ahash-table))
  (set! fold-environments-second (make-ahash-table))
  (fold-get-environments-sub (buffer-tree))
  (with envl (map car (ahash-table->list fold-environments))
    (values
      (list-sort (map symbol->string envl) string<=?)
      fold-environments-first
      fold-environments-second)))

(define (fold-matching-env? t tag)
  (cond ((tree-in? t '(summarized-algorithm detailed-algorithm))
         (== tag 'algorithm))
        ((toggle-first-context? t)
         (fold-matching-env? (tree-ref t 1) tag))
        ((toggle-second-context? t)
         (fold-matching-env? (tree-ref t 1) tag))
        ((tm-func? t 'document 1)
         (fold-matching-env? (tree-ref t 0) tag))
        ((or (tree-atomic? t) (tree-in? t '(document concat)))
         (== tag 'text))
        ((== tag 'proof)
         (tree-in? t '(proof render-proof)))
        (else (tree-is? t tag))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global routines for folding/unfolding/compressing/expanding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-operate-sub t mode)
  (if (tree-compound? t)
      (for-each (lambda (x) (dynamic-operate x mode)) (tree-children t)))
  (cond ((toggle-first-context? t)
         (cond ((== mode :var-last)
                (tree-insert-node! t 0 '(traversed)))
               ((in? mode '(:unfold :expand :var-expand :last))
                (alternate-toggle t))
               ((and (pair? mode) (== (car mode) :unfold)
                     (fold-matching-env? t (cadr mode)))
                (alternate-toggle t))))
        ((toggle-second-context? t)
         (cond ((== mode :var-last)
                (alternate-toggle t)
                (tree-insert-node! t 0 '(traversed)))
               ((in? mode '(:fold :compress :var-compress :first))
                (alternate-toggle t))
               ((and (pair? mode) (== (car mode) :fold)
                     (fold-matching-env? t (cadr mode)))
                (alternate-toggle t))))
        ((and (== mode :expand) (switch-context? t))
         (switch-set-range t 0 :last #t))
        ((and (== mode :compress) (switch-context? t))
         (switch-set t 0 #t)
         (switch-set-range t 1 :last #f))
        ((alternative-context? t)
         (cond ((== mode :first)
                (switch-set-range t 1 :last #f)
                (switch-set t 0 #t))
               ((== mode :last)
                (switch-set-range t 0 :last #f)
                (switch-set t :last #t))
               ((== mode :var-expand)
                (tree-assign-node! t 'expanded)
                (switch-set-range t 0 :last #t))))
        ((unroll-context? t)
         (cond ((== mode :var-last)
                (switch-set-range t 1 :last #f)
                (switch-set t 0 #t)
                (tree-insert-node t 0 '(traversed)))
               ((== mode :first)
                (switch-set-range t 1 :last #f)
                (switch-set t 0 #t))
               ((== mode :last)
                (switch-set-range t 0 :last #t))
               ((== mode :var-expand)
                (switch-set-range t 0 :last #t))
               ((== mode :var-compress)
                (switch-set t 0 #t)
                (switch-set-range t 1 :last #f))))
        ((expanded-context? t)
         (cond ((== mode :var-compress)
                (tree-assign-node! t 'switch)
                (switch-set t 0 #t)
                (switch-set-range t 1 :last #f))))
        ((overlays-context? t)
         (cond ((== mode :first)
                (dynamic-extremal t #f))
               ((== mode :last)
                (dynamic-extremal t #t))))))

(tm-define (dynamic-operate t mode)
  (when (tree-compound? t)
    (cond ((tree-is? t 'traversed)
           (when (!= mode :var-last)
             (dynamic-operate (tree-ref t 0) mode)
             (if (in? mode '(:unfold :expand :var-expand :first))
                 (tree-remove-node! t 0))))
          ((tree-is? t 'fold-back)
           (if (== mode :last) (set! mode :var-last))
           (dynamic-operate (tree-ref t 0) mode))
          ((tree-is? t 'keep-folded)
           (if (== mode :var-last) (set! mode :last))
           (dynamic-operate (tree-ref t 0) mode))
          (else (dynamic-operate-sub t mode)))))

(tm-define (dynamic-operate-on-buffer mode)
  (dynamic-operate (buffer-tree) mode)
  (if (in? mode '(:first :var-first)) (tree-go-to (buffer-tree) :start))
  (if (in? mode '(:last :var-last)) (tree-go-to (buffer-tree) :end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transform presentation into slides
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-first-alternative-list l)
  (if (null? l) #f
      (with r (dynamic-first-alternative (car l))
        (or r (dynamic-first-alternative-list (cdr l))))))

(define (dynamic-first-alternative t)
  (cond ((and (alternative-context? t) (> (tm-arity t) 1)) t)
        ((overlays-context? t)
         (tree-set! t 0 "1")
         (tree-assign-node! t 'overlays-range)
         t)
        ((and (tree-is? t 'overlays-range)
              (< (tree->number (tree-ref t 0))
                 (tree->number (tree-ref t 1)))) t)
        ((not (tm-compound? t)) #f)
        (else (dynamic-first-alternative-list (tm-children t)))))

(define (dynamic-alternative-keep-first slide)
  (and-with t (dynamic-first-alternative slide)
    (cond ((alternative-context? t)
           (tree-remove t 1 (- (tree-arity t) 1)))
          ((tree-is? t 'overlays-range)
           (tree-set t 1 (tree-copy (tree-ref t 0)))))
    (dynamic-alternative-keep-first slide)))

(define (dynamic-alternative-keep-other slide)
  (and-with t (dynamic-first-alternative slide)
    (cond ((alternative-context? t)
           (tree-remove t 0 1))
          ((tree-is? t 'overlays-range)
           (with nr (tree->number (tree-ref t 0))
             (tree-set t 0 (number->string (+ nr 1))))))))

(define (dynamic-make-slide t)
  (when (or (tm-func? t 'shown 1) (hidden-context? t))
    (tree-assign-node! t 'slide))
  (if (and (tm-func? t 'slide 1) (dynamic-first-alternative t))
      (let* ((p (tree-up t))
             (i (tree-index t)))
        (tree-insert p (+ i 1) (list (tree-copy t)))
        (let* ((v  (tree-ref p i))
               (w  (tree-ref p (+ i 1))))
          (dynamic-alternative-keep-first v)
          (dynamic-alternative-keep-other w)
          (dynamic-make-slide v)
          (dynamic-make-slide w)))
      (dynamic-operate (tree-ref t 0) :var-expand)))

(define (keep-shown! t)
  (if (alternative-context? t)
      (map ;; Prune hidden children
       (lambda (i) (if (tm-func? (tree-ref t i) 'hidden)
                       (tree-remove! t i 1)))
       (reverse (.. 0 (tm-arity t)))))
  (if (and (tm-compound? t) (> (tm-arity t) 0))
      (for-each keep-shown! (tree-children t))))

(define (create-slide scr)
  (if (not (tree? scr)) (tree 'slide '(document "")) ;; just in case
      (with t (tree-copy scr)
        (keep-shown! t)
        (tree-assign-node! t 'slide)
        t)))

(define (process-screen scr)
  (cons (create-slide scr)
        (begin
          (dynamic-traverse-buffer :next)
          (if (tm-func? scr 'hidden) '()
              (process-screen scr)))))

(define (list->tree label l)
  (tree-insert (tree label) 0 l))

(define (screens->slides t)
  (if (not (tm-func? t 'screens)) (tree 'document "")
      (with f (lambda (scr) (list->tree 'document (process-screen scr)))
        ;; (system-wait "Generating slides" "please wait") ;crashes if printing
        ;; Insert fake screen at the end
        (tree-insert! t (tree-arity t) 
                      (list (tree 'hidden '(document ""))))
        (dynamic-operate-on-buffer :first)
        ;; Notice that we don't process the last (fake) screen
	(list->tree 'screens (map f (cDr (tree-children t)))))))

(define (transform-last-slide doc)
  (cond ((tree-is? doc 'screens) (transform-last-slide (tm-ref doc :last)))
	((tree-is? doc 'document) (transform-last-slide (tm-ref doc :last)))
	((tree-func? doc 'slide 1) (tree-set! doc (tm-ref doc 0)))))

(tm-define (dynamic-make-slides)
  (init-default "page-medium"
                ;;"page-type" "page-width" "page-height"
                ;;"page-width-margin" "page-height-margin"
                ;;"page-odd" "page-even" "page-right"
                ;;"par-width" "page-odd-shift" "page-even-shift"
                ;;"page-top" "page-bot"
                )
  (add-style-package "slides")
  (if (preference-on? "texmacs->pdf:expand slides")
      (let* ((t (buffer-tree))
             (c (tree-children t)))
        (when (and (tm-func? t 'document) (switch-context? (cAr c)))
          (tree-assign-node (cAr c) 'document)
          (tree-set! t `(document ,@(cDr c) ,@(tree-children (cAr c))))
          ;; (system-wait "Generating slides" "please wait") ;crashes if printing
          (for-each dynamic-make-slide (tree-children t))
	  (transform-last-slide t)))
      (with l (select (buffer-tree) '(screens))
        (and (nnull? l)
             (let* ((scrns (car l))
                    (slides (screens->slides scrns)))
               (tree-set! scrns slides)
	       (transform-last-slide scrns))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global filtering of switches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-filter-remove? t mode)
  (and (tree-is? t :up 'document)
       (cond ((toggle-first-context? t)
              (cond ((== mode :remove-folded) #t)
                    ((== mode :keep-unfolded) #t)
                    (else #f)))
             ((toggle-second-context? t)
              (cond ((== mode :remove-unfolded) #t)
                    ((== mode :keep-folded) #t)
                    (else #f)))
             ((and (or (switch-context? t)
                       (alternative-context? t)
                       (unroll-context? t))
                   (== (switch-last-visible t) 0))
              (cond ((== mode :remove-folded) #t)
                    ((== mode :keep-unfolded) #t)
                    (else #f)))
             ((and (or (switch-context? t)
                       (alternative-context? t)
                       (unroll-context? t))
                   (!= (switch-last-visible t) 0))
              (cond ((== mode :remove-unfolded) #t)
                    ((== mode :keep-folded) #t)
                    (else #f)))
             ((in? mode (list :remove-folded :remove-unfolded)) #f)
             ((in? mode (list :keep-folded :keep-unfolded)) #t)
             (else #f))))

(define (dynamic-filter t mode)
  (if (dynamic-filter-remove? t mode)
      (with p (tree-up t)
        (if (== (tree-arity p) 1)
            (tree-assign! t "")
            (tree-remove! p (tree-index t) 1)))
      (if (and (tree-compound? t)
               (or (not (tree-is? t :up 'document))
                   (in? mode (list :remove-folded :remove-unfolded))))
          (for-each (lambda (x) (dynamic-filter x mode)) (tree-children t)))))

(tm-define (dynamic-filter-buffer mode)
  (dynamic-filter (buffer-tree) mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global navigation in recursive fold/switch structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dynamic-traverse-folded t mode)
  (cond ((in? mode '(:next :var-next))
         (alternate-toggle t)
         (dynamic-operate (tree-ref t 1) :first)
         (tree-go-to t 1 :end)
         #t)
        (else #f)))

(define (dynamic-traverse-unfolded t mode)
  (cond ((== mode :var-next)
         (alternate-toggle t)
         (tree-insert-node! t 0 '(traversed))
         #t)
        ((in? mode '(:previous :var-previous))
         (with last-mode (if (== mode :previous) :last :var-last)
           (alternate-toggle t)
           (dynamic-operate (tree-ref t 0) last-mode)
           (tree-go-to t 0 :end)
           #t))
        (else #f)))

(define (dynamic-traverse-switch t i l mode)
  (cond ((and (== mode :var-next) (= i l) (unroll-context? t))
         (dynamic-operate t :first)
         (tree-insert-node! t 0 '(traversed))
         #t)
        ((and (in? mode '(:next :var-next)) (< i l))
         (dynamic-operate (tree-ref t (1+ i)) :first)
         (tree-go-to t i :end)
         (switch-to t :next)
         #t)
        ((and (in? mode '(:previous :var-previous)) (> i 0))
         (with last-mode (if (== mode :previous) :last :var-last)
           (dynamic-operate (tree-ref t (- i 1)) last-mode)
           (tree-go-to t i :start)
           (switch-to t :previous)
           #t))
        (else #f)))

(define (dynamic-traverse-overlays t mode)
  (let* ((cur (tree->number (tree-ref t 0)))
         (tot (tree->number (tree-ref t 1)))
         (inc (if (in? mode '(:next :var-next)) 1 -1))
         (nxt (min (max (+ cur inc) 1) tot)))
    (and (!= nxt cur)
         (begin
           (overlays-set t nxt)
           (when (not (tree-innermost overlay-context?))
             (tree-go-to t 2 :start))
           #t))))

(define (dynamic-traverse-traversed t mode)
  (and (in? mode '(:previous :var-previous))
       (begin
         (tree-remove-node! t 0)
         (dynamic-operate t :last)
         (if (and (== mode :var-previous) (tree-compound? t))
             (for-each (lambda (x) (dynamic-operate x :var-last))
                       (tree-accessible-children t)))
         #t)))

(define (dynamic-traverse-list l mode)
  (and (nnull? l)
       (or (dynamic-traverse (car l) mode)
           (dynamic-traverse-list (cdr l) mode))))

(tm-define (dynamic-traverse t mode)
  (cond ((tree-atomic? t) #f)
        ((tree-is? t 'traversed)
         (dynamic-traverse-traversed t mode))
        ((tree-is? t 'fold-back)
         (if (== mode :next) (set! mode :var-next))
         (if (== mode :previous) (set! mode :var-previous))
         (dynamic-traverse (tree-ref t 0) mode))
        ((tree-is? t 'keep-folded)
         (if (== mode :var-next) (set! mode :next))
         (if (== mode :var-previous) (set! mode :previous))
         (dynamic-traverse (tree-ref t 0) mode))
        ((toggle-first-context? t)
         (or (dynamic-traverse (tree-ref t 0) mode)
             (dynamic-traverse-folded t mode)))
        ((toggle-second-context? t)
         (or (dynamic-traverse (tree-ref t 1) mode)
             (dynamic-traverse-unfolded t mode)))
        ((or (alternative-context? t) (unroll-context? t))
         (let* ((i (switch-last-visible t))
                (l (- (tree-arity t) 1)))
           (or (dynamic-traverse (tree-ref t i) mode)
               (dynamic-traverse-switch t i l mode))))
        ((overlays-context? t)
         (dynamic-traverse-overlays t mode))
        (else
         (let* ((c (tree-accessible-children t))
                (forward? (in? mode '(:next :var-next)))
                (l (if forward? c (reverse c))))
           (dynamic-traverse-list l mode)))))

(tm-define (dynamic-traverse-buffer mode)
  (dynamic-traverse (buffer-tree) mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific behaviour for switches inside list environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (switch-list-context? t)
  (and (switch-context? t)
       (tree-ref t :up :up)
       (tree-is? (tree-ref t :up) 'document)
       (list-context? (tree-ref t :up :up))))

(tm-define (make-unroll switch-tag)
  (if (and (selection-active?)
           (tree-in? (selection-tree) (list-tag-list))
           (tree-is? (selection-tree) 0 'document))
      (let* ((sel (selection-tree))
             (list-tag (tree-label sel))
             (items (tree-children (tree-ref sel 0)))
             (fun (lambda (c) `(shown (document ,c))))
             (unroll `(,switch-tag ,@(map fun items)))
             (new-list `(,list-tag (document ,unroll))))
        (clipboard-cut "dummy")
        (insert new-list))
      (make-switch switch-tag)))

(tm-define (make-switch-list switch-tag list-tag)
  (with flag? (and (selection-active-non-small?)
                   (in? list-tag (description-tag-list)))
    (wrap-selection-any
      (make list-tag)
      (make-switch switch-tag)      
      (if flag? (insert '(item* "")) (make-item)))))

(tm-define (kbd-enter t shift?)
  (:require (switch-list-context? t))
  (if shift? (make-return-after)
      (begin
        (switch-insert-at t :var-next #f)
        (make-item))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entering slide titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-enter t shift?)
  (:require (tree-is? t 'tit))
  (tree-go-to t :end)
  (insert-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific routines for 'screens' switch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (slideshow-context? t)
  (and (tree-is? t 'slideshow)
       (== (tree-arity t) 1)
       (tree-is? (tree-ref t 0) 'document)))

(tm-define (screens-context? t)
  (or (tree-is? t 'screens)
      (slideshow-context? t)))

(tm-define (nr-pages)
  (:require (tree-innermost screens-context?))
  (with t* (tree-innermost screens-context?)
    (with t (if (slideshow-context? t*) (tree-ref t* 0) t*)
      (tree-arity t))))

(tm-define (screens-switch-to which)
  (:secure #t)
  (and-with t (tree-innermost 'screens)
    (switch-to t which :start)))

(tm-define (screens-show-all)
  (and-with t (tree-innermost 'screens)
    (for (c (tree-children t))
      (when (tree-in? c '(hidden shown))
        (tree-assign-node! c 'slide)))
    (tree-assign-node! t 'document)
    (tree-insert-node! t 0 '(slideshow))))

(tm-define (screens-show-this)
  (and-with t (tree-innermost 'slideshow)
    (when (and (== (tree-arity t) 1) (tree-is? (tree-ref t 0) 'document))
      (tree-remove-node! t 0)
      (tree-assign-node! t 'screens)
      (for (c (tree-children t))
	(when (tree-func? c 'slide)
	  (tree-assign-node! c (if (cursor-inside? c) 'shown 'hidden)))))))

(define (expand-slides? s)
  (in? s (list "paper" "book" "panorama")))

(tm-define (init-page-rendering s)
  (:require (or (inside? 'screens) (inside? 'slideshow)))
  (with o (get-init-page-rendering)
    (when (and (expand-slides? s) (not (expand-slides? o)))
      (screens-show-all))
    (when (and (not (expand-slides? s)) (expand-slides? o))
      (screens-show-this))
    (former s)))

(tm-define (slide-get-switch t)
  (if (slideshow-context? t) (tree-ref t 0) t))

(tm-define (slide-get-document t)
  (cond ((not (tree? t)) #f)
        ((slideshow-context? t)
         (slide-get-document (tree-ref t 0 :down :down)))
        ((screens-context? t)
         (slide-get-document (tree-ref t :down :down)))
        ((tree-in? t '(shown hidden screen slide))
         (slide-get-document (tree-ref t 0)))
        ((tree-in? t '(with with-screen-color))
         (slide-get-document (tree-ref t :last)))
        ((tree-is? t 'document) t)
        (else #f)))

(define (slide-get-bg-color-bis t)
  (cond ((tree-is? t :up 'with)
         (slide-set-bg-color-bis (tree-up t)))
        ((tree-is? t :up 'with-screen-color)
         (tree-ref t :up 0))
        (else "")))

(tm-define (slide-get-bg-color)
  (and-with t (tree-innermost screens-context?)
    (with u (slide-get-document t)
      (slide-get-bg-color-bis u))))

(define (slide-set-bg-color-bis t col)
  (cond ((tree-is? t :up 'with)
         (slide-set-bg-color-bis (tree-up t) col))
        ((tree-is? t :up 'with-screen-color)
         (if (== col "")
             (tree-set t :up t)
             (tree-set t :up 0 col)))
        ((!= col "")
         (tree-set t `(with-screen-color ,col ,t)))))

(tm-define (slide-set-bg-color col)
  (and-with t (tree-innermost screens-context?)
    (with u (slide-get-document t)
      (slide-set-bg-color-bis u col))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing slideshows in expanded form
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (screens-switch-to which)
  (:require (inside? 'slideshow))
  (and-with t (tree-innermost 'slideshow)
    (when (and (== (tree-arity t) 1) (tree-is? (tree-ref t 0) 'document))
      (set! t (tree-ref t 0))
      (when (== which :first) (set! which 0))
      (when (== which :last) (set! which (- (tree-arity t) 1)))
      (tree-go-to t which :start))))

(tm-define (structured-horizontal? t)
  (:require (slideshow-context? t))
  #t)

(tm-define (focus-can-insert? t)
  (:require (slideshow-context? t))
  #t)

(tm-define (focus-can-remove? t)
  (:require (slideshow-context? t))
  #t)

(tm-define (structured-insert-horizontal t forwards?)
  (:require (slideshow-context? t))
  (set! t (tree-ref t 0))
  (with i (+ (tree-index (tree-down t)) (if forwards? 1 0))
    (tree-insert! t i '((slide (document ""))))
    (tree-go-to t i 0 0 0)))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (slideshow-context? t))
  (set! t (tree-ref t 0))
  (let* ((i (- (tree-index (tree-down t)) (if forwards? 0 1)))
         (n (tree-arity t)))
    (when (and (>= i 0) (> n 1))
      (tree-remove! t i 1)
      (tree-go-to t (min i (- n 1)) (if forwards? :start :end)))))

(define (slide-range)
  (and-with t (tree-innermost slideshow-context?)
    (let* ((p (tree->path (tree-ref t 0)))
           (p1 (selection-get-start))
           (p2 (selection-get-end)))
      (and (list-starts? p1 p)
           (list-starts? p2 p)
           (let* ((i1 (list-ref p1 (length p)))
                  (i2 (list-ref p2 (length p)))
                  (s1 (tm-ref t 0 i1))
                  (s2 (tm-ref t 0 i2)))
             (and s1 s2 (< i1 i2)
                  (tree-is? s1 'slide) (tree-is? s2 'slide)
                  (< (+ (- i2 i1) 1) (tree-arity (tree-ref t 0)))
                  (list i1 i2)))))))

(tm-define (clipboard-cut which)
  (:require (and (inside? 'slideshow)
                 (slide-range)))
  (with (i1 i2) (slide-range)
    (with t (tree-innermost slideshow-context?)
      (clipboard-set which (selection-tree))
      (tree-remove (tree-ref t 0) i1 (+ (- i2 i1) 1))
      (if (>= i1 (tree-arity (tree-ref t 0)))
          (tree-go-to t 0 (- i1 1) :end)
          (tree-go-to t 0 i1 :start)))))

(define (slides? t)
  (and (tree-is? t 'tuple)
       (> (tree-arity t) 2)
       (== (tree-ref t 0) (string->tree "texmacs"))
       (with u (tree-ref t 1)
         (and (tree-is? u 'document)
              (> (tree-arity u) 1)
              (list-and (map (cut tree-is? <> 'slide) (tree-children u)))))))

(tm-define (clipboard-paste which)
  (:require (and (inside? 'slideshow)
                 (slides? (clipboard-get which))))
  (let* ((t (tree-innermost slideshow-context?))
         (i (tree-index (tree-down (tree-ref t 0))))
         (ins (tree-ref (clipboard-get which) 1)))
    (tree-insert (tree-ref t 0) i (tree-children ins))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizations for graphical screens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (in-screens-graphics?)
  (and (inside? 'gr-screen) (in-graphics?)))

(define (innermost-gr-screen)
  (and-with t (tree-innermost 'screens)
    (and-with s (slide-get-document t)
      (or (and (tree-is? s 'document)
               (>= (tree-arity s) 1)
               (tree-func? (tree-ref s 0) 'gr-screen 1)
               (tree-ref s 0))
          (and (tree-is? s 'document)
               (>= (tree-arity s) 2)
               (tree-is? (tree-ref s 0) 'tit)
               (tree-func? (tree-ref s 1) 'gr-screen 1)
               (tree-ref s 1))))))

(define (innermost-graphics-screen)
  (and-with t (innermost-gr-screen)
    (set! t (tree-ref t 0))
    (while (or (tree-func? t 'document 1)
               (tree-func? t 'gr-overlays 3)
               (tree-is? t 'with))
      (set! t (tm-ref t :last)))
    (and (tree-is? t 'graphics) t)))

(define (go-to-graphics)
  (and-with t (innermost-graphics-screen)
    (tree-go-to t 0 :start)
    (graphics-reset-state)
    (graphics-decorations-update)))

(tm-define (dynamic-operate-on-buffer mode)
  ;;(:require (in-screens-graphics?))
  (former mode)
  (go-to-graphics))

(tm-define (dynamic-traverse-buffer mode)
  ;;(:require (in-screens-graphics?))
  (former mode)
  (go-to-graphics))

(tm-define (screens-switch-to which)
  ;;(:require (in-screens-graphics?))
  (former which)
  (go-to-graphics))

(define (extract-slide-template t)
  (when (or (tree-func? t 'shown 1) (tree-func? t 'hidden 1))
    (set! t (tree-ref t 0)))
  (cond ((tree-func? t 'document 1)
         `(document ,(extract-slide-template (tree-ref t 0))))
        ((and (tree-func? t 'document)
              (>= (tree-arity t) 2)
              (tree-func? (tree-ref t 0) 'tit))
         `(document ,(extract-slide-template (tree-ref t 0))
                    ,(extract-slide-template (tree-ref t 1))))
        ((and (tree-func? t 'document)
              (>= (tree-arity t) 1))
         `(document ,(extract-slide-template (tree-ref t 0))))
        ((tree-is? t 'tit)
         `(tit ""))
        ((tree-func? t 'gr-screen 1)
         `(gr-screen ,(extract-slide-template (tree-ref t 0))))
        ((tree-func? t 'gr-overlays 3)
         `(gr-overlays "1" "1" ,(extract-slide-template (tree-ref t 2))))
        ((tree-in? t '(with with-screen-color))
         (with l (tree-children t)
           `(,(tree-label t) ,@(cDr l) ,(extract-slide-template (cAr l)))))
        ((tree-is? t 'graphics)
         `(graphics ""))
        (else "")))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (and (screens-context? t) (not (slideshow-context? t))))
  (with tmpl (extract-slide-template (tree-down t))
    (when (not (tree-multi-paragraph? (tm->tree tmpl)))
      (set! tmpl `(document ,tmpl)))
    (switch-insert-at t (if forwards? :var-next :current) tmpl)
    (and-with s (tree-down (tree-innermost 'screens))
      (when (or (tree-func? s 'shown 1) (tree-func? s 'hidden 1))
        (set! s (tree-ref s 0)))
      (cond ((and (tree-is? s 'document)
                  (>= (tree-arity s) 2)
                  (tree-func? (tree-ref s 0) 'tit 1))
             (tree-go-to s 0 0 :start))
            ((and (tree-is? s 'document)
                  (>= (tree-arity s) 1)
                  (tree-func? (tree-ref s 0) 'gr-screen 1))
             (go-to-graphics))))))

(tm-define (make-gr-overlays forwards?)
  (and-with t (tree-innermost 'graphics #t)
    (while (tree-is? t :up 'with)
      (set! t (tree-up t)))
    (when (not (overlays-context? t))
      (tree-set! t `(gr-overlays ,(if forwards? "2" "1") "2" ,t)))))
