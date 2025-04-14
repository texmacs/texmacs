
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-edit.scm
;; DESCRIPTION : routines for formatting text
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic format-edit)
  (:use (utils base environment)
	(utils edit selections)
	(utils library cursor)
	(generic generic-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic with manipulations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-ref* t var i)
  (and (<= i (- (tree-arity t) 3))
       (or (and (tm-equal? (tree-ref t i) var)
                (tree-ref t (+ i 1)))
           (with-ref* t var (+ i 2)))))

(tm-define (with-ref t var)
  (and (tm-is? t 'with)
       (with-ref* t var 0)))

(tm-define (with-set t var val)
  (with old-val (with-ref t var)
    (cond (old-val (tree-set! old-val val))
          ((tree-is? t 'with)
           (tree-insert! t (- (tree-arity t) 1) (list var val)))
          (else (tree-set! t `(with ,var ,val ,t))))
    t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplification
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-simplify-sub t var)
  (cond ((tree-is-buffer? t) (noop))
        ((tree-func? t 'document 1)
         (with-simplify-sub (tree-up t) var))
        ((tree-func? t 'with)
         (for (i (reverse (.. 0 (quotient (tree-arity t) 2))))
           (when (== (tree-ref t (* 2 i)) var)
             (tree-remove! t (* 2 i) 2)))
         (when (and (tree-func? (tm-ref t :last) 'document 1)
                    (tree-func? (tm-ref t :last 0) 'with))
           (tree-remove-node (tm-ref t :last) 0))
         (when (tree-func? t 'with 1)
           (tree-remove-node! t 0))
         (when (tree-up t)
           (with-simplify-sub (tree-up t) var)))))

(tm-define (with-simplify t)
  (when (and (not (tree-is-buffer? t)) (tree->path t))
    (with-simplify (tree-up t))
    (when (tree-is? t 'with)
      (for (var (map car (list->assoc (cDr (tree-children t)))))
        (with-simplify-sub (tree-up t) var)))))

(tm-define (with-merge t)
  (when (and (tree-is? t 'with) (tree-is? t :up 'with))
    (let* ((p (tree-up t))
           (c (map tree-copy (cDr (tree-children t)))))
      (tree-remove-node t (- (tree-arity t) 1))
      (tree-insert p (- (tree-arity p) 1) c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-env? var val)
  (== (get-env var) val))

(tm-property (make-with var val)
  (:check-mark "o" test-env?))

(tm-define (make-interactive-with var)
  (:interactive #t)
  (interactive (lambda (s) (make-with var s))
    (list (logic-ref env-var-description% var) "string" (get-env var))))

(tm-define (make-interactive-with-opacity)
  (:interactive #t)
  (interactive (lambda (s) (make-with-like `(with-opacity ,s "")))
    (list "opacity" "string" '())))

(define (add-with l t)
  (if (tm-is? t 'with)
      (with c (tm-children t)
        `(with ,@(cDr c) ,(add-with l (cAr c))))
      `(with ,@l ,t)))

(define (add-with-path l t)
  (if (tm-is? t 'with)
      (with c (tm-children t)
        (cons (length (cDr c)) (add-with-path l (cAr c))))
      (cons (length l) (path-end t '()))))

(define (get-cars l)
  (if (or (null? l) (null? (cdr l))) (list)
      (cons (car l) (get-cars (cddr l)))))

(define (get-cadrs l)
  (if (or (null? l) (null? (cdr l))) (list)
      (cons (cadr l) (get-cadrs (cddr l)))))

(tm-define (make-multi-with l)
  (when (nnull? l)
    (cond ((selection-active-table?)
           (keep-table-selection
            (for-each cell-set-format (get-cars l) (get-cadrs l))))
          ((selection-active-any?)
           (with t (selection-tree)
             (clipboard-cut "null")
             (insert-go-to (add-with l t) (add-with-path l t))
             (with-simplify (cursor-tree))
             (and-with w (tree-innermost 'with #t)
               (tree-select w))))
          (else
            (insert-go-to `(with ,@l "") (list (length l) 0))
            (with-simplify (cursor-tree))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-line-with var val)
  (:synopsis "Make 'with' with one or more paragraphs as its scope")
  (:check-mark "o" test-env?)
  (if (and (selection-active-table?) #f) ;; FIXME: does not work yet
      (make-with var val)
      (begin
        (if (not (selection-active-normal?))
            (select-line))
        (make-with var val)
        (insert-return)
        (remove-text #f))))

(tm-define (make-interactive-line-with var)
  (:interactive #t)
  (interactive (lambda (s) (make-line-with var s))
    (list (logic-ref env-var-description% var) "string" (get-env var))))

(tm-define (make-multi-line-with l)
  (when (nnull? l)
    (cond ((selection-active-table?)
           (make-multi-with l))
          (else
            (when (not (selection-active-normal?))
              (select-line))
            (make-multi-with l)
            (insert-return)
            (remove-text #f)
            (and-with w (tree-innermost 'with #t)
              (with-simplify w)
              (and-with w* (tree-innermost 'with #t)
                (with-merge w*)
                (and-with w** (tree-innermost 'with #t)
                  (tree-select w**))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting and toggling with-like tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))

(tm-define (with-like-check-insert t)
  (cond ((with u (cursor-tree)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree)
	   (tree-go-to u :last (if (== (cAr (cursor-path)) 0) :start :end))
	   #t))
	((with u (cursor-tree*)
	   (and (with-like? u) (with-same-type? t u)))
	 (with u (cursor-tree*)
	   (tree-go-to u :last :start)
	   #t))
	((and-with u (with-like-search (cursor-tree)) (with-same-type? t u))
	 (with sym (symbol->string (tree-label t))
	   (set-message `(concat "Warning: already inside '" ,sym "'")
			`(concat "make '" ,sym "'"))
	   #t))
	(else #f)))

(tm-define (make-with-like w)
  (cond ((func? w 'with 3)
	 (make-with (cadr w) (caddr w)))
	((and (tm-compound? w) (== (tm-arity w) 1))
	 (make (car w)))
	((selection-active-any?)
	 (let* ((selection (selection-tree))
		(ins `(,@(cDr w) ,selection))
		(end (path-end selection '())))
	   (clipboard-cut "nowhere")
	   (insert-go-to ins (cons (- (tm-arity ins) 1) end))))
	(else
	  (insert-go-to w (list (- (tm-arity w) 1) 0)))))

(tm-define (toggle-with-like w back)
  (with t (if (and (selection-active-any?)
		   (== (selection-tree) (path->tree (selection-path))))
	      (path->tree (selection-path))
	      (with-like-search (tree-ref (cursor-tree) :up)))
    ;;(display* "t= " t "\n")
    (cond ((not (and t (with-like? t) (with-same-type? t w)))
           (make-with-like w))
          ((or (not back) (tree-empty? (tm-ref t :last)))
           (tree-remove-node! t (- (tree-arity t) 1))
           (tree-correct-node (tree-ref t :up)))
          ((tree-at-start? (tm-ref t :last))
           (tree-go-to t 0))
          ((tree-at-end? (tm-ref t :last))
           (tree-go-to t 1))
          (else (make-with-like back)))))

(tm-define (toggle-bold)
  (toggle-with-like '(with "font-series" "bold" "")
                    '(with "font-series" "medium" "")))

(tm-define (toggle-italic)
  (toggle-with-like '(with "font-shape" "italic" "")
                    '(with "font-shape" "right" "")))

(tm-define (toggle-small-caps)
  (toggle-with-like '(with "font-shape" "small-caps" "")
                    '(with "font-shape" "right" "")))

(tm-define (toggle-underlined)
  (toggle-with-like '(underline "") #f))

(tm-define (make-alternate prompt default-val tag)
  (:interactive #t)
  (interactive (lambda (x) (make-with-like `(,tag ,x "")))
    (list prompt "string" default-val)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customizable environments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (customizable-context? t)
  (nnull? (customizable-parameters t)))

(tm-define (customizable-parameters t)
  (list))

(define customizable-cache (make-ahash-table))

(tm-define (customizable-parameters-memo t)
  (with key (tree-label t)
    (when (not (ahash-ref customizable-cache key))
      (ahash-set! customizable-cache key (customizable-parameters t)))
    (ahash-ref customizable-cache key)))

(tm-define (tree-with-set t . l)
  (focus-tree-modified t)
  (tree-set! t `(with ,@l ,t))
  (with-simplify t)
  (with-merge t))

(tm-define (tree-with-get t var)
  (and (tree? t)
       (with hit #f
         (when (tree-is? t 'with)
           (for (i (.. 0 (- (tree-arity t) 1) 2))
             (when (tm-equal? (tree-ref t i) var)
               (set! hit i))))
         (cond (hit (tree-ref t (+ hit 1)))
               ((and (tree-up t) (tree-up (tree-up t)))
                (tree-with-get (tree-up t) var))
               (else #f)))))

(define (tree-with-reset* t var)
  (with hit #f
    (when (tree-is? t 'with)
      (for (i (.. 0 (- (tree-arity t) 1) 2))
        (when (tm-equal? (tree-ref t i) var)
          (set! hit i))))
    (cond (hit
           (tree-remove! t hit 2)
           (when (== (tree-arity t) 1)
             (tree-remove-node t 0)))
          ((and (tree-up t) (tree-up (tree-up t)))
           (tree-with-reset* (tree-up t) var)))))

(tm-define (tree-with-reset t var)
  (when (and (tree? t) (tree->path t))
    (focus-tree-modified t)
    (tree-with-reset* t var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spacing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (make-hspace spc)
  (:synopsis "Insert stretchable space")
  (:argument spc "Horizontal space"))

(tm-property (make-space spc)
  (:synopsis "Insert rigid space")
  (:argument spc "Horizontal space"))

(tm-property (make-var-space spc base top)
  (:synopsis "Insert rigid space")
  (:argument spc "Horizontal space")
  (:argument base "Base level")
  (:argument top "Top level"))

(tm-property (make-htab spc)
  (:synopsis "Insert horizontal tab")
  (:argument spc "Minimal space"))

(tm-property (make-vspace-before spc)
  (:synopsis "Insert space before")
  (:argument spc "Vertical space"))

(tm-property (make-vspace-after spc)
  (:synopsis "Insert space after")
  (:argument spc "Vertical space"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page breaking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-page-break)
  (make 'page-break)
  (insert-return))

(tm-define (make-new-page)
  (make 'new-page)
  (insert-return))

(tm-define (make-new-dpage)
  (make 'new-dpage)
  (insert-return))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple text flows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-has-preferences? t)
  (:require (tree-in? t '(float)))
  #t)

(tm-define (standard-parameters l)
  (:require (== l "float"))
  (list "page-float-sep"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-choice-list var)
  (:require (in? var (list "embold-strength" "embbb-strength")))
  (list "1.5" "2" "2.5" "3" "4" "5" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'embold))
  (list (list "embold-strength" "Strength")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'embbb))
  (list (list "embbb-strength" "Strength")))

(tm-define (parameter-choice-list var)
  (:require (in? var (list "slanted-slope")))
  (list "-1" "-0.5" "-0.33" "-0.25" "-0.2" "-0.15" "-0.1"
        "0.1" "0.15" "0.2" "0.25" "0.33" "0.5" "1" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'slanted))
  (list (list "slanted-slope" "Slope")))

(tm-define (parameter-choice-list var)
  (:require (string-ends? var "magnified-factor"))
  (list "0.2" "0.5" "0.8" "0.9" "1.1" "1.2" "1.5" "2" "5" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'hmagnified))
  (list (list "hmagnified-factor" "Factor")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'vmagnified))
  (list (list "vmagnified-factor" "Factor")))

(tm-define (parameter-choice-list var)
  (:require (== var "condensed-factor"))
  (list "0.5" "0.75" "0.8" "0.85" "0.9" "0.95" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'condensed))
  (list (list "condensed-factor" "Factor")))

(tm-define (parameter-choice-list var)
  (:require (== var "extended-factor"))
  (list "1.05" "1.1" "1.15" "1.2" "1.25" "1.5" "2" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'extended))
  (list (list "extended-factor" "Factor")))

(tm-define (parameter-choice-list var)
  (:require (== var "monospaced-factor"))
  (list "0.6" "0.65" "0.7" "0.75" "0.8" "0.85" "0.9" "0.95" "1" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'extended))
  (list (list "monospaced-factor" "Factor")))

(tm-define (parameter-choice-list var)
  (:require (== var "degraded-threshold"))
  (list "0.5" "0.6" "0.667" "0.75" :other))

(tm-define (parameter-choice-list var)
  (:require (string-ends? var "-frequency"))
  (list "0.5" "0.75" "1.0" "1.5" "2.0" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'degraded))
  (list (list "degraded-threshold" "Threshold")
        (list "degraded-frequency" "Frequency")))

(tm-define (parameter-choice-list var)
  (:require (in? var (list "distorted-strength" "gnawed-strength")))
  (list "0.4" "0.5" "0.6" "0.8" "1.0" "1.5" :other))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'distorted))
  (list (list "distorted-strength" "Strength")
        (list "distorted-frequency" "Frequency")))

(tm-define (customizable-parameters t)
  (:require (tree-is? t 'gnawed))
  (list (list "gnawed-strength" "Strength")
        (list "gnawed-frequency" "Frequency")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parameter-choice-list var)
  (:require (or (string-ends? var "-pen-width")
                (string-ends? var "-pen-height")
                (string-ends? var "-pen-dx")
                (string-ends? var "-pen-dy")
                (string-ends? var "-blur-radius")))
  (list "0.25ln" "0.5ln" "1ln" "1.5ln" "2ln" "2.5ln" "5ln" :other))

(tm-define (parameter-choice-list var)
  (:require (string-ends? var "-pen-angle"))
  (list "-60" "-45" "-30" "0" "30" "45" "60" "90" :other))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(blur gaussian-blur oval-blur rectangular-blur)))
  (list (list "blur-pen-width" "Pen width")
        (list "blur-pen-height" "Pen height")
        (list "blur-pen-angle" "Pen angle")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(motion-blur)))
  (list (list "blur-pen-dx" "Pen dx")
        (list "blur-pen-dy" "Pen dy")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(outline gaussian-outline
                          oval-outline rectangular-outline)))
  (list (list "outline-pen-width" "Pen width")
        (list "outline-pen-height" "Pen height")
        (list "outline-pen-angle" "Pen angle")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(motion-outline)))
  (list (list "outline-pen-dx" "Pen dx")
        (list "outline-pen-dy" "Pen dy")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(thicken gaussian-thicken
                          oval-thicken rectangular-thicken)))
  (list (list "thicken-pen-width" "Pen width")
        (list "thicken-pen-height" "Pen height")
        (list "thicken-pen-angle" "Pen angle")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(motion-thicken)))
  (list (list "thicken-pen-dx" "Pen dx")
        (list "thicken-pen-dy" "Pen dy")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(erode gaussian-erode oval-erode rectangular-erode)))
  (list (list "erode-pen-width" "Pen width")
        (list "erode-pen-height" "Pen height")
        (list "erode-pen-angle" "Pen angle")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(motion-erode)))
  (list (list "erode-pen-dx" "Pen dx")
        (list "erode-pen-dy" "Pen dy")))

(tm-define (parameter-choice-list var)
  (:require (in? var (list "shadow-dx" "shadow-dy"
                           "engrave-dx" "engrave-dy"
                           "emboss-dx" "emboss-dy")))
  (list "-2.5ln" "-2ln" "-1.5ln" "-1ln" "-0.5ln" "0ln"
        "0.5ln" "1ln" "1.5ln" "2ln" "2.5ln" :other))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(shadow shadowed-raise)))
  (list (list "shadow-dx" "Dx")
        (list "shadow-dy" "Dy")
        (list "shadow-color" "Color")
        (list "shadow-blur-radius" "Blur radius")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(engrave)))
  (list (list "engrave-dx" "Dx")
        (list "engrave-dy" "Dy")
        (list "engrave-color" "Color")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(emboss)))
  (list (list "emboss-dx" "Dx")
        (list "emboss-dy" "Dy")
        (list "emboss-color" "Color")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(outlined-engrave)))
  (list (list "engrave-dx" "Dx")
        (list "engrave-dy" "Dy")
        (list "outline-pen-width" "Pen width")
        (list "outline-pen-height" "Pen height")
        (list "outline-pen-angle" "Pen angle")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(outlined-emboss)))
  (list (list "emboss-dx" "Dx")
        (list "emboss-dy" "Dy")
        (list "outline-pen-width" "Pen width")
        (list "outline-pen-height" "Pen height")
        (list "outline-pen-angle" "Pen angle")))

(tm-define (parameter-choice-list var)
  (:require (or (string-ends? var "-wavelen-x")
                (string-ends? var "-wavelen-y")))
  (list "0.1fn" "0.15fn" "0.2fn" "0.25fn" "0.3fn" "0.4fn" "0.5fn" :other))

(tm-define (parameter-choice-list var)
  (:require (== var "degrade-threshold"))
  (list "0.5" "0.6" "0.667" "0.75" :other))

(tm-define (parameter-choice-list var)
  (:require (== var "degrade-sharpness"))
  (list "0.5" "0.75" "1.0" "1.5" "2.0" :other))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(degrade)))
  (list (list "degrade-wavelen-x" "Wave length x")
        (list "degrade-wavelen-y" "Wave length y")
        (list "degrade-threshold" "Threshold")
        (list "degrade-sharpness" "Sharpness")))

(tm-define (parameter-choice-list var)
  (:require (in? var (list "distort-radius-x" "distort-radius-y"
                           "gnaw-radius-x" "gnaw-radius-y")))
  (list "0.05fn" "0.1fn" "0.15fn" "0.2fn" :other))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(distort)))
  (list (list "distort-wavelen-x" "Wave length x")
        (list "distort-wavelen-y" "Wave length y")
        (list "distort-radius-x" "Radius x")
        (list "distort-radius-y" "Radius y")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(gnaw)))
  (list (list "gnaw-wavelen-x" "Wave length x")
        (list "gnaw-wavelen-y" "Wave length y")
        (list "gnaw-radius-x" "Radius x")
        (list "gnaw-radius-y" "Radius y")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(make-transparent)))
  (list (list "make-transparent-bg-color" "Background color")
        (list "make-transparent-threshold" "Threshold")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(make-opaque)))
  (list (list "make-opaque-bg-color" "Background color")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(recolor)))
  (list (list "recolor-color" "New color")))

(tm-define (customizable-parameters t)
  (:require (tree-in? t '(skin)))
  (list (list "skin-color" "Skin color")))

(tm-define (pen-effect-context? t)
  (tree-in? t (pen-effect-tag-list)))

(tm-define (get-effect-pen t)
  (when (not (tree? t))
    (set! t (tree-innermost pen-effect-context?)))
  (cond ((not (tree? t)) #f)
        ((tree-in? t '(blur)) "gaussian")
        ((tree-in? t '(outline)) "oval")
        ((tree-in? t '(thicken erode)) "rectangular")
        ((tree-in? t (gaussian-effect-tag-list)) "gaussian")
        ((tree-in? t (oval-effect-tag-list)) "oval")
        ((tree-in? t (rectangular-effect-tag-list)) "rectangular")
        ((tree-in? t (motion-effect-tag-list)) "motion")
        (else #f)))

(define (test-effect-pen? t pen)
  (== (get-effect-pen t) pen))

(tm-define (set-effect-pen t pen)
  (:check-mark "*" test-effect-pen?)
  (when (not (tree? t))
    (set! t (tree-innermost pen-effect-context?)))
  (cond ((not (tree? t)) (noop))
        ((tree-in? t '(blur gaussian-blur oval-blur
                            rectangular-blur motion-blur))
         (cond ((== pen "gaussian") (variant-set t 'gaussian-blur))
               ((== pen "oval") (variant-set t 'oval-blur))
               ((== pen "rectangular") (variant-set t 'rectangular-blur))
               ((== pen "motion") (variant-set t 'motion-blur))))
        ((tree-in? t '(outline gaussian-outline oval-outline
                               rectangular-outline motion-outline))
         (cond ((== pen "gaussian") (variant-set t 'gaussian-outline))
               ((== pen "oval") (variant-set t 'oval-outline))
               ((== pen "rectangular") (variant-set t 'rectangular-outline))
               ((== pen "motion") (variant-set t 'motion-outline))))
        ((tree-in? t '(thicken gaussian-thicken oval-thicken
                               rectangular-thicken motion-thicken))
         (cond ((== pen "gaussian") (variant-set t 'gaussian-thicken))
               ((== pen "oval") (variant-set t 'oval-thicken))
               ((== pen "rectangular") (variant-set t 'rectangular-thicken))
               ((== pen "motion") (variant-set t 'motion-thicken))))
        ((tree-in? t '(erode gaussian-erode oval-erode
                             rectangular-erode motion-erode))
         (cond ((== pen "gaussian") (variant-set t 'gaussian-erode))
               ((== pen "oval") (variant-set t 'oval-erode))
               ((== pen "rectangular") (variant-set t 'rectangular-erode))
               ((== pen "motion") (variant-set t 'motion-erode))))))
