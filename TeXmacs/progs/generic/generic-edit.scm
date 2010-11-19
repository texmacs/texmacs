
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : generic-edit.scm
;; DESCRIPTION : Generic editing routines
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic generic-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils edit variants)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic editing via the keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-left) (go-left))
(tm-define (kbd-right) (go-right))
(tm-define (kbd-up) (go-up))
(tm-define (kbd-down) (go-down))
(tm-define (kbd-page-up) (go-page-up))
(tm-define (kbd-page-down) (go-page-down))
(tm-define (kbd-start-line) (go-start-line))
(tm-define (kbd-end-line) (go-end-line))

(tm-define (kbd-select r)
  (select-from-shift-keyboard)
  (r)
  (select-from-cursor))

(tm-define (insert-return) (insert-raw-return))
(tm-define (kbd-return) (insert-return))
(tm-define (kbd-shift-return) (insert-return))

(tm-define (kbd-remove forward?) (remove-text forward?))
(tm-define (kbd-remove forward?)
  (:mode with-any-selection?)
  (clipboard-cut "nowhere")
  (clipboard-clear "nowhere"))

(tm-define (kbd-tab)
  (if (not (complete-try?))
      (with sh (kbd-system-rewrite (kbd-find-inv-binding '(make-htab "5mm")))
	(set-message `(concat "Use " ,sh " in order to insert a tab")
		     "tab"))))

(tm-define (kbd-shift-tab)
  (complete-try?))

(tm-define (kbd-tab)
  (:mode in-source?)
  (:inside label reference pageref)
  (if (complete-try?) (noop)))

(tm-define (kbd-shift-tab)
  (:mode in-source?)
  (:inside label reference pageref)
  (if (complete-try?) (noop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (complex-context? t)
  (and (nleaf? t) (nin? (tree-label t) '(concat document))))

(tm-define (simple-context? t)
  (or (leaf? t)
      (and (tree-in? t '(concat document))
	   (simple-context? (tree-down t)))))

(tm-define (similar-complex-context? t)
  (complex-context? t))

(tm-define (document-context? t)
  (tree-is? t 'document))

(tm-define (table-markup-context? t)
  (or (tree-in? t '(table tformat))
      (and (== (tree-arity t) 1)
           (or (tree-in? (tree-ref t 0) '(table tformat))
               (and (tm-func? (tree-ref t 0) 'document 1)
                    (tree-in? (tree-ref t 0 0) '(table tformat)))))))

(tm-define (structured-horizontal? t)
  (or (tree-is-dynamic? t)
      (table-markup-context? t)))

(tm-define (structured-vertical? t)
  (or (tree-in? t '(tree))
      (table-markup-context? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree traversal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (traverse-right) (go-to-next-word))
(tm-define (traverse-left) (go-to-previous-word))
(tm-define (traverse-up) (noop))
(tm-define (traverse-down) (noop))

(tm-define (traverse-up)
  (:context document-context?)
  (go-to-previous-tag 'document))

(tm-define (traverse-down)
  (:context document-context?)
  (go-to-next-tag 'document))

(define (traverse-tree . l)
  (cond ((null? l) (traverse-tree (tree-up (cursor-tree))))
	((tree-in? (car l) '(concat document))
	 (traverse-tree (tree-up (car l))))
	(else (car l))))

(define (traverse-label . l)
  (tree-label (apply traverse-tree l)))

(define (find-similar-upwards t l)
  (cond ((in? (tree-label t) l) t)
        ((and (not (tree-is-buffer? t)) (tree-up t))
         (find-similar-upwards (tree-up t) l))
        (else #f)))

(define-macro (with-focus-in l . body)
  `(begin
     ,@body
     (selection-cancel)
     (and-with t (find-similar-upwards (focus-tree) ,l)
       (tree-focus t))))

(tm-define (traverse-incremental t forward?)
  (let* ((l (similar-to (tree-label t)))
         (fun (if forward? go-to-next-tag go-to-previous-tag)))
    (with-focus-in l (fun l))))

(tm-define (traverse-extremal t forward?)
  (let* ((l (similar-to (tree-label t)))
         (fun (if forward? go-to-next-tag go-to-previous-tag))
         (inc (lambda () (fun l)))
         (end (if forward? structured-end structured-start)))
    (with-focus-in l (go-to-repeat inc) (end))))

(tm-define (traverse-previous)
  (traverse-incremental (focus-tree) #f))

(tm-define (traverse-next)
  (traverse-incremental (focus-tree) #t))

(tm-define (traverse-first)
  (traverse-extremal (focus-tree) #f))

(tm-define (traverse-last)
  (traverse-extremal (focus-tree) #t))

(tm-define (traverse-previous-section-title)
  (go-to-previous-tag (similar-to 'section)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured insert and remove
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert-horizontal t forwards?)
  (focus-next t
    (structured-insert-horizontal (tree-up t) forwards?)))

(tm-define (structured-insert-vertical t downwards?)
  (focus-next t
    (structured-insert-vertical (tree-up t) downwards?)))

(tm-define (structured-remove-horizontal t forwards?)
  (focus-next t
    (structured-remove-horizontal (tree-up t) forwards?)))

(tm-define (structured-remove-vertical t downwards?)
  (focus-next t
    (structured-remove-vertical (tree-up t) downwards?)))

(tm-define (structured-insert-horizontal t forwards?)
  (:require (structured-horizontal? t))
  (when (tree->path t :down)
    (insert-argument-at (tree->path t :down) forwards?)))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (structured-horizontal? t))
  (when (tree->path t :down)
    (remove-argument-at (tree->path t :down) forwards?)))

(tm-define (structured-insert-extremal t forwards?)
  (structured-extremal t forwards?)
  (structured-insert-horizontal t forwards?))

(tm-define (structured-insert-incremental t downwards?)
  (structured-incremental t downwards?)
  (structured-insert-vertical t downwards?))

(tm-define (structured-insert-left)
  (structured-insert-horizontal (focus-tree) #f))
(tm-define (structured-insert-right)
  (structured-insert-horizontal (focus-tree) #t))
(tm-define (structured-remove-left)
  (structured-remove-horizontal (focus-tree) #f))
(tm-define (structured-remove-right)
  (structured-remove-horizontal (focus-tree) #t))
(tm-define (structured-insert-up)
  (structured-insert-vertical (focus-tree) #f))
(tm-define (structured-insert-down)
  (structured-insert-vertical (focus-tree) #t))
(tm-define (structured-remove-up)
  (structured-remove-vertical (focus-tree) #f))
(tm-define (structured-remove-down)
  (structured-remove-vertical (focus-tree) #t))
(tm-define (structured-insert-start)
  (structured-insert-extremal (focus-tree) #f))
(tm-define (structured-insert-end)
  (structured-insert-extremal (focus-tree) #t))
(tm-define (structured-insert-top)
  (structured-insert-incremental (focus-tree) #f))
(tm-define (structured-insert-bottom)
  (structured-insert-incremental (focus-tree) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Structured movements
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-horizontal t forwards?)
  (focus-next t
    (structured-horizontal (tree-up t) forwards?)))

(tm-define (structured-vertical t downwards?)
  (focus-next t
    (structured-vertical (tree-up t) downwards?)))

(tm-define (structured-horizontal t forwards?)
  (:require (structured-horizontal? t))
  (with move (if forwards? path-next-argument path-previous-argument)
    (with p (move (root-tree) (tree->path (tree-down t)))
      (if (nnull? p) (go-to p)))))

(tm-define (structured-left)
  (structured-horizontal (focus-tree) #f))
(tm-define (structured-right)
  (structured-horizontal (focus-tree) #t))
(tm-define (structured-up)
  (structured-vertical (focus-tree) #f))
(tm-define (structured-down)
  (structured-vertical (focus-tree) #t))

(tm-define (structured-start)
  (with-innermost t complex-context?
    (tree-go-to t :down :start)))

(tm-define (structured-end)
  (with-innermost t complex-context?
    (tree-go-to t :down :end)))

(tm-define (structured-exit-left)
  (with-innermost t complex-context?
    (tree-go-to t :start)))

(tm-define (structured-exit-right)
  (with-innermost t complex-context?
    (tree-go-to t :end)))

(tm-define (structured-first)
  (go-to-repeat structured-left)
  (structured-start))

(tm-define (structured-last)
  (go-to-repeat structured-right)
  (structured-end))

(tm-define (structured-top)
  (go-to-repeat structured-up)
  (structured-start))

(tm-define (structured-bottom)
  (go-to-repeat structured-down)
  (structured-end))

(tm-define (structured-extremal t forwards?)
  (if forwards? (structured-last) (structured-first)))

(tm-define (structured-incremental t downwards?)
  (if downwards? (structured-bottom) (structured-top)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multi-purpose alignment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (geometry-speed t down?)
  (focus-next t
    (geometry-speed (tree-up t) down?)))

(tm-define (geometry-variant t forward?)
  (focus-next t
    (geometry-variant (tree-up t) forward?)))

(tm-define (geometry-default t)
  (focus-next t
    (geometry-default (tree-up t))))

(tm-define (geometry-horizontal t forward?)
  (focus-next t
    (geometry-horizontal (tree-up t) forward?)))

(tm-define (geometry-vertical t down?)
  (focus-next t
    (geometry-vertical (tree-up t) down?)))

(tm-define (geometry-extremal t forward?)
  (focus-next t
    (geometry-extremal (tree-up t) forward?)))

(tm-define (geometry-incremental t down?)
  (focus-next t
    (geometry-incremental (tree-up t) down?)))

(tm-define (geometry-slower)
  (geometry-speed (focus-tree) #f))
(tm-define (geometry-faster)
  (geometry-speed (focus-tree) #t))
(tm-define (geometry-circulate forward?)
  (geometry-variant (focus-tree) forward?))
(tm-define (geometry-reset)
  (geometry-default (focus-tree)))
(tm-define (geometry-left)
  (geometry-horizontal (focus-tree) #f))
(tm-define (geometry-right)
  (geometry-horizontal (focus-tree) #t))
(tm-define (geometry-up)
  (geometry-vertical (focus-tree) #f))
(tm-define (geometry-down)
  (geometry-vertical (focus-tree) #t))
(tm-define (geometry-start)
  (geometry-extremal (focus-tree) #f))
(tm-define (geometry-end)
  (geometry-extremal (focus-tree) #t))
(tm-define (geometry-top)
  (geometry-incremental (focus-tree) #f))
(tm-define (geometry-bottom)
  (geometry-incremental (focus-tree) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (structured-insert-horizontal t forwards?)
  (:require (tree-is? t 'tree))
  (if (== (tree-down-index t) 0) (set! t (tree-up t)))
  (if (== (tm-car t) 'tree)
      (with pos (tree-down-index t)
        (if forwards? (set! pos (1+ pos)))
        (tree-insert! t pos '(""))
        (tree-go-to t pos 0))))

(tm-define (structured-remove-horizontal t forwards?)
  (:require (tree-is? t 'tree))
  (if (== (tree-down-index t) 0) (set! t (tree-up t)))
  (if (== (tm-car t) 'tree)
      (with pos (tree-down-index t)
        (cond (forwards?
               (tree-remove! t pos 1)
               (if (== pos (tree-arity t))
                   (tree-go-to t :end)
                   (tree-go-to t pos :start)))
              ((== pos 1) (tree-go-to t 0 :end))
              (else (tree-remove! t (- pos 1) 1))))))

(tm-define (structured-insert-vertical t downwards?)
  (:require (tree-is? t 'tree))
  (if downwards?
      (if (== (tree-down-index t) 0)
          (with pos (tree-arity t)
            (tree-insert! t pos '(""))
            (tree-go-to t pos 0))
          (begin
            (set! t (tree-down t))
            (tree-set! t `(tree ,t ""))
            (tree-go-to t 1 0)))
      (begin
        (if (!= (tree-down-index t) 0) (set! t (tree-down t)))
        (tree-set! t `(tree "" ,t))
        (tree-go-to t 0 0))))

(define (branch-active t)
  (with i (tree-down-index t)
    (if (and (= i 0) (tree-is? t :up 'tree))
        (tree-up t)
        t)))

(define (branch-go-to . l)
  (apply tree-go-to l)
  (if (tree-is? (cursor-tree) 'tree)
      (with last (cAr l)
	(if (nin? last '(:start :end)) (set! last :start))
	(tree-go-to (cursor-tree) 0 last))))

(tm-define (structured-horizontal t* forwards?)
  (:require (tree-is? t* 'tree))
  (let* ((t (branch-active t*))
	 (i (tree-down-index t)))
    (cond ((and (not forwards?) (> i 1))
           (branch-go-to t (- i 1) :end))
          ((and forwards? (!= i 0) (< i (- (tree-arity t) 1)))
           (branch-go-to t (+ i 1) :start)))))

(tm-define (structured-vertical t* downwards?)
  (:require (tree-is? t* 'tree))
  (let* ((t (branch-active t*))
	 (i (tree-down-index t)))
    (cond ((and (not downwards?) (!= i 0))
           (tree-go-to t 0 :end))
          ((and downwards? (== (tree-down-index t*) 0))
           (branch-go-to t* (quotient (tree-arity t*) 2) :start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra editing functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kill-paragraph)
  (selection-set-start)
  (go-end-paragraph)
  (selection-set-end)
  (clipboard-cut "primary"))

(tm-define (select-all)
  (tree-select (buffer-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting various kinds of content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-label)
  (make 'label))

(tm-define (make-specific s)
  (if (or (== s "texmacs") (in-source?))
      (insert-go-to `(specific ,s "") '(1 0))
      (insert-go-to `(inactive (specific ,s "")) '(0 1 0))))

(tm-define (make-include u)
  (insert `(include ,(url->string u))))

(tm-define (make-inline-image l)
  (apply make-image (cons* (url->string (car l)) #f (cdr l))))

(tm-define (make-link-image l)
  (apply make-image (cons* (url->string (car l)) #t (cdr l))))

(tm-define (make-graphics-over-selection)
  (if (selection-active-any?)
  (with selection (selection-tree)
    (clipboard-cut "graphics background")
    (insert-go-to `(draw-over ,selection (graphics)) '(1 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Thumbnails facility
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (thumbnail-suffixes)
  (list->url (map url-wildcard
		  '("*.gif" "*.jpg" "*.jpeg" "*.JPG" "*.JPEG"))))

(define (fill-row l nr)
  (cond ((= nr 0) '())
	((nnull? l) (cons (car l) (fill-row (cdr l) (- nr 1))))
	(else (cons "" (fill-row l (- nr 1))))))

(define (make-rows l nr)
  (if (> (length l) nr)
      (cons (list-head l nr) (make-rows (list-tail l nr) nr))
      (list (fill-row l nr))))

(define (make-thumbnails-sub l)
  (define (mapper x)
    `(image ,(url->string x) "0.22par" "" "" ""))
  (let* ((l1 (map mapper l))
	 (l2 (make-rows l1 4))
	 (l3 (map (lambda (r) `(row ,@(map (lambda (c) `(cell ,c)) r))) l2)))
    (insert `(tabular* (tformat (twith "table-width" "1par")
				(twith "table-hyphen" "yes")
				(table ,@l3))))))

(tm-define (make-thumbnails)
  (:interactive #t)
  (dialogue
    (let* ((dir (dialogue-url "Picture directory" "directory"))
	   (find (url-append dir (thumbnail-suffixes)))
	   (files (url->list (url-expand (url-complete find "r"))))
	   (base (get-name-buffer))
	   (rel-files (map (lambda (x) (url-delta base x)) files)))
      (if (nnull? rel-files) (make-thumbnails-sub rel-files)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for floats
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-insertion s)
  (:synopsis "Make an insertion of type @s.")
  (with pos (if (== s "float") "tbh" "")
    (insert-go-to (list 'float s pos (list 'document ""))
		  (list 2 0 0))))

(tm-define (insertion-positioning what flag)
  (:synopsis "Allow/disallow the position @what for innermost float.")
  (with-innermost t 'float
    (let ((op (if flag string-union string-minus))
	  (st (tree-ref t 1)))
      (tree-set! st (op (tree->string st) what)))))

(define (test-insertion-positioning? what)
  (with-innermost t 'float
    (with c (string-ref what 0)
      (char-in-string? c (tree->string (tree-ref t 1))))))

(define (not-test-insertion-positioning? s)
  (not (test-insertion-positioning? s)))

(tm-define (toggle-insertion-positioning what)
  (:check-mark "v" test-insertion-positioning?)
  (insertion-positioning what (not-test-insertion-positioning? what)))

(tm-define (toggle-insertion-positioning-not s)
  (:check-mark "v" not-test-insertion-positioning?)
  (toggle-insertion-positioning s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sound and video
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-sound u)
  (if (not (url-none? u))
      (insert `(sound ,(url->string u)))))

(tm-define (make-animation u)
  (interactive
      (lambda (w h len rep)
	(if (== rep "no") (set! rep "false"))
	(insert `(video ,(url->string u) ,w ,h ,len ,rep)))
    "Width" "Height" "Length" "Repeat?"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search, replace, spell and tab-completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (key-press-command key)
  ;; FIXME: this routine should do exactly the same as key-press,
  ;; without modification of the internal state and without executing
  ;; the actual shortcut. It should rather return a command which
  ;; does all this, or #f
  (and-with p (kbd-find-key-binding key)
    (car p)))

(tm-define (keyboard-press key time)
  (:mode search-mode?)
  (with cmd (key-press-command (string-append "search " key))
    (cond (cmd (cmd))
	  ((key-press-search key) (noop))
	  (else (key-press key)))))

(tm-define (search-next)
  (key-press-search "next"))

(tm-define (search-previous)
  (key-press-search "previous"))

(tm-define (keyboard-press key time)
  (:mode replace-mode?)
  (with cmd (key-press-command (string-append "replace " key))
    (cond (cmd (cmd))
	  ((key-press-replace key) (noop))
	  (else (key-press key)))))

(tm-define (keyboard-press key time)
  (:mode spell-mode?)
  (with cmd (key-press-command (string-append "spell " key))
    (cond (cmd (cmd))
	  ((key-press-spell key) (noop))
	  (else (key-press key)))))

(tm-define (keyboard-press key time)
  (:mode complete-mode?)
  (with cmd (key-press-command (string-append "complete " key))
    (cond (cmd (cmd))
	  ((key-press-complete key) (noop))
	  (else (key-press key)))))
