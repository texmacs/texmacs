
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : spell-edit.scm
;; DESCRIPTION : editing routines for spell checking
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (version spell-edit)
  (:use (version version-edit)
        (convert tools tmconcat)))

(tm-define spell-language #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spell context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-context? t)
  (spell-tag? (tree-label t)))

(tm-define (inside-spell?)
  (not (not (tree-innermost spell-context?))))

(texmacs-modes
  (in-spell% (inside-spell?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moving across the differences between both versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-go-to-first)
  (go-start)
  (spell-go-to-next))

(tm-define (spell-go-to-previous)
  (go-to-previous-tag-argument (group-resolve 'spell-tag) 0)
  (recenter-window))

(tm-define (spell-go-to-next)
  (go-to-next-tag-argument (group-resolve 'spell-tag) 0)
  (recenter-window))

(tm-define (spell-go-to-last)
  (go-end)
  (spell-go-to-previous))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Retaining particular versions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-replace-cache (make-ahash-table))

(tm-define (spell-go-to-first*)
  (go-start)
  (delayed
    (:idle 10)
    (spell-go-to-next*)))

(tm-define (spell-go-to-next*)
  (go-to-next-tag-argument (group-resolve 'spell-tag) 0)
  (when (not (tree-innermost spell-context?))
    (go-to-previous-tag-argument (group-resolve 'spell-tag) 0))
  (let* ((t (tree-innermost spell-context?))
         (i (and t (ahash-ref spell-replace-cache (tm->stree t)))))
    (if i
        (spell-retain i :recurse)
        (recenter-window))))

(tm-define (spell-retain i mode)
  (with-innermost t spell-context?
    (and-with r (tm-ref t (if (== i 0) i (+ i 1)))
      (when (== mode #t)
        (ahash-set! spell-replace-cache (tm->stree t) i))
      (tree-cut t)
      (insert r)
      (when (!= mode :recurse)
        (refresh-tooltips))
      (delayed
        (:idle 10)
        (spell-go-to-next*)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define spell-dictionaries (make-ahash-table))
(define spell-accepted-words (make-ahash-table))

(define (spell-dictionary lan)
  (url-append "$TEXMACS_HOME_PATH/langs/natural/spell"
              (string-append lan ".scm")))

(define (spell-load-dictionary lan)
  (when (not (ahash-ref spell-dictionaries lan))
    (with dic (spell-dictionary lan)
      (if (url-exists? dic)
          (with l (load-object dic)
            (ahash-set! spell-dictionaries lan l)
            (for (w l)
              (ahash-set! spell-accepted-words (list lan w) #t)))
          (ahash-set! spell-dictionaries lan '())))))

(define (spell-save-dictionary lan)
  (and-with dic (spell-dictionary lan)
    (and-with l (ahash-ref spell-dictionaries lan)
      (save-object dic l))))

(tm-define (spell-retain-permanent)
  (with lan spell-language
    (with-innermost t spell-context?
      (and-with l (ahash-ref spell-dictionaries lan)
        (ahash-set! spell-dictionaries lan (cons (tm->stree (tm-ref t 0)) l))
        (spell-save-dictionary lan))
      (spell-retain 0 #t))))

(tm-define (spell-replace-cached t)
  (let* ((i (ahash-ref spell-replace-cache (tm->stree t)))
         (key (list spell-language (tm->stree (tm-ref t 0)))))
    (cond (i (tree-ref t (if (== i 0) i (+ i 1))))
          ((ahash-ref spell-accepted-words key) (tree-ref t 0))
          (else t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define process-serial 0)

(define process-active (make-ahash-table))

(tm-define (process-active? serial)
  (not (not (ahash-ref process-active serial))))

(tm-define (make-process-end . types)
  (set! process-serial (+ process-serial 1))
  (with serial process-serial
    ;;(display* "Launch process " serial "\n")
    (ahash-set! process-active serial types)
    (lambda ()
      ;;(display* "Terminate process " serial "\n")
      (ahash-remove! process-active serial))))

(tm-define (process-deactivate type)
  (for (serial (map car (ahash-table->list process-active)))
    (when (in? type (ahash-ref process-active serial))
      ;;(display* "Interrupt process " serial "\n")
      (ahash-remove! process-active serial))))

(tm-define ((make-process-range serial fun tp1 tp2 next))
  (let* ((t1 (tree-pointer->tree tp1))
         (t2 (tree-pointer->tree tp2))
         (p1 (and t1 (tree->path t1)))
         (p2 (and t2 (tree->path t2)))
         (l1 (and p1 (cAr p1)))
         (l2 (and p2 (cAr p2))))
    ;;(display* "process " p1 " -- " p2 "\n")
    (cond ((not (and p1 p2 (== (cDr p1) (cDr p2))))
           (next))
          ((== p1 p2)
           (if (ahash-ref process-active serial)
               (fun serial tp1 (tm->stree t1) next)
               (begin
                 (tree-pointer-detach tp1)
                 (next))))
          ((< l1 l2)
           (let* ((mid1 (quotient (+ l1 l2) 2))
                  (mid2 (+ mid1 1))
                  (t1* (tm-ref t1 :up mid1))
                  (t2* (tm-ref t1 :up mid2))
                  (tp1* (if (== mid1 l1) tp1 (tree->tree-pointer t1*)))
                  (tp2* (if (== mid2 l2) tp2 (tree->tree-pointer t2*)))
                  (proc2 (make-process-range serial fun tp2* tp2 next))
                  (proc1 (make-process-range serial fun tp1 tp1* proc2)))
             (proc1)))
          (else (next)))))

(tm-define (make-process-document serial fun t next)
  (when (tree->path t)
    (cond ((tm-func? t 'with)
           (make-process-document serial fun (tm-ref t :last) next))
          ((and (tm-func? t 'document) (> (tm-arity t) 1))
           (let* ((tp1 (tree->tree-pointer (tm-ref t 0)))
                  (tp2 (tree->tree-pointer (tm-ref t :last))))
             (make-process-range serial fun tp1 tp2 next)))
          (else
            (with tp (tree->tree-pointer t)
              (make-process-range serial fun tp tp next))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminate spell checking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (spell-initiate lan)
  (set! spell-language lan)
  (spell-load-dictionary lan))

(tm-define (spell-terminate* t)
  (cond ((tree-atomic? t) (noop))
        ((tm-func? t 'spell-error)
         (tree-set! t (tree-ref t 0)))
        ((tm-func? t 'concat)
         (for-each spell-terminate* (tree-children t))
         (tree-set! t (apply tmconcat (tree-children t))))
        (else (for-each spell-terminate* (tree-children t)))))

(tm-define (spell-terminate . opt-t)
  (set! spell-language #f)
  (process-deactivate 'spell)
  (with-innermost t spell-context?
    (and-with r (tm-ref t 0)
      (tree-cut t)
      (insert r)
      (close-tooltips)))
  (cond ((null? opt-t)
         (spell-terminate* (buffer-tree)))
        ((tree? (car opt-t))
         (spell-terminate* (car opt-t))))
  (delayed
    (:idle 100)
    (recenter-window)
    (refresh-tooltips)))
