
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cite-sort.scm
;; DESCRIPTION : support utilities for sorting citations
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils cite cite-sort))

(define (compare-string s1 s2)
  (if (and (string->number s1) (string->number s2))
    (< (string->number s1) (string->number s2))
    (string<? s1 s2)))

(define (compare-string-inc? s1 s2)
  (if (and (string->number s1) (string->number s2))
      (== 1 (- (string->number s2) (string->number s1)))
      #f))

(define (compare-cite-keys t1 t2 . inc-t)
  (let*
      ((t1 (car t1))
       (t2 (car t2))
       (s1 (if (string? t1) t1 (convert t1 "texmacs-stree" "verbatim-snippet")))
       (s2 (if (string? t2) t2 (convert t2 "texmacs-stree" "verbatim-snippet"))))
    (if (null? inc-t)
        (compare-string s1 s2)
        (compare-string-inc? s1 s2))))

(define (expand-references k)
  (with key (stree->tree `(get-binding ,(cadr k)))
    (with ret (tree->stree (texmacs-exec key))
      (if (!= ret '(uninit)) ret ""))))

(define (merge-contiguous new old present)
  (let ((flush
         (lambda ()
           ;;(display* "present: " present "\n")
           (if (> (length present) 2)
               (list (list (caar present) `(concat ,@(cdar present) "-" ,@(cdAr present))))
               present))))
    (if (null? old)
        (if (null? present)
            new
            (append new (flush)))
        (if (null? present)
            (merge-contiguous new (cdr old) (list (car old)))
            (if (compare-cite-keys (cAr present) (car old) #t)
                (merge-contiguous new (cdr old) (append present (list (car old))))
                (merge-contiguous (append new (flush)) (cdr old) (list (car old)))
                )))))

(tm-define (cite-sort args)
  ;; get a (tuple (tuple key_1 value_1) ... (tuple key_n value_n))
  ;; and sort it according to values.
  (:secure #t)
  (let* ((args (map tree->stree (tree-children args)))
         (keys (map expand-references (map caddr args)))
         (tup (map list keys args))
         (sorted-tup (list-sort tup compare-cite-keys))
         (merged-tup (merge-contiguous '() sorted-tup '()))
         (merged-args (map cadr merged-tup))
         (ret `(concat ,@(list-intersperse merged-args '(cite-sep)))))
    ;;(display* "merged-tup: " merged-tup "\n")
    ;;(display* "merged-args: " merged-args "\n")
    ;;(display* "tup: " tup "\n")
    ;;(display* "ret: " ret "\n")
    ret))
