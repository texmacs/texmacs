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

(define (compare-cite-keys t1 t2 comparator)
  (let*
    ((t1 (car t1))
     (t2 (car t2))
     (s1 (if (string? t1) t1 (convert t1 "stm-snippet" "verbatim-snippet")))
     (s2 (if (string? t2) t2 (convert t2 "stm-snippet" "verbatim-snippet"))))
    (comparator s1 s2)))

(define (expand-references k)
  (with key (stree->tree `(get-binding ,(cadr k)))
    (with ret (tree->stree (texmacs-exec key))
      (if (!= ret '(uninit)) ret ""))))

(define (merge-contiguous new old present)
  (let* 
    ((get-write (lambda (item) (car (cdadr item))))
     (flush 
        (lambda ()
          (if (> (length present) 2)
              (list (list 
                (caar present)
                `(concat 
                  ,@(map get-write present)
                  ,@(cddar (cdar present)) "-" ,@(cddar (cdAr present)))))
              present))))
    (if (null? old)
        (if (null? present)
            new
            (append new (flush)))
        (if (null? present)
            (merge-contiguous new (cdr old) (list (car old)))
            (if (compare-cite-keys (cAr present) (car old) compare-string-inc?)
                (merge-contiguous new (cdr old) (append present (list (car old))))
                (merge-contiguous (append new (flush)) (cdr old) (list (car old))))))))

(tm-define (indice-sort tup)
  (let* ((sorted-tup 
           (list-sort tup (lambda (s1 s2) (compare-cite-keys s1 s2 compare-string))))
         (merged-tup (merge-contiguous '() sorted-tup '()))
         (merged-args (map cadr merged-tup)))
    merged-args))

(tm-define (cite-sort args)
  ;; get a (tuple (concat (write "bib" key_1) (reference value_1))) ... (concat key_n value_n))
  ;; and sort it according to values.
  (:secure #t)
  ;;(display* "args: " (tree->stree args) "\n")
  (let* ((args (map tree->stree (tree-children args)))
         (keys (map expand-references (map caddr args)))
         (tup (map list keys args))
         (merged-args (indice-sort tup))
         (ret `(concat ,@(list-intersperse merged-args '(cite-sep)))))
    ;;(display* "merged: " merged-args)
    ;;(display "merged: ")
    ;;(map (lambda (x) (display* x "\n")) (car merged-args))
    ;;(display "\n")
    ret))
