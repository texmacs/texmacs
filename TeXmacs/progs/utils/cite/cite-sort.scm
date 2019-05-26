
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

(define (compare-cite-keys t1 t2)
  (let*
    ((t1 (car t1))
     (t2 (car t2))
     (s1 (if (string? t1) t1 (convert t1 "stm-snippet" "verbatim-snippet")))
     (s2 (if (string? t2) t2 (convert t2 "stm-snippet" "verbatim-snippet"))))
    (compare-string s1 s2)))

(define (expand-references k)
  (with key (stree->tree `(get-binding ,(cadr k)))
    (with ret (tree->stree (texmacs-exec key))
      (if (!= ret '(uninit)) ret ""))))

(define (consecutive? seq)
  (if (> (length (cdr seq)) 0)
    (if (= (+ 1 (car seq)) (cadr seq))
        (consecutive? (cdr seq))
      #f)
   #t)
)

(tm-define (cite-sort args)
  ;; get a (tuple (tuple key_1 value_1) ... (tuple key_n value_n))
  ;; and sort it according to values.
  (:secure #t)
  (let* ((args (map tree->stree (tree-children args)))
         (keys (map expand-references (map caddr args)))
         (tup (map list keys args))
         (sorted-tup (list-sort tup compare-cite-keys))
         ;; we should merge contiguous number series here...
         (merged (and (> (length sorted-tup) 2) 
                      (consecutive? (map string->number (map car sorted-tup)))))
         (sorted-args (map cadr (if merged (list (first sorted-tup) (last sorted-tup)) 
                                           sorted-tup)))
         (sep (if merged "--" '(cite-sep)))
         (ret `(concat ,@(list-intersperse sorted-args sep))))
    ret))
