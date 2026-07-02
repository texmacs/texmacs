
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : process.scm
;; DESCRIPTION : running asynchroneous processes inside TeXmacs
;; COPYRIGHT   : (C) 2026  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library process)
  (:use (utils library tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serial number and active process management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define process-serial 0)

(define process-active (make-ahash-table))

(tm-define (process-active? serial)
  (not (not (ahash-ref process-active serial))))

(tm-define (process-running? type)
  (in? type (append-map cdr (ahash-table->list process-active))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operating on document snippets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ((make-process-docrange serial fun tp1 tp2 next))
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
                  (proc2 (make-process-docrange serial fun tp2* tp2 next))
                  (proc1 (make-process-docrange serial fun tp1 tp1* proc2)))
             (proc1)))
          (else (next)))))

(tm-define (make-process-document serial fun t next)
  (when (tree->path t)
    (cond ((tm-func? t 'with)
           (make-process-document serial fun (tm-ref t :last) next))
          ((tm-func? t 'document)
           (let* ((tp1 (tree->tree-pointer (tm-ref t 0)))
                  (tp2 (if (== (tm-arity t) 1) tp1
                           (tree->tree-pointer (tm-ref t :last)))))
             (make-process-docrange serial fun tp1 tp2 next)))
          (else
            (with tp (tree->tree-pointer t)
              (make-process-docrange serial fun tp tp next))))))

(tm-define (make-process-documents serial fun ts next)
  (if (null? ts) next
      (with tail (make-process-documents serial fun (cdr ts) next)
        (make-process-document serial fun (car ts) tail))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creating top-level processes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define ((make-process-clean next))
  (with-innermost t 'identity
    (tree-remove-node! t 0)
    (tree-correct-upwards t))
  (next))

(tm-define (make-process fun . types)
  (with end (apply make-process-end types)
    (cond ((not (selection-active-any?))
           (make-process-document process-serial fun (buffer-tree) end))
          ((selection-active-small?)
           (with sel (selection-tree)
             (clipboard-cut "dummy")
             (insert-go-to `(identity ,sel) (cons 0 (path-start sel '())))
             (and-with t (tree-innermost 'identity)
               (with clean (make-process-clean end)
                 (make-process-document process-serial fun t clean)))))
          (else
            (with sels (selection-trees)
              (selection-cancel)
              (make-process-documents process-serial fun sels end))))))
