
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : calc-edit.scm
;; DESCRIPTION : low level linking and evaluation routines for spread sheets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic calc-edit)
  (:use (utils library tree)
	(utils library cursor)
	(utils plugins plugin-cmd)
	(convert tools tmconcat)
        (text tm-structure)
        (dynamic calc-drd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for naming cells
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (number->row r)
  (if (< r 27)
      (list->string (list (integer->char (+ r 96))))
      (string-append (number->row (quotient r 26))
                     (number->row (+ (modulo (- r 1) 26) 1)))))

(tm-define (row->number s)
  (with n (string-length s)
    (if (== n 1)
	(- (char->integer (car (string->list s))) 96)
	(+ (* 26 (row->number (substring s 0 (- n 1))))
	   (row->number (substring s (- n 1) n))))))

(tm-define (cell-ref-encode p)
  (with (r c) p
    (with s (string-append (number->row r) (number->string c))
      (tm->tree `(cell-ref ,s)))))

(tm-define (cell-ref-decode s)
  (if (string? s)
      (and-with i (list-find-index
		    (string->list s)
		    (lambda (c) (and (char>=? c #\0) (char<=? c #\9))))
	(list (row->number (substring s 0 i))
	      (string->number (substring s i (string-length s)))))
      (and (tree-is? s 'cell-ref)
	   (cell-ref-decode (tree->string (tree-ref s 0))))))

(define ((cell-ref-range-sub c1 c2) r)
  (map (lambda (c) (cell-ref-encode (list r c)))
       (.. c1 (+ c2 1))))

(tm-define (cell-ref-range x1 x2)
  (with (r1 c1) (cell-ref-decode x1)
    (with (r2 c2) (cell-ref-decode x2)
      (with rows (map (cell-ref-range-sub c1 c2) (.. r1 (+ r2 1)))
	(apply append rows)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spreadsheet evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define calc-input (make-ahash-table))
(tm-define calc-output (make-ahash-table))
(tm-define calc-invalid (make-ahash-table))
(tm-define calc-todo (make-ahash-table))

(tm-define (calc-updated? prefix t)
  (cond ((tree-atomic? t) #t)
        ((calc-ref-context? t)
         (let* ((var (texmacs->string (tree-ref t 0))))
           (not (ahash-ref calc-invalid var))))
        ((calc-range-context? t)
	 (with l (cell-ref-range (tree-ref t 0) (tree-ref t 1))
           (list-and (map (cut calc-updated? prefix <>) l))))
        (else (list-and (map (cut calc-updated? prefix <>)
                             (tree-children t))))))

(tm-define (calc-update-inputs prefix t)
  (cond ((tree-atomic? t) (noop))
        ((calc-inert-context? t)
         (let* ((var (texmacs->string (tree-ref t 0)))
                (old-val (ahash-ref calc-output var))
                (new-val (tree->stree (tree-ref t 1))))
           (when (!= new-val old-val)
             (ahash-set! calc-output var new-val)
             (ahash-set! calc-invalid var #t))))
        ((calc-toggle-context? t)
         (let* ((var (texmacs->string (tree-ref t 0)))
                (in-tree (tree-ref t 1))
                (in (tree->stree in-tree))
                (out (tree->stree (tree-ref t 2))))
           (when (or (!= (ahash-ref calc-input var) in)
                     (!= (ahash-ref calc-output var) out)
                     (not (calc-updated? prefix in-tree)))
             (ahash-set! calc-input var in)
             (ahash-set! calc-invalid var #t)
             (ahash-set! calc-todo var t))))
        (else (for-each (cut calc-update-inputs prefix <>)
                        (tree-children t)))))

(tm-define (calc-repeat-update-inputs prefix t)
  (with n (ahash-size calc-invalid)
    (calc-update-inputs prefix t)
    (when (!= n (ahash-size calc-invalid))
      (calc-repeat-update-inputs prefix t))))

(tm-define (calc-available? prefix t)
  (cond ((tree-atomic? t) #t)
        ((calc-ref-context? t)
         (let* ((var (texmacs->string (tree-ref t 0))))
           (not (ahash-ref calc-todo var))))
        ((calc-range-context? t)
	 (with l (cell-ref-range (tree-ref t 0) (tree-ref t 1))
           (list-and (map (cut calc-available? prefix <>) l))))
        (else (list-and (map (cut calc-available? prefix <>)
                             (tree-children t))))))

(tm-define (calc-substitute prefix t)
  (cond ((tree-atomic? t) t)
        ((calc-ref-context? t)
         (let* ((var (texmacs->string (tree-ref t 0))))
           (or (ahash-ref calc-output var) t)))
 	((calc-range-context? t)
	 (with l (cell-ref-range (tree-ref t 0) (tree-ref t 1))
	   (with cc `(concat ,@(list-intersperse l ","))
	     (calc-substitute prefix (tm->tree cc)))))
	(else (apply tree
                     (cons (tree-label t)
                           (map (cut calc-substitute prefix <>)
                                (tree-children t)))))))

(tm-define (calc-reevaluate-output prefix t)
  (when (calc-available? prefix (tree-ref t 1))
    ;;(display* "Reevaluate output " t "\n")
    ;;(display* "src= " (calc-substitute prefix (tree-ref t 1)) "\n")
    (let* ((var (texmacs->string (tree-ref t 0)))
           (in (tm->tree (calc-substitute prefix (tree-ref t 1))))
           (out (tree-ref t 2)))
      ;;(display* "var= " var "\n")
      ;;(display* "src= " src "\n")
      ;;(display* "dest= " dest "\n")
      (ahash-set! calc-output var dest)
      (ahash-remove! calc-todo var)
      (tree-set t 2 dest))))

(tm-define (calc-repeat-reevaluate-outputs prefix)
  (with n (ahash-size calc-todo)
    (for (p (ahash-table->list calc-todo))
      (calc-reevaluate-output prefix (cdr p)))
    (when (!= n (ahash-size calc-todo))
      (calc-repeat-reevaluate-outputs prefix))))

(tm-define (calc-reevaluate prefix t)
  (calc-repeat-update-inputs prefix t)
  (calc-repeat-reevaluate-outputs prefix)
  (set! calc-invalid (make-ahash-table))
  (set! calc-todo (make-ahash-table)))

(tm-define (calc-scheme)
  (calc-reevaluate "" (buffer-tree)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Communication with the plug-in
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-with-like t)
  (if (not (with-like? t)) t
      (remove-with-like (tree-ref t :last))))

(define (calc-normalize t)
  (when (and t
	     (tree-in? t '(calc-output cell-output))
	     (== (remove-with-like (tree-ref t 1))
		 (remove-with-like (tree-ref t 2))))
    (let* ((old-l (tree-label t))
	   (new-l (if (== old-l 'calc-output) 'calc-input 'cell-input)))
      (tree-assign-node t new-l)
      (when (cursor-inside? t)
        (tree-go-to t 1 :end)))))

(tm-define (calc-feed var in out)
  (let* ((lan (get-init "prog-scripts"))
	 (ses (get-init "prog-session")))
    (when (supports-scripts? lan)
      (tree-set! out '(script-busy))
      ;;(display* "Calc " var ", " lan ", " ses "\n")
      ;;(display* "Feed " in "\n")
      ;;(display* "Wait " out "\n")
      (with ptr (tree->tree-pointer out)
        (with ret (lambda (r)
                    ;;(display* "r= " r "\n")
                    (with check (tree-pointer->tree ptr)
                      (tree-pointer-detach ptr)
                      (when (== check out)
                        (tree-set! out r)
			(calc-normalize (tree-ref out :up))
                        (ahash-set! calc-output var (tm->stree r))
                        (ahash-remove! calc-todo var)
                        (delayed (calc-continue)))))
          (silent-feed* lan ses in ret '(:simplify-output)))))))

(tm-define (calc-continue-with prefix t)
  (let* ((var (texmacs->string (tree-ref t 0)))
         (in (tm->tree (calc-substitute prefix (tree-ref t 1))))
         (out (tree-ref t 2)))
    (calc-feed var in out)))

(tm-define (calc-continue-first l)
  (cond ((null? l)
         (set! calc-invalid (make-ahash-table))
         (set! calc-todo (make-ahash-table)))
        ((calc-available? "" (tree-ref (cdar l) 1))
         (calc-continue-with "" (cdar l)))
        (else
         (calc-continue-first (cdr l)))))

(tm-define (calc-continue)
  (with n (ahash-size calc-todo)
    (if (== n 0)
        (set! calc-invalid (make-ahash-table))
        (calc-continue-first (ahash-table->list calc-todo)))))

(tm-define (calc)
  (calc-repeat-update-inputs "" (buffer-tree))
  (calc-continue))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard interaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'calc-output))
  (tree-assign-node t 'calc-input)
  (tree-go-to t 1 :end))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'calc-input))
  (tree-assign-node t 'calc-output)
  (tree-go-to t 2 :end)
  (calc)
  (when (== (ahash-size calc-todo) 0)
    (calc-normalize t)))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-output))
  (tree-assign-node t 'cell-input)
  (tree-go-to t 1 :end))

(tm-define (alternate-toggle t)
  (:require (tree-is? t 'cell-input))
  (tree-assign-node t 'cell-output)
  (tree-go-to t 2 :end)
  (calc)
  (when (== (ahash-size calc-todo) 0)
    (calc-normalize t)))

(tm-define (kbd-enter t forwards?)
  (:require (calc-inert-context? t))
  (calc))

(tm-define (kbd-enter t forwards?)
  (:require (calc-toggle-context? t))
  (alternate-toggle t))
