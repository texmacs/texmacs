
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : smart-table.scm
;; DESCRIPTION : smart tables
;; COPYRIGHT   : (C) 2014  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library smart-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining smart tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-cond c)
  (cond ((== (car c) :mode) (list (cadr c)))
	((== (car c) :require) (cadr c))
	(else (texmacs-error "smart-table" "Bad option ~S" c))))

(define (smart-table-insert-one t conds x)
  (if (not (list-2? x))
      (texmacs-error "smart-table" "Invalid table entry in: ~S" x))
  (with (key im) x
    (if (func? key 'unquote 1)
	(set! key (cadr key))
	(set! key `(quote ,key)))
    (if (func? im 'unquote 1)
	(set! im (cadr im))
	(set! im `(quote ,im)))
    `(let* ((smart-key ,key)
	    (smart-im ,im)
	    (smart-former (ahash-ref ,t smart-key)))
       (if (not smart-former)
	   (set! smart-former (lambda () #f)))
       (ahash-set! ,t smart-key
		   (lambda ()
		     (if (and ,@(map build-cond conds))
			 smart-im
			 (smart-former)))))))

(define (smart-table-insert t conds l)
  (cond ((null? l) '())
	((and (list-2? (car l)) (keyword? (caar l)))
	 (smart-table-insert t (rcons conds (car l)) (cdr l)))
	(else (map (lambda (x) (smart-table-insert-one t conds x)) l))))

(tm-define-macro (smart-table t . l)
  (:synopsis "Define a smart table @t with entries @l")
  `(begin
     (tm-define-once ,t (make-ahash-table))
     ,@(smart-table-insert t '() l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for smart tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (smart-ref t key)
  (:synopsis "Get the entry @key in the table @t")
  (let ((fun (ahash-ref t key)))
    (and fun (fun))))
