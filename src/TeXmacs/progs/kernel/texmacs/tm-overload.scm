
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-overload.scm
;; DESCRIPTION : Contextual overloading for functions and data
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-overload)
  (:export
    root? ovl-insert ovl-lookup ovl-apply
    new-define new-define-sub get-synopsis
    ovl-table define-overloaded))

(define (root? t)
  (== (reverse (tree-ip t)) (the-buffer-path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The creation of overloaded structures
;; Overloading is done according to three levels:
;;   1) Mode     (a predicate)
;;   2) Context  (a predicate with a path argument: an ancestor of the cursor)
;;   3) Pattern  (a pattern which has to be matched at lookup time)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ovl-pattern-create key data)
  (list (cons (car key) data)))

(define (ovl-pattern-insert ovl key data)
  (cond ((or (null? ovl) (== (caar ovl) (car key)))
	 (ovl-pattern-create key data))
	(else (cons (car ovl) (ovl-pattern-insert (cdr ovl) key data)))))

(define (ovl-context-create key data)
  (list (cons (car key) (ovl-pattern-create (cdr key) data))))

(define (ovl-context-insert ovl key data)
  (cond ((null? ovl) (ovl-context-create key data))
	((== (caar ovl) (car key))
	 (cons (cons (caar ovl)
		     (ovl-pattern-insert (cdar ovl) (cdr key) data))
	       (cdr ovl)))
	(else (cons (car ovl) (ovl-context-insert (cdr ovl) key data)))))

(define (ovl-mode-create key data)
  (list (cons (car key) (ovl-context-create (cdr key) data))))

(define (ovl-mode-insert ovl key data)
  (cond ((null? ovl) (ovl-mode-create key data))
	((== (caar ovl) (car key))
	 (cons (cons (caar ovl)
		     (ovl-context-insert (cdar ovl) (cdr key) data))
	       (cdr ovl)))
	(else (cons (car ovl) (ovl-mode-insert (cdr ovl) key data)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding the best method in an overloaded structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ovl-pattern-best match-1 match-2)
  (if (== (car match-1) :*) match-2 match-1))

(define (ovl-pattern-search ovl expr)
  (cond ((null? ovl) #f)
	((match? expr (caar ovl))
	 (with match (ovl-pattern-search (cdr ovl) expr)
	   (if match
	       (ovl-pattern-best (car ovl) match)
	       (car ovl))))
	(else (ovl-pattern-search (cdr ovl) expr))))

(define (ovl-pattern-lookup ovl expr)
  (with match (ovl-pattern-search ovl expr)
    (if match (cdr match) #f)))

(define (ovl-context-best match-1 match-2)
  (if (== (car match-1) root?) match-2 match-1))

(define (ovl-context-search ovl path)
  (cond ((null? ovl) #f)
	(((caar ovl) (tm-subtree path))
	 (with match (ovl-context-search (cdr ovl) path)
	   (if match
	       (ovl-context-best (car ovl) match)
	       (car ovl))))
	(else (ovl-context-search (cdr ovl) path))))

(define (ovl-context-lookup ovl expr path)
  (with match (ovl-context-search ovl path)
    (cond (match (ovl-pattern-lookup (cdr match) expr))
	  ((or (== path (the-buffer-path)) (null? path)) #f)
	  (else (ovl-context-lookup ovl expr (cDr path))))))

(define (ovl-mode-best match-1 match-2)
  (if (texmacs-submode? (car match-2) (car match-1)) match-2 match-1))

(define (ovl-mode-search ovl)
  (cond ((null? ovl) #f)
	(((caar ovl))
	 (with match (ovl-mode-search (cdr ovl))
	   (if match
	       (ovl-mode-best (car ovl) match)
	       (car ovl))))
	(else (ovl-mode-search (cdr ovl)))))

(define (ovl-mode-lookup ovl expr)
  (with match (ovl-mode-search ovl)
    (if match
	(ovl-context-lookup (cdr match) expr (cDr (the-path)))
	#f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ovl-insert ovl data mode-pred? context-pred? pattern)
  (ovl-mode-insert ovl (list mode-pred? context-pred? pattern) data))

(define (ovl-lookup ovl expr)
  (ovl-mode-lookup ovl expr))

(define (ovl-apply ovl args)
  (with fun (ovl-mode-lookup ovl args)
    (if fun
	(apply fun args)
	(texmacs-error "ovl-apply"
		       "Conditions of overloaded function cannot be met"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New style overloaded function declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define define-option-table (make-hash-table 100))

(define (new-define-sub head body)
  (if (or (not (pair? (car body)))
	  (not (keyword? (caar body))))
      (cons 'define (cons head body))
      (let ((decl (new-define-sub head (cdr body))))
	((hash-ref define-option-table (caar body)) (cdar body) decl))))

(define-macro (new-define head . body)
  (new-define-sub head body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Synopsis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define define-synopsis-table (make-ahash-table))

(define (define-option-synopsis opt decl)
  (ahash-set! define-synopsis-table (caadr decl) opt)
  decl)

(hash-set! define-option-table :synopsis define-option-synopsis)

(define (get-synopsis sym)
  (ahash-ref define-synopsis-table sym))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Overloading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ovl-table (make-ahash-table))
(define ovl-cur '(always? root? ':*))

(define (define-option-mode opt decl)
  (set! ovl-cur (list (car opt) (cadr ovl-cur) (caddr ovl-cur)))
  (cons 'define-overloaded (cdr decl)))

(define (define-option-context opt decl)
  (set! ovl-cur (list (car ovl-cur) (car opt) (caddr ovl-cur)))
  (cons 'define-overloaded (cdr decl)))

(define (define-option-pattern opt decl)
  (set! ovl-cur (list (car ovl-cur) (caddr ovl-cur) (list 'quote opt)))
  (cons 'define-overloaded (cdr decl)))

(hash-set! define-option-table :mode define-option-mode)
(hash-set! define-option-table :context define-option-context)
(hash-set! define-option-table :pattern define-option-pattern)

(define-macro (define-overloaded head . body)
  (let* ((var (car head))
	 (conds ovl-cur))
    (if (not (ahash-ref ovl-table var)) (ahash-set! ovl-table var '()))
    (set! ovl-cur '(always? root? ':*))
    `(begin
       (ahash-set! ovl-table ',var
		   (ovl-insert (ahash-ref ovl-table ',var)
			       (lambda ,(cdr head) ,@body)
			       ,@conds))
       (define (,var . args)
	 (ovl-apply (ahash-ref ovl-table ',var) args)))))
