
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : logic-rules.scm
;; DESCRIPTION : defining rules for logical programs
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The macro 'logic-rules' can be used to add new rules to the global
;; logical program. In principle, this program is a list with instructions
;; which can be executed in order when solving the logical program.
;; However, for efficiency reasons, we use a recursive hashtable structure
;; which can extract a (usually small) subset of potentially relevant rules
;; for a given goal (by matching a greatest possible recursive prefix
;; without free variables on both sides).
;;
;; Our present solution could be further improved by continuing the matching
;; process even further than the greatest possible recursive prefix.
;; Also, the order of the logical program is not preserved by
;; the present rule extraction mechanism (rules with free variables
;; are moved to the end). This can be annoying if we ever want to
;; implement the cut instruction.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel logic logic-rules)
  (:use (kernel logic logic-bind) (kernel logic logic-unify)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ahash-list-ref table key)
  (let ((val (ahash-ref table key)))
    (if val val '())))

(define (ahash-list-add! table key val)
  "Add @value to list of values for @key in @table."
  (ahash-set! table key (cons val (ahash-list-ref table key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting new rules
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logic-rules-table (make-ahash-table))

(define (logic-add-rule-advance table symb todo rule)
  (if (not (ahash-ref table symb))
      (ahash-set! table symb (make-ahash-table)))
  (logic-add-rule-sub (ahash-ref table symb) todo rule))

(define (logic-add-rule-sub table todo rule)
  "Add @rule to @table with @todo yet to be 'read'."
  (ahash-list-add! table :all rule)
  (if (nnull? todo)
      (let ((next (car todo)))
	(cond ((npair? next)
	       (logic-add-rule-advance table next (cdr todo) rule))
	      ((free-variable? next)
	       (ahash-list-add! table :free rule))
	      (else (logic-add-rule-advance
		     table :down
		     (cons (car next) (cons (cdr next) (cdr todo)))
		     rule))))))

(define (logic-add-rule rule)
  "Add the rule @rule to the global database."
  (logic-add-rule-sub logic-rules-table (list (car rule)) rule))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting filtered rules which may possibly be applied for proving a goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logic-get-rules-advance table symb todo)
  (let ((next-table (ahash-ref table symb)))
    (if next-table (logic-get-rules-sub next-table todo) '())))

(define (logic-get-rules-sub table todo)
  "Get rules for @todo from @table."
  (if (null? todo)
      (ahash-list-ref table :all)
      (let ((next (car todo)))
	(cond ((npair? next)
	       (append (ahash-list-ref table :free)
		       (logic-get-rules-advance table next (cdr todo)))
	       ;; FIXME: (difficult with present algorithm) messes up order
	       )
	      ((free-variable? next)
	       (ahash-list-ref table :all))
	      (else (logic-get-rules-advance
		     table :down
		     (cons (car next) (cons (cdr next) (cdr todo)))))))))

(define-public (logic-get-rules goal)
  "Get all rules which may be applied to prove @goal."
  (reverse (logic-get-rules-sub logic-rules-table (list goal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (logic-rules-decls l extra)
  (cond ((npair? l) (noop))
	((and (pair? (car l)) (== (caar l) 'assume))
	 (logic-rules-decls (cdr l) (append (cdar l) extra)))
	(else
	 (logic-add-rule (cons (caar l) (append extra (cdar l))))
	 (logic-rules-decls (cdr l) extra))))

(define-public-macro (logic-rules . l)
  `(begin
     (logic-rules-decls ,(list 'quasiquote l) '())
     (display "")))

(define-public-macro (logic-rule . l)
  `(logic-rules ,l))
