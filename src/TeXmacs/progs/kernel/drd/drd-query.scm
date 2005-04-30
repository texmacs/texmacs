
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : drd-query.scm
;; DESCRIPTION : execution of logical programs
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel drd drd-query)
  (:use (kernel drd drd-bind) (kernel drd drd-unify) (kernel drd drd-rules))
  (:export
    query ???))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructing closures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define logic-serial 0)

(define (logic-new)
  "Return a new free variable"
  (set! logic-serial (+ logic-serial 1))
  logic-serial)

(define (logic-closure expr bl)
  "Return the closure and bindings for @expr with bindings @bl"
  (cond ((or (null? expr) (npair? expr)) (cons expr bl))
	((free-variable? expr)
	 (let ((val (assoc-ref bl (cadr expr))))
	   (if val (cons (free-variable val) bl)
	       (let ((new-val (logic-new)))
		 (cons (free-variable new-val)
		       (acons (cadr expr) new-val bl))))))
	(else (let* ((head (logic-closure (car expr) bl))
		     (tail (logic-closure (cdr expr) (cdr head))))
		(cons (cons (car head) (car tail)) (cdr tail))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Solving logical programs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logic-remove-above bl serial)
  "Remove local variables (above @serial) from bindings @bl."
  (cond ((null? bl) bl)
	((and (number? (caar bl)) (> (caar bl) serial))
	 (logic-remove-above (cdr bl) serial))
	(else (cons (car bl) (logic-remove-above (cdr bl) serial))))) 

(define (logic-return bls2 serial)
  "Return from a rule with solutions @bls2 and locals above @serial."
  ;; returns list of expanded bindings
  (if (null? bls2) '()
      (let* ((bl (bind-expand (car bls2)))
	     (tail (logic-return (cdr bls2) serial)))
	(if bl
	    (cons (logic-remove-above bl serial) tail)
	    tail))))

(define (logic-try goal rule2 extra bl)
  "Try to prove @goal via @rule2 using extra rules @extra under bindings @bl."
  ;; returns list of expanded bindings
  ;; (for-each display (list "Try: " goal ", " rule2 "\n"))
  ;; (for-each display (list "Try: " goal ", " rule2 ", " bl "\n"))
  (let ((serial logic-serial)
	(rule (car (logic-closure rule2 '()))))
    (let* ((sols2 (unify (list goal) (list (car rule)) bl))
	   (sols (map bind-expand sols2)))
      (if (null? sols) sols
	  (logic-return (logic-prove-any (cdr rule) extra sols) serial)))))

(define (logic-exec goal prg extra bl)
  "Prove @goal from @prg using extra rules @extra under bindings @bl."
  ;; requires expanded bindings
  (if (null? prg) '()
      (let ((sols (logic-try goal (car prg) extra bl)))
	(if (== sols (list bl)) sols
	    (append sols (logic-exec goal (cdr prg) extra bl))))))

(define (logic-prove-any goals extra bls)
  "Prove @goals using extra rules @extra under any of the bindings in @bls."
  ;; requires list of expanded bindings
  (if (null? bls) '()
      (append (logic-prove goals extra (car bls))
	      (logic-prove-any goals extra (cdr bls)))))

(define (logic-prove goals extra bl)
  "Prove @goals using extra rules @extra under bindings @bl."
  ;; requires expanded bindings
  (if (null? goals) (list bl)
      (let* ((goal (bind-substitute (car goals) bl))
	     (prg (append extra (logic-get-rules goal))))
	(logic-prove-any (cdr goals) extra (logic-exec goal prg extra bl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (query goal . extra)
  "Prove @goal using extra rules @extra."
  (logic-prove (list goal) (map list extra) '()))

(define-macro (??? . l)
  (cons 'query (map (lambda (x) (list 'quote x)) l)))
