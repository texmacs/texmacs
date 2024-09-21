
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scmcoqml.scm
;; DESCRIPTION : conversion of CoqML Scheme commands to suitable XML trees.
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq scmcoqml)
  (:use (convert coq coqmlout)))

(define map map-in-order)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bool? b)
  (or (== b #t) (== b #f)))

(define (pair? t)
  (func? t 'pair 2))

(define (call? t)
  (and (func? t 'call 2) (string? (cadr t))))

(define (state-id? t)
  (and (func? t 'state-id 1) (integer? (cadr t))))

(define (unit? t)
  (== t '(unit)))

(define (union? t)
  (and (func? t 'union 2) (or (== (cadr t) "in_l")
                              (== (cadr t) "in_r"))))

(define (option? t)
  (or (func? t 'option 1) (func? t 'option 0)))

(define (option-value? t)
  (and (func? t 'option-value 1) (or (integer? (cadr t))
                                     (bool?    (cadr t))
                                     (string?  (cadr t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bool->xml-stree b)
  (with val (if b "true" "false")
    `(bool (@ (val ,val)))))

(define (pair->xml-stree p)
  `(pair ,@(map stree->coqml-stree (cdr p))))

(define (option->xml-stree t)
  (if (== (length t) 2)
    (with child (stree->coqml-stree (cadr t))
      `(option (@ (val "some")) ,child))
    `(option (@ (val "none")))))

(define (option-value->xml-stree t)
  (with o (cadr t)
    (with val
      (cond ((string? o)  "stringvalue")
            ((bool? o)    "boolvalue")
            ;; note: this behavior with int values will be changed in coq
            ((integer? o) "intvalue"))
      (if (integer? o) (set! o `(option ,o)))
      `(option_value (@ (val ,val)) ,(stree->coqml-stree o)))))

(define (union->xml-stree u)
  (let ((val (cadr u))
        (child (stree->coqml-stree (caddr u))))
    `(union (@ (val ,val)) ,child)))

(define (call->xml-stree u)
  (let ((val (cadr u))
        (child (stree->coqml-stree (caddr u))))
    `(call (@ (val ,val)) ,child)))

(define (state-id->xml-stree u)
  (with val (number->string (cadr u))
    `(state_id (@ (val ,val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This implementation is partial. 
;; Only intended for outputing values toward Coq.
(define (stree->coqml-stree t)
  (with (node proc children)
    (cond ((bool? t)         (list #f      bool->xml-stree         #f))
          ((pair? t)         (list #f      pair->xml-stree         #f))
          ((union? t)        (list #f      union->xml-stree        #f))
          ((call? t)         (list #f      call->xml-stree         #f))
          ((state-id? t)     (list #f      state-id->xml-stree     #f))
          ((option? t)       (list #f      option->xml-stree       #f))
          ((option-value? t) (list #f      option-value->xml-stree #f))
          ((integer? t)      (list 'int    number->string          #f))
          ((string? t)       (list 'string identity                #f))
          ((unit? t)         (list 'unit   #f                      #f))
          ((list? t)         (list 'list   stree->coqml-stree      #t))
          (else '(#f #f #f)))
    (cond ((and (not node) proc)          (proc t))
          ((and node (not proc))          `(,node))
          ((and node proc (not children)) `(,node ,(proc t)))
          ((and node proc children)       `(,node ,@(map proc t)))
          (else
            `(error ,(string-append "stree->coqml: cannot translate "
                                    (object->string t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stree->coqml t)
  (serialize-coqml
    `(*TOP* ,(stree->coqml-stree t))))
