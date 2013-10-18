
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scmcoqtopml.scm
;; DESCRIPTION : conversion of Coqtop Scheme commands to suitable XML trees.
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.coqml>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml scmcoqtopml)
  (:use (convert coqml coqtopmlout)))

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
  (and (func? t 'option 1) (or (integer? (cadr t))
                               (bool?    (cadr t))
                               (string?  (cadr t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Conversion routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (bool->xml-stree b)
  (with val (if b "true" "false")
    `(bool (@ (val ,val)))))

(define (pair->xml-stree p)
  `(pair ,@(map stree->coqtopml-stree (cdr p))))

(define (option->xml-stree t)
  (with o (cadr t)
    (with val
      (cond ((string? o)  "stringvalue")
            ((bool? o)    "boolvalue")
            ((integer? o) "intvalue"))
      `(option_value (@ (val ,val)) ,(stree->coqtopml-stree o)))))

(define (union->xml-stree u)
  (let ((val (cadr u))
        (child (stree->coqtopml-stree (caddr u))))
    `(union (@ (val ,val)) ,child)))

(define (call->xml-stree u)
  (let ((val (cadr u))
        (child (stree->coqtopml-stree (caddr u))))
    `(call (@ (val ,val)) ,child)))

(define (state-id->xml-stree u)
  (with val (number->string (cadr u))
    `(state_id (@ (val ,val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This implementation is partial. 
;; Only intended for outputing values toward Coq.
(define (stree->coqtopml-stree t)
  (with (node proc childs)
    (cond ((bool? t)     (list #f      bool->xml-stree         #f))
          ((pair? t)     (list #f      pair->xml-stree         #f))
          ((union? t)    (list #f      union->xml-stree        #f))
          ((call? t)     (list #f      call->xml-stree         #f))
          ((state-id? t) (list #f      state-id->xml-stree     #f))
          ((option? t)   (list #f      option->xml-stree       #f))
          ((integer? t)  (list 'int    number->string          #f))
          ((string? t)   (list 'string identity                #f))
          ((unit? t)     (list 'unit   #f                      #f))
          ((list? t)     (list 'list   stree->coqtopml-stree   #t))
          (else '(#f #f #f)))
    (cond ((and (not node) proc)        (proc t))
          ((and node (not proc))        `(,node))
          ((and node proc (not childs)) `(,node ,(proc t)))
          ((and node proc childs)       `(,node ,@(map proc t)))
          (else
            (display* "Cannot translate: " t "\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (stree->coqtopml t)
  (serialize-coqtopml
    `(*TOP* ,(stree->coqtopml-stree t))))
