
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqtopmlscm.scm
;; DESCRIPTION : conversion of Coqtop XML commands to suitables strees.
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.coqtopml>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coqml coqtopmlscm)
  (:use (convert tools tmtable)
	(convert tools sxml)
	(convert tools xmltm)))

(define map map-in-order)

(define (coqtop-error message)
  `((with "color" "red" ,message)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unescape-xml-string s)
  (set! s (string-replace s "&quot;" "\""))
  (set! s (string-replace s "&apos;" "'"))
  (set! s (string-replace s "&lt;"   "<"))
  (set! s (string-replace s "&gt;"   ">"))
  (set! s (string-replace s "&amp;"  "&"))
  (set! s (string-replace s "`"      "\xe2\x80\x98"))
  s)

(define (import-string s)
  (utf8->cork (unescape-xml-string s)))

(define (coqtop-str env s)
  (import-string s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtop-get-attributes att att-l)
  (if (nsymbol? att) #f
    (filter
      nnull?
      (map (lambda (x)
             (if (!= (car x) att) '()
               (coqtop-as-serial (environment) (cadr x)))) att-l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtop-string env a c)
  (if (and (== (length c) 1) (string? (first c)))
    `(,(coqtop-as-serial env (first c)))
    (coqtop-error "bad string")))

(define (coqtop-int env a c)
  (if (and (== (length c) 1)
           (string? (first c))
           (string->number (first c)))
    `(,(string->number (first c)))
    (coqtop-error "bad int")))

(define (coqtop-bool env a c)
  (if (and (== (length c) 0)
           (or (== '("true")  (coqtop-get-attributes 'val a))
               (== '("false") (coqtop-get-attributes 'val a))))
    `(,(== (coqtop-get-attributes 'val a) '("true")))
    (coqtop-error "bad bool")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtop-list env a c)
  `(,(map (cut coqtop-as-serial env <>) c)))

(define (coqtop-pair env a c)
  (if (== (length c) 2)
    `((pair ,@(map (cut coqtop-as-serial env <>) c)))
    (coqtop-error "bad pair")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compounds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtop-unit env a c)
  (if (== (length c) 0)
    `((unit))
    (coqtop-error "bad unit")))

(define (coqtop-call env a c)
  (if (and (== (length c) 1)
           (with val (coqtop-get-attributes 'val a)
             (and (list-1? val) (string? (first val)))))
    (let* ((val  (first (coqtop-get-attributes 'val a)))
           (body (coqtop-as-serial env (first c))))
      `((call ,val ,body)))
    (coqtop-error "bad call")))

(define (coqtop-state-id env a c)
  (if (and (== (length c) 0)
           (with val (coqtop-get-attributes 'val a)
             (and (list-1? val) (string? (first val))
                  (string->number (first val)))))
    (with val (string->number (first (coqtop-get-attributes 'val a)))
      `((state-id ,val)))
    (coqtop-error "bad state-id")))

(define (bool? b)
  (or (== b #t) (== b #f)))

(define (coqtop-option env a c)
  (with val (coqtop-get-attributes 'val a)
    (if (and (list-1? val) (or (and (== val '("none")) (== (length c) 0))
                               (and (== val '("some")) (== (length c) 1))))
      (if (== val '("none")) `((option))
        (with body (coqtop-as-serial env (first c))
          `((option ,body))))
      (coqtop-error "bad option"))))

(define (coqtop-option-value env a c)
  (if (and (== (length c) 1)
           (with val (coqtop-get-attributes 'val a)
             (and (list-1? val) (string? (first val)))))
    (let* ((val  (first (coqtop-get-attributes 'val a)))
           (body (coqtop-as-serial env (first c))))
              ;; note: this behavior with int values will be changed in coq
      (if (or (and (== val "intvalue")
                   (or (func? body 'option 0)
                       (and (func? body 'option 1) (integer? (cAr body)))))
              ;; this is the future expected behavior
              (and (== val "intvalue")    (integer? body))
              (and (== val "stringvalue") (string? body))
              (and (== val "boolvalue")   (bool? body)))
        (begin
          (if (and (func? body 'option) (integer? (cAr body)))
            (set! body (cAr body)))
          `((option-value ,body)))
      (coqtop-error "bad option-value type")))
    (coqtop-error "bad option-value")))

(define (coqtop-union env a c)
  (if (and (== (length c) 1)
           (or (== '("in_l") (coqtop-get-attributes 'val a))
               (== '("in_r") (coqtop-get-attributes 'val a))))
    (let* ((val  (first (coqtop-get-attributes 'val a)))
           (body (coqtop-as-serial env (first c))))
      `((union ,val ,body)))
    (coqtop-error "bad union")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqtop-drop env a c) '())

(define (coqtop-pass env a c)
  (let ((l (coqtop-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate
                a 'id (coqtop-serial (htmltm-preserve-space? env) l))))))

(define (coqtop-args env l)
  (append-map (lambda (x) (coqtop env x)) l))

(define (coqtop-args-serial env l)
  (coqtop-serial env (coqtop-args env l)))

(define (coqtop env t)
  (sxml-dispatch (lambda (env t) (list (coqtop-str env t)))
		 coqtop-pass env t))

(tm-define coqtop-as-serial
    (case-lambda
      ((t) (with env (environment)
              (ahash-set! env 'preserve-space? #t)
              (coqtop-as-serial env t)))
      ((env t) (coqtop-serial env (coqtop env t)))))

(define handler coqtop-handler)

(logic-dispatcher coqtop-methods%
  ;; basic type
  (string       (handler :pre  coqtop-string))
  (int          (handler :pre  coqtop-int))
  (bool         (handler :pre  coqtop-bool))

  ;; containers
  (list         (handler :elem coqtop-list))
  (pair         (handler :elem coqtop-pair))

  ;; compounds
  (unit         (handler :elem coqtop-unit))
  (union        (handler :elem coqtop-union))
  (call         (handler :elem coqtop-call))
  (state_id     (handler :elem coqtop-state-id))
  (option       (handler :elem coqtop-option))
  (option_value (handler :elem coqtop-option-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (coqtopml->stree s)
  (coqtop-as-serial (coqtopml-parse s)))
