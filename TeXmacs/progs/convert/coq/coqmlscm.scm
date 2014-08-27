
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : coqmlscm.scm
;; DESCRIPTION : conversion of Coq XML commands to suitables strees.
;; COPYRIGHT   : (C) 2013 François Poulain, Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq coqmlscm)
  (:use (convert tools tmtable)
	(convert tools sxml)
	(convert tools xmltm)))

(define map map-in-order)

(define (coqml-error message . tree)
  (if (nlist-1? tree)
    `((error ,message))
    `((error ,(string-append message (object->string (car tree)))))))

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

(define (coqml-str env s)
  (import-string s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqml-get-attributes att att-l)
  (if (nsymbol? att) #f
    (filter
      nnull?
      (map (lambda (x)
             (if (!= (car x) att) '()
               (coqml-as-serial (environment) (cadr x)))) att-l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqml-string env a c)
  (cond
    ((== (length c) 0) '(""))
    ((and (== (length c) 1) (string? (first c)))
     `(,(import-string (string-replace  (first c) "\n" ""))))
    (else (coqml-error "bad string: " c))))

(define (coqml-int env a c)
  (if (and (== (length c) 1)
           (string? (first c))
           (string->number (first c)))
    `(,(string->number (first c)))
    (coqml-error "bad int")))

(define (coqml-bool env a c)
  (if (and (== (length c) 0)
           (or (== '("true")  (coqml-get-attributes 'val a))
               (== '("false") (coqml-get-attributes 'val a))))
    `(,(== (coqml-get-attributes 'val a) '("true")))
    (coqml-error "bad bool")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Containers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqml-list env a c)
  `(,(map (cut coqml-as-serial env <>) c)))

(define (coqml-pair env a c)
  (if (== (length c) 2)
    `((pair ,@(map (cut coqml-as-serial env <>) c)))
    (coqml-error "bad pair")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compounds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqml-unit env a c)
  (if (== (length c) 0)
    `((unit))
    (coqml-error "bad unit")))

(define (coqml-value env a c)
  (cond ((and (== (length c) 1) (== (coqml-get-attributes 'val a) '("good")))
         (with body (coqml-as-serial env (first c))
           `((value "good" ,body))))
        ((and (== (length c) 2) (== (coqml-get-attributes 'val a) '("fail")))
         (let ((loce (coqml-get-attributes 'loc_e a))
               (locs (coqml-get-attributes 'loc_s a))
               (stat (coqml-as-serial env (first c)))
               (msg  `((document ,@(cdr (second c))))))
           (with loc
             (if (and (list-1? locs) (list-1? loce)) `((,locs ,loce)) '())
             `((value "fail" ,@loc ,stat ,msg)))))
        (else
          (coqml-error "bad value: "(list 'c:value a c)))))

(define (coqml-call env a c)
  (if (and (== (length c) 1)
           (with val (coqml-get-attributes 'val a)
             (and (list-1? val) (string? (first val)))))
    (let* ((val  (first (coqml-get-attributes 'val a)))
           (body (coqml-as-serial env (first c))))
      `((call ,val ,body)))
    (coqml-error "bad call")))

(define (coqml-state-id env a c)
  (if (and (== (length c) 0)
           (with val (coqml-get-attributes 'val a)
             (and (list-1? val) (string? (first val))
                  (string->number (first val)))))
    (with val (string->number (first (coqml-get-attributes 'val a)))
      `((state-id ,val)))
    (coqml-error "bad state-id")))

(define (coqml-status env a c)
  (if (== (length c) 4)
    (with bodies (map (cut coqml-as-serial env <>) c)
      `((status ,@bodies)))
    (coqml-error "bad status")))

(define (bool? b)
  (or (== b #t) (== b #f)))

(define (coqml-option env a c)
  (with val (coqml-get-attributes 'val a)
    (if (and (list-1? val) (or (and (== val '("none")) (== (length c) 0))
                               (and (== val '("some")) (== (length c) 1))))
      (if (== val '("none")) `((option))
        (with body (coqml-as-serial env (first c))
          `((option ,body))))
      (coqml-error "bad option"))))

(define (coqml-option-value env a c)
  (if (and (== (length c) 1)
           (with val (coqml-get-attributes 'val a)
             (and (list-1? val) (string? (first val)))))
    (let* ((val  (first (coqml-get-attributes 'val a)))
           (body (coqml-as-serial env (first c))))
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
      (coqml-error "bad option-value type")))
    (coqml-error "bad option-value")))

(define (coqml-union env a c)
  (if (and (== (length c) 1)
           (or (== '("in_l") (coqml-get-attributes 'val a))
               (== '("in_r") (coqml-get-attributes 'val a))))
    (let* ((val  (first (coqml-get-attributes 'val a)))
           (body (coqml-as-serial env (first c))))
      `((union ,val ,body)))
    (coqml-error "bad union")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (coqml-pass env a c)
  (let ((l (coqml-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate
                a 'id (coqml-serial (htmltm-preserve-space? env) l))))))

(define (coqml-args env l)
  (append-map (lambda (x) (coqml env x)) l))

(define (coqml-args-serial env l)
  (coqml-serial env (coqml-args env l)))

(define (coqml env t)
  (sxml-dispatch (lambda (env t) (list (coqml-str env t)))
		 coqml-pass env t))

(tm-define coqml-as-serial
    (case-lambda
      ((t) (with env (environment)
              (ahash-set! env 'preserve-space? #t)
              (coqml-as-serial env t)))
      ((env t) (coqml-serial env (coqml env t)))))

(define handler coqml-handler)

(logic-dispatcher coqml-methods%
  ;; basic type
  (string       (handler :pre  coqml-string))
  (int          (handler :pre  coqml-int))
  (bool         (handler :elem coqml-bool))

  ;; containers
  (list         (handler :elem coqml-list))
  (pair         (handler :elem coqml-pair))

  ;; compounds
  (unit         (handler :elem coqml-unit))
  (union        (handler :elem coqml-union))
  (call         (handler :elem coqml-call))
  (state_id     (handler :elem coqml-state-id))
  (status       (handler :elem coqml-status))
  (value        (handler :elem coqml-value))
  (status       (handler :elem coqml-status))
  (value        (handler :pre  coqml-value))
  (option       (handler :elem coqml-option))
  (option_value (handler :elem coqml-option-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (coqml->stree s)
  (coqml-as-serial (coqml-parse s)))
