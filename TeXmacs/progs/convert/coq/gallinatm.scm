
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gallinatm.scm
;; DESCRIPTION : conversion of Gallina trees to TeXmacs trees
;; COPYRIGHT   : (C) 2013 François Poulain and Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert coq gallinatm)
  (:use (convert tools tmtable)
	(convert tools sxml)
	(convert tools xmltm)))

(define map map-in-order)

(define (gallinatm-error message)
  `((with "color" "red" ,message)))

(define (gallinatm-string s)
  (utf8->cork s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gallinatm-get-attributes att att-l)
  (if (nsymbol? att) #f
    (filter nnull?
            (map (lambda (x)
                   (if (!= (car x) att) '()
                     (gallinatm-as-serial
                       (environment)
                       (gallinatm-string (cadr x))))) att-l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gallinatm-token env a c)
  (if (== (length c) 1)
    `((coq-token
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env (first c))))
    (gallinatm-error "bad token")))

(define (gallinatm-reference env a c)
  (if (== (length c) 0)
    `((coq-reference
        ,@(gallinatm-get-attributes 'name a)
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)))
    (gallinatm-error "bad reference")))

(define (gallinatm-require env a c)
  (if (== (length c) 1)
    `((coq-require
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env (first c))))
    (gallinatm-error "bad require")))

(define (gallinatm-apply env a c)
  (if (> (length c) 0)
    `((coq-apply
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,@(map (cut gallinatm-as-serial env <>) c)))
    (gallinatm-error "bad apply")))

(define (make-tex-args s)
  (cond ((< (string-length s) 2) "")
        ((string-starts? s "#") (string-append
                                  (string-take s 2)
                                  (make-tex-args (string-tail s 2))))
        ((string-starts? s "\\")  (make-tex-args (string-tail s 2)))
        (else (make-tex-args (string-tail s 1)))))

(define (downgrade-args arg s)
  (cond ((string? arg) (downgrade-args (string->number (string-tail arg 1)) s))
        ((< (string-length s) 2) s)
        ((string-starts? s "#")
         (string-append "#"
           (number->string
             (with n (string->number (substring s 1 2))
               (if (> n arg) (- n 1) n)))
           (downgrade-args arg (string-tail s 2))))
        ((string-starts? s "\\")
         (string-append
           (string-take s 2)
           (downgrade-args arg (string-tail s 2))))
        (else (string-append
                (string-take s 1)
                (downgrade-args arg (string-tail s 1))))))

(define (make-notation-sep l)
  (if (nlist>1? l) l
    (let* ((fst  (car l))
           (snd  (cadr l))
           (tl   (cddr l))
           (idx  (string-overlapping fst snd))
           (fst* (string-drop-right fst idx))
           (snd* (string-tail snd idx))
           (sep  (string-take snd idx))
           (arg  (string-tail fst* (- (string-length fst*) 2)))
           (chck (string-append
                  (substring fst* (- (string-length fst*) 2)
                                  (- (string-length fst*) 1))
                  (string-take snd* 1)))
           (snd* (string-tail snd* 2))
           (fst* (string-drop-right fst* 2))
           (sep* (string-append  "\\TMDoCoqRecNotationSep{" sep "}{" arg  "}")))
      (if (!= chck "##")
        (list (string-append "\\text{\\color{red}Recursive TeX notation: "
                             "unable to identify the separation pattern.}"))
        `(,fst* ,sep* ,@(make-notation-sep
                          (map (cut downgrade-args arg <>) `(,snd* ,@tl))))))))

(define (prepare-notation-recursive-pattern s)
  (if (not (string-occurs? " .. " s)) s
    (apply string-append (make-notation-sep (string-decompose s " .. ")))))

(define (gallinatm-operator env a c)
  (if (== (length c) 0)
    (let ((names  (gallinatm-get-attributes 'name a))
          (texes  (gallinatm-get-attributes 'format-tex a))
          (begins (gallinatm-get-attributes 'begin a))
          (ends   (gallinatm-get-attributes 'end a)))
      (if (and (list>0? names) (list>0? texes))
        (let* ((val  (car texes))
               (val  (prepare-notation-recursive-pattern val))
               (args (make-tex-args val))
               (ltxd (string-append "\\def\\dummy" args "{$" val "$}"))
               (m    (tree->stree (generic->texmacs ltxd "latex-snippet"))))
          (if (func? m 'assign 2) (set! names (cddr m)))))
      `((coq-operator ,@names ,@begins ,@ends)))
      (gallinatm-error "bad operator")))

(define (gallinatm-notation env a c)
  (if (== (length c) 1)
    `((coq-notation
        ,@(gallinatm-get-attributes 'name a)
        ,(gallinatm-as-serial env `(math ,(first c)))
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)))
    (gallinatm-error "bad notation")))

(define (gallinatm-constant env a c)
  (if (== (length c) 0)
    `((coq-constant
        ,@(gallinatm-get-attributes 'name a)
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)))
    (gallinatm-error "bad constant")))

(define (gallinatm-recurse env a c)
  (if (> (length c) 0)
    `((coq-intersperse-recurse-args
        ,@(map (cut gallinatm-as-serial env <>) c)))
    (gallinatm-error "bad recurse")))

(define (gallinatm-typed env a c)
  (if (> (length c) 1)
    (with args (map (cut gallinatm-as-serial env <>) c)
      (if (and (func? (cAr args) 'coq-constant 3) (== (cadr (cAr args)) "_"))
        `((coq-untyped ,@(cDr args)))
        `((coq-typed ,@args))))
    (gallinatm-error "bad typed")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vernacular commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gallinatm-check env a c)
  (if (== (length c) 1)
    `((coq-check
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env `(math ,(first c)))))
    (gallinatm-error "bad check")))

(define (gallinatm-definition env a c)
  (if (== (length c) 1)
    `((coq-definition
        ,@(gallinatm-get-attributes 'type a)
        ,@(gallinatm-get-attributes 'name a)
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env `(math ,(first c)))))
    (gallinatm-error "bad definition")))

(define (gallinatm-theorem env a c)
  (if (== (length c) 1)
    `((coq-theorem
        ,@(gallinatm-get-attributes 'type a)
        ,@(gallinatm-get-attributes 'name a)
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env `(math ,(first c)))))
    (gallinatm-error "bad theorem")))

(define (gallinatm-gallina env a c)
  (if (== (length c) 1)
    `((coq-gallina
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)
        ,(gallinatm-as-serial env (first c))))
    (gallinatm-error "bad gallina")))

(define (gallinatm-body env a c)
    `((document ,@(map (cut gallinatm-as-serial env <>) c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ltac commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gallinatm-ltac env a c)
  (if (== (length c) 1)
    `((coq-ltac
        ,@(map (cut gallinatm-as-serial env <>) c)
        ,@(gallinatm-get-attributes 'begin a)
        ,@(gallinatm-get-attributes 'end a)))
    (gallinatm-error "bad ltac")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main translation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gallinatm-drop env a c) '())

(define (gallinatm-pass env a c)
  (let ((l (gallinatm-args env c)))
    (if (and (null? l) (not (assoc 'id a))) '()
	(list (xmltm-label-decorate
                a 'id (gallinatm-serial (htmltm-preserve-space? env) l))))))

(define (gallinatm-args env l)
  (append-map (lambda (x) (gallinatm env x)) l))

(define (gallinatm-args-serial env l)
  (gallinatm-serial env (gallinatm-args env l)))

(define (gallinatm env t)
  (sxml-dispatch (lambda (env t) (list t)) gallinatm-pass env t))

(tm-define gallinatm-as-serial
    (case-lambda
      ((t) (with env (environment)
              (ahash-set! env 'preserve-space? #f)
              (gallinatm-as-serial env t)))
      ((env t) (gallinatm-serial env (gallinatm env t)))))

(define handler gallinatm-handler)

(logic-dispatcher gallinatm-methods%
  ;; Raw
  (token      (handler :raw    gallinatm-token))

  ;; Terms
  (apply      (handler :terms  gallinatm-apply))
  (operator   (handler :terms  gallinatm-operator))
  (notation   (handler :terms  gallinatm-notation))
  (reference  (handler :terms  gallinatm-reference))
  (require    (handler :terms  gallinatm-require))
  (constant   (handler :terms  gallinatm-constant))
  (recurse    (handler :terms  gallinatm-recurse))
  (typed      (handler :terms  gallinatm-typed))

  ;; Vernacular
  (check      (handler :vernac gallinatm-check))
  (definition (handler :vernac gallinatm-definition))
  (theorem    (handler :vernac gallinatm-theorem))

  ;; Tactics
  (ltac       (handler :raw    gallinatm-ltac))

  ;; Toplevel
  (body       (handler :toplvl gallinatm-body))
  (gallina    (handler :toplvl gallinatm-gallina)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (parse-gallina-snippet s)
  (gallinatm-parse s))

(tm-define (parse-gallina-document s)
  `(!file ,(gallinatm-parse (string-append "<body>" s "</body>"))))

(tm-define (gallina->texmacs gallina)
  (:type (-> stree stree))
  (:synopsis "Convert a parsed Gallina stree @t into a TeXmacs stree")
  (let* ((snippet? (not (func? gallina '!file 1)))
	 (body (if snippet? gallina (cadr gallina)))
	 (tm (filter (lambda (x) (!= x "\n")) (gallinatm-as-serial body))))
    ;; (display* "gallina->texmacs: tm:    " tm "\n\n")
    (if snippet? tm
	(let* ((aux (stm-unary-document tm))
	       (doc (tree->stree (tree-simplify (stree->tree aux))))
	       (body `(body ,doc))
	       (style `(style (tuple "generic" "docoq"))))
	  `(document ,body ,style)))))
