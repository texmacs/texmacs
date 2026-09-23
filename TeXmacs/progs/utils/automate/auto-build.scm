
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : auto-build.scm
;; DESCRIPTION : Editing markup for automated document generation
;; COPYRIGHT   : (C) 2015  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils automate auto-build)
  (:use (utils automate auto-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Consuming the answer in the right format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (consume as t)
  (cond ((== as :any)
         (cond ((func? t :block) `(document ,@(cdr t)))
               ((func? t :inline) (apply tmconcat (cdr t)))
               ((func? t 'document) t)
               ((func? t 'concat) t)
               (else t)))
        ((== as :block)
         (cond ((func? t :block) t)
               ((func? t :inline) (cons :block (apply tmconcat (cdr t))))
               ((func? t 'document) (cons :block (cdr t)))
               ((func? t 'inline) (list :block t))
               (else (list :block t))))
        ((== as :inline)
         (cond ((func? t :block) (cons :inline `(document ,@(cdr t))))
               ((func? t :inline) t)
               ((func? t 'document) (cons :inline `(document ,@(cdr t))))
               ((func? t 'concat) (cons :inline (cdr t)))
               (else (list :inline t))))
        ((== as 'document)
         (cond ((func? t :block) `(document ,@(cdr t)))
               ((func? t :inline) `(document ,(apply tmconcat (cdr t))))
               ((func? t 'document) t)
               ((func? t 'inline) `(document ,t))
               (else `(document ,t))))
        ((== as 'concat)
         (cond ((func? t :block) `(document ,@(cdr t)))
               ((func? t :inline) (apply tmconcat (cdr t)))
               ((func? t 'document) t)
               ((func? t 'concat) t)
               (else t)))
        (else (texmacs-error "consume" "~S incorrect format" as))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scripting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define auto-safe-mode? #f)
(define current-variables (list))

(define (build-variable t)
  (cond ((string? t) (string->symbol t))
        ((tm-func? t 'implied-scm 1) (build-variable (tm-ref t 0)))
        (else #f)))

(define (build-scheme* t)
  (let* ((expr (string->object (texmacs->code t "iso-8859-1")))
         (decls (map (lambda (p) (list (car p) (list 'quote (cdr p))))
                     current-variables))
         (expr* `(let ,decls ,expr)))
    ;;(display* "Evaluating " auto-safe-mode? " " (secure? expr*) " " expr* "\n")
    (and (or auto-safe-mode? (secure? expr*))
         (eval expr*))))

(define (build-scheme t)
  (with r (build-scheme* t)
    ;;(display* t " -> " r ", " (if r "yes" "no") "\n")
    r))

(define (build-extern t)
  (or (and (tm-func? t 'implied-scm 1)
           (build-scheme (tm-ref t 0)))))

(define (build-condition c)
  (build-extern c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual builders
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-compound tag . l)
  (let* ((bl (map-in-order build l))
         (r (map (cut consume :any <>) bl)))
    `(,tag ,@r)))

(define (build-document . l)
  (let* ((bl (map-in-order build l))
         (cl (map (cut consume 'document <>) bl))
         (r (append-map cdr cl)))
    `(document ,@r)))

(define (build-concat . l)
  (let* ((bl (map-in-order build l))
         (cl (map (cut consume 'concat <>) bl))
         (cc (lambda (x) (if (tm-func? x 'concat) (cdr x) (list x))))
         (r (append-map cc cl)))
    (apply tmconcat r)))

(define (build-if as c body)
  (if (build-condition c)
      (build* as body)
      (list as)))

(define (build-if-else as c body1 body2)
  (if (build-condition c)
      (build* as body1)
      (build* as body2)))

(define (build-with* as var val body)
  (with v (build-variable var)
    (if (not v) (list as)
        (with-global current-variables (acons v val current-variables)
          (build* as body)))))

(define (build-for as var vals body)
  (let* ((l (build-extern vals))
         (bl (map (cut build-with* as var <> body) (or l (list))))
         (r (append-map cdr bl)))
    (cons as r)))

(define (build-assign as var val)
  (let* ((var* (build-variable var))
         (val* (build-extern val)))
    (when var*
      (set! current-variables (assoc-set! current-variables var* val*)))
    (list as)))

(define (build-intersperse as sep body)
  (let* ((s (cdr (build* as sep)))
         (l (map list (cdr (build* as body))))
         (r (apply append (list-intersperse l s))))
    (cons as r)))

(define (build-tag as tag . args)
  (let* ((f (cadr (build* :inline tag)))
         (a (map (lambda (x) (cadr (build* :inline x))) (cDr args)))
         (b (build* as (cAr args)))
         (l (cond ((== as :block) `(document ,@(cdr b)))
                  ((== as :inline) (apply tmconcat (cdr b)))
                  (else b)))
         (r (if (string? f) `(,(string->symbol f) ,@a ,l) `(compound ,f ,@a ,l))))
    (cons as (list r))))

(define (build-output-string expr)
  (let* ((val (build-extern expr))
         (s (if (string? val) val (object->string val))))
    s))

(define (build-output-tm as expr)
  (with val (build-extern expr)
    (if (tm? val)
	(consume as (tm->stree val))
	(list as))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build t)
  ;;(display* "Building " t "\n")
  (cond ((tm-atomic? t) t)
        ((tm-func? t 'document)
         (apply build-document (tm-children t)))
        ((tm-func? t 'concat)
         (apply build-concat (tm-children t)))
        ((tm-func? t 'block-if 2)
         (apply build-if :block (tm-children t)))
        ((tm-func? t 'block-if-else 3)
         (apply build-if-else :block (tm-children t)))
        ((tm-func? t 'block-for 3)
         (apply build-for :block (tm-children t)))
        ((tm-func? t 'block-assign 2)
         (apply build-assign :block (tm-children t)))
        ((tm-func? t 'block-intersperse 2)
         (apply build-intersperse :block (tm-children t)))
        ((tm-func? t 'block-texmacs-tag)
         (apply build-tag :block (tm-children t)))
        ((tm-func? t 'inline-if 2)
         (apply build-if :inline (tm-children t)))
        ((tm-func? t 'inline-if-else 3)
         (apply build-if-else :inline (tm-children t)))
        ((tm-func? t 'inline-for 3)
         (apply build-for :inline (tm-children t)))
        ((tm-func? t 'inline-intersperse 2)
         (apply build-intersperse :inline (tm-children t)))
        ((tm-func? t 'inline-texmacs-tag)
         (apply build-tag :inline (tm-children t)))
        ((tm-func? t 'inline-assign 2)
         (apply build-assign :inline (tm-children t)))
        ((tm-func? t 'output-string 1)
         (apply build-output-string (tm-children t)))
        ((tm-func? t 'inline-output 1)
         (apply build-output-tm :inline (tm-children t)))
        ((tm-func? t 'block-output 1)
         (apply build-output-tm :block (tm-children t)))
        (else (apply build-compound (cons (tm-label t) (tm-children t))))))

(define (build* as t)
  (consume as (build t)))

(tm-define (build-body t . initial)
  (set! current-variables initial)
  (build* 'document (tm->stree t)))

(tm-define (build-body* t)
  (if (tm-func? t 'body 1)
      `(body ,(build* 'document (tm-ref t 0)))
      t))

(tm-define (build-document t . initial)
  (set! current-variables initial)
  (with doc (tm->stree t)
    (if (not (pair? doc)) doc
        (cons (car doc)
              (map build-body* (cdr doc))))))
