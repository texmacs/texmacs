
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : markup-funcs.scm
;; DESCRIPTION : additional rendering macros written in scheme
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc markup-funcs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TeXmacs version and release
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (texmacs-version-release* t)
  (:secure #t)
  (texmacs-version-release (tree->string t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-map fun to)
  (:secure #t)
  (with (op . args) (tree->list to)
    (with f (lambda (x) (list 'compound fun x))
      (list 'quote (cons 'tuple (map f args))))))

(tm-define (ext-concat-tuple tup sep fin)
  (:secure #t)
  (with (op . l) (tree->list tup)
    (cond ((null? l) "")
	  ((null? (cdr l)) (car l))
	  (else `(concat ,(car l)
			 ,@(map (lambda (x) (list 'concat sep x)) (cDdr l))
			 ,(if (tm-equal? fin '(uninit)) sep fin)
			 ,(cAr l))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rewrite-select pat)
  (if (atomic-tree? pat)
      (with s (tree->string pat)
	(if (not (string-starts? s "("))
	    (string->object s)
	    s))
      (with (op . r) (tree->list pat)
	(cond ((== op 'pat-any) :%1)
	      ((== op 'pat-any-repeat) :*)
	      ((== op 'pat-or) (cons :or (map rewrite-select r)))
	      ((== op 'pat-and) (cons :and (map rewrite-select r)))
	      ((== op 'pat-group) (cons :group (map rewrite-select r)))
	      ((== op 'pat-and-not) (cons :and-not (map rewrite-select r)))
	      (else #f)))))

(tm-define (ext-select body args)
  (:secure #t)
  (with (op body2 . pat) (tree->list args)
    (list 'quote (cons 'tuple (select body (map rewrite-select pat))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (decode-tm-arg a)
  (with s (and (tree? a) (tree-atomic? a) (tree->string a))
    (cond ((not s) :same)
          ((and (string->number s) (integer? (string->number s)))
           (string->number s))
          ((== s "up") :up)
          ((== s "next") :next)
          ((== s "previous") :previous)
          ((== s "first") :first)
          ((== s "last") :last)
          (else :same))))

(tm-define (ext-tm-ref body args)
  (:secure #t)
  (let* ((a (map decode-tm-arg (tm-children args)))
         (r (apply tm-ref (cons body a))))
    (if (tree? r) r "false")))

(tm-define (ext-tm-arity body args)
  (:secure #t)
  (let* ((a (map decode-tm-arg (tm-children args)))
         (r (apply tm-ref (cons body a))))
    (if (tree? r) (number->string (tree-arity r)) "false")))

(tm-define (ext-tm-index body args)
  (:secure #t)
  (let* ((a (map decode-tm-arg (tm-children args)))
         (r (apply tm-ref (cons body a))))
    (if (tree? r) (number->string (tree-index r)) "false")))

(tm-define (ext-tm-last? body args)
  (:secure #t)
  (let* ((a (map decode-tm-arg (tm-children args)))
         (r (apply tm-ref (cons body a))))
    (if (and (tree? r) (tree-up r)
             (== (tree-index r) (- (tree-arity (tree-up r)) 1)))
        "true" "false")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language suffix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (ext-language-suffix)
  (:secure #t)
  (with s (language-to-locale (get-output-language))
    (if (>= (string-length s) 2) (substring s 0 2) "en")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applying a macro recursively to paragraphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ext-apply-on-paragraphs-root #f)

(define (ext-mark r t var rew)
  (let* ((rp (tree->path r))
         (tp (tree->path t)))
    (if (and rp tp (list-starts? tp rp))
        (let* ((p (list-drop tp (length rp)))
               (ss (map number->string p)))
          `(mark (arg ,var ,@ss) ,rew))
        rew)))

(define (ext-apply-on-paragraphs-sub macro-name t)
  (cond ((tree-is? t 'document)
         (with fun (cut ext-apply-on-paragraphs macro-name <>)
           `(document ,@(map fun (tm-children t)))))
        ((tree-multi-line? t)
         (with fun (cut ext-apply-on-paragraphs-sub macro-name <>)
           (with rew (cons (tm-label t) (map fun (tm-children t)))
             (ext-mark ext-apply-on-paragraphs-root t "body" rew))))
        (else t)))

(tm-define (ext-apply-on-paragraphs macro-name t)
  (:secure #t)
  (set! ext-apply-on-paragraphs-root t)
  (cond ((tree-multi-line? t)
         (ext-apply-on-paragraphs-sub macro-name t))
        ((tree-atomic? macro-name)
         `(,(string->symbol (tree->string macro-name)) ,t))
        (else t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line numbering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ext-numbered-root #f)

(define (ext-numbered-sub t)
  (cond ((tree-is? t 'document)
         `(document ,@(map ext-numbered-line (tm-children t))))
        ((tree-multi-line? t)
         (with rew (cons (tm-label t) (map ext-numbered-sub (tm-children t)))
           (ext-mark ext-numbered-root t "body" rew)))
        (else t)))

(define (ext-numbered-line t)
  (if (tree-multi-line? t)
      (ext-numbered-sub t)
      `(numbered-line ,t)))

(tm-define (ext-numbered body)
  (:secure #t)
  (set! ext-numbered-root body)
  (if (tm-func? body 'document)
      `(numbered-block (document ,@(map ext-numbered-line (tm-children body))))
      body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fancy listings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ext-listing-row body row)
  `(row (cell (with "color" "dark grey" "prog-language" "verbatim"
                    ,(number->string (+ row 1))))
        (cell (document ,(tm-ref body row)))))

(tm-define (ext-listing body)
  (:secure #t)
  (if (tm-func? body 'document)
      `(tformat
         (twith "table-width" "1par")
         (twith "table-hmode" "exact")
         (twith "table-hyphen" "y")
         (cwith "1" "-1" "1" "1" "cell-halign" "r")
         (cwith "1" "-1" "1" "1" "cell-lsep" "0em")
         (cwith "1" "-1" "2" "2" "cell-halign" "l")
         (cwith "1" "-1" "2" "2" "cell-rsep" "0em")
         (cwith "1" "-1" "2" "2" "cell-hpart" "1")
         (cwith "1" "-1" "2" "2" "cell-hyphen" "t")
         (cwith "1" "-1" "1" "-1" "cell-background"
                (if (equal (mod (value "cell-row-nr") "2") "0") "#f4f4ff" ""))
         (table ,@(map (lambda (row) (ext-listing-row body row))
                       (.. 0 (tm-arity body)))))
      body))
