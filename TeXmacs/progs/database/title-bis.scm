
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : title-bis.scm
;; DESCRIPTION : Translation of metadata markup for typesetting purpose
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-bis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adding notes to the document title
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (remove-annotations t)
  (if (tm-func? t 'doc-note-ref 3)
      (remove-annotations (tm-ref t 2))
      t))

(define (replace-title-note c syms ids)
  (cond ((or (null? c) (null? syms)) c)
        ((tm-func? (car c) 'doc-note 1)
         (let* ((h (car c))
                (sym (car syms))
                (id (car ids))
                (note `(doc-note-text ,sym ,id ,(tm-ref h 0))))
           (cons note (replace-title-note (cdr c) (cdr syms) (cdr ids)))))
        (else (cons (car c) (replace-title-note (cdr c) syms ids)))))

(define (annotate-title c syms ids)
  (cond ((or (null? c) (null? syms)) c)
        ((tm-func? (car c) 'doc-title 1)
         (let* ((h (car c))
                (sym (car syms))
                (id (car ids))
                (tit `(doc-title (doc-note-ref ,sym ,id ,(tm-ref h 0)))))
           (annotate-title (cons tit (cdr c)) (cdr syms) (cdr ids))))
        (else (cons (car c) (annotate-title (cdr c) syms ids)))))

(define (make-title-note-sym nr)
  ;;`(number ,(number->string nr) "fnsymbol")
  `(number ,(number->string nr) "alpha"))

(define (make-title-note-id nr)
  ;; TODO: use hard-id of the doc-data tree as a prefix
  ;; otherwise, links will be ambiguous in case of multiple titles
  (string-append "title-note-" (number->string nr)))

(tm-define (add-title-notes t)
  (with l (select t '(doc-note))
    (if (null? l) l
        (let* ((c1  (tm-children t))
               (nrs (.. 1 (+ (length l) 1)))
               (syms (map make-title-note-sym nrs))
               (ids (map make-title-note-id nrs))
               (c2  (replace-title-note c1 syms ids))
               (c3  (annotate-title c2 syms ids)))
          `(,(tm-label t) ,@c3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main document data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (title->running-title t)
  `(doc-running-title ,(remove-annotations (tm-ref t 0))))

(tm-define (doc-data-hidden t)
  `(concat
     ,@(select t '(doc-note-text))
     ,@(let* ((h (map author-data-hidden (select t '(doc-author author-data))))
              (c (map tm-children h)))
         (apply append c))
     ,@(map title->running-title (select t '(doc-title)))
     ,@(select t '(doc-running-title))
     (doc-running-author
       (comma-separated
         ,@(select t '(doc-author author-data author-name 0))))
     ,@(select t '(doc-running-author))))

(tm-define (doc-data-main t)
  `(document
     ,@(select t '(doc-title))
     ,@(select t '(doc-subtitle))
     ,@(with authors (select t '(doc-author author-data))
         (cond ((null? authors)
                (list))
               ((list-1? authors)
                (list `(render-doc-author ,@authors)))
               (else
                (list `(render-doc-authors ,@authors)))))
     ,@(select t '(doc-date))
     ,@(select t '(doc-inactive))))

(tm-define (doc-data-sub t)
  `(surround
     (assign "the-doc-data" (quote ,t))
     (with "doc-note-nr" "0" ,(doc-data-hidden t))
     (document
       (doc-make-title
         (with "doc-note-nr" "0"
           ,(doc-data-main t))))))

(tm-define (doc-data t)
  (:secure #t)
  ;;(display* "t= " t "\n")
  ;;(display* "r= " (add-title-notes t) "\n")
  (doc-data-sub (add-title-notes t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (author-misc->note t)
  `(doc-author-note
     (tuple ,(tm-ref t 0))))

(tm-define (author-data-hidden t)
  `(concat
     ,@(map author-misc->note (select t '(author-misc)))))

(tm-define (doc-author-main t)
  `(document
     ,@(select t '(author-name))
     ,@(select t '(author-affiliation))
     ,@(select t '(author-email))
     ,@(select t '(author-homepage))))

(tm-define (author-data t)
  (:secure #t)
  `(with "the-author-data" (quote ,t)
     (doc-author-block
       ,(doc-author-main t))))

(tm-define (authors-data t)
  (:secure #t)
  `(with "the-author-data" (quote ,t)
     (doc-authors-block
       ,(doc-author-main t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-data-abstract t)
  (:secure #t)
  `(tuple
     (document
       ,@(select t '(doc-keywords))
       ,@(select t '(doc-msc)))))
