
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : title-markup.scm
;; DESCRIPTION : Translation of metadata markup for typesetting purpose
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-markup)
  (:use (database title-transform)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collect notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-note note h style)
  (with n (tm->stree note)
    (when (not (ahash-ref h n))
      (let* ((nr (+ (ahash-size h) 1))
             (sym (if (zero? (string-length style))
                      '(phantom a)
                      `(number ,(number->string nr) ,style)))
             (id (string-append "title-note-" (number->string nr))))
        ;; TODO: use hard-id of the doc-data tree as a prefix
        ;; otherwise, links will be ambiguous in case of multiple titles
        (ahash-set! h n (list note sym id))))))

(define (retrieve-note note h)
  (with n (tm->stree note)
    (with val (ahash-ref h n)
      (if (eq? (car val) note)
          (list (cons n val))
          (list)))))

(tm-define (collect-notes t style tags)
  (let* ((l (apply append (map (cut select t <>) tags)))
         (h (make-ahash-table)))
    (for-each (cut insert-note <> h style) l)
    (append-map (cut retrieve-note <> h) l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying notes into footnotes and references
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remove-notes c)
  (cond ((null? c) c)
        ((tm-func? (car c) 'doc-note 1) (remove-notes (cdr c)))
        ((tm-func? (car c) 'author-note 1) (remove-notes (cdr c)))
        ((tm-in? (car c) '(doc-author author-data))
         (let* ((f (car c))
                (h `(,(tm-label f) ,@(remove-notes (tm-children f))))
                (t (remove-notes (cdr c))))
           (cons h t)))
        (else (cons (car c) (remove-notes (cdr c))))))

(tm-define (retain-title-notes notes)
  (cond ((null? notes) notes)
        ((tm-is? (caar notes) 'doc-note)
         (cons (car notes) (retain-title-notes (cdr notes))))
        (else (retain-title-notes (cdr notes)))))

(tm-define (find-note sel notes)
  (cond ((null? notes) (list))
        ((== (caar notes) sel)
         (list (car notes)))
        (else (find-note sel (cdr notes)))))

(tm-define (add-annotations c notes)
  (if (null? notes) c
      (with (n note sym id) (cAr notes)
        (if (eqv? (car sym) 'phantom) c
          (with c2 (add-annotations c (cDr notes))
            `(doc-note-ref ,sym (noteref-sep) ,id ,c2))))))

(define (annotate c notes)
  (cond ((tm-func? c 'doc-title 1)
         (with new-notes (retain-title-notes notes)
           `(doc-title ,(add-annotations (tm-ref c 0) new-notes))))
        ((tm-func? c 'author-name 1)
         `(author-name ,(add-annotations (tm-ref c 0) notes)))
        ((tm-func? c 'doc-author 1)
         (let* ((sels (map tm->stree (select c '(author-data author-note))))
                (new-notes (append-map (cut find-note <> notes) sels)))
           (with ann (cut annotate <> new-notes)
             `(doc-author ,@(map ann (tm-children c))))))
        ((tm-is? c 'author-data)
         (with ann (cut annotate <> notes)
           `(author-data ,@(map ann (tm-children c)))))
        (else c)))

(tm-define (make-note l)
  (with (n note sym id) l
    `(doc-footnote-text ,sym ,id ,(tm-ref note 0))))

(tm-define (add-notes t)
  (let* ((tags '((doc-note) (doc-author author-data author-note)))
         (notes (collect-notes t "fnsymbol" tags)))
    (if (null? notes) t
        (let* ((c1 (tm-children t))
               (c2 (map (cut annotate <> notes) c1))
               (c3 (remove-notes c2))
               (c4 (map make-note notes)))
          `(,(tm-label t) ,@c3 ,@c4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main document data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (remove-annotations t)
  (cond ((tm-func? t 'doc-note-ref 4) (remove-annotations (tm-ref t 3)))
        ((tm-func? t 'new-line) " ")
        ((tm-func? t 'next-line) " ")
        ((tm-func? t 'concat)
         `(concat ,@(map remove-annotations (tm-children t))))
        (else t)))

(tm-define (title->running-title t)
  `(doc-running-title ,(remove-annotations (tm-ref t 0))))

(tm-define (doc-data-hidden t)
  (with names (select t '(doc-author author-data
                          (:or author-name author-name-affiliation) 0))
    `(concat
       ,@(select t '(doc-footnote-text))
       ,@(map title->running-title (select t '(doc-title)))
       ,@(select t '(doc-running-title))
       (doc-running-author
        (comma-separated
         ,@(map remove-annotations names)))
       ,@(select t '(doc-running-author)))))

(tm-define (doc-data-main t)
  `(document
     ,@(select t '(doc-title))
     ,@(select t '(doc-subtitle))
     ,@(with authors (select t '(doc-author))
         (if (<= (length authors) 1) authors
             (list `(doc-authors ,@authors))))
     ,@(select t '(doc-date))
     ,@(select t '(doc-misc))
     ,@(select t '(doc-inactive))))

(tm-define (xdoc-data-sub t)
  `(surround
     ,(doc-data-hidden t) (concat)
     (document
       (doc-make-title
         ,(doc-data-main t)))))

(tm-define (doc-data-sub t)
  ;;(display* "--------------------------------------------------\n")
  ;;(display* "source= " t "\n")
  ;;(display* "--------------------------------------------------\n")
  ;;(display* "hidden= " (doc-data-hidden t) "\n")
  ;;(display* "--------------------------------------------------\n")
  ;;(display* "main  = " (doc-data-main t) "\n")
  `(doc-make-rich-title
    ,(doc-data-hidden t)
    (document ,(doc-data-main t))))

(tm-define (doc-data-impl t opts)
  ;;(display* "t1= " t "\n")
  (cond ((in? "abbreviate-authors" opts)
         (set! t (abbreviate-authors t))))
  (cond ((in? "cluster-all" opts)
         (set! t (single-author-list t)))
          ((in? "cluster-by-affiliation" opts)
           (set! t (factor-affiliation t))))           
  ;;(display* "t2= " t "\n")
  (set! t (add-notes t))
  ;;(display* "t3= " t "\n")
  (cond ((in? "abbreviate-authors" opts)
         (set! t (abbreviate-authors-bis t))))
  ;;(display* "t4= " t "\n")
  (set! t (doc-data-sub t))
  ;;(display* "t5= " t "\n")
  t)

(tm-define (doc-data t xopts)
  (:secure #t)
  (let* ((opts1 (select t '(doc-title-options :%1)))
         (opts2 (select xopts '(:%1)))
         (opts  (map tree->stree (append opts1 opts2))))
    (doc-data-impl t opts)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AMS style titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plain-footnote t) `(render-plain-footnote ,t))

(define (ams-author-data t)
  `(render-plain-footnote
    (document ,@(select t '(author-affiliation))
              ,@(select t '(author-email))
              ,@(select t '(author-homepage))
              ,@(select t '(author-note))
              ,@(select t '(author-misc)))))

(define (ams-doc-data-hidden t)
  `(concat
     ,@(map plain-footnote (select t '(doc-date)))
     ,@(map plain-footnote (select t '(doc-note)))
     ,@(cdr (doc-data-hidden t))
     ,@(map ams-author-data (select t '(doc-author author-data)))))

(define (ams-doc-data-main t)
  `(document
     ,@(select t '(doc-title))
     ,@(select t '(doc-subtitle))
     ,@(with authors (select t '(doc-author author-data author-name))
         (cond ((null? authors) `())
               ((null? (cdr authors)) `((author-name ,@authors)))
               ((null? (cddr authors))
                `((author-name (concat ,(car authors) " " (localize "and")
                                       " " ,(cadr authors)))))
               (else
                 (with l (list-intersperse authors ", ")
                   `((author-name (concat ,@(cDr l) (localize "and")
                                          " " ,(cAr l))))))))
     ,@(select t '(doc-misc))
     ,@(select t '(doc-inactive))))

(tm-define (doc-data-impl t opts)
  (:require (in? "ams-title" opts))
  `(doc-make-rich-title
    ,(ams-doc-data-hidden t)
    ,(ams-doc-data-main t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (author-data t)
  (:secure #t)
  `(document
     ,@(select t '(author-name))
     ,@(select t '(author-name-affiliation))
     ,@(select t '(author-affiliation))
     ,@(select t '(author-affiliation-note))
     ,@(select t '(author-email))
     ,@(select t '(author-email-note))
     ,@(select t '(author-homepage))
     ,@(select t '(author-homepage-note))
     ,@(select t '(author-misc))
     ,@(select t '(author-misc-note))
     ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (abstract-data t)
  (:secure #t)
  (let ((opts `(document ,@(select t '(abstract-keywords))
                         ,@(select t '(abstract-acm))
                         ,@(select t '(abstract-arxiv))
                         ,@(select t '(abstract-pacs))
                         ,@(select t '(abstract-msc))))
        (abst (select t '(:* abstract 0))))
    (if (list>1? opts)
      `(render-abstract* (document ,@abst) ,opts)
      `(render-abstract  (document ,@abst)))))
