
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

(texmacs-module (database title-markup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collect notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insert-note note h style)
  (with n (tm->stree note)
    (when (not (ahash-ref h n))
      (let* ((nr (+ (ahash-size h) 1))
             (sym `(number ,(number->string nr) ,style))
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

(define (remove-notes c)
  (cond ((null? c) c)
        ((tm-func? (car c) 'doc-note 1) (remove-notes (cdr c)))
        ((tm-func? (car c) 'author-misc 1) (remove-notes (cdr c)))
        ((tm-in? (car c) '(doc-author author-data))
         (let* ((f (car c))
                (h `(,(tm-label f) ,@(remove-notes (tm-children f))))
                (t (remove-notes (cdr c))))
           (cons h t)))
        (else (cons (car c) (remove-notes (cdr c))))))

(define (retain-title-notes notes)
  (cond ((null? notes) notes)
        ((tm-is? (caar notes) 'doc-note)
         (cons (car notes) (retain-title-notes (cdr notes))))
        (else (retain-title-notes (cdr notes)))))

(define (find-note sel notes)
  (cond ((null? notes) (list))
        ((== (caar notes) sel)
         (list (car notes)))
        (else (find-note sel (cdr notes)))))

(define (add-annotations c notes)
  (if (null? notes) c
      (with (n note sym id) (cAr notes)
        (with c2 (add-annotations c (cDr notes))
          `(doc-note-ref ,sym ,id ,c2)))))

(define (annotate c notes)
  (cond ((tm-func? c 'doc-title 1)
         (with new-notes (retain-title-notes notes)
           `(doc-title ,(add-annotations (tm-ref c 0) new-notes))))
        ((tm-func? c 'author-name 1)
         `(author-name ,(add-annotations (tm-ref c 0) notes)))
        ((tm-func? c 'doc-author 1)
         (let* ((sels (map tm->stree (select c '(author-data author-misc))))
                (new-notes (append-map (cut find-note <> notes) sels)))
           (with ann (cut annotate <> new-notes)
             `(doc-author ,@(map ann (tm-children c))))))
        ((tm-is? c 'author-data)
         (with ann (cut annotate <> notes)
           `(author-data ,@(map ann (tm-children c)))))
        (else c)))

(define (make-note l)
  (with (n note sym id) l
    `(doc-footnote-text ,sym ,id ,(tm-ref note 0))))

(tm-define (add-notes t)
  (let* ((tags '((doc-note) (doc-author author-data author-misc)))
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

(define (remove-annotations t)
  (if (tm-func? t 'doc-note-ref 3)
      (remove-annotations (tm-ref t 2))
      t))

(define (title->running-title t)
  `(doc-running-title ,(remove-annotations (tm-ref t 0))))

(tm-define (doc-data-hidden t)
  `(concat
     ,@(select t '(doc-footnote-text))
     ,@(map title->running-title (select t '(doc-title)))
     ,@(select t '(doc-running-title))
     (doc-running-author
       (comma-separated
         ,@(map remove-annotations
                (select t '(doc-author author-data author-name 0)))))
     ,@(select t '(doc-running-author))))

(tm-define (doc-data-main t)
  `(document
     ,@(select t '(doc-title))
     ,@(select t '(doc-subtitle))
     ,@(with authors (select t '(doc-author))
         (if (<= (length authors) 1) authors
             (list `(doc-authors ,@authors))))
     ,@(select t '(doc-date))
     ,@(select t '(doc-inactive))))

(tm-define (doc-data-sub t)
  `(surround
     ,(doc-data-hidden t)
     (document
       (doc-make-title
         ,(doc-data-main t)))))

(tm-define (doc-data t)
  (:secure #t)
  ;;(display* "t= " t "\n")
  ;;(display* "r= " (add-notes t) "\n")
  (doc-data-sub (add-notes t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (author-data t)
  (:secure #t)
  `(document
     ,@(select t '(author-name))
     ,@(select t '(author-affiliation))
     ,@(select t '(author-email))
     ,@(select t '(author-homepage))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abstract data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (abstract-data t)
  (:secure #t)
  (let ((opts `(document ,@(select t '(doc-keywords)) ,@(select t '(doc-msc))))
        (abst (select t '(:* abstract 0))))
    (if (list>1? opts)
      `(render-abstract* (document ,@abst) ,opts)
      `(render-abstract  (document ,@abst)))))
