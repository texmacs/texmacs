
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : title-markup-elsarticle.scm
;; DESCRIPTION : Translation of metadata markup for elsarticle style
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-markup-elsarticle)
  (:use (database title-markup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying notes into footnotes and references
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (collect-emails l)
  (let* ((l1 (map cadr (map tree->stree l)))
         (s (string-recompose-comma l1))
         ;FIXME : USE RENDER-EMAIL !!!
         (t (stree->tree `(author-email (concat (em (email-text)) " " ,s)))))
    `((,(tree->stree t) ,t (phantom a) email))))

(define (collect-urls l)
  (let* ((l1 (map cadr (map tree->stree l)))
         (s (string-recompose-comma l1))
         ;FIXME : USE RENDER-URL !!!
         (t (stree->tree `(author-homepage (concat (em (homepage-text)) " " ,s)))))
    `((,(tree->stree t) ,t (phantom a) url))))

(define (add-notes t)
  (let* ((title-notes (collect-notes t "" '((doc-note))))
         ; Todo: add corresponding author
         (emails-notes (collect-emails (select t '(doc-author author-data author-email))))
         (urls-notes (collect-urls (select t '(doc-author author-data author-homepage))))
         (authors-notes (collect-notes t "arabic"
                                       '((doc-author author-data author-misc))))
         (notes (append title-notes emails-notes urls-notes authors-notes)))
         (display* "title-notes: " title-notes "\n")
         (display* "emails-notes: " emails-notes "\n")
    (if (null? notes) t
        (let* ((c1 (tm-children t))
               (c2 (map (cut annotate <> notes) c1))
               (c3 (remove-notes c2))
               (c4 (map make-note notes)))
          `(,(tm-label t) ,@c3 ,@c4)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main document data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (annotate c notes)
  (cond ((tm-func? c 'doc-title 1)
         (with new-notes (retain-title-notes notes)
           `(doc-title ,(add-annotations (tm-ref c 0) new-notes))))
        ((tm-func? c 'author-name 1)
         `(author-name ,(add-annotations (tm-ref c 0) notes)))
        ((tm-func? c 'doc-author 1)
         (let* ((sels (map 
                        tm->stree
                        (append (select c '(author-data author-misc))
                                (select c '(author-data author-affiliation)))))
                (new-notes (append-map (cut find-note <> notes) sels)))
           (with ann (cut annotate <> new-notes)
             `(doc-author ,@(map ann (tm-children c))))))
        ((tm-is? c 'author-data)
         (with ann (cut annotate <> notes)
           `(author-data ,@(map ann (tm-children c)))))
        (else c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-affiliation l)
  (with (n note sym id) l
    `(author-affiliation-text ,sym ,id ,(tm-ref note 0))))

(define (add-affiliations t)
  (with notes (reverse (collect-notes t "alpha" '((doc-author
                                                   author-data
                                                   author-affiliation))))
    (if (null? notes) t
        (let* ((c1 (tm-children t))
               (c2 (map (cut annotate <> notes) c1))
               (c3 (map make-affiliation (reverse notes))))
          `(,(tm-label t) ,@c3 ,@c2)))))

(tm-define (author-data-elsa t)
  (:secure #t)
  `(document
     ,@(select t '(author-name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main document data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-data-hidden-elsa t)
  `(concat
     ,@(select t '(doc-note-text))
     ,@(map title->running-title (select t '(doc-title)))
     ,@(select t '(doc-running-title))
     (doc-running-author
       (comma-separated
         ,@(map remove-annotations
                (select t '(doc-author author-data author-name 0)))))
     ,@(select t '(doc-running-author))))

(tm-define (doc-data-main-elsa t)
  `(document
     ,@(select t '(doc-title))
     ,@(select t '(doc-subtitle))
     ,@(with authors (select t '(doc-author))
         (if (<= (length authors) 1) authors
             (list `(doc-authors ,@authors))))
     (doc-affiliations-bloc (document ,@(select t '(author-affiliation-text))))
     ,@(select t '(doc-date))
     ,@(select t '(doc-inactive))))

(tm-define (doc-data-sub-elsa t)
  `(surround
     ,(doc-data-hidden-elsa t)
     (document
       (doc-make-title
         ,(doc-data-main-elsa t)))))

(tm-define (doc-data-elsa t)
  (:secure #t)
  ;;(display* "t= " t "\n")
  ;;(display* "r= " (add-notes t) "\n")
  (doc-data-sub-elsa (add-notes (add-affiliations t))))
  ;(doc-data-sub-elsa (add-notes t)))


