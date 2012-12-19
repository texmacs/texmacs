;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; module      : title-bis.scm
;; description : Translation of metadata markup for typesetting purpose
;; copyright   : (c) 2012--2013 Joris van der Hoeven, Francois Poulain
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-bis))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main document data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-data-main t)
  (:secure #t)
  (with authors (select t '(doc-author author-data))
    `(document
       ,@(select t '(doc-title))
       ,@(select t '(doc-subtitle))
       ,@(cond ((null? authors)
                (list))
               ((list-1? authors)
                (list `(render-doc-author ,@authors)))
               (else
                (list `(render-doc-authors ,@authors))))
       ,@(select t '(doc-date))
       ,@(select t '(doc-inactive)))))

(tm-define (doc-data-hidden t)
  (:secure #t)
  `(concat
     (doc-note ,@(select t '(doc-note)))
     (doc-data-bis ,@(tm-children t))
     (doc-authors-data-bis ,@(select t '(doc-author author-data)))
     (doc-running-title ,@(select t '(doc-title 0)))
     (doc-running-title ,@(select t '(doc-running-title 0)))
     (doc-running-author
       (author-from-authors
         ,@(select t '(doc-author author-data author-name 0))))
     (doc-running-author ,@(select t '(doc-running-author 0)))))

(tm-define (doc-data-abstract t)
  (:secure #t)
  `(tuple
     (document
       ,@(select t '(doc-keywords))
       ,@(select t '(doc-msc)))))

(tm-define (doc-data-note t)
  (:secure #t)
  `(document
     ,@(select t '(doc-note document :%1))))

(tm-define (doc-data t)
  (:secure #t)
  `(surround
     (assign "the-doc-data" (quote ,t))
     (with "doc-note-nr" "0" (doc-data-hidden ,@(tm-children t)))
     (document
       (doc-make-title
         (with "doc-note-nr" "0"
           (doc-data-main ,@(tm-children t)))))))

(tm-define (doc-data-bis t)
  (:secure #t)
  `(doc-title-note
     (tuple
       (doc-data-note
         ,@(tm-children t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-author-main t)
  (:secure #t)
  `(document
     ,@(select t '(author-name))
     ,@(select t '(author-affiliation))
     ,@(select t '(author-email))
     ,@(select t '(author-homepage))))

(tm-define (doc-author-data-note t)
  (:secure #t)
  `(document
     ,@(select t '(author-misc document :%1))))

(tm-define (author-data t)
  (:secure #t)
  `(with "the-author-data" (quote ,t)
     (doc-author-block
       (doc-author-main ,t))))

(tm-define (authors-data t)
  (:secure #t)
  `(with "the-author-data" (quote ,t)
     (doc-authors-block
       (doc-author-main ,t))))

(tm-define (doc-author-data-bis t)
  (:secure #t)
  `(doc-author-note
     (tuple
       (doc-author-data-note ,@(tm-children t)))))

(tm-define (doc-authors-data-bis t)
  (:secure #t)
  `(concat
     ,@(map doc-author-data-bis (tm-children t))))
