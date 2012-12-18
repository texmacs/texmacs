;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; module      : title-base.scm
;; description : Tranlation of metadata markup for typesetting purpose
;; copyright   : (c) 2012--2013 François Poulain, Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-base))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; temporary debbuging
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (print-tree t) (:secure #t) (display* "tree : " t "\n\n\n") t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Renderers ; for style application.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; FIXME: this may be factorized using Scheme macros.

(define (render-bloc-data body)
  `(render-data-bloc (render-data-inline ,body)))

(define (render-bloc-title body)
  `(render-title-bloc (render-title-inline ,body)))

(define (render-bloc-subtitle body)
  `(render-subtitle-bloc (render-subtitle-inline ,body)))

(define (render-bloc-author body)
  `(render-author-bloc (render-author-inline ,body)))

(define (render-bloc-author* body)
  `(render-author-bloc* (render-author-inline ,body)))

(define (render-bloc-date body)
  `(render-date-bloc (render-date-inline ,body)))

(define (render-bloc-title-note body)
  (if (and (string? body) (string-null? body)) ""
    `(render-title-note-bloc (render-title-note-inline ,body))))

(define (render-bloc-author-name body)
  `(render-author-name-bloc (render-author-name-inline ,body)))

(define (render-bloc-author-affiliation body)
  `(render-author-affiliation-bloc (render-author-affiliation-inline ,body)))

(define (render-bloc-author-homepage body)
  `(render-author-homepage-bloc (render-author-homepage-inline ,body)))

(define (render-bloc-author-email body)
  `(render-author-email-bloc (render-author-email-inline ,body)))

(define (render-bloc-author-misc body)
  `(render-author-misc-bloc (render-author-misc-inline ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Content management macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (render-title-note-labels n)
  ; return a concat of each title-note label
    `(concat ,@(map (lambda (l) `(render-title-note-label)) n)))

(define (render-author-misc-labels n)
  ; return a concat of each title-note label
    `(concat ,@(map (lambda (l) `(render-author-misc-label)) n)))

(define (recompose l r sep)
  ; transferts r in l, with a separator between each element of l.
  (cond
    ((and (null? l) (null? r)) r)
    ((and (null? l) (nlist? r)) r)
    ((null? r) (cDr l))
    (else (recompose (append l (list (first r) sep)) (cdr r) sep))))

(define (render-grouped-title-note-labels n)
  ; return a label of the concat of title-notes
    `(render-title-note-label ,@(recompose '() n '(sep-text))))

(define (append-title-labels t)
  ; append notes labels to the titles
  (let ((titles (select t '(doc-title :%1)))
        (notes  (select t '(doc-note  :%1))))
    (if (null? notes) titles
      (map (lambda (title) `(concat ,title ,(render-grouped-title-note-labels notes))) titles))))

(define (append-name-labels t)
  ; append notes labels to the authors names
  (let ((names (select t '(author-name :%1)))
        (notes (select t '(author-misc :%1))))
    (map (lambda (name) `(concat ,name ,(render-author-misc-labels notes))) names)))

(define (amap c f l)
  ; map l using f and apply the result to c
  (if (null? l) ""
    (apply c (list (map f l)))))

(define (create-authors-bloc t)
  (cond ((list-1? t) `(render-authors-bloc ,(render-bloc-author* (car t))))
        ((list>1? t) `(render-authors-bloc (concat ,@(recompose '() (map render-bloc-author t) '(render-authors-sep)))))
        (else "")))

(define (typesetted? t)
  (cond ((list? t)   (nnull? t))
        ((pair? t)   (nnull? t))
        ((tree? t)   (not (tree-empty? t)))
        ((string? t) (not (string-null? t)))
        (else #t)))

(define (document-simplify . t)
  `(document ,@(filter typesetted? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public interfaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (doc-data t)
  (:secure #t)
  (document-simplify
     (amap document render-bloc-title (append-title-labels t))
     (amap document render-bloc-subtitle (select t '(doc-subtitle :%1)))
     `(concat
       ,(create-authors-bloc (select t '(doc-author)))
       (assign "count-title-note-nr" (quote "0"))
       ,(render-bloc-title-note (apply tmconcat (recompose '() (select t '(doc-note :%1)) '(sep-text))))
       ,(amap concat render-bloc-author-misc (select t '(doc-author author-data author-misc :%1))))
     (amap document render-bloc-date (select t '(doc-date :%1)))))

(tm-define (doc-author t)
           (:secure #t)
                 (document-simplify
                    (amap document render-bloc-author-name (append-name-labels t))
                    (amap document render-bloc-author-affiliation (select t '(author-affiliation :%1)))
                    (amap document render-bloc-author-email (select t '(author-email :%1)))
                    (amap document render-bloc-author-homepage (select t '(author-homepage :%1)))))
