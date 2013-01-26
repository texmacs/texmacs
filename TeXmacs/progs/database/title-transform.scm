
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : title-transform.scm
;; DESCRIPTION : Title transformations for various presentation styles
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; this software falls under the gnu general public license version 3 or later.
;; it comes without any warranty whatsoever. for details, see the file license
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (database title-transform))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List all authors into a single author field and create references
;; for the individual author information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-author-refs-to name author data l)
  (if (null? l) name
      (with item (car l)
        (if (ahash-ref data item)
            (let* ((nr (ahash-ref data item))
                   (sym* `(number ,(number->string nr) "alpha"))
                   (sym `(with "font-shape" "italic" ,sym*))
                   (id (string-append "authref-" (number->string nr)))
                   (nname `(doc-note-ref ,sym ,id ,name)))
              (add-author-refs-to nname author data (cdr l)))
            (let* ((fields (ahash-ref data :fields))
                   (nr (+ (length fields) 1))
                   (sym* `(number ,(number->string nr) "alpha"))
                   (sym `(with "font-shape" "italic" ,sym*))
                   (id (string-append "authref-" (number->string nr)))
                   (fld `(,(tm-label item)
                          (doc-note-text ,sym ,id ,(tm-ref item 0))))
                   (nfields (cons fld fields))
                   (nname `(doc-note-ref ,sym ,id ,name)))
              (ahash-set! data item nr)
              (ahash-set! data :fields nfields)
              (add-author-refs-to nname author data (cdr l)))))))

(define (add-author-refs name author data type)
  (with l (select author (list type))
    (add-author-refs-to name author data l)))

(define (build-author-refs name author data)
  (set! name (add-author-refs name author data 'author-affiliation))
  (set! name (add-author-refs name author data 'author-email))
  (set! name (add-author-refs name author data 'author-homepage))
  name)

(define (build-authors-refs l data)
  (if (null? l) l
      (with names (select (car l) '(author-name 0))
        (if (null? l) (build-authors-refs (cdr l) data)
            (with name (build-author-refs (car names) (car l) data)
              (cons name (build-authors-refs (cdr l) data)))))))

(tm-define (single-author-list t)
  (:secure #t)
  (with authors (select t '(doc-author author-data))
    (if (<= (length authors) 1) t
        (with other (select t '((:exclude doc-author)))
          (with data (make-ahash-table)
            (ahash-set! data :fields '())
            (let* ((names (build-authors-refs authors data))
                   (fields (reverse (ahash-ref data :fields)))
                   (uname `(concat ,@(list-intersperse names ", ")))
                   (adata `(author-data (author-name ,uname) ,@fields))
                   (uauthor `(doc-author ,adata)))
              `(document ,@other ,uauthor)))))))
