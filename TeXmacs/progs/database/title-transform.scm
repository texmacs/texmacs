
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

(define (add-author-refs-to name author data where l)
  (if (null? l) name
      (with item (car l)
        (if (ahash-ref data item)
            (let* ((nr (ahash-ref data item))
                   (num (if (== where :notes) "alpha" "Alpha"))
                   (sym* `(number ,(number->string nr) ,num))
                   (sym `(with "font-shape" "italic" ,sym*))
                   (id (string-append "authref-" (number->string nr)))
                   (nname `(doc-note-ref ,sym (noteref-sep) ,id ,name)))
              (add-author-refs-to nname author data where (cdr l)))
            (let* ((fields (ahash-ref data where))
                   (nr (+ (length fields) 1))
                   (num (if (== where :notes) "alpha" "Alpha"))
                   (sym* `(number ,(number->string nr) ,num))
                   (sym `(with "font-shape" "italic" ,sym*))
                   (id (string-append "authref-" (number->string nr)))
                   (lab* (symbol-append (tm-label item) '-note))
                   (lab (if (== where :notes) lab* 'doc-footnote-text))
                   (fld `(,lab ,sym ,id ,(tm-ref item 0)))
                   (nfields (cons fld fields))
                   (nname `(doc-note-ref ,sym (noteref-sep) ,id ,name)))
              (ahash-set! data item nr)
              (ahash-set! data where nfields)
              (add-author-refs-to nname author data where (cdr l)))))))

(define (add-author-refs name author data where type)
  (with l (select author (list type))
    (add-author-refs-to name author data where l)))

(define (build-author-refs name author data)
  (set! name (add-author-refs name author data :notes 'author-affiliation))
  (set! name (add-author-refs name author data :notes 'author-email))
  (set! name (add-author-refs name author data :notes 'author-homepage))
  (set! name (add-author-refs name author data :footnotes 'author-note))
  name)

(define (build-authors-refs l data)
  (if (null? l) l
      (with names (select (car l) '(author-name 0))
        (if (null? l) (build-authors-refs (cdr l) data)
            (with name (build-author-refs (car names) (car l) data)
              (cons name (build-authors-refs (cdr l) data)))))))

(tm-define (single-author-list t)
  (with authors (select t '(doc-author author-data))
    (if (<= (length authors) 1) t
        (with other (select t '((:exclude doc-author)))
          (with data (make-ahash-table)
            (ahash-set! data :notes '())
            (ahash-set! data :footnotes '())
            (let* ((names (build-authors-refs authors data))
                   (notes (reverse (ahash-ref data :notes)))
                   (fnotes (reverse (ahash-ref data :footnotes)))
                   (uname `(concat ,@(list-intersperse names ", ")))
                   (adata `(author-data (author-name ,uname) ,@notes))
                   (uauthor `(doc-author ,adata)))
              `(,(tm-label t) ,@other ,uauthor ,@fnotes)))))))
