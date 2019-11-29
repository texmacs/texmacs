
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
                   (nr (+ 1 (length (ahash-ref data :notes))
                            (length (ahash-ref data :footnotes))))
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
  (set! name (add-author-refs name author data :notes 'author-misc))
  (set! name (add-author-refs name author data :footnotes 'author-note))
  name)

(define (build-authors-refs l data)
  (if (null? l) l
      (with names (select (car l) '(author-name 0))
        (if (or (null? l) (null? names)) (build-authors-refs (cdr l) data)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Factor by affiliation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (assoc-add-to-group t key val)
  (with old (or (assoc-ref t key) (list))
    (assoc-set! t key (cons val old))))

(tm-define (list-group l fun)
  (with t '()
    (for-each (lambda (x) (set! t (assoc-add-to-group t (fun x) x))) l)
    (map reverse (map cdr (reverse t)))))

(define (get-affiliations author)
  (map tm->stree (select author '(author-affiliation))))

(define (remove-affiliations adata)
  (with fields (select adata '((:exclude author-affiliation)))
    `(,(tm-label adata) ,@fields)))

(define (rewrite-by-affiliation-bis group data)
  (ahash-set! data :notes '())
  (let* ((affs (select (car group) '(author-affiliation)))
         (clean-authors (map remove-affiliations group))
         (names (build-authors-refs clean-authors data))
         (notes (reverse (ahash-ref data :notes)))
         (uname `(concat ,@(list-intersperse names ", ")))
         (adata `(author-data (author-name ,uname) ,@affs ,@notes))
         (uauthor `(doc-author ,adata)))
    uauthor))

(define ((rewrite-by-affiliation data) group)
  (if (null? (cdr group))
      (let* ((f (car group))
             (l '(author-email author-homepage author-misc))
             (notes  (select f `((:or ,@l))))
             (nnotes (select f '((:exclude author-email))))
             (nf `(,(tm-label f) ,@nnotes))
             (rewr (rewrite-by-affiliation-bis (list nf) data)))
        `(doc-author (author-data ,@(cdadr rewr) ,@notes)))
      (rewrite-by-affiliation-bis group data)))

(tm-define (factor-affiliation t)
  (with authors (select t '(doc-author author-data))
    (if (<= (length authors) 1) t
        (with groups (list-group authors get-affiliations)
          (with data (make-ahash-table)
            (ahash-set! data :footnotes '())
            (let* ((other (select t '((:exclude doc-author))))
                   (new-authors (map (rewrite-by-affiliation data) groups))
                   (fnotes (reverse (ahash-ref data :footnotes))))
              `(,(tm-label t) ,@other ,@new-authors ,@fnotes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Abbreviated authors (only mention first line of affiliation)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-single l)
  (cond ((null? l) "")
        ((null? (cdr l)) (car l))
        (else `(comma-separated ,@l))))

(define (abbreviate-author t)
  (let* ((name (select t '(author-name 0)))
         (institute (select t '(author-affiliation document 0)))
         (affiliation (select t '(author-affiliation document :%1)))
         (affiliation* (if (list-1? affiliation) (list) affiliation))
         (email (select t '(author-email 0)))
         (webpage (select t '(author-webpage 0)))
         (note (select t '(author-note 0)))
         (misc (select t '(author-misc 0)))
         (other (append affiliation* email webpage note misc)))
    `(doc-author
      (,(tm-label t)
       (author-name ,(make-single name))
       (author-affiliation (document ,(make-single institute)))
       ,@(if (null? other)
             (list)
             (list `(author-note ,(make-single other))))))))

(tm-define (abbreviate-authors t)
  (with authors (select t '(doc-author author-data))
    (let* ((other (select t '((:exclude doc-author))))
           (new-authors (map abbreviate-author authors)))
      `(,(tm-label t) ,@other ,@new-authors))))

(define (abbreviate-author-bis t)
  (let* ((name (select t '(author-name 0)))
         (affiliation (select t '(author-affiliation document :%1)))
         (new-name (if (null? affiliation)
                       `(author-name ,(make-single name))
                       `(author-name-affiliation
                         ,(make-single name)
                         ,(make-single affiliation))))
         (other (select t '((:exclude author-name author-affiliation)))))
    `(doc-author (,(tm-label t) ,new-name ,@other))))

(tm-define (abbreviate-authors-bis t)
  (with authors (select t '(doc-author author-data))
    (let* ((other (select t '((:exclude doc-author))))
           (new-authors (map abbreviate-author-bis authors)))
      `(,(tm-label t) ,@other ,@new-authors))))
