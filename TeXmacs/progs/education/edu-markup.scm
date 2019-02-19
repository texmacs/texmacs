
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edu-markup.scm
;; DESCRIPTION : special external markup for educational purposes
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (education edu-markup)
  (:use (database title-markup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization of titles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (title-prefix pre tit)
  (cond ((tm-is? tit 'doc-make-rich-title)
	 (with l (tm-children tit)
	   `(doc-make-rich-title ,@(cDr l) ,(title-prefix pre (cAr l)))))
	((tm-is? tit 'document)
	 `(document ,pre ,@(tm-children tit)))
	(else tit)))

(tm-define (doc-data-exam t xopts)
  (:secure #t)
  (let* ((doc   (doc-data t xopts))
	 (class (select t '(doc-exam-class :%1)))
	 (date  (select t '(doc-exam-date :%1))))
    (cond ((and (null? class) (null? date))
	   doc)
	  ((and (nnull? class) (null? date))
	   (title-prefix `(doc-exam-class ,(car class)) doc))
	  ((and (null? class) (nnull? date))
	   (title-prefix `(doc-exam-date ,(car date)) doc))
	  ((and (nnull? class) (nnull? date))
	   (title-prefix
	    `(doc-exam-class-date ,(car class) ,(car date)) doc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table like
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ext-tile-item t)
  `(cell (document ,t)))

(define (ext-tiled-row l cols)
  (let* ((r (map (lambda (x) "") (.. (length l) cols)))
         (a (append l r)))
    `(row ,@(map ext-tile-item a))))
  
(define (ext-tiled-rows l cols)
  (if (> (length l) cols)
      (cons (ext-tiled-row (sublist l 0 cols) cols)
            (ext-tiled-rows (sublist l cols (length l)) cols))
      (list (ext-tiled-row l cols))))

(tm-define (ext-tiled-items t c)
  (:secure #t)
  (let* ((cols (or (string->number (tree->string c)) 5))
         (w (string-append (number->string (/ 0.999999 cols)) "par")))
    `(tformat
      (twith "table-valign" "T")
      (twith "table-hmode" "min")
      (twith "table-width" "1par")
      (cwith "1" "-1" "1" "-1" "cell-hmode" "exact")
      (cwith "1" "-1" "1" "-1" "cell-width" ,w)
      (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
      (cwith "1" "-1" "1" "-1" "cell-lsep" "-0.2ln")
      (cwith "1" "-1" "1" "-1" "cell-rsep" "-0.2ln")
      (cwith "1" "-1" "1" "-1" "cell-bsep" "0ln")
      (cwith "1" "-1" "1" "-1" "cell-tsep" "0ln")
      (table ,@(ext-tiled-rows (tree-children t) cols)))))

(define (ext-vertical-item t)
  `(row (cell (document ,t))))

(tm-define (ext-vertical-items t out inn)
  (:secure #t)
  `(tformat
    (twith "table-valign" "T")
    (twith "table-hmode" "min")
    (twith "table-width" "1par")
    (twith "table-lborder" ,(tree-copy out))
    (twith "table-rborder" ,(tree-copy out))
    (twith "table-tborder" ,(tree-copy out))
    (twith "table-bborder" ,(tree-copy out))
    (cwith "1" "-1" "1" "-1" "cell-hyphen" "t")
    (cwith "1" "-1" "1" "-1" "cell-lsep" "0ln")
    (cwith "1" "-1" "1" "-1" "cell-rsep" "0ln")
    (cwith "1" "-1" "1" "-1" "cell-bsep" "-0.2ln")
    (cwith "1" "-1" "1" "-1" "cell-tsep" "-0.2ln")
    (cwith "2" "-1" "1" "-1" "cell-tborder" ,inn)
    (table ,@(map ext-vertical-item (tree-children t)))))
