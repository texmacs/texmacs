
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: ee875718-b922-4f92-95d4-d1d1832045dd
;;
;; MODULE      : slides.scm
;; DESCRIPTION : Create nested switches
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (slides slides)
  (:use (utils library tree)
	(convert tools stm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating slides (nested switches) from the section structure of a document.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	Public commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (make-slides)
  (apply-on-new-buffer-stree
   (lambda (x) (sections->switch x '(section subsection subsubsection)))))

(tm-define (make-slides-here)
  (apply-on-buffer-stree
   (lambda (x) (sections->switch x '(section subsection subsubsection)))))

;;;;;;;;;;;;;;;;;;;;;;;	Buffer transformation utilities ;;;;;;;;;;;;;;;;;;;;;;;

;; Apply a procedure on a buffer's content.

(define (apply-on-buffer-stree proc)
  ;; Apply @proc to the buffer content as scheme, and replace the buffer
  ;; content by the result of @proc.
  (let ((t (stree->tree
	    (htmltm-unary-document
	     (proc (tree->stree (buffer-tree)))))))
    (tree-assign (buffer-tree) t)))

(define (apply-on-new-buffer-stree proc)
  ;; Do as apply-on-buffer-stree but create a new buffer, do not overwrite the
  ;; current buffer.
  (let ((t (stree->tree
	    (htmltm-unary-document
	     (proc (tree->stree (buffer-tree)))))))
    (new-buffer)
    (tree-assign (buffer-tree) t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	List utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (list-split-at x i)
  ;; SRFI-1
  (let lp ((l x) (n i) (acc '()))
    (if (<= n 0)
      (values (reverse! acc) l)
      (lp (cdr l) (- n 1) (cons (car l) acc)))))


(define (list-assign l i x)
  (receive (left right) (list-split-at l i)
    (append left (list x) (cdr right))))


;;;;;;;;;;;;;;;;;;;;;;;;;; Slide creation utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Depends: htmltm-list->document htmltm-serial htmltm-unary-document
;;          tmhtml-list-map

(define htmltm-list->document stm-list->document)
(define htmltm-serial stm-serial)
(define htmltm-unary-document stm-unary-document)
(define tmhtml-list-map stm-list-map)

(define (sections->switch doc sections)
  (define (make-section title doc-items)
    (let ((doc2 (htmltm-list->document
		 (list-filter doc-items (lambda (x) (!= x ""))))))
      (if (not title) doc2
	  (htmltm-serial (list title
			       (sections->switch doc2 (cdr sections)))))))
  (define (section? x)
    (func? x (car sections) 1))
  (htmltm-unary-document
   (if (null? sections) doc
       (make-switch-maybe
	(tmhtml-list-map make-section section? (cdr doc))
	0))))

(define (list-length<=1 l)
  (or (null? l) (null? (cdr l))))

(define (make-switch-maybe doclist pos)
  (if (list-length<=1 doclist)
      (htmltm-list->document doclist)
      (make-switch doclist pos)))

(define (make-switch doclist pos)
  `(hide_expand "switch" ,(list-ref doclist pos)
		(tuple ,@(list-assign doclist pos '(tmarker)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deconstructing switches (flattening slides)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Not yet implemented

(define (flatten-switch)
  (switch-unselect-recursive)
  (apply-on-buffer-stree flatten-switch-sub))

(define (switch-unselect-recursive)
  (go-innermost-switch)
  (switch-unselect-recursive-sub))

(define (switch-unselect-recursive-sub)
  (switch-unselect)
  (let ((oldp (cursor-path)))
    (go-outer-switch)
    (if (!= (oldp (cursor-path)))
	(switch-unselect-recursive-sub))))

(define (flatten-switch-sub x)
  (postorder x flatten-switch-sub2))
