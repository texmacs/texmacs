
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : edit-title.scm
;; DESCRIPTION : editing document titles, authors, etc.
;; COPYRIGHT   : (C) 2004  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-title)
  (:export
    make-doc-data make-doc-data-element
    make-author-data-element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting document and author data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-doc-data)
  (insert-go-to '(doc-data (doc-title "")) '(0 0 0)))

(define (make-doc-data-element l)
  (let* ((p (search-parent-upwards 'doc-data))
	 (q (rcons (cDr p) (+ (cAr p) 1))))
    (cond ((== l 'doc-author-data)
	   (tm-insert q `(doc-data (,l (author-name ""))))
	   (tm-go-to (rcons* q 0 0 0)))
	  (else
	   (tm-insert q `(doc-data (,l "")))
	   (tm-go-to (rcons* q 0 0))))))

(define (make-author-data-element l)
  (let* ((p (search-parent-upwards 'doc-author-data))
	 (q (rcons (cDr p) (+ (cAr p) 1))))
    (cond ((in? l '(author-address author-note))
	   (tm-insert q `(doc-author-data (,l (document ""))))
	   (tm-go-to (rcons* q 0 0 0)))
	  (else
	   (tm-insert q `(doc-author-data (,l "")))
	   (tm-go-to (rcons* q 0 0))))))
