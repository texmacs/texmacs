
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-part.scm
;; DESCRIPTION : managing document parts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-part)
  (:use (generic document-edit) (text tm-structure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main internal representations for document parts:
;;   :preamble -> (document (show-preamble preamble) (hidden body))
;;   :one, :several -> (document [optional (hide-preamble pre)] hide-show-list)
;;   :all -> (document [optional (hide-preamble preamble)] body)
;; In the case of :one and :several, the hide-show-list contains items
;;   (show-part id body alt-body)
;;   (hide-part id body alt-body)
;; Here id is an identifier for menus (auto -> determined from section title)
;; The alt-body is evaluated in the background in the case of hidden parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define part-mode :one)

(define (buffer-body-paragraphs)
  (with t (buffer-tree)
    (if (tree-in? (tree-ref t 0) '(show-preamble hide-preamble))
	(cdr (tree-children t))
	(tree-children t))))

(define (buffer-show-preamble)
  (with t (buffer-tree)
    (when (tree-is? (tree-ref t 0) 'hide-preamble)
      (buffer-flatten-parts)
      (let* ((t (buffer-tree))
	     (preamble `(show-preamble ,(tree-ref t 0 0)))
	     (body `(hidden (document ,@(cdr (tree-children t))))))
	(tree-assign! t `(document ,preamble ,body))))))

(define (buffer-hide-preamble)
  (with t (buffer-tree)
    (when (match? t '(document (show-preamble :1) (hidden (document :*))))
      (tree-assign t `(document (hide-preamble ,(tree-ref t 0 0))
				,@(tree-children (tree-ref t 1 0)))))))

(define (buffer-flatten-part t)
  (if (tree-in? t '(show-part hide-part))
      (tree-children (tree-ref t 1))
      (list t)))

(define (buffer-flatten-parts)
  (when (tree-in? (car (buffer-body-paragraphs)) '(hide-part show-part))
    (let* ((t (buffer-tree))
	   (l (buffer-body-paragraphs))
	   (parts (apply append (map buffer-flatten-part l)))
	   (preamble? (tree-in? (tree-ref t 0) '(show-preamble hide-preamble)))
	   (r (if preamble? (cons (tree-ref t 0) parts) parts)))
      (tree-assign! t `(document ,@r)))))

(define (buffer-make-parts)
  (when (not (tree-in? (car (buffer-body-paragraphs)) '(hide-part show-part)))
    (let* ((t (buffer-tree))
	   (l (buffer-body-paragraphs))
	   (parts (principal-sections-to-document-parts l))
	   (preamble? (tree-in? (tree-ref t 0) '(show-preamble hide-preamble)))
	   (r (if preamble? (cons (tree-ref t 0) parts) parts)))
      (tree-assign! t `(document ,@r)))))

(tm-define (buffer-get-part-mode)
  (:synopsis "Get the mode for document part selections")
  (cond ((tree-is? (tree-ref (buffer-tree) 0) 'show-preamble) :preamble)
	((tree-in? (car (buffer-body-paragraphs)) '(show-part hide-part))
	 part-mode)
	(else :all)))

(define (buffer-test-part-mode? mode)
  (== (buffer-get-part-mode) mode))

(tm-define (buffer-set-part-mode mode)
  (:synopsis "Set the mode for document part selections")
  (:check-mark "v" buffer-test-part-mode?)
  (with old-mode (buffer-get-part-mode)
    (cond ((== mode old-mode) (noop))
	  ((== mode :preamble)
	   (when (tree-is? (tree-ref (buffer-tree) 0) 'hide-preamble)
	     (buffer-show-preamble)
	     (tree-go-to (buffer-tree) 0 0 :start)))
	  ((== mode :all)
	   (buffer-hide-preamble)
	   (buffer-flatten-parts)
	   (tree-go-to (car (buffer-body-paragraphs)) :start)
	   (update-buffer))
	  (else
	   (buffer-hide-preamble)
	   (buffer-make-parts)
	   (set! part-mode mode)
	   (with first (car (buffer-parts-list #f))
	     (if (== mode :one)
		 (buffer-show-part first)
		 (buffer-go-to-part first)))
	   (update-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listing the document parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-part-name t)
  (if (== (tree-ref t 0) (string->tree "auto"))
      (principal-section-title (tree-ref t 1))
      (tree->string (tree-ref t 0))))

(define (document-get-parts t all?)
  (cond ((tree-atomic? t) '())
	((or (tree-is? t 'show-part) (and all? (tree-is? t 'hide-part)))
	 (list (document-part-name t)))
	((principal-section? t)
	 (list (tm/section-get-title-string t)))
	((not (tree-in? t '(document hidden))) '())
	(else (with ls (map (lambda (x) (document-get-parts x all?))
			    (tree-children t))
		(apply append ls)))))

(tm-define (buffer-has-preamble?)
  (:synopsis "Does the current buffer contain a preamble?")
  (with t (buffer-tree)
    (tree-in? (tree-ref t 0) '(show-preamble hide-preamble))))

(tm-define (buffer-parts-list all?)
  (:synopsis "Get the list of all document parts of the current buffer")
  (with l (buffer-body-paragraphs)
    (if (match? l '((hidden (document :*))))
	(set! l (tree-children (tree-ref (car l) 0))))
    (with parts (document-get-parts (tm->tree `(document ,@l)) all?)
      (if (and (not (tree-in? (car l) '(show-part hide-part)))
	       (not (principal-section? (car l)))
	       (or all? (== (buffer-get-part-mode) :all)))
	  (cons "front matter" parts)
	  parts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selection of specific document parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-find-part t id)
  (cond ((tree-atomic? t) #f)
	((tree-in? t '(show-part hide-part))
	 (and (== id (document-part-name t)) t))
	((not (tree-in? t '(document hidden))) #f)
	(else (list-find (tree-children t)
			 (lambda (x) (document-find-part x id))))))

(define (document-select-part t id)
  (cond ((tree-atomic? t) (noop))
	((tree-in? t '(show-part hide-part))
	 (with show? (== id (document-part-name t))
	   (if (and show? (tree-is? t 'hide-part))
	       (tree-assign-node! t 'show-part))
	   (if (and (not show?) (tree-is? t 'show-part))
	       (tree-assign-node! t 'hide-part))))
	((not (tree-is? t 'document)) (noop))
	(else (for-each (lambda (x) (document-select-part x id))
			(tree-children t)))))

(tm-define (buffer-go-to-part id)
  (:synopsis "Go to the part with name @id")
  (with t (document-find-part (buffer-tree) id)
    (and t (tree-go-to t 1 :start))))

(tm-define (buffer-show-part id)
  (:synopsis "Show the document part with name @id")
  (when (== (buffer-get-part-mode) :one)
    (document-select-part (buffer-tree) id)
    (buffer-go-to-part id)))

(tm-define (buffer-toggle-part id)
  (:synopsis "Toggle the visibility of the document part with name @id")
  (when (and (== (buffer-get-part-mode) :several)
	     (list-find (buffer-parts-list #f) (lambda (x) (!= x id))))
    (with t (document-find-part (buffer-tree) id)
      (cond ((not t) (noop))
	    ((tree-is? t 'show-part)
	     (tree-assign-node t 'hide-part)
	     (buffer-go-to-part (car (buffer-parts-list #f))))
	    ((tree-is? t 'hide-part)
	     (tree-assign-node t 'show-part)
	     (buffer-go-to-part id))))))

(tm-define (buffer-make-preamble)
  (:synopsis "Create a preamble for the current document")
  (when (not (buffer-has-preamble?))
    (with t (buffer-tree)
      (tree-insert! t 0 '(document (hide-preamble (document ""))))
      (buffer-set-part-mode :preamble))))
