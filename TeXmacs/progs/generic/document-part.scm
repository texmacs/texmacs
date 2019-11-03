
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-part.scm
;; DESCRIPTION : managing document parts
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-part)
  (:use (generic document-edit) (text text-structure)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flatten old-style projects into one file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (inclusion-children t)
  (cond ((tree-is? t 'with) (inclusion-children (cAr (tree-children t))))
	((tree-is? t 'document) (tree-children t))
	(else (list t))))

(define (expand-includes-one t r)
  (if (tree-is? t 'include)
      (with u (url-relative r (unix->url (tree->string (tree-ref t 0))))
	(inclusion-children (tree-load-inclusion u)))
      (list (expand-includes t r))))

(define (expand-includes t r)
  (cond ((tree-atomic? t) t)
	((tree-is? t 'document)
	 (with l (map (lambda (x) (expand-includes-one x r)) (tree-children t))
	   (cons 'document (apply append l))))
	(else
	 (with l (map (lambda (x) (expand-includes x r)) (tree-children t))
	   (cons (tree-label t) l)))))

(tm-define (buffer-expand-includes)
  (with t (buffer-tree)
    (tree-assign! t (expand-includes (buffer-tree) (buffer-master)))))

(define (buffer-master?) (== (get-init "project-flag") "true"))
(tm-define (buffer-toggle-master)
  (:synopsis "Toggle using current buffer as master file of project.")
  (:check-mark "v" buffer-master?)
  (init-env "project-flag"
            (if (== (get-init "project-flag") "true") "false" "true")))

(define (project-attach* u)
  (with name (url->unix (url-delta (current-buffer) u))
    (project-attach name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main internal representations for document parts:
;;   :preamble -> (document (show-preamble preamble) (ignore body))
;;   :one, :several -> (document [optional (hide-preamble pre)] hide-show-list)
;;   :all -> (document [optional (hide-preamble preamble)] body)
;; In the case of :one and :several, the hide-show-list contains items
;;   (show-part id body alt-body)
;;   (hide-part id body alt-body)
;; Here id is an identifier for referencing purposes
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
	     (body `(ignore (document ,@(cdr (tree-children t))))))
	(tree-assign! t `(document ,preamble ,body))))))

(define (buffer-hide-preamble)
  (with t (buffer-tree)
    (when (match? t '(document (show-preamble :%1) (ignore (document :*))))
      (tree-assign! t `(document (hide-preamble ,(tree-ref t 0 0))
				 ,@(tree-children (tree-ref t 1 0)))))))

(tm-define (kbd-remove t forwards?)
  (:require (and (tree-is? t 'show-preamble) (tree-empty? (tree-ref t 0))))
  (buffer-set-part-mode :all)
  (when (buffer-has-preamble?)
    (tree-remove (buffer-tree) 0 1)))

(define (buffer-flatten-subpart t)
  (if (tree-in? t '(show-part hide-part))
      (tree-children (tree-ref t 1))
      (list t)))

(define (buffer-flatten-part t)
  (if (tree-in? t '(show-part hide-part))
      (append-map buffer-flatten-subpart (tree-children (tree-ref t 1)))
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
          ((and (== mode :preamble) (not (buffer-has-preamble?)))
           (buffer-make-preamble))
	  ((== mode :preamble)
	   (when (tree-is? (tree-ref (buffer-tree) 0) 'hide-preamble)
	     (buffer-show-preamble)
	     (tree-go-to (buffer-tree) 0 0 :start)))
	  ((== mode :all)
	   (buffer-hide-preamble)
	   (buffer-flatten-parts)
	   (tree-go-to (car (buffer-body-paragraphs)) :start)
	   (update-current-buffer))
	  (else
	   (buffer-hide-preamble)
	   (buffer-make-parts)
	   (set! part-mode mode)
	   (with first (car (buffer-parts-list #f))
	     (if (== mode :one)
		 (buffer-show-part first)
		 (buffer-go-to-part first)))
	   (update-current-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listing the document parts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (document-part-name t)
  (if (== (tree-ref t 1) (string->tree ""))
      (string->tree "no title")
      (principal-section-title (tree-ref t 1))))

(define (document-get-parts t all?)
  (cond ((tree-atomic? t) '())
	((or (tree-is? t 'show-part) (and all? (tree-is? t 'hide-part)))
	 (list (document-part-name t)))
	((principal-section? t)
	 (list (tm/section-get-title-string t)))
	((not (tree-in? t '(document ignore))) '())
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
    (if (match? l '((ignore (document :*))))
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
	((not (tree-in? t '(document ignore))) #f)
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
	     (tree-assign-node! t 'hide-part)
	     (buffer-go-to-part (car (buffer-parts-list #f))))
	    ((tree-is? t 'hide-part)
	     (tree-assign-node! t 'show-part)
	     (buffer-go-to-part id))))))

(tm-define (buffer-make-preamble)
  (:synopsis "Create a preamble for the current document")
  (when (not (buffer-has-preamble?))
    (with t (buffer-tree)
      (tree-insert! t 0 '((hide-preamble (document ""))))
      (buffer-set-part-mode :preamble))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Making hidden parts visible
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-show-hidden t)
  (:require (and (tree-is? t 'hide-part) (== (tree-up t) (buffer-tree))))
  (if (== (buffer-get-part-mode) :one)
      (with id (document-part-name t)
	(document-select-part (tree-up t) id))
      (tree-assign-node! t 'show-part)))

(tm-define (show-hidden-part id)
  (:synopsis "Make hidden part with identifier @id visible")
  (with search? (lambda (t) (match? t `(hide-part ,id :%2)))
    (and-with t (list-find (tree-children (buffer-tree)) search?)
      (tree-show-hidden t)
      #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer with included files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tm-include? t)
  (and (tm-func? t 'include 1)
       (tm-atomic? (tm-ref t 0))))

(tm-define (tm-get-includes doc)
  (cond ((tm-func? doc 'with)
	 (tm-get-includes (tm-ref doc :last)))
	((tm-func? doc 'document)
	 (append-map tm-get-includes (tm-children doc)))
	((tm-include? doc)
	 (list (tm->string (tm-ref doc 0))))
	(else (list))))

(tm-define (buffer-get-includes)
  (tm-get-includes (buffer-tree)))

(tm-define (buffer-contains-includes?)
  (nnull? (buffer-get-includes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The dynamic document part menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (document-parts-menu)
  (let* ((all (buffer-parts-list #t))
	 (active (buffer-parts-list #f))
	 (make (lambda (id) (document-parts-menu-entry id (in? id active)))))
    (for (id all)
      ((check (eval (upcase-first id)) "v" (in? id active))
       (if (== (buffer-get-part-mode) :one)
           (buffer-show-part id)
           (buffer-toggle-part id))))))

(menu-bind preamble-menu
  (if (buffer-has-preamble?)
      ("Show preamble" (buffer-set-part-mode :preamble)))
  (if (not (buffer-has-preamble?))
      ("Create preamble" (buffer-set-part-mode :preamble)))
  ("Show main document" (buffer-set-part-mode :all)))

(menu-bind document-part-menu
  (if (buffer-has-preamble?)
      ("Show preamble" (buffer-set-part-mode :preamble)))
  (if (not (buffer-has-preamble?))
      ("Create preamble" (buffer-set-part-mode :preamble)))
  ("Show one part" (buffer-set-part-mode :one))
  ("Show several parts" (buffer-set-part-mode :several))
  ("Show all parts" (buffer-set-part-mode :all))
  (if (or (in? (buffer-get-part-mode) '(:one :several))
	  (!= (get-init-tree "sectional-short-style") (tree 'macro "false")))
      ---
      (when (in? (buffer-get-part-mode) '(:one :several))
	(link document-parts-menu))))

(menu-bind document-part-menu
  (:require (buffer-contains-includes?))
  (link document-master-menu))

(menu-bind project-manage-menu
  (if (!= (url-suffix (current-buffer)) "tp")
      ("Use as master" (buffer-toggle-master)))
  (when (buffer-contains-includes?)
    ("Expand inclusions" (buffer-expand-includes)))
  ---
  (when (not (project-attached?))
    ("Attach master"
     (choose-file project-attach* "Attach master file for project" "texmacs")))
  (when (project-attached?)
    ("Detach master" (project-detach))))
