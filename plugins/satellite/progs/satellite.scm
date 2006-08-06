;Needs sat.ts
;Env and tags to which this plug is applied need to be redefined in sat.ts

(texmacs-module (satellite)
  (:use (utils library tree)
	(generic document-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Paths in trees (by David Allouche)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-func? t s)
  (or (in? (object->string (tree-label t))  s)
      (and (equal? 'expand (tree-label t))
	   (in? (tree->string (tree-ref t 0)) s))))

(define (tree-compound-arity t)
  (if (tree-atomic? t) 0 (tree-arity t)))

(define (tree-iterate t listlabel proc)
  (let down ((t t) (ip '()))
    (if (tree-func? t listlabel)
	(proc t (reverse ip))
	(let right ((i 0))
	  (and (< i (tree-compound-arity t))
	       (or (down (tree-ref t i) (cons i ip))
		   (right (1+ i))))))))

(define (search-nth-in-tree t listlabel n)
  (define (sub t p)
    (if (<= n 1) p
	(begin (set! n (1- n)) #f)))
  (tree-iterate t listlabel sub))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Operations on lists
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Extracts all the lists contain in l satisfaying the condition cond. The extraction is apply recursively to the lists in l.
;example : (extract (lambda(x) (and (pair? x) (equal? (car x) 1))) lst) gives the list of the lists in l with the first term equal to  1.

(define (extract-included cond lst)
  (if (pair? lst)
      (append (extract-included/sub cond (car lst)) (extract-included cond (car lst)) (extract-included cond (cdr lst)))
      '()))

(define (extract-included/sub cond l)
  (if (cond l)
      (list l)
      '()))


;converts an atom list to a string list.

(define (atom->string l)
  (cond ((> (length l) 1)
	 (cons (symbol->string (car l))
	       (atom->string (cdr l))))
	((= (length l) 1)
	 (list (symbol->string (car l))))
      (else '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creation of the file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (define (get-strg-name-buffer)
   (url->string (get-name-buffer)))

(define (cons-file-name name l)
  (with first (if (string? (car l)) (car l) (symbol->string (car l)))
    (string-append name "-" first "s")))

(define (sublist-listenv lterm l)
  (extract-included (env-in? lterm) l))

(define (env-in? lterm)
  (lambda(x) (and (pair? x) (in? (car x) lterm))))

;listenv contains a list of environment and tag names :
; (section subsection subsubsection)
;Extracts and  copy in a new buffer the text contains in an env.
; or tags of listenv. If the  buffer doesn't exist, creates it,
; otherwise refreshs it.

(tm-define (create-file-with-env lenv) 
  (let* ((src-buff (get-strg-name-buffer))
	 (the-nw-buff (cons-file-name (get-strg-name-buffer) lenv))
	 (the-tree
	  (stree->tree
	   (cons 'document
		 (sublist-listenv lenv (tree->stree (buffer-tree))))))) 
      (if (!= (convert the-tree "texmacs-tree" "verbatim-snippet") "")
	  (begin     
	    (switch-to-active-buffer the-nw-buff) ;"trick" to test if the buffer already exists... 
;Joris : il faudrait faire qch de mieux pour tester si une fenêtre existe déjà...
	    (if (not (equal? src-buff  (get-strg-name-buffer))) (kill-buffer))
	    (new-buffer)  
	    (set-name-buffer the-nw-buff)
	    (if (in? 'chapter lenv) (init-env "magnification" "0.8"))
	    (init-style "generic")
	    (init-add-package "sat")
	    (init-env "srce" src-buff)
	    (init-env "def-satellite" (object->string lenv))
	    (insert  the-tree)))))

(define list-env-satellite '())

(tm-define (create-satellite)
  (:interactive #t)
  (set! list-env-satellite '())
  (create-satellite/sub))

(define (create-satellite/sub)
  (interactive
      (lambda (s)
	(if (string-null? s)
	    (create-file-with-env list-env-satellite)
	    (begin (set-cons! list-env-satellite (string->object s))
		   (create-satellite/sub))))
    "Environnement ou rien"))

(define (go-to-nth-label listlabel n)
  (go-to (rcons
	     (search-nth-in-tree (buffer-tree) listlabel n) 
	     0)))

(tm-define (back-to-me-in-source)
  (:secure #t)
  (let ((srce (get-env "srce"))
	(listlab (get-env "def-satellite"))
	(n (1+ (car (cursor-path)))))
    (switch-to-active-buffer srce) 
    (go-to-nth-label (atom->string (string->object listlab)) n)))
