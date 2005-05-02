
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 0d81db85-48d0-4fa4-b563-e01aa2ea1341
;;
;; MODULE      : search-in-tree.scm
;; DESCRIPTION : Iterating over trees
;; COPYRIGHT   : (C) 2003--2004  David Allouche
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (search-in-tree))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tree-func? t s)
  (eq? s (tree-get-label t)))

(define (safe-tree-ref t i)
  (if (and (tree-compound? t)
           (< i (tree-arity t)))
      (tree-ref t i)
      (error "safe-tree-ref, index out of range")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search forward from a point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (search-in-tree-from t path label proc)
  ;; Traverse the given tree  @t is preorder, starting at the given path.
  ;; If a subtree is found whose label matches @label, return (@proc pp tt)
  ;; where tt is is the matching subtree and pp its patch. If no match is
  ;; found, return #f.
  ;;
  ;; NOTE: a tail recursive implementation was used to allow searching in trees
  ;; which are not subtrees of (subtree t path). This could be implemented more
  ;; simply using the call stack by extending tree-iterate using path-less-eq?.
  (if (null? path)
      (search-in-tree-from/down label proc '() '() t)
      (let ((t+ts (subtrees-on-path t path)))
	(search-in-tree-from/down
	 label proc (cdr t+ts) (reverse path) (car t+ts)))))

(define (subtrees-on-path t p)
  ;; Stack of all the subtrees traversed when getting (subtree t p).
  ;; (subtree t p) is the first item, and t is the last item.
  ;; This is useful to initialize the backtracking stack for tree searches.
  (define (kons i ts)
    (cons (safe-tree-ref (first ts) i) ts))
  (list-fold kons (list t) p))

(define (search-in-tree-from/down label proc ts ip t)
  (if (tree-func? t label)
      (or (proc (reverse ip) t)
	  (search-in-tree-from/up label proc ts ip))
      (search-in-tree-from/right label proc ts ip t 0)))

(define (search-in-tree-from/up label proc t+ts i+ip)
  (and (pair? t+ts)
       (search-in-tree-from/right
	label proc (cdr t+ts) (cdr i+ip) (car t+ts) (1+ (car i+ip)))))

(define (search-in-tree-from/right label proc ts ip t i)
  (if (and (tree-compound? t) (< i (tree-arity t)))
      (search-in-tree-from/down
       label proc (cons t ts) (cons i ip) (tree-ref t i))
      (search-in-tree-from/up label proc ts ip)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search ...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-iterate t s proc)
  ;; Traverse the tree in preorder until the current subtree label matches the
  ;; symbol @s, then return (proc t p), where t is the matching subtree and p
  ;; its patch. If no matching subtree is found, return #f.
  (let down ((t t) (ip '()))
    (if (tree-func? t s)
	(proc t (reverse ip))
	(let right ((i 0))
	  (and (tree-compound? t)
               (< i (tree-arity t))
	       (or (down (tree-ref t i) (cons i ip))
		   (right (1+ i))))))))

(define (search-in-tree t s)
  ;; Path of the first subtree (in preorder) of t whose label is s, or #f.
  (tree-iterate t s (lambda (t p) p)))

(tm-define (search-in-tree-previous p s)
  ;; Unclear... intent seems to be: return the path the last tree in the buffer
  ;; whose label is @s and which is before @p.
  (let sub ((pred (search-in-tree (the-buffer) s))
            (pos (but-last (but-last p))))
    (let ((p (search-in-tree-from (the-buffer)
                                  (rcons pred 0)
                                  s
                                  (lambda (p t) p))))
      (if (path-less-eq? pos p)
          pred (sub p pos)))))

