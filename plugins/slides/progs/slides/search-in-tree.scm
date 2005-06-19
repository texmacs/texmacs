
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: 704aee96-fd4d-41d9-8dc4-e7f1646e03f1
;;
;; MODULE      : search-in-tree.scm
;; DESCRIPTION : Search in a texmacs tree
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (slides search-in-tree)
  (:use (utils library tree)
	(convert tools stm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utility functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-func? t s)
  (or (== s (symbol->string (tree-label t)))
      (and (in? (tree-label t) '(expand var_expand hide_expand))
           (== s (tree->string (tree-ref t 0))))))

(tm-define (tree-compound-arity t)
  (if (tree-atomic? t) 0 (tree-arity t)))

(tm-define (safe-tree-ref t i)
  (if (< i (tree-compound-arity t))
      (tree-ref t i)
      (error "safe-tree-ref, index out of range")))

;;;;;;;;;;;;;;;;;;;;;;;;;; Search from the tree root ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; t: a texmacs tree
;; label: a string, matches node names and expansion names

;; returns: the path of the first (depth first) matching subtree
;;          or #f if no match is found.

(tm-define (search-in-tree t label)
  (let down ((t t) (ip '()))
    (if (tree-func? t label)
        (reverse ip)
        (let right ((i 0))
          (and (< i (tree-compound-arity t))
               (or (down (tree-ref t i) (cons i ip))
		   (right (1+ i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;; Search from a given path ;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; t: a texmac tree
;; path: a list of integers. Search in @t from this position.
;; label: a string. Matches primitive and expansion names.

;; proc: a procedure of two parameters to call for each occurence
;;    p: path of the matching tree
;;    t: matching tree
;;    returns: true to stop searching, #f to continue searching.

;; return: value of last invokation of @proc or #f if no match was found.

(tm-define (search-in-tree-from t path label proc)
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
  (if (< i (tree-compound-arity t))
      (search-in-tree-from/down
       label proc (cons t ts) (cons i ip) (tree-ref t i))
      (search-in-tree-from/up label proc ts ip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(set-trace-level! search-in-tree-from search-in-tree-from/down
;		  search-in-tree-from/up search-in-tree-from/right)

; (define (display-found-em p+t)
;   (display* "Found em at: " (first p+t) "\n")
;   (display* "Found em data: " (tree->stree (second p+t)) "\n"))

; (define (find-next-em)
;   (let ((p+t (search-in-tree-from
; 	      (buffer-tree) (but-last (cursor-path)) "em" list)))
;     (go-to (rcons (first p+t) 0))
;     (display-found-em p+t)))

; (define (find-all-em)
;   (define found '())
;   (define (proc p t) (set-rcons! found (list p t)) #f)
;   (search-in-tree-from
;    (buffer-tree) (but-last (cursor-path)) "em" proc)
;   (for-each display-found-em found))

; (kbd-map
;   ("C-x e 2" (find-next-em))
;   ("C-x e 1" (find-all-em)))
