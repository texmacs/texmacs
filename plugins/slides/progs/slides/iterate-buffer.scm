
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; arch-tag: df83048f-c3c1-464f-94bd-cabef20d87da
;;
;; MODULE      : slides/iterate-buffer.scm
;; DESCRIPTION : Iterate in buffer, robustly while modifying the buffer
;; COPYRIGHT   : (C) 2003  David Allouche
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (slides iterate-buffer)
  (:use (utils library tree)
	(slides search-in-tree)))

(tm-define (excursion thunk)
  (let ((marker #f))
    (dynamic-wind
	(lambda ()
	  (set! marker (position-new))
	  (position-set marker (cursor-path)))
	thunk
	(lambda ()
	  (go-to (position-get marker))
	  (position-delete marker)))))

(define (go-root)
  (go-to '(0 0)))

(define (go-child i)
  (let ((p (but-last (cursor-path))))
    (if (< i (tree-compound-arity (path->tree p)))
        (begin (go-to (append p `(,i 0))) #t)
        #f)))

(define (go-parent)
  (let ((p (but-last (cursor-path))))
    (and (pair? p)
         (begin (go-to (rcons (but-last p) 0)) #t))))

(define (go-next-sibling)
  (and-let* ((p (but-last (cursor-path)))
             ((pair? p))
             (i (1+ (last p)))
             (pp (but-last p)))
    (if (< i (tree-compound-arity (path->tree pp)))
        (begin (go-to (append pp `(,i 0))) #t)
        #f)))

(define (go-preorder)
  (or (go-child 0)
      (go-next-sibling)
      (go-preorder/backtrack)))

(define (go-preorder/backtrack)
  (and (go-parent)
       (or (go-next-sibling)
           (go-preorder/backtrack))))

(define (clear-output/sub)
  (trace-display "cursor-path:" (cursor-path))
  (if (go-preorder)
      (begin (and-let*
            ((p (but-last (cursor-path)))
             ((tree-func? (path->tree p) "output"))
             (pp (but-last p))
             ((tree-func? (path->tree pp) "document"))
             ((pair? pp))
             (ppp (but-last pp))
             ((pair? ppp))
             ((tree-func? (path->tree ppp) "session")))
          (path-remove p 1))
     (clear-output/sub))))

(tm-define (clear-output)
  (excursion (lambda () (go-root) (clear-output/sub))))
