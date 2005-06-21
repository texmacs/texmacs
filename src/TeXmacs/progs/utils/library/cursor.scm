
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cursor.scm
;; DESCRIPTION : routines for cursor movement
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils library cursor))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Temporary changes of the cursor position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public-macro (with-cursor p . body)
  (let* ((pos (gensym))
	 (res (gensym)))
    `(with ,pos (position-new)
       (position-set ,pos (cursor-path))
       (go-to ,p)
       (with ,res (begin ,@body)
	 (go-to (position-get ,pos))
	 (position-delete ,pos)
	 ,res))))

(define-public-macro (cursor-after . body)
  (let* ((pos (gensym))
	 (res (gensym)))
    `(with ,pos (position-new)
       (position-set ,pos (cursor-path))
       ,@body
       (with ,res (cursor-path)
	 (go-to (position-get ,pos))
	 (position-delete ,pos)
	 ,res))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifying the behaviour of a cursor movement routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-repeat fun)
  (with p (cursor-path)
    (fun)
    (if (!= (cursor-path) p)
	(go-to-repeat fun))))

(define (label-in-range? lab p until)
  (cond ((== p until) #f)
	((tree-is? (path->tree p) lab) #t)
	(else (label-in-range? lab (cDr p) until))))

(tm-define (go-to-remain-inside fun lab)
  (with p (cursor-path)
    (fun)
    (let* ((q (cursor-path))
	   (r (list-head q (list-common-left p q))))
      (if (or (tree-is? (path->tree (cDr q)) lab)
	      (label-in-range? lab (cDr q) (cDr r)))
	  (go-to p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-same-buffer fun)
  (with p (fun (root-tree) (cursor-path))
    (if (list-starts? (cDr p) (buffer-path)) (go-to p))))

(tm-define (go-to-next) (go-to-same-buffer path-next))
(tm-define (go-to-previous) (go-to-same-buffer path-previous))
(tm-define (go-to-next-word) (go-to-same-buffer path-next-word))
(tm-define (go-to-previous-word) (go-to-same-buffer path-previous-word))
(tm-define (go-to-next-node) (go-to-same-buffer path-next-node))
(tm-define (go-to-previous-node) (go-to-same-buffer path-previous-node))

(tm-define (go-to-next-tag lab)
  (go-to-same-buffer (lambda (t p) (path-next-tag t p lab))))

(tm-define (go-to-previous-tag lab)
  (go-to-same-buffer (lambda (t p) (path-previous-tag t p lab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-buffer name)
  (:argument  name "Switch to buffer")
  (:proposals name (map car (get-buffer-menu)))
  (let* ((l1 (assoc name (get-buffer-menu)))
	 (l2 (assoc (string-append name " *") (get-buffer-menu))))
    (cond (l1 ((cadr l1)))
	  (l2 ((cadr l2)))
	  (else (set-message (string-append "No buffer#" name)
			     "switch to buffer")))))

(tm-define (with-active-buffer-sub name cmd)
  (let ((old (get-name-buffer)))
    (switch-to-active-buffer name)
    (eval cmd)
    (switch-to-active-buffer old)))

(tm-define-macro (with-active-buffer . l)
  (with-active-buffer-sub (car l) (cons 'begin (cdr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (replace-start-forward what by)
  (:argument what "Replace")
  (:argument by "Replace by")
  (replace-start what by #t))
