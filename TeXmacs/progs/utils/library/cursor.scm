
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : cursor.scm
;; DESCRIPTION : routines for cursor movement
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
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

(define (check-pattern p l)
  (or (null? l)
      (with t (path->tree (list-drop-right p (length l)))
	(cond ((and (symbol? (car l)) (== (tm-car t) (car l)))
	       (check-pattern p (cdr l)))
	      ((and (procedure? (car l)) ((car l) t))
	       (check-pattern p (cdr l)))
	      ((and (number? (car l))
		    (== (car l) (list-ref p (- (length p) (length l) 1)))
		    (> (length p) 1))
	       (check-pattern p (cdr l)))
	      (else #f)))))

(define (innermost-pattern p l)
  (cond ((<= (length p) (length l)) #f)
	((check-pattern p l) (cDr p))
	(else (innermost-pattern (cDr p) l))))

(tm-define (go-to-remain-inside fun . l)
  (with p (cursor-path)
    (fun)
    (let* ((q (cursor-path))
	   (pp (innermost-pattern p l))
	   (qq (innermost-pattern q l)))
      (if (!= pp qq) (go-to p)))))

(define (go-to-next-inside-sub fun l)
  (do ((p (cursor-path) (cursor-path))
       (q (begin (fun) (cursor-path)) (begin (fun) (cursor-path))))
      ((or (== p q) (innermost-pattern q l)) (noop))))

(tm-define (go-to-next-inside fun . l)
  (with p (cursor-path)
    (go-to-next-inside-sub fun l)
    (if (not (innermost-pattern (cursor-path) l)) (go-to p))))

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
(tm-define (go-to-next-tag-same-argument lab)
  (go-to-same-buffer (lambda (t p) (path-next-tag-same-argument t p lab))))
(tm-define (go-to-previous-tag-same-argument lab)
  (go-to-same-buffer (lambda (t p) (path-previous-tag-same-argument t p lab))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cursor-history (make-ahash-table))
(define cursor-future (make-ahash-table))

(define (history-get) (ahash-ref* cursor-history (window-get-id) '()))
(define (history-set l) (ahash-set! cursor-history (window-get-id) l))
(define (future-get) (ahash-ref* cursor-future (window-get-id) '()))
(define (future-set l) (ahash-set! cursor-future (window-get-id) l))

(define (cursor-same? l p)
  (and (nnull? l) (== (position-get (car l)) p)))

(tm-define (cursor-history-add p)
  (:synopsis "Add current cursor position into the history")
  (if (cursor-same? (future-get) p)
      (with pos (car (future-get))
	(future-set (cdr (future-get)))
	(history-set (cons pos (history-get))))
      (when (not (cursor-same? (history-get) p))
	(with pos (position-new)
	  (position-set pos p)
	  (history-set (cons pos (history-get)))))))

(define (position-valid? pos)
  (and-with t (path->tree (cDr (position-get pos)))
    (not (tm-func? t 'uninit))))

(tm-define (cursor-has-history?)
  (:synopsis "Does there exist a previous position in history?")
  (nnull? (history-get)))

(tm-define (cursor-history-backward)
  (:synopsis "Go to previous position in history")
  (when (nnull? (history-get))
    (with pos (car (history-get))
      (history-set (cdr (history-get)))
      (if (position-valid? pos)
	  (begin
	    (future-set (cons pos (future-get)))
	    (if (== (cursor-path) (position-get pos))
		(cursor-history-backward)
		(begin
		  (go-to (position-get pos))
		  (cursor-show-if-hidden))))
	  (begin
	    (position-delete pos)
	    (cursor-history-backward))))))

(tm-define (cursor-has-future?)
  (:synopsis "Does there exist a next position in history?")
  (nnull? (future-get)))

(tm-define (cursor-history-forward)
  (:synopsis "Go to next position in history")
  (when (nnull? (future-get))
    (with pos (car (future-get))
      (future-set (cdr (future-get)))
      (if (position-valid? pos)
	  (begin
	    (history-set (cons pos (history-get)))
	    (if (== (cursor-path) (position-get pos))
		(cursor-history-forward)
		(begin
		  (go-to (position-get pos))
		  (cursor-show-if-hidden))))
	  (begin
	    (position-delete pos)
	    (cursor-future-backward))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Changing buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-buffer name)
  (:argument  name "Switch to buffer")
  (:proposals name (map car (list-filter (get-buffer-menu) pair?)))
  (let* ((m  (list-filter (get-buffer-menu) pair?))
	 (l1 (assoc name m))
	 (l2 (assoc (string-append name " *") m)))
    (cond (l1 ((cadr l1)))
	  (l2 ((cadr l2)))
	  (else (set-message (string-append "No buffer#" name)
			     "switch to buffer")))))

(tm-define (with-active-buffer-sub name cmd)
  (let ((old (get-name-buffer)))
    (switch-to-active-buffer name)
    (eval cmd)
    (switch-to-active-buffer old)))

(tm-define-macro (with-active-buffer buf . body)
  `(with-active-buffer-sub ,buf `(begin ,@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (replace-start-forward what by)
  (:argument what "Replace")
  (:argument by "Replace by")
  (replace-start what by #t))
