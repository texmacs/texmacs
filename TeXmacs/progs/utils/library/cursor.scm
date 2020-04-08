
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
      ((or (== p q) (innermost-pattern q l))
       ;;(display* "  End " q ", " (path->tree (cDr q)) "\n")
       q)
    ;;(display* "  Next " q ", " (path->tree (cDr q)) "\n")
    (noop)))

(tm-define (go-to-next-inside fun . l)
  (with p (cursor-path)
    ;;(display* "  First " p ", " (path->tree (cDr p)) ", " (procedure-source fun) ", " l "\n")
    (go-to-next-inside-sub fun l)
    (if (not (innermost-pattern (cursor-path) l)) (go-to p))))

(define (go-to-next-such-that fun pred?)
  (do ((p (cursor-path) (cursor-path))
       (q (begin (fun) (cursor-path)) (begin (fun) (cursor-path))))
      ((or (== p q) (pred? (path->tree (cDr q))))
       ;;(display* "End " q ", " (path->tree (cDr q)) "\n")
       q)
    ;;(display* "Next " q ", " (path->tree (cDr q)) "\n")
    (noop)))

(tm-define (go-to-next-such-that fun pred?)
  (with p (cursor-path)
    ;;(display* "First " p ", " (path->tree (cDr p)) ", " (procedure-source fun) ", " pred? "\n")
    (go-to-next-such-that fun pred?)
    (if (not (pred? (path->tree (cDr (cursor-path))))) (go-to p))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for cursor movement
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (go-to-same-buffer fun)
  (with p (fun (root-tree) (cursor-path))
    (when (list-starts? (cDr p) (buffer-path))
      (go-to p)
      (select-from-cursor))))

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

; Hook for notifications of any cursor movement after it happens (mouse too)
(tm-define (notify-cursor-moved status)
  (noop))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (tree-at-start? t)
  (with p (cursor-path)
    (or (== p (tree->path t 0 :start))
        (== p (tree->path t :start)))))

(tm-define (tree-at-end? t)
  (with p (cursor-path)
    (or (== p (tree->path t 1 :end))
        (== p (tree->path t :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cursor history
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cursor-history (make-ahash-table))
(define cursor-future (make-ahash-table))

(define (history-get) (ahash-ref* cursor-history (window-get-serial) '()))
(define (history-set l) (ahash-set! cursor-history (window-get-serial) l))
(define (future-get) (ahash-ref* cursor-future (window-get-serial) '()))
(define (future-set l) (ahash-set! cursor-future (window-get-serial) l))

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

(define (list-abbrs)
  (map buffer-get-title (buffer-sorted-list)))

(define (abbr->buffer abbr)
  (assoc-ref (map (lambda (x) (cons (buffer-get-title x) x))
                  (buffer-sorted-list))
             abbr))

(tm-define (go-to-buffer name)
  (:argument  name "Switch to buffer")
  (:proposals name (list-abbrs))
  (cond ((in? name (buffer-list))
         (switch-to-buffer name))
        ((abbr->buffer name)
         (switch-to-buffer (abbr->buffer name)))
        ((in? (unix->url name) (buffer-list))
         (switch-to-buffer (unix->url name)))
        (else (set-message `(concat "Error: no buffer " (verbatim ,name))
                           "switch to buffer"))))

(define-public-macro (with-buffer name . body)
  (let* ((old (gensym))
         (new (gensym))
         (res (gensym)))
    `(if (== ,name (current-buffer))
         (begin ,@body)
         (let* ((,old (current-buffer))
                (,new ,name))
           (and (or (url? ,new) (string? ,new))
                (buffer-exists? ,new)
                (buffer-focus ,new)
                (with ,res (begin ,@body)
                  (buffer-focus ,old)
                  ,res))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search and replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (replace-start-forward what by)
  (:argument what "Find text")
  (:argument by "Replace by")
  (replace-start what by #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define-macro (keep-table-selection . body)
  `(with (sr1 sr2 sc1 sc2) (table-which-cells)
     (with ktres (begin ,@body)
       (if (or (!= sr2 sr1) (!= sc2 sc1))
           ;; FIXME: find a robust way to keep the selection
           (delayed
             (:pause 10)
             (table-select-cells sr1 sr2 sc1 sc2)))
       ktres)))       
