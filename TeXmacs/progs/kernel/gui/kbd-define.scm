
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-define.scm
;; DESCRIPTION : Definition of keyboard shortcuts/wildcards
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui kbd-define)
  (:use (kernel texmacs tm-define)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-keyboard-waiting '())
(tm-define lazy-keyboard-done (make-ahash-table))

(tm-define (lazy-keyboard-do module mode*)
  (with mode (texmacs-mode-mode mode*)
    (set! lazy-keyboard-waiting (acons mode module lazy-keyboard-waiting))))

(tm-define-macro (lazy-keyboard module . modes)
  (for-each (lambda (mode) (lazy-keyboard-do module mode)) modes)
  `(delayed
     (:idle 250)
     (ahash-set! lazy-keyboard-done ',module #t)
     (import-from ,module)))

(define lazy-force-all? #f)
(define lazy-force-busy? #f)

(define (lazy-keyboard-force-do l)
  (cond ((null? l) l)
	((ahash-ref lazy-keyboard-done (cdar l))
	 (lazy-keyboard-force-do (cdr l)))
	((or lazy-force-all? (texmacs-in-mode? (caar l)))
	 (module-load (cdar l))
	 (ahash-set! lazy-keyboard-done (cdar l) #t)
	 (lazy-keyboard-force-do (cdr l)))
	(else (cons (car l) (lazy-keyboard-force-do (cdr l))))))

(tm-define (lazy-keyboard-force . opt)
  (set! lazy-force-all? (or lazy-force-all? (nnull? opt)))
  (when (not lazy-force-busy?)
    (set! lazy-force-busy? #t)
    (let* ((l1 (reverse lazy-keyboard-waiting))
	   (l2 (lazy-keyboard-force-do l1)))
      (set! lazy-keyboard-waiting (reverse l2))
      (set! lazy-force-busy? #f)
      (when (null? lazy-keyboard-waiting)
	(set! lazy-force-all? #f))
      (when (and lazy-force-all? (nnull? lazy-keyboard-waiting))
	(lazy-keyboard-force #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-wildcards-sub l post)
  (if (nnull? l)
      (let* ((w (car l))
	     (key (car w))
	     (im (cadr w))
	     (left (if (>= (length w) 3) (caddr w) #f))
	     (right (if (>= (length w) 4) (cadddr w) #t)))
	(insert-kbd-wildcard key im post left right)
	(kbd-wildcards-sub (cdr l) post))))

(tm-define (kbd-wildcards-body l)
  (:synopsis "Helper routine for kbd-wildcards macro")
  (cond ((null? l) (noop))
	((== (car l) 'pre) (kbd-wildcards-sub (cdr l) #f))
	((== (car l) 'post) (kbd-wildcards-sub (cdr l) #t))
	(else (kbd-wildcards-sub l #t))))

(tm-define-macro (kbd-wildcards . l)
  (:synopsis "Add entries in @l to the keyboard wildcard table")
  `(kbd-wildcards-body ,(list 'quasiquote l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-map-table (make-ahash-table))
(define kbd-inv-table (make-ahash-table))
(define kbd-rev-table (make-ahash-table))
(define (kbd-set-map! key im) (ahash-set! kbd-map-table key im))
(define (kbd-set-inv! key im) (ahash-set! kbd-inv-table key im))
(define (kbd-set-rev! key im) (ahash-set! kbd-rev-table key im))
(define (kbd-get-map key) (ahash-ref kbd-map-table key))
(define (kbd-get-inv key) (ahash-ref kbd-inv-table key))
(tm-define (kbd-get-rev key) (ahash-ref kbd-rev-table key))
(define (kbd-remove-map! key) (ahash-remove! kbd-map-table key))

(define (kbd-source cmd)
  (if (procedure? cmd) (promise-source cmd) cmd))

(define (simple-insert l x)
  (if (nlist? l) (list x)
      (list-union (list x) l)))

(define (simple-remove l x)
  (if (nlist? l) (list)
      (list-difference l (list x))))

(define (kbd-insert-key-binding conds key im)
  (let* ((com (kbd-source (car im)))
	 (cmd (if (string? com) com (object->string com))))
    ;;(display* "Binding '" key "' when " conds " to " com "\n")
    (kbd-delete-key-binding2 conds key)
    (kbd-set-map! key (ovl-insert (kbd-get-map key) im conds))
    (kbd-set-inv! com (ovl-insert (kbd-get-inv com) key conds))
    (kbd-set-rev! cmd (simple-insert (kbd-get-rev cmd) key))
    ;;(display* key " > " (kbd-get-map key) "\n")
    ;;(display* com " < " (kbd-get-inv com) "\n")
    ;;(display* cmd " < " (kbd-get-rev cmd) "\n")
    ))

(tm-define (kbd-delete-key-binding2 conds key)
  ;;(display* "Deleting binding '" key "' when " conds "\n")
  (with im (ovl-find (kbd-get-map key) conds)
    (if im
	(let* ((com (kbd-source (car im)))
	       (cmd (object->string com)))
	  (kbd-set-map! key (ovl-remove (kbd-get-map key) conds))
	  (kbd-set-inv! com (ovl-remove (kbd-get-inv com) conds))
	  (kbd-set-rev! cmd (simple-remove (kbd-get-inv cmd) key))))))

(tm-define (kbd-find-key-binding key)
  (:synopsis "Find the command associated to the keystroke @key")
  ;;(display* "Find binding '" key "'\n")
  (lazy-keyboard-force)
  (ovl-resolve (kbd-get-map key) #f))

(tm-define (kbd-find-inv-binding com)
  (:synopsis "Find keyboard binding for command @com")
  ;;(display* "Find inverse binding '" com "'\n")
  (lazy-keyboard-force)
  (with r (ovl-resolve (kbd-get-inv com) #f)
    (if r r "")))

(tm-define (kbd-find-rev-binding cmd)
  (:synopsis "Find modeless keyboard binding for command @cmd")
  ;;(display* "Find reverse binding '" com "'\n")
  (lazy-keyboard-force)
  (cond ((tree? cmd)
	 (kbd-find-rev-binding (tree->stree cmd)))
	((string? cmd)
	 (with l (kbd-get-rev (object->string (string->object cmd)))
	   (and l (nnull? l) (string? (car l)) (string->tmstring (car l)))))
	(else #f)))

(define (kbd-find-key-binding2 conds key)
  ;;(display* "Find binding '" key "' when " conds "\n")
  ;; FIXME: we really need an ovl-find which does mode inference
  (or (ovl-find (kbd-get-map key) conds)
      (ovl-find (kbd-get-map key) '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yet more subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-append prefix s1)
  (let* ((s2 (string-replace s1 " " ""))
	 (s3 (string-replace s2 "<" "<."))
	 (s4 (string-replace s3 ">" "<gtr>"))
	 (s5 (string-replace s4 "<." "<less>")))
    (string-append prefix s5)))

(define (kbd-sub-binding conds s prev-end end)
  (let* ((this-ss (substring s 0 end))
	 (this (kbd-find-key-binding2 conds this-ss)))
    (if (not this)
	(let* ((prev-ss (substring s 0 prev-end))
	       (prev (kbd-find-key-binding2 conds prev-ss)))
	  (if (and (list? prev) (= (length prev) 2)) (set! prev (car prev)))
	  (if (or (not prev) (nstring? prev)) (set! prev prev-ss))
	  (with im (kbd-append prev (substring s prev-end end))
	    (kbd-insert-key-binding conds this-ss (list im "")))))))

(define (kbd-sub-bindings-sub conds s prev-end end)
  (cond ((== end (string-length s)) (noop))
        ((== (string-ref s end) #\space)
	 (kbd-sub-binding conds s prev-end end)
	 (kbd-sub-bindings-sub conds s end (+ end 1)))
	(else (kbd-sub-bindings-sub conds s prev-end (+ end 1)))))

(define (kbd-sub-bindings conds s)
  (kbd-sub-bindings-sub conds s 0 0))

(tm-define (kbd-binding conds key2 cmd help)
  (:synopsis "Helper routine for kbd-map macro")
  ;;(display* conds ", " key2 ", " cmd ", " help "\n")
  (with key (kbd-pre-rewrite key2)
    (kbd-sub-bindings conds key)
    (kbd-insert-key-binding conds key (list cmd help))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-add-condition conds opt)
  (cond ((== (car opt) :mode) (conditions-insert conds 0 (cadr opt)))
	((== (car opt) :context)
	 (if (predicate-option? (cadr opt))
	     (conditions-insert conds 1 (cadr opt))
	     (with pred `(lambda (t) (match? t ',(cadr opt)))
	       (conditions-insert conds 1 pred))))
	((== (car opt) :inside)
	 (with pred `(lambda (t) (and (tm-compound? t)
				      (in? (tm-car t) ',(cdr opt))))
	   (conditions-insert conds 1 pred)))
	(else (texmacs-error "kbd-add-condition"
			     "Bad keyboard option ~S" opt))))

(define (kbd-map-one conds l)
  (if (not (and (pair? l) (string? (car l)) (pair? (cdr l))))
      (texmacs-error "kbd-map-pre-one" "Bad keymap in: ~S" l))
  (with (key action . opt) l
    (if (string? action)
	(with help (if (null? opt) "" (car opt))
	  `(kbd-binding (list ,@conds) ,key ,action ,help))
	`(kbd-binding (list ,@conds) ,key (lambda () ,action ,@opt) ""))))

(define (kbd-map-body conds l)
  (cond ((null? l) '())
	((symbol? (car l))
	 (kbd-map-body (list 0 (car l)) (cdr l)))
	((and (pair? (car l)) (== (caar l) :profile))
	 (if (not (has-look-and-feel? (cdar l))) '((noop))
	     (kbd-map-body conds (cdr l))))
	((and (pair? (car l)) (keyword? (caar l)))
	 (kbd-map-body (kbd-add-condition conds (car l)) (cdr l)))
	(else (map (lambda (x) (kbd-map-one conds x)) l))))

(tm-define-macro (kbd-map . l)
  (:synopsis "Add entries in @l to the keyboard mapping")
  `(begin ,@(kbd-map-body '() l)))

(define (kbd-remove-one conds key)
  `(kbd-delete-key-binding2 (list ,@conds) ,key))

(define (kbd-remove-body conds l)
  (cond ((null? l) '())
	((symbol? (car l))
	 (kbd-remove-body (list 0 (car l)) (cdr l)))
	((and (pair? (car l)) (keyword? (caar l)))
	 (kbd-remove-body (kbd-add-condition conds (car l)) (cdr l)))
	(else (map (lambda (x) (kbd-remove-one conds x)) l))))

(tm-define-macro (kbd-remove . l)
  (:synopsis "Remove entries in @l from keyboard mapping")
  `(begin ,@(kbd-remove-body '() l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard (backslashed) commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-command-table (make-ahash-table))

(define (kbd-set-command! key im)
  (ahash-set! kbd-command-table key im))

(tm-define (kbd-get-command key)
  (lazy-keyboard-force)
  (ahash-ref kbd-command-table key))

(tm-define (kbd-command-pre arg)
  (:synopsis "Helper routine for kbd-commands macro")
  (with (cmd help . action) arg
    (list cmd help (list 'unquote `(lambda () ,@action)))))

(tm-define (kbd-command arg)
  (:synopsis "Helper routine for kbd-commands macro")
  (with (cmd help action) arg
    (kbd-set-command! cmd (cons help action))))

(tm-define-macro (kbd-commands . l)
  (:synopsis "Add backslashed commands in @l to keyboard mapping")
  `(for-each kbd-command ,(list 'quasiquote (map kbd-command-pre l))))

(tm-define-macro (kbd-symbols . l)
  (:synopsis "Add symbols in @l to keyboard mapping")
  (define (fun s)
    (list s (string-append "insert#<" s ">")
	  (list 'insert (string-append "<" s ">"))))
  `(kbd-commands ,@(map fun l)))

(tm-define (emulate-keyboard k)
  (delayed (raw-emulate-keyboard k)))
