
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

(texmacs-module (kernel gui kbd-define))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy keyboard bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-keyboard-waiting '())
(define-public lazy-keyboard-done (make-ahash-table))

(define-public (lazy-keyboard-do module mode*)
  (with mode (texmacs-mode-mode mode*)
    (set! lazy-keyboard-waiting (acons mode module lazy-keyboard-waiting))))

(define-public-macro (lazy-keyboard module . modes)
  (for-each (lambda (mode) (lazy-keyboard-do module mode)) modes)
  `(delayed
     (:idle 250)
     (ahash-set! lazy-keyboard-done ',module #t)
     (import-from ,module)))

(define (lazy-keyboard-force-do l)
  (cond ((null? l) l)
	((ahash-ref lazy-keyboard-done (cdar l))
	 (lazy-keyboard-force-do (cdr l)))
	((texmacs-in-mode? (caar l))
	 (module-load (cdar l))
	 (ahash-set! lazy-keyboard-done (cdar l) #t)
	 (lazy-keyboard-force-do (cdr l)))
	(else (cons (car l) (lazy-keyboard-force-do (cdr l))))))

(define-public (lazy-keyboard-force)
  (set! lazy-keyboard-waiting
	(reverse (lazy-keyboard-force-do (reverse lazy-keyboard-waiting)))))

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

(define-public (kbd-wildcards-body l)
  "Helper routine for kbd-wildcards macro"
  (cond ((null? l) (noop))
	((== (car l) 'pre) (kbd-wildcards-sub (cdr l) #f))
	((== (car l) 'post) (kbd-wildcards-sub (cdr l) #t))
	(else (kbd-wildcards-sub l #t))))

(define-public-macro (kbd-wildcards . l)
  "Add entries in @l to the keyboard wildcard table"
  `(kbd-wildcards-body ,(list 'quasiquote l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-map-table (make-ahash-table))
(define kbd-inv-table (make-ahash-table))
(define (kbd-set-map! key im) (ahash-set! kbd-map-table key im))
(define (kbd-set-inv! key im) (ahash-set! kbd-inv-table key im))
(define (kbd-get-map key) (ahash-ref kbd-map-table key))
(define (kbd-get-inv key) (ahash-ref kbd-inv-table key))
(define (kbd-remove-map! key) (ahash-remove! kbd-map-table key))

(define (kbd-source cmd)
  (if (procedure? cmd) (promise-source cmd) cmd))

(define (kbd-insert-key-binding conds key im)
  ;;(display* "Binding '" key "' when " conds " to " im "\n")
  (with com (kbd-source (car im))
    (kbd-delete-key-binding2 conds key)
    (kbd-set-map! key (ovl-insert (kbd-get-map key) im conds))
    (kbd-set-inv! com (ovl-insert (kbd-get-inv com) key conds))
    ;;(display* key ": " (kbd-get-map key) "\n")
    ;;(display* com "] " (kbd-get-inv com) "\n")
    ))

(define-public (kbd-delete-key-binding2 conds key)
  ;;(display* "Deleting binding '" key "' when " conds "\n")
  (with im (ovl-find (kbd-get-map key) conds)
    (if im
	(with com (kbd-source (car im))
	  (kbd-set-map! key (ovl-remove (kbd-get-map key) conds))
	  (kbd-set-inv! com (ovl-remove (kbd-get-inv com) conds))))))

(define-public (kbd-find-key-binding key)
  "Find the command associated to the keystroke @key"
  ;;(display* "Find binding '" key "'\n")
  (lazy-keyboard-force)
  (ovl-resolve (kbd-get-map key) #f))

(define-public (kbd-find-inv-binding com)
  "Find keyboard binding for command @com"
  ;;(display* "Find inverse binding '" com "'\n")
  (lazy-keyboard-force)
  (with r (ovl-resolve (kbd-get-inv com) #f)
    (if r r "")))

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

(define-public (kbd-binding conds key2 cmd help)
  "Helper routine for kbd-map macro"
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
	((and (pair? (car l)) (keyword? (caar l)))
	 (kbd-map-body (kbd-add-condition conds (car l)) (cdr l)))
	(else (map (lambda (x) (kbd-map-one conds x)) l))))

(define-public-macro (kbd-map . l)
  "Add entries in @l to the keyboard mapping"
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

(define-public-macro (kbd-remove . l)
  "Remove entries in @l from keyboard mapping"
  `(begin ,@(kbd-remove-body '() l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard (backslashed) commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-command-table (make-ahash-table))

(define (kbd-set-command! key im)
  (ahash-set! kbd-command-table key im))

(define-public (kbd-get-command key)
  (lazy-keyboard-force)
  (ahash-ref kbd-command-table key))

(define-public (kbd-command-pre arg)
  "Helper routine for kbd-commands macro"
  (with (cmd help . action) arg
    (list cmd help (list 'unquote `(lambda () ,@action)))))

(define-public (kbd-command arg)
  "Helper routine for kbd-commands macro"
  (with (cmd help action) arg
    (kbd-set-command! cmd (cons help action))))

(define-public-macro (kbd-commands . l)
  "Add backslashed commands in @l to keyboard mapping"
  `(for-each kbd-command ,(list 'quasiquote (map kbd-command-pre l))))

(define-public-macro (kbd-symbols . l)
  "Add symbols in @l to keyboard mapping"
  (define (fun s)
    (list s (string-append "insert#<" s ">")
	  (list 'insert (string-append "<" s ">"))))
  `(kbd-commands ,@(map fun l)))

(define-public (emulate-keyboard k)
  (delayed (raw-emulate-keyboard k)))
