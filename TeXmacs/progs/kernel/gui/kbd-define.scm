
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : kbd-define.scm
;; DESCRIPTION : Definition of keyboard shortcuts/wildcards
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui kbd-define)
  (:use (kernel gui menu-define))
  (:export
    ;; exported macros
    kbd-wildcards-body ;; for kbd-wildcards macro
    kbd-wildcards
    kbd-binding kbd-map-pre ;; for kbd-map macro
    kbd-map
    kbd-remove-body ;; for kbd-remove macro
    kbd-remove
    kbd-command-pre kbd-command ;; for kbd-commands macro
    kbd-commands kbd-symbols
    ;; other exported routines
    kbd-find-inv-binding ;; for menu-widget
    kbd-get-command kbd-find-key-binding))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard wildcards
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-wildcards-sub l post)
  (if (not (null? l))
      (let* ((w (car l))
	     (key (car w))
	     (im (cadr w))
	     (left (if (>= (length w) 3) (caddr w) #f))
	     (right (if (>= (length w) 4) (cadddr w) #t)))
	(insert-kbd-wildcard key im post left right)
	(kbd-wildcards-sub (cdr l) post))))

(define (kbd-wildcards-body l)
  (cond ((null? l) (noop))
	((== (car l) 'pre) (kbd-wildcards-sub (cdr l) #f))
	((== (car l) 'post) (kbd-wildcards-sub (cdr l) #t))
	(else (kbd-wildcards-sub l #t))))

(define-macro (kbd-wildcards . l)
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

(define (kbd-insert-key-binding pred key im)
  ;(display* "Binding '" key "' when " pred " to " im "\n")
  (with com (kbd-source (car im))
    (if (not (kbd-get-map key)) (kbd-set-map! key '()))
    (if (not (kbd-get-inv com)) (kbd-set-inv! com '()))
    (kbd-delete-key-binding2 pred key)
    (kbd-set-map! key (cons (list pred im) (kbd-get-map key)))
    (kbd-set-inv! com (cons (list pred key) (kbd-get-inv com)))
    ;(display* key ": " (kbd-get-map key) "\n")
    ;(display* com "] " (kbd-get-inv com) "\n")
    ))

(define (kbd-delete-key-binding key)
  ;(display* "Deleting binding '" key "'\n")
  (define (remove! t)
    (kbd-delete-inv-binding (list (car t) key) (kbd-source (cadr t))))
  (with l (kbd-get-map key)
    (if l (for-each remove! l))
    (kbd-remove-map! key)))

(define (kbd-delete-key-binding2 pred key)
  ;(display* "Deleting binding '" key "' when " pred "\n")
  (define (test? t) (== (car t) pred))
  (define (remove! t)
    (kbd-delete-inv-binding (list pred key) (kbd-source (cadr t))))
  (with l (kbd-get-map key)
    (if l (receive (in out) (list-partition l test?)
	    (for-each remove! in)
	    (kbd-set-map! key out)))))

(define (kbd-delete-inv-binding what com)
  ;(display* "Deleting inverse binding '" com "' for " what "\n")
  (with l (kbd-get-inv com)
    (if l (kbd-set-inv! com (list-filter l (lambda (x) (== x what)))))))

(define (kbd-find-sub l)
  (cond ((not (pair? l)) (values #f #f))
	((texmacs-in-mode? (caar l))
	 (receive (mode im) (kbd-find-sub (cdr l))
	   (cond ((not im) (values (caar l) (cadar l)))
	         ((texmacs-submode? mode (caar l)) (values mode im))
	         ((texmacs-submode? (caar l) mode) (values (caar l) (cadar l)))
		 (else (values mode im)))))
	(else (kbd-find-sub (cdr l)))))

(define (kbd-find-key-binding key)
  ;(display* "Find binding '" key "'\n")
  (receive (mode im) (kbd-find-sub (kbd-get-map key))
    im))

(define (kbd-find-inv-binding com)
  ;(display* "Find inverse binding '" com "'\n")
  (receive (mode im) (kbd-find-sub (kbd-get-inv com))
    (if im im "")))

(define (kbd-find-sub2 pred l)
  (cond ((not (pair? l)) #f)
	((texmacs-submode? pred (caar l)) (cadar l))
	(else (kbd-find-sub2 pred (cdr l)))))

(define (kbd-find-key-binding2 pred key)
  ;(display* "Find binding '" key "' when " pred "\n")
  (kbd-find-sub2 pred (kbd-get-map key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Yet more subroutines for the definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-append prefix s1)
  (let* ((s2 (string-replace s1 " " ""))
	 (s3 (string-replace s2 "<" "<."))
	 (s4 (string-replace s3 ">" "<gtr>"))
	 (s5 (string-replace s4 "<." "<less>")))
    (string-append prefix s5)))

(define (kbd-sub-binding pred s prev-end end)
  (let* ((this-ss (substring s 0 end))
	 (this (kbd-find-key-binding2 pred this-ss)))
    (if (not this)
	(let* ((prev-ss (substring s 0 prev-end))
	       (prev (kbd-find-key-binding2 pred prev-ss)))
	  (if (and (list? prev) (= (length prev) 2)) (set! prev (car prev)))
	  (if (or (not prev) (not (string? prev))) (set! prev prev-ss))
	  (with im (kbd-append prev (substring s prev-end end))
	    (kbd-insert-key-binding pred this-ss (list im "")))))))

(define (kbd-sub-bindings-sub pred s prev-end end)
  (cond ((== end (string-length s)) (noop))
        ((== (string-ref s end) #\space)
	 (kbd-sub-binding pred s prev-end end)
	 (kbd-sub-bindings-sub pred s end (+ end 1)))
	(else (kbd-sub-bindings-sub pred s prev-end (+ end 1)))))

(define (kbd-sub-bindings pred s)
  (kbd-sub-bindings-sub pred s 0 0))

(define (kbd-binding pred key2 cmd help)
  (with key (kbd-pre-rewrite key2)
    (kbd-sub-bindings pred key)
    (kbd-insert-key-binding pred key (list cmd help))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (kbd-map-pre-one pred2 l)
  (if (not (and (or (string? pred2) (symbol? pred2))
		(pair? l) (string? (car l)) (pair? (cdr l))))
      (texmacs-error "kbd-map-pre-one" "Bad keymap in: ~S" l))
  (with pred (if (symbol? pred2) pred2 (string->symbol pred2))
    (with (key action . opt) l
      (cond ((string? action)
	     (with help (if (null? opt) "" (car opt))
	       (list pred key action help)))
	    ((null? opt) (list pred key (make-promise action) ""))
	    (else (with action* `(begin ,action ,@opt)
		    (list pred key (make-promise action*) "")))))))

(define (kbd-map-pre-list pred l)
  (map (lambda (x) (kbd-map-pre-one pred x)) l))

(define (kbd-map-pre l)
  (cond ((null? l) '())
	((symbol? (car l)) (kbd-map-pre-list (car l) (cdr l)))
	((and (list? (car l)) (not (null? (car l))) (== (caar l) :or))
	 (with sub (lambda (pred) (kbd-map-pre-list pred (cdr l)))
	   (apply append (map sub (cdar l)))))
	(else (kbd-map-pre-list 'always? l))))

(define-macro (kbd-map . l)
  `(for-each (lambda (x) (apply kbd-binding x))
	     ,(list 'quasiquote (kbd-map-pre l))))

(define (kbd-remove-list pred l)
  (for-each (lambda (x) (kbd-delete-key-binding2 pred x)) l))

(define (kbd-remove-body l)
  (cond ((null? l) (noop))
	((symbol? (car l)) (kbd-remove-list (car l) (cdr l)))
	((and (list? (car l)) (not (null? (car l))) (== (caar l) :or))
	 (for-each (lambda (pred) (kbd-remove-list pred (cdr l))) (cdar l)))
	(else (kbd-remove-list 'always? l))))

(define-macro (kbd-remove . l)
  `(kbd-remove-body ,(list 'quasiquote l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Definition of keyboard (backslashed) commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define kbd-command-table (make-ahash-table))
(define (kbd-set-command! key im) (ahash-set! kbd-command-table key im))
(define (kbd-get-command key) (ahash-ref kbd-command-table key))

(define (kbd-command-pre arg)
  (with (cmd help . action) arg
    (list cmd help (list 'unquote `(lambda () ,@action)))))

(define (kbd-command arg)
  (with (cmd help action) arg
    (kbd-set-command! cmd (cons help action))))

(define-macro (kbd-commands . l)
  `(for-each kbd-command ,(list 'quasiquote (map kbd-command-pre l))))

(define-macro (kbd-symbols . l)
  (define (fun s)
    (list s (string-append "insert#<" s ">")
	  (list 'insert (string-append "<" s ">"))))
  `(kbd-commands ,@(map fun l)))
