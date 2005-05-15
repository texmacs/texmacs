
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-overload.scm
;; DESCRIPTION : Contextual overloading for functions and data
;; COPYRIGHT   : (C) 2005  Joris van der Hoeven
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In TeXmacs, "contextual overloading" is done sequentially on
;; several "kinds" of conditions: mode, context and patterns.
;;   Mode) A predicate with no arguments, corresponding to a TeXmacs mode.
;;   Context) A predicate with a tree argument or a pattern.
;;     It is called outwards for the in-most tree at the cursor position
;;     until we find a match or we attain the root of the document.
;;   Match) A predicate or pattern which has to be matched by
;;     the arguments to the function call.
;; The last kind of condition "match" admits an accelerated variant "case",
;; which allows fast dispatching on the tag of the first argument.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-overload))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Construction of overloaded structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (conditions-insert l kind opt)
  (cond ((null? l) (list kind opt))
	((== kind (car l))
	 (texmacs-error "conditions-insert" "Conflicting option"))
	((< kind (car l)) (cons kind (cons opt l)))
	(else (cons (car l)
		    (cons (cadr l)
			  (conditions-insert (cddr l) kind opt))))))

(define (assoc-set l key val)
  (cond ((or (not l) (null? l)) (list (cons key val)))
	((== (caar l) key) (cons (cons key val) (cdr l)))
	(else (cons (car l) (assoc-set (cdr l) key val)))))

(define (ahash-set t key val)
  (if (not t) (set! t (make-ahash-table)))
  (if (vector? key)
      (for-each (lambda (x) (ahash-set! t x val)) (vector->list key))
      (ahash-set! t key val)) ;; a bit dangerous, because destructive...
  t)

(define ovl-setter
  (vector assoc-set ; mode
	  assoc-set ; context
	  ahash-set ; case
	  assoc-set ; match
	  ))

(define ovl-getter
  (vector assoc-ref ; mode
	  assoc-ref ; context
	  ahash-ref ; case
	  assoc-ref ; match
	  ))

(define ovl-always
  (vector always?   ; mode
	  root?     ; context
	  #t        ; case
	  true?     ; match
	  ))

(define-public (ovl-insert ovl data conds)
  "Insert @data under conditions @conds in overloaded structure @ovl"
  ;; ovl:   overloaded structure of the form kind . contents (or #f),
  ;;        where kind is the kind of condition and
  ;;        contents the corresponding information
  ;; data:  the overloaded data we want to insert
  ;; conds: the conditions under which the data are valid
  ;;        this is a list of the form (kind_1 cond_1 ... kind_n cond_n)
  ;;        with kind_1 < ... < kind_n
  (cond ((null? conds)
	 (if (or (not ovl) (= (car ovl) 100)) (cons 100 data)
	     (let* ((kind (car ovl))
		    (set (vector-ref ovl-setter kind))
		    (get (vector-ref ovl-getter kind))
		    (always (vector-ref ovl-always kind))
		    (old (get (cdr ovl) always))
		    (new (ovl-insert old data conds)))
	       (cons kind (set (cdr ovl) always new)))))
	((not ovl)
	 (let* ((key (cadr conds))
		(kind (car conds))
		(set (vector-ref ovl-setter kind))
		(new (ovl-insert #f data (cddr conds))))
	   (cons kind (set #f key new))))
	((== (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(old (get (cdr ovl) key))
		(new (ovl-insert old data (cddr conds))))
	   (cons kind (set (cdr ovl) key new))))
	((> (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(cont (set #f (vector-ref ovl-always kind) ovl))
		(old (get cont key))
		(new (ovl-insert old data (cddr conds))))
	   (cons kind (set cont key new))))
	((< (car ovl) (car conds))
	 (let* ((kind (car ovl))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(always (vector-ref ovl-always kind))
		(old (get (cdr ovl) always))
		(new (ovl-insert old data conds)))
	   (cons kind (set (cdr ovl) always new))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Finding and removing data associated to specific conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (ovl-find ovl conds)
  "Find entry under the conditions @conds from overloaded structure @ovl"
  (cond ((not ovl) #f)
	((null? conds)
	 (if (= (car ovl) 100) (cdr ovl)
	     (let* ((kind (car ovl))
		    (get (vector-ref ovl-getter kind))
		    (always (vector-ref ovl-always kind))
		    (sub (get (cdr ovl) always)))
	       (ovl-find sub (cddr conds)))))
	((== (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(get (vector-ref ovl-getter kind))
		(sub (get (cdr ovl) key)))
	   (ovl-find sub (cddr conds))))
	((> (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(always (vector-ref ovl-always kind)))
	   (if (== key always) (ovl-find ovl (cddr conds)) #f)))
	((< (car ovl) (car conds))
	 (let* ((kind (car ovl))
		(get (vector-ref ovl-getter kind))
		(always (vector-ref ovl-always kind))
		(sub (get (cdr ovl) always)))
	   (ovl-find sub (cddr conds))))))

(define-public (ovl-remove ovl conds)
  "Remove entry under conditions @conds in overloaded structure @ovl"
  (cond ((not ovl) ovl)
	((null? conds)
	 (if (= (car ovl) 100) #f
	     (let* ((kind (car ovl))
		    (set (vector-ref ovl-setter kind))
		    (get (vector-ref ovl-getter kind))
		    (always (vector-ref ovl-always kind))
		    (old (get (cdr ovl) always))
		    (new (ovl-remove old conds)))
	       (cons kind (set (cdr ovl) always new)))))
	((== (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(old (get (cdr ovl) key))
		(new (ovl-remove old (cddr conds))))
	   (cons kind (set (cdr ovl) key new))))
	((> (car ovl) (car conds))
	 (let* ((key (cadr conds))
		(kind (car conds))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(cont (set #f (vector-ref ovl-always kind) ovl))
		(old (get cont key))
		(new (ovl-remove old (cddr conds))))
	   (cons kind (set cont key new))))
	((< (car ovl) (car conds))
	 (let* ((kind (car ovl))
		(set (vector-ref ovl-setter kind))
		(get (vector-ref ovl-getter kind))
		(always (vector-ref ovl-always kind))
		(old (get (cdr ovl) always))
		(new (ovl-remove old conds)))
	   (cons kind (set (cdr ovl) always new))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resolve overloaded retrieval and function applications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mode-based

(define (ovl-mode-resolve-sub ovl args)
  (cond ((null? ovl) (values #f #f))
	(((caar ovl))
	 (let* ((match (ovl-resolve (cdar ovl) args))
		(key (caar ovl)))
	   (receive (match2 key2) (ovl-mode-resolve-sub (cdr ovl) args)
	     (cond ((not match) (values match2 key2))
		   ((not match2) (values match key))
		   ((texmacs-submode? key key2) (values match key))
		   (else (values match2 key2))))))
	(else (ovl-mode-resolve-sub (cdr ovl) args))))

(define (ovl-mode-resolve ovl args)
  (receive (match key) (ovl-mode-resolve-sub ovl args)
    match))

;; Context-based

(define (ovl-context-resolve-sub ovl args t)
  (cond ((null? ovl) (values #f #f))
	(((caar ovl) t)
	 (let* ((match (ovl-resolve (cdar ovl) args))
		(key (caar ovl)))
	   (receive
	       (match2 key2)
	       (ovl-context-resolve-sub (cdr ovl) args t)
	     (cond ((not match) (values match2 key2))
		   ((not match2) (values match key))
		   ((== key2 root?) (values match key))
		   (else (values match2 key2))))))
	(else (ovl-context-resolve-sub (cdr ovl) args t))))

(define (ovl-context-resolve-path ovl args path)
  (receive (match key) (ovl-context-resolve-sub ovl args (tm-subtree path))
    (cond (match match)
	  ((or (== path (the-buffer-path)) (null? path)) #f)
	  (else (ovl-context-resolve-path ovl args (cDr path))))))

(define (ovl-context-resolve ovl args)
  (let* ((p (cDr (the-path)))
	 (t (tm-subtree p))
	 (q (if (tm-compound? t) (cDr p) p)))
    (ovl-context-resolve-path ovl args q)))

;; Case-based

(define (ovl-case-resolve ovl args)
  (cond ((not ovl) #f)
	((and (pair? args) (tm-compound? (car args)))
	 (with r (ovl-resolve (ahash-ref ovl (tm-car (car args))) args)
	   (if r r (ovl-resolve (ahash-ref ovl #t) args))))
	(else (ovl-resolve (ahash-ref ovl #t) args))))

;; Match-based

(define (ovl-match-resolve-sub ovl args)
  (cond ((null? ovl) (values #f #f))
	((apply (caar ovl) args)
	 (let* ((match (ovl-resolve (cdar ovl) args))
		(key (caar ovl)))
	   (receive (match2 key2) (ovl-match-resolve-sub (cdr ovl) args)
	     (cond ((not match) (values match2 key2))
		   ((not match2) (values match key))
		   ((== key2 true?) (values match key))
		   (else (values match2 key2))))))
	(else (ovl-match-resolve-sub (cdr ovl) args))))

(define (ovl-match-resolve ovl args)
  (receive (match key) (ovl-match-resolve-sub ovl args)
    match))

;; General case

(define ovl-resolver
  (vector ovl-mode-resolve
	  ovl-context-resolve
	  ovl-case-resolve
	  ovl-match-resolve))

(define-public (ovl-resolve ovl args)
  "Get method for arguments @args in overloaded structure @ovl"
  (cond ((not ovl) #f)
	((== (car ovl) 100) (cdr ovl))
	(else ((vector-ref ovl-resolver (car ovl)) (cdr ovl) args))))

(define-public (ovl-apply ovl args)
  "Apply appropriate overloaded method from @ovl to @args"
  (with fun (ovl-resolve ovl args)
    (if fun
	(apply fun args)
	(texmacs-error "ovl-apply"
		       "Conditions of overloaded function cannot be met"))))
