
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-modes.scm
;; DESCRIPTION : defining new TeXmacs modes and some frequently used modes
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-modes)
  (:use
    (kernel drd drd-rules) (kernel drd drd-query) (kernel drd drd-data)
    (kernel texmacs tm-plugins) (kernel texmacs tm-preferences))
  (:export
    texmacs-mode ;; for texmacs-modes macro
    texmacs-modes
    texmacs-in-mode? texmacs-submode?
    lazy-in-mode-do ;; for lazy-in-mode macro
    lazy-in-mode lazy-in-mode-force
    ;; general texmacs modes
    always? in-text? in-math? in-prog? in-math-not-hybrid?
    in-table? in-io? in-session? not-in-session? in-math-in-session?
    in-math-not-in-session? in-plugin-with-converters?
    ;; language related modes
    in-cyrillic?
    in-czech? in-dutch? in-english? in-finnish? in-french?
    in-german? in-hungarian? in-italian? in-polish?
    in-portugese? in-romanian? in-russian? in-slovene?
    in-spanish? in-swedish? in-ukrainian?
    ;; keyboard related modes
    like-emacs? like-windows? like-old? like-old-text? like-old-math?
    in-cyrillic-cp1251? in-cyrillic-jcuken? in-cyrillic-koi8?
    in-cyrillic-translit? in-cyrillic-yawerty?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining new modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-mode-pred mode)
  (let* ((mode-str (symbol->string mode))
	 (mode-root (substring mode-str 0 (- (string-length mode-str) 1)))
	 (pred-str (string-append mode-root "?")))
    (string->symbol pred-str)))

(define (texmacs-mode item)
  (with (mode action . deps) item
    (let* ((pred (texmacs-mode-pred mode))
	   (deps* (map list (map texmacs-mode-pred deps)))
	   (l (if (== action #t) deps* (cons action deps*)))
	   (test (if (null? l) #t (if (null? (cdr l)) (car l) (cons 'and l))))
	   (defn `(define (,pred) ,test))
	   (rules (map (lambda (dep) (list dep mode)) deps))
	   (drd-cmd `(drd-rules ,@rules))
	   (arch1 `(set-symbol-procedure! ',mode ,pred))
	   (arch2 `(set-symbol-procedure! ',pred ,pred)))
      (if (null? deps)
	  (list 'begin defn arch1 arch2)
	  (list 'begin defn arch1 arch2 drd-cmd)))))

(define-macro (texmacs-modes . l)
  `(begin ,@(map texmacs-mode l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-in-mode? mode)
  (with proc (symbol-procedure mode)
    (if proc (proc)
	(catch #t (lambda () (eval (list mode))) (lambda err #f)))))

(define (texmacs-mode-mode pred)
  (let* ((pred-str (symbol->string pred))
	 (pred-root (substring pred-str 0 (- (string-length pred-str) 1)))
	 (mode-str (string-append pred-root "%")))
    (string->symbol mode-str)))

(define texmacs-submode-table (make-ahash-table))

(define (texmacs-submode? what* of*)
  (let* ((key (cons what* of*))
	 (handle (ahash-get-handle texmacs-submode-table key)))
    (if handle (cdr handle)
	(let* ((what (texmacs-mode-mode what*))
	       (of (texmacs-mode-mode of*))
	       (result (or (== of 'always%) (not (null? (query of what))))))
	  (ahash-set! texmacs-submode-table key result)
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy mode dependent actions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lazy-in-mode-list '())

(define (lazy-in-mode-do module mode*)
  (with mode (texmacs-mode-mode mode*)
    (set! lazy-in-mode-list (acons mode module lazy-in-mode-list))))

(define-macro (lazy-in-mode module . modes)
  (for-each (lambda (mode) (lazy-in-mode-do module mode)) modes)
  '(noop))

(define (lazy-in-mode-force-do l)
  (cond ((null? l) l)
	((texmacs-in-mode? (caar l))
	 (module-load (cdar l))
	 (lazy-in-mode-force-do (cdr l)))
	(else (cons (car l) (lazy-in-mode-force-do (cdr l))))))

(define (lazy-in-mode-force)
  (set! lazy-in-mode-list
	(reverse (lazy-in-mode-force-do (reverse lazy-in-mode-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (always% #t)
  (in-text% (and (== (get-env "mode") "text") (not (in-graphics?))))
  (in-math% (and (== (get-env "mode") "math") (not (in-graphics?))))
  (in-prog% (and (== (get-env "mode") "prog") (not (in-graphics?))))
  (in-math-not-hybrid% (not (inside? "hybrid")) in-math%)
  (in-table% (and (inside? "table") (not (in-graphics?))))
  (in-io% (and (or (inside? "input") (inside? "output")) (not (in-graphics?))))
  (in-session% (inside? "session"))
  (not-in-session% (not (inside? "session")))
  (in-math-in-session% #t in-math% in-session%)
  (in-math-not-in-session% #t in-math% not-in-session%)
  (in-plugin-with-converters%
   (plugin-supports-math-input-ref (get-env "prog-language"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-cyrillic% (in? (get-env "language") '("russian" "ukrainian")) in-text%)
  (in-czech% (== (get-env "language") "czech") in-text%)
  (in-dutch% (== (get-env "language") "dutch") in-text%)
  (in-english% (== (get-env "language") "english") in-text%)
  (in-finnish% (== (get-env "language") "finnish") in-text%)
  (in-french% (== (get-env "language") "french") in-text%)
  (in-german% (== (get-env "language") "german") in-text%)
  (in-hungarian% (== (get-env "language") "german") in-text%)
  (in-italian% (== (get-env "language") "italian") in-text%)
  (in-polish% (== (get-env "language") "polish") in-text%)
  (in-portugese% (== (get-env "language") "portugese") in-text%)
  (in-romanian% (== (get-env "language") "romanian") in-text%)
  (in-russian% (== (get-env "language") "russian") in-cyrillic%)
  (in-slovene% (== (get-env "language") "slovene") in-text%)
  (in-spanish% (== (get-env "language") "spanish") in-text%)
  (in-swedish% (== (get-env "language") "swedish") in-text%)
  (in-ukrainian% (== (get-env "language") "ukrainian") in-cyrillic%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cyrillic-input-method? what)
  (== (get-preference "cyrillic input method") what))

(texmacs-modes
  (like-emacs% (== (get-preference "look and feel") "emacs"))
  (like-windows% (== (get-preference "look and feel") "windows"))
  (like-old% (== (get-preference "look and feel") "old style"))
  (like-old-text% #t like-old% in-text%)
  (like-old-math% #t like-old% in-math%)
  (in-cyrillic-cp1251% (cyrillic-input-method? "cp1251") in-cyrillic%)
  (in-cyrillic-jcuken% (cyrillic-input-method? "jcuken") in-cyrillic%)
  (in-cyrillic-koi8% (cyrillic-input-method? "koi8") in-cyrillic%)
  (in-cyrillic-translit% (cyrillic-input-method? "translit") in-cyrillic%)
  (in-cyrillic-yawerty% (cyrillic-input-method? "yawerty") in-cyrillic%))
