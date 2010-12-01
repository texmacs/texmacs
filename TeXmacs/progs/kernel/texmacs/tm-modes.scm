
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-modes.scm
;; DESCRIPTION : defining new TeXmacs modes and some frequently used modes
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel texmacs tm-modes)
  (:use
    (kernel drd drd-rules) (kernel drd drd-query) (kernel drd drd-data)
    (kernel texmacs tm-plugins) (kernel texmacs tm-preferences)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defining new modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (texmacs-mode-pred mode)
  (let* ((mode-str (symbol->string mode))
	 (mode-root (substring mode-str 0 (- (string-length mode-str) 1)))
	 (pred-str (string-append mode-root "?")))
    (string->symbol pred-str)))

(define-public (texmacs-mode item)
  (with (mode action . deps) item
    (let* ((pred (texmacs-mode-pred mode))
	   (deps* (map list (map texmacs-mode-pred deps)))
	   (l (if (== action #t) deps* (cons action deps*)))
	   (test (if (null? l) #t (if (null? (cdr l)) (car l) (cons 'and l))))
	   (defn `(define-public (,pred) ,test))
	   (rules (map (lambda (dep) (list dep mode)) deps))
	   (drd-cmd `(drd-rules ,@rules))
	   (arch1 `(set-symbol-procedure! ',mode ,pred))
	   (arch2 `(set-symbol-procedure! ',pred ,pred)))
      (if (== mode 'always%) (set! defn '(noop)))
      (if (null? deps)
	  (list 'begin defn arch1 arch2)
	  (list 'begin defn arch1 arch2 drd-cmd)))))

(define-public-macro (texmacs-modes . l)
  `(begin
     (set! temp-module ,(current-module))
     (set-current-module texmacs-user)
     ,@(map texmacs-mode l)
     (set-current-module temp-module)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (texmacs-in-mode? mode)
  (with proc (symbol-procedure mode)
    (if proc (proc)
	(catch #t (lambda () (eval (list mode))) (lambda err #f)))))

(define-public (texmacs-mode-mode pred)
  "Get drd predicate name associated to scheme predicate or symbol"
  (if (procedure? pred)
      (with name (procedure-name pred)
	(if name (texmacs-mode-mode name) 'unknown%))
      (let* ((pred-str (symbol->string pred))
	     (pred-root (substring pred-str 0 (- (string-length pred-str) 1)))
	     (mode-str (string-append pred-root "%")))
	(string->symbol mode-str))))

(define texmacs-submode-table (make-ahash-table))

(define-public (texmacs-submode? what* of*)
  "Test whether @what* is a sub-mode of @of*"
  (let* ((key (cons what* of*))
	 (handle (ahash-get-handle texmacs-submode-table key)))
    (if handle (cdr handle)
	(let* ((what (texmacs-mode-mode what*))
	       (of (texmacs-mode-mode of*))
	       (result (or (== of 'always%)
			   (== what 'prevail%)
			   (nnull? (query of what)))))
	  (ahash-set! texmacs-submode-table key result)
	  result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking whether certain features are supported
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public (supports-chinese?)
  (!= (default-chinese-font) "roman"))

(define-public (supports-japanese?)
  (!= (default-japanese-font) "roman"))

(define-public (supports-korean?)
  (!= (default-korean-font) "roman"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mode related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (always% #t)
  (prevail% #t)
  (in-source% (== (get-env "mode") "src"))
  (in-text% (and (== (get-env "mode") "text") (not (in-graphics?))))
  (in-math% (and (== (get-env "mode") "math") (not (in-graphics?))))
  (in-prog% (and (== (get-env "mode") "prog") (not (in-graphics?))))
  (in-math-not-hybrid% (not (inside? 'hybrid)) in-math%)
  (in-table% (and (inside? 'table) (not (in-graphics?))))
  (in-session% (and (inside? 'session) (not (in-graphics?))))
  (in-session% (inside? 'session))
  (not-in-session% (not (inside? 'session)))
  (in-math-in-session% #t in-math% in-session%)
  (in-math-not-in-session% #t in-math% not-in-session%)
  (in-std% (style-has? "std-dtd"))
  (in-std-text% #t in-text% in-std%)
  (in-tmdoc% (style-has? "tmdoc-style"))
  (in-mmxdoc% (style-has? "mmxdoc-style") in-tmdoc%)
  (in-plugin-with-converters%
   (plugin-supports-math-input-ref (get-env "prog-language")))
  (in-screens% (inside? 'screens))
  (with-any-selection% (selection-active-any?))
  (with-active-selection% (selection-active-normal?))
  (in-scheme% (== (get-env "prog-language") "scheme"))
  (in-prog-scheme% #t in-prog% in-scheme%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-cyrillic% (in? (get-env "language")
		     '("bulgarian" "russian" "ukrainian")) in-text%)
  (in-oriental% (in? (get-env "language")
		     '("chinese" "japanese" "korean" "taiwanese")) in-text%)
  (in-bulgarian% (== (get-env "language") "bulgarian") in-cyrillic%)
  (in-chinese% (== (get-env "language") "chinese") in-oriental%)
  (in-czech% (== (get-env "language") "czech") in-text%)
  (in-danish% (== (get-env "language") "danish") in-text%)
  (in-dutch% (== (get-env "language") "dutch") in-text%)
  (in-english% (== (get-env "language") "english") in-text%)
  (in-finnish% (== (get-env "language") "finnish") in-text%)
  (in-french% (== (get-env "language") "french") in-text%)
  (in-german% (== (get-env "language") "german") in-text%)
  (in-hungarian% (== (get-env "language") "hungarian") in-text%)
  (in-italian% (== (get-env "language") "italian") in-text%)
  (in-japanese% (== (get-env "language") "japanese") in-oriental%)
  (in-korean% (== (get-env "language") "korean") in-oriental%)
  (in-polish% (== (get-env "language") "polish") in-text%)
  (in-portugese% (== (get-env "language") "portugese") in-text%)
  (in-romanian% (== (get-env "language") "romanian") in-text%)
  (in-russian% (== (get-env "language") "russian") in-cyrillic%)
  (in-slovene% (== (get-env "language") "slovene") in-text%)
  (in-spanish% (== (get-env "language") "spanish") in-text%)
  (in-swedish% (== (get-env "language") "swedish") in-text%)
  (in-taiwanese% (== (get-env "language") "taiwanese") in-oriental%)
  (in-ukrainian% (== (get-env "language") "ukrainian") in-cyrillic%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cyrillic-input-method? what)
  (== (get-preference "cyrillic input method") what))

(texmacs-modes
  (like-emacs% (has-look-and-feel? "emacs"))
  (like-gnome% (has-look-and-feel? "gnome"))
  (like-kde% (has-look-and-feel? "kde"))
  (like-macos% (has-look-and-feel? "macos"))
  (like-windows% (has-look-and-feel? "windows"))
  (like-std% (has-look-and-feel? "std"))
  (simple-menus% (== (get-preference "detailed menus") "simple"))
  (detailed-menus% (== (get-preference "detailed menus") "detailed"))
  (with-debugging-tool% (== (get-preference "debugging tool") "on"))
  (with-linking-tool% (== (get-preference "linking tool") "on"))
  (with-source-tool% (== (get-preference "source tool") "on"))
  (with-versioning-tool% (== (get-preference "versioning tool") "on"))
  (with-remote-connections% (== (get-preference "remote connections") "on"))
  (search-mode% (== (get-input-mode) 1))
  (replace-mode% (== (get-input-mode) 2))
  (spell-mode% (== (get-input-mode) 3))
  (complete-mode% (== (get-input-mode) 4))
  (in-cyrillic-cp1251% (cyrillic-input-method? "cp1251") in-cyrillic%)
  (in-cyrillic-jcuken% (cyrillic-input-method? "jcuken") in-cyrillic%)
  (in-cyrillic-koi8% (cyrillic-input-method? "koi8") in-cyrillic%)
  (in-cyrillic-translit% (cyrillic-input-method? "translit") in-cyrillic%)
  (in-cyrillic-yawerty% (cyrillic-input-method? "yawerty") in-cyrillic%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy initializations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public lazy-initialize-id 0)
(define-public lazy-initialize-pending '())

(define-public (lazy-initialize-impl id pred? module)
  (set! lazy-initialize-pending
	(cons (list id pred? module) lazy-initialize-pending)))

(define-public (lazy-initialize-do l id)
  (cond ((null? l) l)
	((or (== (caar l) id) (and (== id #t) ((cadar l))))
	 ((caddar l))
	 (lazy-initialize-do (cdr l) id))
	(else (cons (car l) (lazy-initialize-do (cdr l) id)))))

(define-public-macro (lazy-initialize module pred?)
  `(with id lazy-initialize-id
     (set! lazy-initialize-id (+ id 1))
     (lazy-initialize-impl id
       (lambda ()
	 ,pred?)
       (lambda ()
	 (import-from ,module)))
     (delayed
       (:idle 5000)
       (set! lazy-initialize-pending
	     (lazy-initialize-do lazy-initialize-pending id))
       (import-from ,module))))

(define-public (lazy-initialize-force)
  (set! lazy-initialize-pending
	(lazy-initialize-do lazy-initialize-pending #t)))
