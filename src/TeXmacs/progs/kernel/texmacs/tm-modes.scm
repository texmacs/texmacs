
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
      (if (null? deps)
	  (list 'begin defn arch1 arch2)
	  (list 'begin defn arch1 arch2 drd-cmd)))))

(define-public-macro (texmacs-modes . l)
  `(begin ,@(map texmacs-mode l)))

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
  (in-io% (and (or (inside? 'input) (inside? 'output)) (not (in-graphics?))))
  (in-session% (inside? 'session))
  (not-in-session% (not (inside? 'session)))
  (in-math-in-session% #t in-math% in-session%)
  (in-math-not-in-session% #t in-math% not-in-session%)
  (in-std% (style-has? "std-dtd"))
  (in-std-text% #t in-text% in-std%)
  (in-plugin-with-converters%
   (plugin-supports-math-input-ref (get-env "prog-language")))
  (with-active-selection% (selection-active-normal?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-modes
  (in-cyrillic% (in? (get-env "language") '("russian" "ukrainian")) in-text%)
  (in-czech% (== (get-env "language") "czech") in-text%)
  (in-danish% (== (get-env "language") "danish") in-text%)
  (in-dutch% (== (get-env "language") "dutch") in-text%)
  (in-english% (== (get-env "language") "english") in-text%)
  (in-finnish% (== (get-env "language") "finnish") in-text%)
  (in-french% (== (get-env "language") "french") in-text%)
  (in-german% (== (get-env "language") "german") in-text%)
  (in-hungarian% (== (get-env "language") "hungarian") in-text%)
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
  (in-cyrillic-cp1251% (cyrillic-input-method? "cp1251") in-cyrillic%)
  (in-cyrillic-jcuken% (cyrillic-input-method? "jcuken") in-cyrillic%)
  (in-cyrillic-koi8% (cyrillic-input-method? "koi8") in-cyrillic%)
  (in-cyrillic-translit% (cyrillic-input-method? "translit") in-cyrillic%)
  (in-cyrillic-yawerty% (cyrillic-input-method? "yawerty") in-cyrillic%))
