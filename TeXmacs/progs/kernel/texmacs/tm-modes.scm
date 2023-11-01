
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
    (kernel logic logic-rules) (kernel logic logic-query) (kernel logic logic-data)
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
           (defn `(varlet *texmacs-module* ',pred (lambda () ,test)))
           (rules (map (lambda (dep) (list dep mode)) deps))
           (logic-cmd `(logic-rules ,@rules))
           (arch1 `(set-symbol-procedure! ',mode ,pred))
           (arch2 `(set-symbol-procedure! ',pred ,pred)))
      (if (== mode 'always%) (set! defn '(noop)))
      (if (null? deps)
          (list 'begin defn arch1 arch2)
          (list 'begin defn arch1 arch2 logic-cmd)))))

(define-public-macro (texmacs-modes . l)
  `(begin
     ,@(map texmacs-mode l)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Checking modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;FIXME: in-active-graphics% and developer-mode%
; seems to be the only two modes which do not have the associated procedure
; is the code below meaningful? why we need to do the eval?
; I want maybe to have the catch inside the eval

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

(define-public (supports-db?)
  (== (get-preference "database tool") "on"))

(define-public (side-tools?)
  (and (== (get-preference "side tools") "on")
       (== (get-preference "developer tool") "on")))

(define-public (left-tools?)
  (and (== (get-preference "left tools") "on")
       (== (get-preference "developer tool") "on")))

(define-public (has-side-tools? n)
  (cond ((== n 0) (side-tools?))
        ((== n 1) (left-tools?))
        (else #f)))

(define-public (has-markup-gui?)
  (and (== (get-preference "markup gui") "on")
       (== (get-preference "developer tool") "on")))

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
  (in-hybrid% (inside? 'hybrid))
  (in-hybrid-math% #t in-hybrid% in-math%)
  (in-math-not-hybrid% (not (inside? 'hybrid)) in-math%)
  (in-math-or-hybrid% (or (in-math?) (inside? 'hybrid)))
  (in-sem-math% (== (get-preference "semantic correctness") "on") in-math%)
  (in-table% (and (inside? 'table) (not (in-graphics?))))
  (in-session% (and (or (inside? 'session) (inside? 'program))
                    (not (in-graphics?))))
  (not-in-session% (not (inside? 'session)))
  (in-math-in-session% #t in-math% in-session%)
  (in-math-not-in-session% #t in-math% not-in-session%)
  (in-multicol-style% (!= (get-init "par-columns") "1"))
  (in-std% (style-has? "std-dtd"))
  (in-std-text% #t in-text% in-std%)
  (in-tmdoc% (style-has? "tmdoc-style"))
  (in-tmweb% (style-has? "tmweb-style") in-tmdoc%)
  (in-mmxdoc% (style-has? "mmxdoc-style") in-tmdoc%)
  (in-manual% (not (url-rooted-tmfs? (current-buffer))) in-tmdoc%)
  (in-database% (style-has? "database-style"))
  (in-bib% (style-has? "database-bib-style") in-database%)
  (in-preview-ref% (style-has? "preview-ref-package"))
  (in-smart-ref% (style-has? "smart-ref-package"))
  (in-plugin-with-converters%
   (plugin-supports-math-input-ref (get-env "prog-language")))
  (in-screens% (inside? 'screens))
  (in-article% (style-has? "header-article-package"))
  (in-book% (style-has? "header-book-package"))
  (in-letter% (style-has? "header-letter-package"))
  (in-seminar% (style-has? "header-seminar-package"))
  (in-generic% (style-has? "generic-style"))
  (in-code% (style-has? "code-style"))
  (in-browser% (style-has? "browser-style"))
  (in-beamer% (style-has? "beamer-style"))
  (in-poster% (style-has? "poster-style"))
  (in-edu% (style-has? "std-edu-dtd"))
  (in-edu-text% #t in-text% in-edu%)
  (in-edu-math% #t in-math% in-edu%)
  (in-auto% (style-has? "automate-dtd"))
  (in-comment% (style-has? "comment-dtd"))
  (with-any-selection% (selection-active-any?))
  (with-active-selection% (selection-active-normal?))
  (in-cpp% (== (get-env "prog-language") "cpp"))
  (in-prog-cpp% #t in-prog% in-cpp%)
  (in-dot% (== (get-env "prog-language") "dot"))
  (in-prog-dot% #t in-prog% in-dot%)
  (in-octave% (== (get-env "prog-language") "octave"))
  (in-prog-octave% #t in-prog% in-octave%)
  (in-java% (== (get-env "prog-language") "java"))
  (in-prog-java% #t in-prog% in-java%)
  (in-javascript% (== (get-env "prog-language") "javascript"))
  (in-prog-javascript% #t in-prog% in-javascript%)
  (in-json% (== (get-env "prog-language") "json"))
  (in-prog-json% #t in-prog% in-json%)
  (in-fortran% (== (get-env "prog-language") "fortran"))
  (in-prog-fortran% #t in-prog% in-fortran%)
  (in-scala% (== (get-env "prog-language") "scala"))
  (in-prog-scala% #t in-prog% in-scala%)
  (in-scheme% (== (get-env "prog-language") "scheme"))
  (in-prog-scheme% #t in-prog% in-scheme%)
  (in-python% (== (get-env "prog-language") "python"))
  (in-prog-python% #t in-prog% in-python%)
  (in-julia% (== (get-env "prog-language") "julia"))
  (in-prog-julia% #t in-prog% in-julia%)
  (in-verbatim% (or (inside? 'verbatim) (inside? 'verbatim-code) 
                    (inside? 'code)) in-text%)
  (in-variants-disabled% 
   (tree-in? (tree-up (cursor-tree)) '(hlink reference pageref label))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public supported-languages
  '("british" "bulgarian" "chinese" "croatian" "czech"
    "danish" "dutch" "english" "esperanto" "finnish" "french" "german" "greek"
    "hungarian" "italian" "japanese" "korean" "polish"
    "portuguese" "romanian" "russian" "slovak" "slovene" "spanish"
    "swedish" "taiwanese" "ukrainian"))

(define-public (supported-language? lan)
  (and (in? lan supported-languages)
       (cond ((== lan "chinese") (supports-chinese?))
             ((== lan "japanese") (supports-japanese?))
             ((== lan "korean") (supports-korean?))
             ((== lan "taiwanese") (supports-chinese?))
             (else #t))))

(texmacs-modes
  (in-cyrillic% (in? (get-env "language")
                     '("bulgarian" "russian" "ukrainian")) in-text%)
  (in-oriental% (in? (get-env "language")
                     '("chinese" "japanese" "korean" "taiwanese")) in-text%)
  (in-english% (in? (get-env "language")
                    '("british" "english")) in-text%)
  (in-american% (== (get-env "language") "english") in-text%)
  (in-british% (== (get-env "language") "british") in-text%)
  (in-bulgarian% (== (get-env "language") "bulgarian") in-cyrillic%)
  (in-chinese% (== (get-env "language") "chinese") in-oriental%)
  (in-croatian% (== (get-env "language") "croatian") in-text%)
  (in-czech% (== (get-env "language") "czech") in-text%)
  (in-danish% (== (get-env "language") "danish") in-text%)
  (in-dutch% (== (get-env "language") "dutch") in-text%)
  (in-esperanto% (== (get-env "language") "esperanto") in-text%)
  (in-finnish% (== (get-env "language") "finnish") in-text%)
  (in-french% (== (get-env "language") "french") in-text%)
  (in-german% (== (get-env "language") "german") in-text%)
  (in-greek% (== (get-env "language") "greek") in-text%)
  (in-hungarian% (== (get-env "language") "hungarian") in-text%)
  (in-italian% (== (get-env "language") "italian") in-text%)
  (in-japanese% (== (get-env "language") "japanese") in-oriental%)
  (in-korean% (== (get-env "language") "korean") in-oriental%)
  (in-polish% (== (get-env "language") "polish") in-text%)
  (in-portugese% (== (get-env "language") "portugese") in-text%)
  (in-romanian% (== (get-env "language") "romanian") in-text%)
  (in-russian% (== (get-env "language") "russian") in-cyrillic%)
  (in-slovak% (== (get-env "language") "slovak") in-text%)
  (in-slovene% (== (get-env "language") "slovene") in-text%)
  (in-spanish% (== (get-env "language") "spanish") in-text%)
  (in-swedish% (== (get-env "language") "swedish") in-text%)
  (in-taiwanese% (== (get-env "language") "taiwanese") in-oriental%)
  (in-ukrainian% (== (get-env "language") "ukrainian") in-cyrillic%)

  (in-math-english% (in? (get-env "language")
                         '("british" "english")) in-math%)
  (in-math-american% (== (get-env "language") "english") in-math%)
  (in-math-british% (== (get-env "language") "british") in-math%)
  (in-math-dutch% (== (get-env "language") "dutch") in-math%)
  (in-math-french% (== (get-env "language") "french") in-math%)
  (in-math-german% (== (get-env "language") "german") in-math%)
  (in-math-italian% (== (get-env "language") "italian") in-math%)
  (in-math-portuguese% (== (get-env "language") "portuguese") in-math%)
  (in-math-spanish% (== (get-env "language") "spanish") in-math%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard related
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public remote-control-flag? #f)
(define-public remote-control-remap (make-ahash-table))

(define-public (cyrillic-input-method? what)
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
  (window-per-buffer% (== (get-preference "buffer management") "separate"))
  (buffers-share-window% (== (get-preference "buffer management") "share"))
  (with-database-tool% (== (get-preference "database tool") "on"))
  (with-debugging-tool% (== (get-preference "debugging tool") "on"))
  (with-developer-tool% (== (get-preference "developer tool") "on"))
  (with-linking-tool% (== (get-preference "linking tool") "on"))
  (with-presentation-tool% (== (get-preference "presentation tool") "on"))
  (with-remote-tool% (== (get-preference "remote tool") "on"))
  (with-source-tool% (== (get-preference "source tool") "on"))
  (with-versioning-tool% (== (get-preference "versioning tool") "on"))
  (with-keyboard-tool% (== (get-preference "keyboard tool") "on"))
  (in-presentation% (or (style-has? "beamer-style")
                        (== (get-preference "presentation tool") "on")
                        (inside? 'screens)) in-beamer%)
  (search-mode% (== (get-input-mode) 1))
  (replace-mode% (== (get-input-mode) 2))
  (spell-mode% (== (get-input-mode) 3))
  (complete-mode% (== (get-input-mode) 4))
  (remote-control-mode% (== remote-control-flag? #t))
  (in-cyrillic-jcuken% (cyrillic-input-method? "jcuken") in-cyrillic%)
  (in-cyrillic-translit% (cyrillic-input-method? "translit") in-cyrillic%)
  (in-cyrillic-yawerty% (cyrillic-input-method? "yawerty") in-cyrillic%)
  (in-math-like-macos% #t in-math% like-macos%))

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
