;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-tools.scm
;; DESCRIPTION : Tools for scheme sessions
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The contents of this file are preliminary and simple. Things TO-DO are:
;;  - Use gui:help-window-visible in init-texmacs.scm (or elsewhere)
;;  - this list 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-tools)
  (:use (convert rewrite init-rewrite) 
        (doc apidoc-collect)
        (doc apidoc-widgets)
        (kernel texmacs tm-preferences)
        (kernel gui kbd-handlers)))

(tm-define char-set:stopmark
           (char-set-adjoin char-set:whitespace #\( #\) #\" #\'))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rudimentary editing aids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scheme-auto-parenthesis)
  (insert "()") (emulate-keyboard "left"))

(kbd-map
  (:require (in-prog-scheme?))
  ("(" (scheme-auto-parenthesis)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Importing and exporting of sessions (incomplete, ugly and fragile)
;; TODO:
;;  - convert text into comments and viceversa
;;  - split by expressions, not by double newlines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (list-split lst what)
  (:synopsis 
   "Return a list of lists splitting @lst by items equal? to @what")
  (letrec 
    ((loop (lambda (lst what acc)
      (cond ((null? lst) (list acc))
            ((equal? what (car lst)) (cons acc (loop (cdr lst) what '())))
            (else
              (loop (cdr lst) what (append acc (list (car lst)))))))))
    (loop lst what '())))

(define (sessions->verbatim t)
  (with tx (select t '(:* (:or input unfolded-io folded-io) 1))
    (string-join 
      (map-in-order 
        (lambda (x) 
          (texmacs->verbatim x 
            (acons "texmacs->verbatim:encoding" "SourceCode" '())))
        tx) "\n\n")))

(tm-define (export-sessions url)
  (string-save (sessions->verbatim (buffer-tree)) url))

(tm-define (export-selected-sessions url)
  (string-save (sessions->verbatim (selection-tree)) url))

; This won't preserve indentation (and also needs escaping of < and >)
;(define (session-read port)
; (let ((form (read port)))
;    (if (eof-object? form) '()
;        (cons `(input "Scheme] " (document ,(object->string form)))
;              (session-read port)))))

;(tm-define (import-sessions file)
;  (with f (open-input-file (url->string file))
;    (insert `(session "scheme" "default" (document ,@(session-read f))))))

; This is surely already done elsewhere...
(define (string-load-clean file)
  (let* ((str (string-load file))
         (str1 (string-replace str "<" "&lt;"))
         (str2 (string-replace str1 ">" "<gtr>")))
    (string-replace str2 "&lt;" "<less>")))

(tm-define (import-sessions file)
  (let* ((str (string-load-clean file))
         (lst (list-split (string-split str #\newline) ""))
         (lst2 (list-filter lst (lambda (x) (nnull? x))))
         (inputs 
           (map-in-order (lambda (x) `(input "Scheme] " (document ,@x))) 
                         lst2)))
   (insert `(session "scheme" "default" (document ,@inputs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for contextual help
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (word-at str pos)
  "Returns the word at @pos in @str, delimited by char-set:stopmark"
  (if (<= pos (string-length str))
      (let* ((beg (string-rindex str char-set:stopmark 0 pos))
             (end (string-index str char-set:stopmark pos)))
        (if (== end #f) (set! end (string-length str)))
        (if (== beg #f) (set! beg 0) (set! beg (+ 1 beg)))
        (substring str beg end))
      ""))

(tm-define (cursor-word)
  (:synopsis "Returns the word under the cursor, delimited by char-set:stopmark")
  (with ct (cursor-tree)
    (word-at (tree->string ct) (car (tree-cursor-path ct)))))

(define (check-build-cache cont)
  (let  ((t (get-preference "doc:collect-timestamp"))
         (lan (get-output-language))
         (langs (get-preference "doc:collect-languages")))
    (if (not (and t langs (member lan langs)))
      (doc-collect-all lan cont) (cont))))

(tm-define (scheme-popup-help word)
  (:synopsis "Pops up the help window for the scheme symbol @word")
  (check-build-cache (lambda () (help-window "scheme" word))))

(tm-define (scheme-inbuffer-help word)
  (load-buffer (string-append "tmfs://apidoc/type=symbol&what=" word)))

; TODO: check if a symbol is in the glue
(tm-define (scheme-go-to-definition ssym)
  (with sym (string->symbol ssym)
    (let ((line (symbol-property sym 'line))
          (column (symbol-property sym 'column))
          (filename (symbol-property sym 'filename)))
      (if (and line filename)
        (let ((lno (number->string line))
              (cno (number->string column)))
          (go-to-url (string-append filename "?line=" lno "&column=" cno
                                             "&select=" ssym))
          (set-message filename (string-append lno ":" cno)))
        (set-message "Symbol properties not found." ssym)))))

(kbd-map
  (:require (and developer-mode-on (in-prog-scheme?)))
  ("A-F1" (scheme-popup-help (cursor-word)))
  ("S-A-F1" (scheme-inbuffer-help (cursor-word)))
  ("M-F1" (scheme-go-to-definition (cursor-word))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handy.. (stuff previously in apidoc-funcs.scm)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-current-doc-module)
  (let ((tt (select (buffer-tree) '(doc-module-header 0))))
    (if (null? tt)
      '()
      (string->module (tree->string (car tt))))))

(define (exp-modules)
  (map symbol->string (or (module-exported (get-current-doc-module)) '(""))))

(tm-define (ask-insert-symbol-doc ssym)
  (:argument ssym "Symbol")
  (:proposals ssym (exp-modules))
  ;(:check-mark "*" (symbol-documented?)) ; right?
  (insert ($doc-symbol-template (string->symbol ssym) "")))

(kbd-map
  (:require (in-tmdoc?))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing of sources with the mouse.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cw "")

(define (cmd-click? mods) 
  (== (logand mods Mod2Mask) Mod2Mask))

(define (opt-click? mods) 
  (== (logand mods Mod1Mask) Mod1Mask))

;Original definition for reference
;(tm-define (mouse-event key x y mods time)
;  (mouse-any key x y mods (+ time 0.0)))

; Override mouse events. Because the Mod1Mask and Mod2Mask modifiers are used
; in MacOS for emulation of middle and right button, under this OS, whenever
; we have these modifiers, the buttons sent are middle and right, so we must
; check for events of type "press-" and "release-" in order to be compatible
; across platforms. (We could use :require for this too)
(tm-define (mouse-event key x y mods time)
  (:require (and developer-mode-on (opt-click? mods) (in-prog-scheme?)))
  (with short (string-take key 4)
    (cond ((== short "pres")
           ; emulate a click to move the cursor
           (mouse-any "release-left" x y 1 (+ time 0.0))
           (set! cw (cursor-word))
           (select-word cw (cursor-tree) (cAr (cursor-path))))
          ((== short "rele")
           (with cw2 (cursor-word)
             (if (== cw cw2) (help-window "scheme" cw))))
          (else (mouse-any key x y mods (+ time 0.0))))))

(tm-define (mouse-event key x y mods time)
  (:require (and developer-mode-on (cmd-click? mods) (in-prog-scheme?)))
  (with short (string-take key 4)
    (cond ((== short "pres")
           ; emulate a click to move the cursor
           (mouse-any "release-left" x y 1 (+ time 0.0))
           (set! cw (cursor-word))
           (select-word cw (cursor-tree) (cAr (cursor-path))))
          ((== short "rele")
           (with cw2 (cursor-word)
             (if (== cw cw2) (scheme-go-to-definition cw))))
          (else (mouse-any key x y mods (+ time 0.0))))))

