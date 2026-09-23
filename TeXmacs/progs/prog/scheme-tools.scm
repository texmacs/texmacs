
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

(define (prep-math t props)
  (cond ((atomic-tree? t) t)
        ((tree-in? t '(math equation equation*))
         (string->tree (texmacs->latex-document t props)))
        ((> (tree-arity t) 0) (tree-map-children (cut prep-math <> props) t))
        (else t)))

(define (sessions->verbatim t)
  (with tx (select t '(:* (:or input unfolded-io folded-io) 1))
    (with props (acons "texmacs->verbatim:encoding" "SourceCode" '())
      (string-join
       (map-in-order (lambda (x) (texmacs->verbatim x props))
                     (map-in-order (cut prep-math <> props) tx)) "\n\n"))))

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
      (let* ((beg (string-rindex (substring str 0 pos) char-set:stopmark))
             (end (string-index (substring str pos (string-length str)) char-set:stopmark)))
        (if (== end #f) (set! end (string-length str)))
        (if (== beg #f) (set! beg 0) (set! beg (+ 1 beg)))
        (substring str beg end))
      ""))

(tm-define (cursor-word)
  (:synopsis "Returns the word under the cursor, delimited by char-set:stopmark")
  (with ct (cursor-tree)
    (word-at (tree->string ct) (car (tree-cursor-path ct)))))

(tm-define (scheme-popup-help word)
  (:synopsis "Pops up the help window for the scheme symbol @word")
  (help-window "scheme" word))

(tm-define (scheme-inbuffer-help word)
  (:synopsis "Opens a help buffer for the scheme symbol @word")
  (load-document (string-append "tmfs://apidoc/type=symbol&what="
                                (string-replace word ":" "%3A")))); HACK

(define (url-for-symbol s props)
  (with (file line column) props
    (if (and file line column)
        (let ((lno (number->string line))
              (cno (number->string column))
              (ss (string-replace s ":" "%3A"))) ; HACK! see link-navigate.scm
          (string-append file "?line=" lno "&column=" cno "&select=" ss))
        (url-none))))

; TODO: check if a symbol is in the glue
(tm-define (scheme-go-to-definition tmstr)
  (let* ((str (tmstring->string tmstr))
         (sym (string->symbol str))
         (defs (or (symbol-property sym 'defs) '()))
         (urls (map (lambda (x) (url-for-symbol tmstr x)) defs)))
    (if (null? urls)
        (set-message "Symbol properties not found" tmstr)
        (go-to-url (list-fold url-or (car urls) (cdr urls))
                   (cursor-path)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Miscelaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-current-doc-module)
  (let ((tt (select (buffer-tree) '(doc-module-header 0))))
    (if (null? tt)
      '()
      (string->module (tree->string (car tt))))))

(define (exp-modules)
  (map symbol->string (or (module-exported (get-current-doc-module)) '())))

(tm-define (ask-insert-symbol-doc ssym)
  (:argument ssym "Symbol")
  (:proposals ssym (exp-modules))
  ;(:check-mark "*" (symbol-documented?)) ; right?
  (insert ($doc-symbol-template (string->symbol ssym) #t "")))

(kbd-map
  (:require (and developer-mode? (in-tmdoc?)))
  ("M-A-x" (interactive ask-insert-symbol-doc)))

(tm-define (run-scheme-file u)
  (:synopsis "Load the file @u into the scheme interpreter")
  (with s (url->string u)
    (with run (lambda (save?)          
                (if save? (buffer-save u))
                (load s)
                (set-message `(replace "File %1 was executed" (verbatim ,s)) 
                             ""))
      (if (and (buffer-exists? u) (buffer-modified? u))
          (user-confirm 
             `(replace
                "File %1 is currently open and modified. Save before running?"
                (verbatim ,s))
            #t run)
          (run #f)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Browsing of sources with the mouse.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cw "")

(define (cmd-click? mods) 
  (== (logand mods Mod2Mask) Mod2Mask))

(define (opt-click? mods) 
  (== (logand mods Mod1Mask) Mod1Mask))

;Original definition for reference
;(tm-define (mouse-event key x y mods time data)
;  (mouse-any key x y mods (+ time 0.0) data))

; Override mouse events. Because the Mod1Mask and Mod2Mask modifiers are used
; in MacOS for emulation of middle and right button, under this OS, whenever
; we have these modifiers, the buttons sent are middle and right, so we must
; check for events of type "press-" and "release-" in order to be compatible
; across platforms. (We could use :require for this too)
(tm-define (mouse-event key x y mods time data)
  (:require (and developer-mode? (opt-click? mods) (in-prog-scheme?)))
  (with short (string-take key 4)
    (cond ((== short "pres")
           ; emulate a click to move the cursor
           (mouse-any "release-left" x y 1 (+ time 0.0) data)
           (set! cw (cursor-word))
           (select-word cw (cursor-tree) (cAr (cursor-path))))
          ((== short "rele")
           (with cw2 (cursor-word)
             (if (== cw cw2) (help-window "scheme" cw))))
          (else (mouse-any key x y mods (+ time 0.0) data)))))

(tm-define (mouse-event key x y mods time data)
  (:require (and developer-mode? (cmd-click? mods) (in-prog-scheme?)))
  (with short (string-take key 4)
    (cond ((== short "pres")
           ; emulate a click to move the cursor
           (mouse-any "release-left" x y 1 (+ time 0.0) data)
           (set! cw (cursor-word))
           (select-word cw (cursor-tree) (cAr (cursor-path))))
          ((== short "rele")
           (with cw2 (cursor-word)
             (if (== cw cw2) (scheme-go-to-definition cw))))
          (else (mouse-any key x y mods (+ time 0.0) data)))))
