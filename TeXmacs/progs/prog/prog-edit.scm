
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : prog-edit.scm
;; DESCRIPTION : editing verbatim programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog prog-edit)
  (:use (utils library tree)
        (utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic routines for textual programs
;; WARNING: most of these fail for non-verbatim content!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (inside-program?)
  (:synopsis "are we inside the line of a textual document?")
  (let* ((ct (cursor-tree))
         (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document))))

(tm-define (program-tree)
  (:synopsis "get the entire program tree")
  (let* ((ct (cursor-tree))
         (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document) dt)))

(tm-define (program-row row)
  (:synopsis "get the string at a given @row")
  (and-with doc (program-tree)
    (and-with par (tree-ref doc row)
      (and (tree-atomic? par) (tree->string par)))))

(tm-define (program-row-number)
  (:synopsis "get the vertical position on the current line")
  (and (inside-program?) (cADr (cursor-path))))

(tm-define (program-column-number)
  (:synopsis "get the horizontal position on the current line")
  (and (inside-program?) (cAr (cursor-path))))

(tm-define (program-go-to row col)
  (:synopsis "go to the character at a given @row and @col")
  (and-with doc (program-tree)
    (tree-go-to doc row col)))

(tm-define (program-character path)
  (let ((s (tree->string (path->tree (cDr path))))
        (pos (cAr path)))
    (if (or (string-null? s) (>= pos (string-length s)) (< pos 0)) #\nul
        (string-ref s pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define prog-auto-close-brackets? #f)
(tm-define prog-highlight-brackets? #f)
(tm-define prog-select-brackets? #f)

(define (notify-auto-close-brackets var val)
  (set! prog-auto-close-brackets? (== val "on")))
(define (notify-highlight-brackets var val)
  (set! prog-highlight-brackets? (== val "on")))
(define (notify-select-brackets var val)
  (set! prog-select-brackets? (== val "on")))

(define-preferences
  ("prog:automatic brackets" "off" notify-auto-close-brackets)
  ("prog:highlight brackets" "off" notify-highlight-brackets)
  ("prog:select brackets" "off" notify-select-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket handling for strings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-bracket-find* s pos inc br ibr level)
  ;(display* "find: pos= " pos ", level= " level "\n")
  (cond ((or (< pos 0) (>= pos (string-length s))) (- -1 (abs level)))
        ((and (== level 1) (== (string-ref s pos) br)) 
         ;(display* "returning at " pos "\n")
         pos)
        ((== (string-ref s pos) br)
         ;(display* "found at " pos "\n")
         (string-bracket-find* s (+ pos inc) inc br ibr (- level 1)))
        ((== (string-ref s pos) ibr)
         (string-bracket-find* s (+ pos inc) inc br ibr (+ level 1)))
        (else (string-bracket-find* s (+ pos inc) inc br ibr level))))

(define (string-bracket-find s pos inc br ibr level)
  (with r (string-bracket-find* s pos inc br ibr level)
    (and (>= r 0) r)))

(tm-define (string-bracket-level s pos inc br ibr)
  (with ret (string-bracket-find* s pos inc br ibr 0)
    (if (< ret 0) (- -1 ret)
        (string-bracket-level s (+ ret inc) inc br ibr))))

(tm-define (string-bracket-forward s pos br ibr)
  (:synopsis "find previous bracket @br with inverse @ibr in @s at @pos")
  (string-bracket-find s pos 1 br ibr 0))

(tm-define (string-bracket-backward s pos br ibr)
  (:synopsis "find next bracket @br with inverse @ibr in @s at @pos")
  (string-bracket-find s pos -1 br ibr 0))

(define (program-bracket-find row col inc br ibr level)
  (and-with s (program-row row)
    (with ret (string-bracket-find* s col inc br ibr level)   
      (if (>= ret 0) (cons row ret)
	  (with level* (- -1 ret)
	    (and-with s* (program-row (+ row inc))
	      (with col* (if (> inc 0) 0 (- (string-length s*) 1))
		(program-bracket-find (+ row inc) col* inc
				      br ibr level*))))))))

(tm-define (program-previous-match row br ibr)
  (:synopsis "find matching opening row for @row and bracket @br")
  (let* ((s (program-row row))
         (last (- (string-length s) 1)))    
    (if (not s) row
        (with ret (string-bracket-level s last -1 br ibr)
          (if (== ret 0) row
              (with pos (program-bracket-find row last -1 br ibr -1)
                (if (not pos) row
                    (car pos))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (path++ p)
  (rcons (cDr p) (+ 1 (cAr p))))

(define (path-- p)
  (rcons (cDr p) (- (cAr p) 1)))

(tm-define (select-brackets path lb rb)
  (:synopsis "Highlights innermost matching brackets around given @path")
  (let ((prev (find-left-bracket path lb rb))
        (next (find-right-bracket path lb rb)))
    (if (or (null? prev) (null? next))
      (if (nnull? (get-alt-selection "brackets"))
          (cancel-alt-selection "brackets"))
      (set-alt-selection "brackets" 
                         (list prev (path++ prev) next (path++ next))))))

(define (string-ref* s i)
  (char->string (string-ref s i)))

(tm-define (select-brackets-after-movement lbs rbs esc)
  (:synopsis "Highlight any of @lbs (matching @rbs) after a cursor movement")
  (let* ((p (cursor-path))
         (p* (path-- p))
         (ch (program-character p))
         (lch (program-character p*))
         (i1 (string-index lbs ch))
         (i2 (string-index rbs ch))
         (i3 (string-index rbs lch)))
    (cond (i1 (select-brackets p (string-ref* lbs i1) (string-ref* rbs i1)))
          (i2 (select-brackets p* (string-ref* lbs i2) (string-ref* rbs i2)))
          (i3 (select-brackets p* (string-ref* lbs i3) (string-ref* rbs i3)))
          ((nnull? (get-alt-selection "brackets"))
           (cancel-alt-selection "brackets")))))

(tm-define (bracket-open lb rb esc)
  (if prog-auto-close-brackets?
      (if (selection-active-normal?)
          (begin
            (clipboard-cut "temp")
            (insert-go-to (string-append lb rb) '(1))
            (clipboard-paste "temp"))
          (with ch (or (before-cursor) "")
            ; Don't create right bracket if prev char is escape char
            (if (== ch esc)
                (insert lb)
                (insert-go-to (string-append lb rb) '(1)))))
      (insert lb))
  (if prog-highlight-brackets? (select-brackets (cursor-path) lb rb)))

; TODO: warn if unmatched
(tm-define (bracket-close lb rb esc)
  (with p (cursor-path)
    (insert rb)
    (if prog-highlight-brackets? (select-brackets p lb rb))))

; HACK! I'd like to use selection-active-enlarging? But current C++ code
; doesn't mix well with this selection mechanism. Also: if mouse selections
; ever use program-select-enlarge, this has to be disabled.
(define kbd-select-enlarging 0) ; 0=no, 1=started, 2=selecting

(tm-define (program-select-enlarge lb rb)
  (if (== 0 kbd-select-enlarging)
      (set! kbd-select-enlarging 1)
      (if (and (not (selection-active-any?)) (== 2 kbd-select-enlarging))
          (set! kbd-select-enlarging 1)
          (let* ((start (selection-get-start))
                 (end (selection-get-end))
                 (start* (if (== start end) start (path-- start)))
                 (prev (find-left-bracket start* lb rb))
                 (next (find-right-bracket end lb rb)))
            (if (or (and (== start prev) (== end next))
                    (null? prev) (null? next))
                (begin
                  (set! kbd-select-enlarging 0)
                  (selection-cancel))
                (begin
                  (set! kbd-select-enlarging 2)
                  (selection-set prev (path++ next))))))))

; Cancel any active selection when we leave a code fragment
(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:require (not (in-prog?)))
  (if (nnull? (get-alt-selection "brackets"))
      (cancel-alt-selection "brackets")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-preferences
  ("editor:verbatim:tabstop" 4 (lambda (pref val) (noop))))

(define (char-whitespace? c)
  (== c #\space))

(define (char-non-whitespace? c)
  (!= c #\space))

(tm-define (string-whitespace? s)
  (:synopsis "does @s only contain whitespace?")
  (list-and (map char-whitespace? (string->list s))))
      
(tm-define (get-tabstop)
  (with tabstop* (get-preference "editor:verbatim:tabstop")
    (cond ((and (string? tabstop*) (string->number tabstop*))
           (string->number tabstop*))
          ((and (number? tabstop*) (> tabstop* 0)) tabstop*)
          (else (set-message
                 `(replace "Wrong tabstop: %1" ,tabstop*) "User preferences")
                8))))

(tm-define (insert-tabstop)
  (with w (get-tabstop)
    (with fill (- w (remainder (cAr (cursor-path)) w))
      (if (> fill 0) (insert (make-string fill #\space))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (string-get-indent s)
  (:synopsis "get the indentation of @s")
  (with pos (list-find-index (string->list s) char-non-whitespace?)
    (or pos (string-length s))))

(tm-define (string-set-indent s i)
  (:synopsis "set the indentation of @s to @i spaces")
  (let* ((l (make-string i #\space))
         (r (substring s (string-get-indent s) (string-length s))))
    (string-append l r)))

(tm-define (program-get-indent)
  (:synopsis "get the indentation of the current line")
  (and (inside-program?)
       (string-get-indent (program-row (program-row-number)))))

(tm-define (program-set-indent i)
  (:synopsis "set the indentation of the current line to @i spaces")
  (when (inside-program?)
    (with t (cursor-tree)
      (tree-set t (string-set-indent (tree->string t) i)))))

; Redefine this for specific languages
(tm-define (program-compute-indentation doc row col)
  0)

(tm-define (program-indent-line doc row unindent?)
  ; TODO: implement unindent for general languages
  (let* ((i (program-compute-indentation doc row -1))
         (t (tree-ref doc row)))
    ; HACK: I should change program-set-indent to accept line numbers
    (tree-set t (string-set-indent (tree->string t) i))
    i))

(tm-define (program-indent-all unindent?)
  (:synopsis "indent a whole program")
  (and-with doc (program-tree)
    (for-each (lambda (r) (program-indent-line doc r unindent?))
              (iota (tree-arity doc)))))

(tm-define (program-indent unindent?)
  (and-with doc (program-tree)
    (let* ((r (program-row-number))
           (c (program-indent-line doc r unindent?)))
      (program-go-to r c))))

(tm-define (insert-return)
  (:mode in-prog?)
  (insert-raw-return)
  (program-indent #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-paste)
  (:mode in-prog?)
  (clipboard-paste-import "verbatim" "primary"))

(tm-define (kbd-copy)
  (:mode in-prog-scheme?)
  (clipboard-copy-export "scheme" "primary"))

(tm-define (kbd-cut)
  (:mode in-prog-scheme?)
  (clipboard-cut-export "scheme" "primary"))

(tm-define (kbd-paste)
  (:mode in-prog-scheme?)
  (clipboard-paste-import "scheme" "primary"))
