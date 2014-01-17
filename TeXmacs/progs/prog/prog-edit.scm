
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

(tm-define (program-character row col)
  (:synopsis "get the character at a given @row and @col")
  (and-with par (program-row row)
    (and (>= col 0) (< col (string-length par)) (string-ref par col))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-public prog-auto-close-brackets? #f)
(define-public prog-highlight-brackets? #f)

(define (notify-auto-close-brackets var val)
  (set! prog-auto-close-brackets? (== val "on")))

(define (notify-highlight-brackets var val)
  (set! prog-highlight-brackets? (== val "on")))

(define-preferences
  ("prog:automatic brackets" "on" notify-auto-close-brackets)
  ("prog:highlight brackets" "on" notify-highlight-brackets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bracket handling
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

(define (program-bracket-find* row col inc br ibr level)
  (and-with s (program-row row)
    (with ret (string-bracket-find* s col inc br ibr level)
      (cond ((>= ret 0) (cons row (min ret (- (string-length s) 1))))
            ((== ret -1) (cons row 0))
            (else
              (and-with s* (program-row (+ row inc))
                (let ((level* (- -1 ret))
                      (col* (if (> inc 0) 0 (- (string-length s*) 1))))
                  (program-bracket-find* (+ row inc) col* 
                                         inc br ibr level*))))))))

(define (program-bracket-find row col inc br ibr level)
  (let* ((p (tree->path (program-tree)))
         (rc (program-bracket-find* row col inc br ibr level))
         (row* (max 0 (or (and (pair? rc) (car rc)) row)))
         (col* (max -1 (or (and (pair? rc) (cdr rc)) (- col 1))))
         (left (append p `(,row* ,(max 0 col*))))
         (right (append p `(,row* ,(+ 1 col*)))))
    (list left right)))

(tm-define (program-bracket-backward row col br ibr)
  (:synopsis 
   "Returns the paths to the left and right of a bracket @br matching @ibr")
  (with level (if (== (program-character row col) ibr) 0 1)
    (program-bracket-find row col -1 br ibr level)))

(tm-define (program-bracket-forward row col br ibr)
  (:synopsis 
   "Returns the paths to the left and right of a bracket @br matching @ibr")
  (with level (if (== (program-character row col) br) 0 1)
    (program-bracket-find row col 1 ibr br level)))

(define (select-brackets* row col br ibr)
  (let* ((prev (program-bracket-backward row col br ibr ))
         (next (program-bracket-forward row col br ibr ))
         ; avoid too many delayed commands while moving fast
         (sel? (nnull? (get-alt-selection "alternate"))))
    (if sel? (cancel-alt-selection "alternate"))
    (set-alt-selection "alternate" (append prev next))
    (if (not sel?) (delayed (:pause 1000) (cancel-alt-selection "alternate")))))

(tm-define (select-brackets br ibr)
  (:synopsis "Highlights the innermost matching brackets around the cursor")
  (let* ((row (program-row-number))
         (col (program-column-number)))
    (select-brackets* row col br ibr)))

(tm-define (select-brackets-after-movement br ibr esc)
  (:synopsis "Hihghlight brackets after a cursor movement")
  (let* ((row (program-row-number))
         (col (program-column-number))
         (col* (max 0 (- (or col 0) 1)))
         (ch (program-character row col*)))
    (cond ((== esc ch) (noop))
          ((== ibr ch) (select-brackets* row col* br ibr))
          ((== br ch) (select-brackets* row col br ibr)))))

(tm-define (bracket-open br ibr esc)
  (with sel? (selection-active-normal?)
    (if prog-auto-close-brackets?
        (if sel? 
            (begin
              (clipboard-cut "temp")
              (insert-go-to (list->string `(,br ,ibr)) '(1))
              (clipboard-paste "temp"))
            (let* ((ch (or (before-cursor) ""))
                   (srow (program-row (program-row-number)))
                   (col (program-column-number))
                   (llev (string-bracket-level srow col -1 br ibr))
                   (rlev (string-bracket-level srow col 1 br ibr)))
              ; Don't create right bracket if there are unmatched ones
              ; or the previous char is an escape char.
              (if (or (== ch (char->string esc)) (> rlev llev))
                  (insert (char->string br))
                  (insert-go-to (list->string `(,br ,ibr)) '(1)))))
        (insert (char->string br))))
  (select-brackets br ibr))

; TODO: warn if unmatched
(tm-define (bracket-close br ibr esc)
  (insert (char->string ibr))
  (select-brackets-after-movement br ibr esc))

(tm-define (prog-select-enlarge br ibr)
  (let* ((start (selection-get-start))
         (end (selection-get-end))
         (start-row (cADr start))
         (start-col (- (cAr start) 1))
         (end-row (cADr end))
         (end-col (cAr end))
         (prev (program-bracket-backward start-row start-col br ibr))
         (next (program-bracket-forward end-row end-col br ibr)))
    (selection-set (car prev) (cadr next))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (char-whitespace? c)
  (== c #\space))

(define (char-non-whitespace? c)
  (!= c #\space))

(tm-define (string-whitespace? s)
  (:synopsis "does @s only contain whitespace?")
  (list-and (map char-whitespace? (string->list s))))

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
