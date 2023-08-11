
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-keyboard.scm
;; DESCRIPTION : generation of custom keyboards
;; COPYRIGHT   : (C) 2023  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc gui-keyboard)
  (:use (utils misc gui-utils)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating the keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-keys k)
  (cond ((func? k 'key 3)
         (with (w x c) (cdr k)
           (with cmd (if (string? c)
                         (string-append "(emu-key \"" c "\")")
                         (object->string c))
             ;;(display* "cmd= " cmd "\n")
             `((extended-key ,x ,cmd ,(number->string w))))))
        ((func? k 'modifier 3)
         (with (w x c) (cdr k)
           (let* ((c* (symbol->string (keyword->symbol c)))
                  (cmd (string-append "(emu-toggle-modifier \"" c* "\")"))
                  (on? (if (emu-active-modifier? c*) "true" "false")))
             ;;(display* "cmd= " cmd "\n")
             `((modifier-key ,x ,cmd ,(number->string w) ,on?)))))
        ((func? k 'keys)
         (map (lambda (x) `(simple-key ,x)) (cdr k)))))

(define (generate-row l)
  (with r (append-map generate-keys l)
    `(row (cell (concat ,@r)))))

(tm-define (generate-keyboard kbd)
  `(keyboard (tformat (table ,@(map generate-row kbd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various standard keyboard layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (narrow-us-lowercase-keyboard)
  `(((key 1 "esc" "escape")
     (keys "1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
     (key 1.5 "<mapsfrom>" "backspace"))
    ((key 1.5 "<mapsto>" "tab")
     (keys "q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
     (key 1 "<backslash>" "\\"))
    ((modifier 1.75 "sym" :Sym)
     (keys "a" "s" "d" "f" "g" "h" "j" "k" "l")
     (key 1.75 "<hookleftarrow>" "return"))
    ((modifier 2.25 "shift" :Shift)
     (keys "z" "x" "c" "v" "b" "n" "m" "," ".")
     (modifier 1.25 "shift" :Shift))
    ((modifier 1.75 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (key 7.5 "" "space")
     (modifier 1 "alt" :Alt)
     (modifier 1 "ctrl" :Control))))

(tm-define (narrow-us-uppercase-keyboard)
  `(((key 1 "esc" "S-escape")
     (keys "!" "@" "#" "$" "%" "^" "&" "*" "(" ")")
     (key 1.5 "<mapsfrom>" "S-backspace"))
    ((key 1.5 "<mapsto>" "S-tab")
     (keys "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P")
     (key 1 "<backslash>" "|"))
    ((modifier 1.75 "sym" :Sym)
     (keys "A" "S" "D" "F" "G" "H" "J" "K" "L")
     (key 1.75 "<hookleftarrow>" "S-return"))
    ((modifier 2.25 "shift" :Shift)
     (keys "Z" "X" "C" "V" "B" "N" "M" "<" ">")
     (modifier 1.25 "shift" :Shift))
    ((modifier 1.75 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (key 7.5 "" "S-space")
     (modifier 1 "alt" :Alt)
     (modifier 1 "ctrl" :Control))))

(tm-define (narrow-us-keyboard)
  (cond ((not (emu-active-modifier? "Shift"))
         (narrow-us-lowercase-keyboard))
        ((emu-active-modifier? "Shift")
         (narrow-us-uppercase-keyboard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-keyboard)
  (generate-keyboard (narrow-us-keyboard)))
