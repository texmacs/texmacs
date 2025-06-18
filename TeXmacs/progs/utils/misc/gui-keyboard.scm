
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
                         (string-append "(emu-key " (string-quote c) ")")
                         (object->string c))
             ;;(display* "cmd= " cmd "\n")
             `((extended-key ,x ,cmd ,(number->string w))))))
        ((func? k 'modifier 3)
         (with (w x c) (cdr k)
           (let* ((c* (symbol->string (keyword->symbol c)))
                  (c** (string-quote c*))
                  (cmd (string-append "(emu-toggle-modifier " c** ")"))
                  (on? (if (emu-active-modifier? c*) "true" "false")))
             ;;(display* "cmd= " cmd "\n")
             `((modifier-key ,x ,cmd ,(number->string w) ,on?)))))
        ((func? k 'keys)
         (map (lambda (x)
                (with cmd (string-append "(emu-key " (string-quote x) ")")
                  ;;(display* "cmd= " cmd "\n")
                  `(std-key ,x ,cmd)))
              (cdr k)))))

(define (generate-row l)
  (with r (append-map generate-keys l)
    `(row (cell (concat ,@r)))))

(tm-define (generate-keyboard kbd)
  `(keyboard (tformat (table ,@(map generate-row kbd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various standard keyboard layouts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (narrow-us-lowercase-keyboard)
  `(((modifier 1.25 "fn" :Fn)
     (keys "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=")
     (key 1.25 "<mapsfrom>" "backspace"))
    ((key 1.5 "<mapsto>" "tab")
     (keys "q" "w" "e" "r" "t" "y" "u" "i" "o" "p" "[" "]" "\\"))
    ((modifier 2 "lock" :Lock)
     (keys "a" "s" "d" "f" "g" "h" "j" "k" "l" ";" "'")
     (key 1.5 "<hookleftarrow>" "return"))
    ((modifier 1.5 "shift" :Shift)
     (keys "`" "z" "x" "c" "v" "b" "n" "m" "," "." "/")
     (key 1 "<uparrow>" "up")
     (modifier 1 "shift" :Shift))
    ((modifier 1.25 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (modifier 1.25 "cmd" :Meta)
     (key 7.75 "" "space")
     (key 1 "<leftarrow>" "left")
     (key 1 "<downarrow>" "down")
     (key 1 "<rightarrow>" "right"))))

(tm-define (narrow-us-uppercase-keyboard)
  `(((modifier 1.25 "fn" :Fn)
     (keys "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "+")
     (key 1.25 "<mapsfrom>" "S-backspace"))
    ((key 1.5 "<mapsto>" "S-tab")
     (keys "Q" "W" "E" "R" "T" "Y" "U" "I" "O" "P" "{" "}" "|"))
    ((modifier 2 "lock" :Lock)
     (keys "A" "S" "D" "F" "G" "H" "J" "K" "L" ":" "\"")
     (key 1.5 "<hookleftarrow>" "S-return"))
    ((modifier 1.5 "shift" :Shift)
     (keys "~" "Z" "X" "C" "V" "B" "N" "M" "<" ">" "?")
     (key 1 "<uparrow>" "S-up")
     (modifier 1 "shift" :Shift))
    ((modifier 1.25 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (modifier 1.25 "cmd" :Meta)
     (key 7.75 "" "S-space")
     (key 1 "<leftarrow>" "S-left")
     (key 1 "<downarrow>" "S-down")
     (key 1 "<rightarrow>" "S-right"))))

(tm-define (narrow-us-lowercase-fn-keyboard)
  `(((modifier 1.25 "fn" :Fn)
     (keys "F1" "F2" "F3" "F4" "F5" "F6" "F7" "F8" "F9" "F10" "F11" "F12")
     (key 1.25 "del" "delete"))
    ((key 1.5 "<mapsto>" "tab")
     (keys "" "" "" "" "" "" "" "" "" "" "" "" ""))
    ((modifier 2 "lock" :Lock)
     (keys "" "" "" "" "" "" "" "" "" "" "")
     (key 1.5 "<hookleftarrow>" "return"))
    ((modifier 1.5 "shift" :Shift)
     (keys "" "" "" "" "" "" "" "" "" "" "")
     (key 1 "<Uparrow>" "pageup")
     (modifier 1 "shift" :Shift))
    ((modifier 1.25 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (modifier 1.25 "cmd" :Meta)
     (key 7.75 "" "space")
     (key 1 "<nwarrow>" "home")
     (key 1 "<Downarrow>" "pagedown")
     (key 1 "<searrow>" "end"))))

(tm-define (narrow-us-uppercase-fn-keyboard)
  `(((modifier 1.25 "fn" :Fn)
     (key 1 "F1" "S-F1")
     (key 1 "F2" "S-F2")
     (key 1 "F3" "S-F3")
     (key 1 "F4" "S-F4")
     (key 1 "F5" "S-F5")
     (key 1 "F6" "S-F6")
     (key 1 "F7" "S-F7")
     (key 1 "F8" "S-F8")
     (key 1 "F9" "S-F9")
     (key 1 "F10" "S-F10")
     (key 1 "F11" "S-F11")
     (key 1 "F12" "S-F12")
     (key 1.25 "del" "S-delete"))
    ((key 1.5 "<mapsto>" "S-tab")
     (keys "" "" "" "" "" "" "" "" "" "" "" "" ""))
    ((modifier 2 "lock" :Lock)
     (keys "" "" "" "" "" "" "" "" "" "" "")
     (key 1.5 "<hookleftarrow>" "S-return"))
    ((modifier 1.5 "shift" :Shift)
     (keys "" "" "" "" "" "" "" "" "" "" "")
     (key 1 "<Uparrow>" "S-pageup")
     (modifier 1 "shift" :Shift))
    ((modifier 1.25 "ctrl" :Control)
     (modifier 1.25 "alt" :Alt)
     (modifier 1.25 "cmd" :Meta)
     (key 7.75 "" "S-space")
     (key 1 "<nwarrow>" "S-home")
     (key 1 "<Downarrow>" "S-pagedown")
     (key 1 "<searrow>" "S-end"))))

(tm-define (narrow-us-keyboard)
  (cond ((and (not (emu-active-modifier? "Shift"))
              (not (emu-active-modifier? "Fn")))
         (narrow-us-lowercase-keyboard))
        ((and (emu-active-modifier? "Shift")
              (not (emu-active-modifier? "Fn")))
         (narrow-us-uppercase-keyboard))
        ((and (not (emu-active-modifier? "Shift"))
              (emu-active-modifier? "Fn"))
         (narrow-us-lowercase-fn-keyboard))
        ((and (emu-active-modifier? "Shift")
              (emu-active-modifier? "Fn"))
         (narrow-us-uppercase-fn-keyboard))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-keyboard)
  (generate-keyboard (narrow-us-keyboard)))
