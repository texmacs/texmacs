
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

(texmacs-module (utils misc gui-keyboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generating the keyboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-keys k)
  (cond ((func? k 'key 3)
         (with (w x c) (cdr k)
           (with cmd (if (string? c)
                         (string-append "(emu-key \"" c "\")")
                         (object->string c))
             `((extended-key ,x ,cmd ,(number->string w))))))
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

(tm-define (keyboard-us)
  `(((key 1 "Esc" "escape")
     (keys "1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
     (key 1.5 "<mapsfrom>" "backspace"))
    ((key 1.5 "<mapsto>" "tab")
     (keys "q" "w" "e" "r" "t" "y" "u" "i" "o" "p")
     (key 1 "<backslash>" "\\"))
    ((key 1.75 "Alt" (noop))
     (keys "a" "s" "d" "f" "g" "h" "j" "k" "l")
     (key 1.75 "<hookleftarrow>" "return"))
    ((key 2.25 "Shift" (noop))
     (keys "z" "x" "c" "v" "b" "n" "m" "," ".")
     (key 1.25 "Shift" (noop)))
    ((key 1.75 "Ctrl" (noop))
     (key 1.25 "Alt" (noop))
     (key 7.5 "" "space")
     (key 1 "Alt" (noop))
     (key 1 "Ctrl" (noop)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-keyboard)
  (generate-keyboard (keyboard-us)))
