
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : chinese.scm
;; DESCRIPTION : keystrokes for the Chinese language
;; COPYRIGHT   : (C) 2022  Darcy Shen
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text chinese chinese)
  (:use (text text-kbd)))

(kbd-map
  (:mode in-chinese?)
  ; Convert Cork to Chinese punctuations directly
  ("\x10" "<#201C>")
  ("\x11" "<#201D>")
  ("\x16" "<#2014>")
  ; Pinyin Support via Tab Cycling
  ("a var" "<#0101>")
  ("a var var" "<#00E1>")
  ("a var var var" "<#01CE>")
  ("a var var var var" "<#00E0>")
  ("o var" "<#014D>")
  ("o var var" "<#00F3>")
  ("o var var var" "<#01D2>")
  ("o var var var var" "<#00F2>")
  ("e var" "<#0113>")
  ("e var var" "<#00E9>")
  ("e var var var" "<#011B>")
  ("e var var var var" "<#00E8>")
  ("i var" "<#012B>")
  ("i var var" "<#00ED>")
  ("i var var var" "<#01D0>")
  ("i var var var var" "<#00EC>")
  ("u var" "<#016B>")
  ("u var var" "<#00FA>")
  ("u var var var" "<#01D4>")
  ("u var var var var" "<#00F9>")
  ("v var" "<#00FC>")
  ("v var var" "<#01D6>")
  ("v var var var" "<#01D8>")
  ("v var var var var" "<#01DA>")
  ("v var var var var var" "<#01DC>")
  ("m var" "<#1E3F>")
  ("n var" "<#0144>")
  ("n var var" "<#0148>")
  ("n var var var" "<#01F9>")
  ; Full Width and Half Width Switching via Tab Cycling
  (". var" "<#3002>")
  (": var" "<#FF1A>")
  ("; var" "<#FF1B>")
  (", var" "<#FF0C>")
  ("( var" "<#FF08>")
  (") var" "<#FF09>")
  ("? var" "<#FF1F>")
  ("< var" "<#300A>")
  ("> var" "<#300B>")
  ("' var" "<#2018>")
  ("` var" "<#2019>")
  ("\" var" "<#201C>")
  ("\" var var" "<#201D>")
  ("[ var" "<#300C>")
  ("[ var var" "<#300E>")
  ("] var" "<#300D>")
  ("] var var" "<#300F>")
  ; Special numbering
  ("1 @" "<#2460>")
  ("2 @" "<#2461>")
  ("3 @" "<#2462>")
  ("4 @" "<#2463>")
  ("5 @" "<#2464>")
  ("6 @" "<#2465>")
  ("7 @" "<#2466>")
  ("8 @" "<#2467>")
  ("9 @" "<#2468>")
  ("1 0 @" "<#2469>")
  ("1 @ var" "1@")
  ("2 @ var" "2@")
  ("3 @ var" "3@")
  ("4 @ var" "4@")
  ("5 @ var" "5@")
  ("6 @ var" "6@")
  ("7 @ var" "7@")
  ("8 @ var" "8@")
  ("9 @ var" "9@")
  ("1 0 @ var" "10@"))
