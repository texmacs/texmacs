
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : preamble.scm
;; DESCRIPTION : edit routines for preamble mode
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs edit edit-preamble)
  (:export
    ;; executable markup
    make-inactive-assign make-inactive-with make-inactive-expand
    make-inactive-var-expand make-inactive-hide-expand make-inactive-apply
    make-inactive-include make-inactive-macro make-inactive-function
    make-inactive-drd-props make-inactive-xmacro
    make-inactive-eval make-inactive-provides make-inactive-value
    make-inactive-argument make-inactive-get-label make-inactive-get-arity
    make-inactive-map-args make-inactive-quote make-inactive-delay
    make-inactive-hold make-inactive-release
    ;; computational markup
    make-inactive-or make-inactive-xor make-inactive-and make-inactive-not
    make-inactive-plus make-inactive-minus make-inactive-times
    make-inactive-over make-inactive-div make-inactive-mod
    make-inactive-merge make-inactive-length make-inactive-range
    make-inactive-number make-inactive-date make-inactive-translate
    make-inactive-find-file make-inactive-is-tuple make-inactive-look-up
    make-inactive-equal make-inactive-unequal make-inactive-less
    make-inactive-lesseq make-inactive-greater make-inactive-greatereq
    make-inactive-if make-inactive-var-if make-inactive-case
    make-inactive-for make-inactive-while make-inactive-extern
    make-inactive-authorize
    ;; inserting active objects
    make-tuple make-attr make-date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for macro handling and main executable markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-assign) (make-inactive "assign" 2))
(define (make-inactive-with) (make-inactive "with" 3))
(define (make-inactive-expand) (make-inactive "expand" 1))
(define (make-inactive-var-expand) (make-inactive "var_expand" 1))
(define (make-inactive-hide-expand) (make-inactive "hide_expand" 1))
(define (make-inactive-apply) (make-inactive "apply" 1))
(define (make-inactive-include) (make-inactive "include" 1))
(define (make-inactive-macro) (make-inactive "macro" 1))
(define (make-inactive-xmacro) (make-inactive "xmacro" 2))
(define (make-inactive-function) (make-inactive "func" 1))
(define (make-inactive-drd-props) (make-inactive "drd_props" 3))
(define (make-inactive-eval) (make-inactive-message "eval" 1 "evaluate"))
(define (make-inactive-provides) (make-inactive "provides" 1))
(define (make-inactive-value) (make-inactive "value" 1))
(define (make-inactive-argument) (make-inactive-message "arg" 1 "argument"))
(define (make-inactive-get-label) (make-inactive "get_label" 1))
(define (make-inactive-get-arity) (make-inactive "get_arity" 1))
(define (make-inactive-map-args) (make-inactive "map_args" 3))
(define (make-inactive-quote) (make-inactive "quote" 1))
(define (make-inactive-delay) (make-inactive "delay" 1))
(define (make-inactive-hold) (make-inactive "hold" 1))
(define (make-inactive-release) (make-inactive "release" 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting computational markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-or) (make-inactive "or" 2))
(define (make-inactive-xor) (make-inactive "xor" 2))
(define (make-inactive-and) (make-inactive "and" 2))
(define (make-inactive-not) (make-inactive "not" 1))
(define (make-inactive-plus) (make-inactive "plus" 2))
(define (make-inactive-minus) (make-inactive "minus" 2))
(define (make-inactive-times) (make-inactive "times" 2))
(define (make-inactive-over) (make-inactive "over" 2))
(define (make-inactive-div) (make-inactive "div" 2))
(define (make-inactive-mod) (make-inactive "mod" 2))
(define (make-inactive-merge) (make-inactive "merge" 2))
(define (make-inactive-length) (make-inactive "length" 1))
(define (make-inactive-range) (make-inactive "range" 3))
(define (make-inactive-number) (make-inactive "number" 2))
(define (make-inactive-date) (make-inactive "date" 1))
(define (make-inactive-translate) (make-inactive "translate" 3))
(define (make-inactive-find-file) (make-inactive "find_file" 2))
(define (make-inactive-is-tuple) (make-inactive "is_tuple" 1))
(define (make-inactive-look-up) (make-inactive "look_up" 2))
(define (make-inactive-equal) (make-inactive "equal" 2))
(define (make-inactive-unequal)
  (make-inactive-message "unequal" 2 "not equal"))
(define (make-inactive-less) (make-inactive "less" 2))
(define (make-inactive-lesseq)
  (make-inactive-message "lesseq" 2 "less or equal"))
(define (make-inactive-greater) (make-inactive "greater" 2))
(define (make-inactive-greatereq)
  (make-inactive-message "greatereq" 2 "greater or equal"))
(define (make-inactive-if) (make-inactive "if" 2))
(define (make-inactive-var-if) (make-inactive "var_if" 2))
(define (make-inactive-case) (make-inactive "case" 2))
(define (make-inactive-for) (make-inactive "for" 4))
(define (make-inactive-while) (make-inactive "while" 2))
(define (make-inactive-extern) (make-inactive "extern" 1))
(define (make-inactive-authorize) (make-inactive "authorize" 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting active objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tuple) (make-active "tuple" 1))
(define (make-attr) (make-active "attr" 2))
(define (make-date)
  (if (== (get-env "language") "english")
      (insert-tree (object->tree '(date "%B %d, %Y")))
      (insert-tree (object->tree '(date "%d %B %Y")))))
