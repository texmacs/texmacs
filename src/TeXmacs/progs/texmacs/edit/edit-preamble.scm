
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
    make-inactive-assign make-inactive-with make-inactive-compound
    make-inactive-include make-inactive-macro make-inactive-function
    make-inactive-drd-props make-inactive-xmacro
    make-inactive-eval make-inactive-provides make-inactive-value
    make-inactive-argument make-inactive-get-label make-inactive-get-arity
    make-inactive-map-args make-inactive-eval-args
    make-inactive-quote make-inactive-delay
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
    make-inactive-while make-inactive-extern
    make-inactive-authorize make-inactive-flag
    ;; inserting active objects
    make-tuple make-attr make-date))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for macro handling and main executable markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-assign) (make 'assign))
(define (make-inactive-with) (make-arity 'with 3))
(define (make-inactive-compound) (make 'compound))
(define (make-inactive-include) (make 'include))
(define (make-inactive-macro) (make 'macro))
(define (make-inactive-xmacro) (make 'xmacro))
(define (make-inactive-function) (make 'func))
(define (make-inactive-drd-props) (make 'drd_props))
(define (make-inactive-eval) (make 'eval))
(define (make-inactive-provides) (make 'provides))
(define (make-inactive-value) (make 'value))
(define (make-inactive-argument) (make 'arg))
(define (make-inactive-get-label) (make 'get_label))
(define (make-inactive-get-arity) (make 'get_arity))
(define (make-inactive-map-args) (make 'map_args))
(define (make-inactive-eval-args) (make 'eval_args))
(define (make-inactive-quote) (make 'quote))
(define (make-inactive-delay) (make 'delay))
(define (make-inactive-hold) (make 'hold))
(define (make-inactive-release) (make 'release))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for inserting computational markup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-inactive-or) (make 'or))
(define (make-inactive-xor) (make 'xor))
(define (make-inactive-and) (make 'and))
(define (make-inactive-not) (make 'not))
(define (make-inactive-plus) (make 'plus))
(define (make-inactive-minus) (make 'minus))
(define (make-inactive-times) (make 'times))
(define (make-inactive-over) (make 'over))
(define (make-inactive-div) (make 'div))
(define (make-inactive-mod) (make 'mod))
(define (make-inactive-merge) (make 'merge))
(define (make-inactive-length) (make 'length))
(define (make-inactive-range) (make 'range))
(define (make-inactive-number) (make 'number))
(define (make-inactive-date) (make 'date))
(define (make-inactive-translate) (make 'translate))
(define (make-inactive-find-file) (make 'find_file))
(define (make-inactive-is-tuple) (make 'is_tuple))
(define (make-inactive-look-up) (make 'look_up))
(define (make-inactive-equal) (make 'equal))
(define (make-inactive-unequal) (make 'unequal))
(define (make-inactive-less) (make 'less))
(define (make-inactive-lesseq) (make 'lesseq))
(define (make-inactive-greater) (make 'greater))
(define (make-inactive-greatereq) (make 'greatereq))
(define (make-inactive-if) (make 'if))
(define (make-inactive-var-if) (make 'var_if))
(define (make-inactive-case) (make 'case))
(define (make-inactive-while) (make 'while))
(define (make-inactive-extern) (make 'extern))
(define (make-inactive-authorize) (make 'authorize))
(define (make-inactive-flag) (make 'flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting active objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-tuple) (make 'tuple))
(define (make-attr) (make 'attr))
(define (make-date)
  (if (== (get-env "language") "english")
      (insert-tree (object->tree '(date "%B %d, %Y")))
      (insert-tree (object->tree '(date "%d %B %Y")))))
