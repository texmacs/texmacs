
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme-edit.scm
;; DESCRIPTION : editing scheme programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog scheme-edit)
  (:use (prog prog-edit)
        (prog scheme-tools) (prog scheme-autocomplete)
        (utils misc tm-keywords)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Treatment of special characters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (previous-special s col)
  (cond ((< col 0) col)
        ((in? (string-ref s col) '(#\( #\) #\space #\")) col)
(else (previous-special s (- col 1)))))

(define (next-special s col)
  (cond ((>= col (string-length s)) col)
        ((in? (string-ref s col) '(#\( #\) #\space #\")) col)
(else (next-special s (+ col 1)))))

(define (next-word doc row col)
  (and-with par (program-row row)
    (and (>= col 0)
         (<= col (string-length par))
         (substring par col (next-special par col)))))

(define (quoted-backwards s col)
  (cond ((< col 0) col)
        ((== (string-ref s col) #\") (- col 1))
(else (quoted-backwards s (- col 1)))))

(define (quoted-forwards s col)
  (cond ((>= col (string-length s)) col)
        ((== (string-ref s col) #\") (+ col 1))
(else (quoted-forwards s (+ col 1)))))

(define (string-uncommented s col)
  (cond ((>= col (string-length s)) col)
        ((== (string-ref s col) #\;) col)
             ((== (string-ref s col) #\")
         (string-uncommented s (quoted-forwards s (+ col 1))))
        (else (string-uncommented s (+ col 1)))))

(define (string-uncommented-length s)
  (string-uncommented s 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search for previous arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (previous-argument doc row col level)
  ;;(display* "search " row ", " col "\n")
  (and (>= row 0)
       (< row (tree-arity doc))
       (tree-atomic? (tree-ref doc row))
       (with par (tree->string (tree-ref doc row))
         (cond ((>= col (string-length par))
                (with len (string-uncommented-length par)
                  (previous-argument doc row (- len 1) level)))
               ((< col 0)
                (previous-argument doc (- row 1) 1000000000 level))
               ((== (string-ref par col) #\()
                    (cond ((== level 0) #f)
                          ((== level 1) (cons row col))
                          (else (previous-argument
                                 doc row (- col 1) (- level 1)))))
                ((== (string-ref par col) #\))
                (previous-argument doc row (- col 1) (+ level 1)))
               ((== (string-ref par col) #\space)
                (previous-argument doc row (- col 1) level))
               ((== (string-ref par col) #\")
         (with ncol (quoted-backwards par (- col 1))
           (if (== level 0) (cons row (+ ncol 1))
               (previous-argument doc row ncol level))))
       (else (with ncol (previous-special par (- col 1))
               (if (== level 0) (cons row (+ ncol 1))
                   (previous-argument doc row ncol level))))))))

(define (previous-arguments doc row col bound)
  (with arg (previous-argument doc row col 0)
    (cond ((not arg) '())
          ((<= bound 0) '((-1 -1)))
          (else (let* ((nrow (car arg))
                       (ncol (- (cdr arg) 1))
                       (nbound (if (== nrow row) bound (- bound 1))))
                  (cons arg (previous-arguments doc nrow ncol nbound)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic indentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reference-type doc l)
  (if (null? l) ""
      (with arg (cAr l)
        (or (next-word doc (car arg) (cdr arg)) ""))))

(define (reference-argument l)
  (cond ((null? l) #f)
        ((null? (cdr l)) (car l))
        ((null? (cddr l)) (car l))
        (else (let* ((a1 (car l))
                     (a2 (cadr l)))
                (if (< (car a2) (car a1)) a1
                    (reference-argument (cdr l)))))))   

(define (tm-count l inc)
  ;; helper routine for correct indentation of <less> and <gtr>
  (cond ((null? l) 0)
        ((== (car l) #\<) (tm-count (cdr l) 0))
        ((== (car l) #\>) (+ (tm-count (cdr l) 1) 1))
        (else (+ (tm-count (cdr l) inc) inc))))

(define (get-offset doc a)
  ;; helper routine for correct indentation of <less> and <gtr>
  (with s (tree->string (tree-ref doc (car a)))
    (tm-count (string->list (substring s 0 (cdr a))) 1)))

(tm-define (program-compute-indentation doc row col)
  (:require in-prog-scheme?)
  (let* ((l (previous-arguments doc row col 10))
         (t (reference-type doc l))
         (i (indent-get-arity t))
         (n (length l))
         (a (reference-argument l)))
    (cond ((not a) 0)
          ((not i) (get-offset doc a))
          ((<= n i) (+ (get-offset doc (cAr l)) 3))
          ((== n (+ i 1)) (+ (get-offset doc (cAr l)) 1))
          (else (get-offset doc a)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface for autocompletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-variant t forwards?)
  (:mode in-prog-scheme?)
  (if (not scheme-completions-built?) (scheme-completions-rebuild))
  (custom-complete (tm->tree (scheme-completions (cursor-word)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic insertion, highlighting and selection of brackets and quotes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (scheme-bracket-open lbr rbr)
  (bracket-open lbr rbr "\\"))

(tm-define (scheme-bracket-close lbr rbr)
  (bracket-close lbr rbr "\\"))

; TODO: select strings first
(tm-define (kbd-select-enlarge)
  (:require prog-select-brackets?)
  (:mode in-prog-scheme?)
  (program-select-enlarge "(" ")"))

(tm-define (notify-cursor-moved status)
  (:require prog-highlight-brackets?)
  (:mode in-prog-scheme?)
  (select-brackets-after-movement "([{" ")]}" "\\"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preferences for syntax highlighting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (notify-scheme-syntax var val)
  (syntax-read-preferences "scheme"))

(define-preferences
  ("syntax:scheme:none" "red" notify-scheme-syntax)
  ("syntax:scheme:comment" "brown" notify-scheme-syntax)
  ("syntax:scheme:keyword" "#309090" notify-scheme-syntax)
  ("syntax:scheme:error" "dark red" notify-scheme-syntax)
  ("syntax:scheme:constant_number" "#4040c0" notify-scheme-syntax)
  ("syntax:scheme:constant_string" "dark grey" notify-scheme-syntax)
  ("syntax:scheme:constant_char" "#333333" notify-scheme-syntax)
  ("syntax:scheme:variable_identifier" "#204080" notify-scheme-syntax)
  ("syntax:scheme:declare_category" "#d030d0" notify-scheme-syntax))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy and Paste
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (textual? x)
  (or (tm-atomic? x)
    (and (tm-in? x '(concat document))
      (forall? textual? (tm-children x)))))

(tm-define (kbd-copy)
  (:mode in-prog-scheme?)
  (:require (textual? (selection-tree)))
  (clipboard-copy-export "scheme" "primary"))

(tm-define (kbd-cut)
  (:mode in-prog-scheme?)
  (:require (textual? (selection-tree)))
  (clipboard-cut-export "scheme" "primary"))

(tm-define (kbd-paste)
  (:mode in-prog-scheme?)
  (:require (textual? (clipboard-get "primary")))
  (clipboard-paste-import "scheme" "primary"))
