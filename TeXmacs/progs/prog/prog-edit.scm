
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
;; Bracket handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-bracket-find* s pos inc br ibr level)
  (cond ((or (< pos 0) (>= pos (string-length s))) (- -1 level))
	((and (== level 0) (== (string-ref s pos) br)) pos)
	((== (string-ref s pos) br)
	 (string-bracket-find* s (+ pos inc) inc br ibr (- level 1)))
	((== (string-ref s pos) ibr)
	 (string-bracket-find* s (+ pos inc) inc br ibr (+ level 1)))
	(else (string-bracket-find* s (+ pos inc) inc br ibr level))))

(define (string-bracket-find s pos inc br ibr level)
  (with r (string-bracket-find* s pos inc br ibr level)
    (and (>= r 0) r)))

(define (string-bracket-level s pos inc br ibr)
  (with ret (string-bracket-find* s pos inc br ibr 0)
    (if (< ret 0) (- -1 ret)
	(string-bracket-level s (+ ret inc) br ibr))))

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
