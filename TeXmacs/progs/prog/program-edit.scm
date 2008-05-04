
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : program-edit.scm
;; DESCRIPTION : editing verbatim programs
;; COPYRIGHT   : (C) 2008  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (prog program-edit)
  (:use (utils library tree)
	(utils library cursor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Getting lines in a textual document
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (program-tree)
  (:synopsis "get the entire program tree")
  (let* ((ct (cursor-tree))
	 (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document) dt)))

(tm-define (line-ok?)
  (:synopsis "are we inside the line of a textual document?")
  (let* ((ct (cursor-tree))
	 (dt (tree-ref ct :up)))
    (and (tree-atomic? ct) (tree-is? dt 'document))))

(tm-define (line-tree . opt)
  (:synopsis "get current line or the line with a specified offset")
  (with doc (tree-ref (cursor-tree) :up)
      (and-let* ((ok? (tree-is? doc 'document))
		 (cln (tree-index (cursor-tree)))
		 (off (if (null? opt) 0 (car opt)))
		 (sln (+ cln off))
		 (OK? (and (>= sln 0) (< sln (tree-arity doc)))))
	(tree-ref doc sln))))

(tm-define (line-string . opt)
  (:synopsis "get current line or the line with a specified offset")
  (and-with t (apply line-tree opt)
    (and (tree-atomic? t) (tree->string t))))

(tm-define (line-row)
  (:synopsis "get the vertical position on the current line")
  (and (line-ok?) (cADr (cursor-path))))

(tm-define (line-column)
  (:synopsis "get the horizontal position on the current line")
  (and (line-ok?) (cAr (cursor-path))))

(tm-define (program-line row)
  (:synopsis "get the character at a given @row and @column")
  (and-with doc (program-tree)
    (and-with par (tree-ref doc row)
      (and (tree-atomic? par) (tree->string par)))))

(tm-define (program-character row col)
  (:synopsis "get the character at a given @row and @col")
  (and-with par (program-line row)
    (and (>= col 0) (< col (string-length par)) (string-ref par col))))

(tm-define (program-go-to row col)
  (:synopsis "go to the character at a given @row and @col")
  (and-with doc (program-tree)
    (tree-go-to doc row col)))

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

(tm-define (line-get-indent)
  (:synopsis "get the indentation of the current line")
  (and (line-ok?) (string-get-indent (line-string))))

(tm-define (line-set-indent i)
  (:synopsis "set the indentation of the current line to @i spaces")
  (when (line-ok?)
    (with t (cursor-tree)
      (tree-set t (string-set-indent (tree->string t) i)))))
