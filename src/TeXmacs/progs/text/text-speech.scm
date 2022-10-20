
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : text-speech.scm
;; DESCRIPTION : control textual editing via speech
;; COPYRIGHT   : (C) 2022  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text text-speech)
  (:use (text text-kbd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech with mathematical subexpressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (math-start-letter lan l)
  (if (null? l) (list)
      (with r (speech-rewrite lan 'math (car l))
        (cond ((speech-has? lan 'roman r) (list (car l)))
              ((speech-has? lan 'greek r) (list (car l)))
              ((speech-has? lan 'modify-letters r)
               (with t (math-start-letter lan (cdr l))
                 (if (null? t) t (cons (car l) t))))
              (else (list))))))

(define (math-start-other lan l)
  (if (null? l) (list)
      (let* ((r  (speech-rewrite lan 'math (car l)))
             (r2 (and (nnull? (cdr l))
                      (with s (string-append (car l) " " (cadr l))
                        (speech-rewrite lan 'math s)))))
        (cond ((string->number (car l)) (list (car l)))
              ((speech-has? lan 'numbers r) (list (car l)))
              ((speech-has? lan 'accept-start r) (list (car l)))
              ((and r2 (speech-has? lan 'accept-start r2))
               (list (car l) (cadr l)))
              (else (list))))))

(define (math-complete-prefix lan l)
  (if (null? l) (list)
      (let* ((l* (map locase-all l))
             (s (string-recompose l* " "))
             (r* (speech-rewrite lan 'math s))
             (rl (string-decompose r* " "))
             (r (cAr rl))
             (ok? (speech-recognizes? lan 'math s)))
        (cond ((or (not ok?)
                   (speech-has? lan 'forbid s))
               (math-complete-prefix lan (cDr l)))
              ((and (speech-has? lan 'roman r)
                    (>= (length rl) 2)
                    (speech-has? lan 'modify-letters (cAr (cDr l))))
               l)
              ((or (speech-has? lan 'dangerous-end (cAr l*))
                   (exists? (cut speech-has? lan 'dangerous <>) l*))
               (math-complete-prefix lan (cDr l)))
              ((string->number r) l)
              ((speech-has? lan 'numbers r) l)
              ((speech-has? lan 'roman r) l)
              ((speech-has? lan 'greek r) l)
              ((speech-has? lan 'accept-end r) l)
              (else (math-complete-prefix lan (cDr l)))))))

(define (math-cont-prefix lan h l)
  (if (and (nnull? l) (speech-accepts? lan 'math (car l)))
      (math-cont-prefix lan (append h (list (car l))) (cdr l))
      (math-complete-prefix lan h)))

(define (math-prefix lan l)
  (cond ((null? l) l)
        ((not (speech-accepts? lan 'math (car l))) (list))
        ((nnull? (math-start-letter lan l))
         (let* ((h (math-start-letter lan l))
                (t (sublist l (length h) (length l))))
           (math-cont-prefix lan h t)))
        ((nnull? (math-start-other lan l))
         (let* ((h (math-start-other lan l))
                (t (sublist l (length h) (length l))))
           (math-cont-prefix lan h t)))
        (else (list))))

(define (text-speech* lan h t)
  ;;(display* "  Try " (string-recompose t " ") "\n")
  (if (null? t)
      (when (nnull? h)
        (kbd-insert (string-recompose h " ")))
      (with l (math-prefix lan t)
        (if (null? l)
            (text-speech* lan (rcons h (car t)) (cdr t))
            (let* ((r (sublist t (length l) (length t)))
                   (s1 (string-recompose h " "))
                   (s2 (string-recompose l " "))
                   (s3 (string-recompose r " ")))
              ;;(display* "  Found " s1 " / " s2 " / " s3 "\n")
              (if (not (speech-recognizes? lan 'math s2))
                  (kbd-insert (kbd-insert (string-recompose (append h t) " ")))
                  (begin
                    (kbd-insert s1)
                    (speech-inline 'math)
                    (kbd-speech s2)
                    (with t (tree-innermost 'math)
                      (when t (tree-go-to t :end))
                      (when (!= s3 "")
                        (kbd-speech s3))))))))))

(define (text-speech s*)
  (let* ((lan (speech-language))
         (s (speech-rewrite lan 'text-hack s*))
         (l (string-decompose s " ")))
    (when (nnull? l)
      (text-speech* lan (list (car l)) (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Speech helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (requires-lowercase? t)
  (and (tree? t)
       (tree-empty? t)
       (tree-ref t :up)
       (tree-in? (tree-ref t :up) '(abbr em name samp strong verbatim))))

(tm-define (kbd-speech S)
  (:mode in-std-text?)
  ;;(display* "Text speech " S "\n")
  (let* ((prev1 (before-cursor))
         (prev2 (before-before-cursor))
         (prev  (if (== prev1 " ") prev2 prev1))
         (spc?  (!= prev1 " ")))
    (cond ((speech-make S) (noop))
          ((in? prev (list "." "!" "?"))
           (when spc? (kbd-space))
           (text-speech S))
          ((in? prev (list "," ":" ";"))
           (when spc? (kbd-space))
           (text-speech (locase-first S)))
          (prev
           (when (and spc? (nin? S (list "." "," ":" ";" "!" "?")))
             (kbd-space))
           (text-speech (locase-first S)))
          ((requires-lowercase? (cursor-tree))
           (text-speech (locase-first S)))
          (else (text-speech S)))))

(tm-define (speech-inline . args)
  (with prev (before-cursor)
    (when (and prev (!= prev " "))
      (kbd-space))
    (apply make args)))

(tm-define (speech-proof)
  (with-innermost t enunciation-context?
    (tree-go-to t :end))
  (make 'proof))
