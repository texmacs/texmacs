
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
  (:use (text text-kbd)
        (math math-speech)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text with inline mathematical formulas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (accept-middle? lan l r) #t)

(define (accept-start? lan l r)
  (cond ((< (length r) 1) #f)
        ((speech-has? lan 'dangerous (car l))
         (and (>= (length l) 2) (>= (length r) 2)
              (let* ((l1 (speech-rewrite lan 'math (car l)))
                     (r1 (car r))
                     (l2 (speech-rewrite lan 'math (cadr l)))
                     (r2 (cadr r)))
                (cond ((in? r1 lowercase-letters)
                       (cond ((!= l1 r1) #f)
                             ((!= l2 r2) #f)
                             ((string-number? (cadr l)) #t)
                             ((speech-has? lan 'number (cadr l)) #t)
                             ((speech-has? lan 'infix (cadr l)) #t)
                             ((speech-has? lan 'postfix (cadr l)) #t)
                             ((speech-has? lan 'prefix-infix (cadr l)) #t)
                             ((speech-has? lan 'separator (cadr l)) #t)
                             (else #f)))
                      (else #f)))))
        (else #t)))

(define (accept-end? lan l r)
  (cond ((< (length r) 1) #f)
        ((in? (cAr l) punctuation-symbols) #f)
        ((in? (string-take-right (cAr l) 1) punctuation-symbols)
         (with h (string-drop-right (cAr l) 1)
           (accept-end? lan (rcons (cDr l) h) (cDr r))))
        ((speech-has? lan 'dangerous (cAr l))
         (and (>= (length l) 2) (>= (length r) 2)
              (let* ((l1 (speech-rewrite lan 'math (cAr l)))
                     (r1 (cAr r))
                     (l2 (speech-rewrite lan 'math (cADr l)))
                     (r2 (cADr r)))
                (cond ((in? r1 lowercase-letters)
                       (cond ((!= l1 r1) #f)
                             ((!= l2 r2) #f)
                             ((speech-has? lan 'infix (cADr l)) #t)
                             ((speech-has? lan 'prefix (cADr l)) #t)
                             ((speech-has? lan 'prefix-infix (cADr l)) #t)
                             ((speech-has? lan 'separator (cADr l)) #t)
                             (else #f)))
                      (else #f)))))
        (else #t)))

(define (text-math-speech* lan pre l post)
  (let* ((s1 (string-recompose pre " "))
         (s2 (string-recompose l " "))
         (s3 (string-recompose post " ")))
    ;;(display* "  Found " s1 " / " s2 " / " s3 "\n")
    (kbd-insert s1)
    (speech-inline 'math)
    (kbd-speech s2)
    (with t (tree-innermost 'math)
      (when t
        (tree-go-to t 0 :end)
        (with prev (before-cursor)
          (if (in? prev punctuation-symbols)
              (begin
                (cut-before-cursor)
                (tree-go-to t :end)
                (insert prev))
              (tree-go-to t :end))))
      (when (!= s3 "")
        (kbd-speech s3)))))

(define (text-math-speech lan pre l post)
  (let* ((s (string-recompose l " "))
         (w (speech-rewrite lan 'math s))
         (r (string-decompose w " ")))
    (cond ((or (null? l) (null? r))
           (text-speech* lan pre post))
          ((speech-has? lan 'math-mode (car l))
           (set! l (cdr l))
           (when (speech-has? lan 'text-mode (cAr l)) (set! l (cDr l)))
           (text-math-speech* lan pre l post))
          ((not (accept-middle? lan l r))
           (text-speech* lan (append pre l) post))
          ((not (accept-start? lan l r))
           (text-math-speech lan (append pre (list (car l))) (cdr l) post))
          ((and (null? (cdr l)) (not (accept-end? lan l r)))
           (text-speech* lan (append pre l) post))
          ((not (accept-end? lan l r))
           (text-math-speech lan pre (cDr l) (cons (cAr l) post)))
          ((not (speech-recognizes? lan 'math s))
           (text-math-speech lan pre (cDr l) (cons (cAr l) post)))
          (else (text-math-speech* lan pre l post)))))

(define (longest-math-prefix* lan l)
  (cond ((null? l) l)
        ((with s (locase-all (car l))
           (!= (letterize s) s))
         (cons (car l) (longest-math-prefix* lan (cdr l))))
        ((speech-has? lan 'skip (car l)) (list))
        ((not (speech-accepts? lan 'math (car l))) (list))
        (else (cons (car l) (longest-math-prefix* lan (cdr l))))))

(define (trim-longest-math-prefix lan l)
  (cond ((null? l) l)
        ((speech-border-accepts? lan 'math (cAr l)) l)
        (else (trim-longest-math-prefix lan (cDr l)))))

(define (speech-until-text lan l)
  (cond ((null? l) l)
        ((speech-has? lan 'text-mode (car l)) (list (car l)))
        (else (cons (car l) (speech-until-text lan (cdr l))))))

(define (longest-math-prefix lan l)
  (cond ((null? l) l)
        ((speech-has? lan 'math-mode (car l)) (speech-until-text lan l))
        ((not (speech-border-accepts? lan 'math (car l))) (list))
        (else (trim-longest-math-prefix lan (longest-math-prefix* lan l)))))

(define (text-speech* lan h t)
  (if (null? t)
      (when (nnull? h)
        (kbd-insert (string-recompose h " ")))
      (with l (longest-math-prefix lan t)
        (if (null? l)
            (text-speech* lan (rcons h (car t)) (cdr t))
            (with r (sublist t (length l) (length t))
              ;;(display* "Mathematics " (string-recompose l " ") "\n")
              (text-math-speech lan h l r))))))

(define (text-speech s*)
  (let* ((lan (speech-language))
         (s (speech-rewrite lan 'text-hack s*))
         (l (string-decompose s " ")))
    (when (nnull? l)
      (text-speech* lan (list (car l)) (cdr l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized speech driver routines for text mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (clean-text-speech l)
  (cond ((or (null? l) (null? (cdr l))) l)
        ((and (string-locase? (car l)) (string-upcase? (cadr l)))
         (cons* (car l) " " (clean-text-speech (cdr l))))
        ((and (string-number? (car l)) (not (string-number? (cadr l))))
         (cons* (car l) " " (clean-text-speech (cdr l))))
        (else (cons (car l) (clean-text-speech (cdr l))))))

(define (requires-lowercase? t)
  (and (tree? t)
       (tree-empty? t)
       (tree-ref t :up)
       (tree-in? (tree-ref t :up) '(abbr em name samp strong verbatim))))

(tm-define (kbd-speech S)
  (:mode in-std-text?)
  (set! S (list->tmstring (clean-text-speech (tmstring->list S))))
  (display* "Text speech " S "\n")
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further textual speech commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (speech-inline . args)
  (with prev (before-cursor)
    (when (and prev (!= prev " "))
      (kbd-space))
    (apply make args)))

(tm-define (speech-proof)
  (with-innermost t enunciation-context?
    (tree-go-to t :end))
  (make 'proof))
