
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-sample.scm
;; DESCRIPTION : Sample characters in one or several fonts
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-sample))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting font tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-font-cells r c1 c2 c)
  (cond ((== c -1)
         (cons `(cell ,(integer->hexadecimal r))
               (build-font-cells r c1 c2 0)))
        ((>= c 16) (list))
        ((or (< c c1) (> c c2))
         (cons `(cell "") (build-font-cells r c1 c2 (+ c 1))))
        (else
          (let* ((i (+ (* 16 r) c))
                 (hex (integer->hexadecimal i))
                 (s (string-append "<#" hex ">")))
            (cons `(cell ,s) (build-font-cells r c1 c2 (+ c 1)))))))

(define (build-font-row r c1 c2)
  `(row ,@(build-font-cells r c1 c2 -1)))

(tm-define (build-font-table i1 i2)
  (let* ((r1 (quotient i1 16))
         (r2 (quotient i2 16))
         (c1 (remainder i1 16))
         (c2 (remainder i2 16))
         (hc (lambda (i) `(cell ,(integer->hexadecimal i))))
         (fr `(row (cell "") ,@(map hc (.. 0 16)))))
    (if (== r1 r2)
        `(block (tformat (table ,fr
                                ,(build-font-row r1 c1 c2))))
        `(block (tformat (table ,fr
                                ,(build-font-row r1 c1 15)
                                ,@(map (cut build-font-row <> 0 15)
                                       (.. (+ r1 1) r2))
                                ,(build-font-row r2 0 c2)))))))
