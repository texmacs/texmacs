
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tiles.scm
;; DESCRIPTION : automatic generation of hatch tiles
;; COPYRIGHT   : (C) 2018  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (utils misc tiles))

(define pattern-path "$TEXMACS_PATH/misc/patterns")
(define dummy-pattern "$TEXMACS_PATH/misc/patterns/pine.png")
(define line-hatches-path "$TEXMACS_PATH/misc/patterns/lines")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-line-hatch nums perc art?)
  (let* ((name (cond (art? "lines-artistic")
                     ((> (car nums) 3) "lines-basic")
                     (else "lines-default")))
         (num  (apply string-append (map number->string nums)))
         (dest (string-append pattern-path "/" name "/"
                              name "-" num "-" perc ".png"))
         (fill (string-append "0." perc))
         (art  (if art? "0.1" "0"))
         (eff0 `())
         (eff1 `(eff-hatch "0" "6" "10" ,fill ,art))
         (eff2 `(eff-hatch "0" "-6" "9" ,fill ,art))
         (eff3 `(eff-hatch "0" "12" "2" ,fill ,art))
         (eff4 `(eff-hatch "0" "12" "0" ,fill ,art))
         (eff5 `(eff-hatch "0" "0" "12" ,fill ,art))
         (eff6 `(eff-hatch "0" "8" "8"  ,fill ,art))
         (eff7 `(eff-hatch "0" "-8" "8" ,fill ,art))
         (effl (list eff0 eff1 eff2 eff3 eff4 eff5 eff6 eff7))
         (effs (map (cut list-ref effl <>) nums))
         (eff  (if (= (length nums) 1) (car effs) `(eff-superpose ,@effs))))
    (display* "Generating " dest "\n")
    (apply-effect eff (list dummy-pattern) dest 360 360)))

(define (generate-line-hatches)
  (for (art? (list #f #t))
    (for (nums '((1) (2) (3) (1 2) (1 2 3)))
      (for (perc (list "05" "10" "15" "20" "25" "50"))
        (generate-line-hatch nums perc art?))))
  (for (nums '((4) (5) (6) (7) (4 5) (6 7) (4 5 6 7)))
    (for (perc (list "05" "10" "15" "20" "25" "50"))
      (generate-line-hatch nums perc #f))))

(define (generate-dots-hatch num fp)
  (let* ((name (list-ref (list "A" "B" "C") num))
         (dest (string-append pattern-path "/dots-hatches/dots-"
                              name "-" (number->string fp) ".png"))
         (fact (if (== num 1) 0.6 0.55))
         (fill (number->string (* fact (sqrt (* 0.01 fp)))))
         (eff1 `(eff-dots "0" "12" "0" "6" "10" ,fill "0"))
         (eff2 `(eff-dots "0" "12" "0" "0" "12" ,fill "0"))
         (eff3 `(eff-dots "0" "12" "0" "6" "10" ,fill "0.15"))
         (effl (list eff1 eff2 eff3))
         (eff  (list-ref effl num)))
    (display* "Generating " dest "\n")
    (apply-effect eff (list dummy-pattern) dest 360 360)))

(define (generate-dots-hatches)
  (for (num '(0 1 2))
    (for (fp '(5 10 15 20 25 30 40 50 60 70 80 90))
      (generate-dots-hatch num fp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Master routine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (generate-tiles)
  (when (not (url-exists? (string-append pattern-path "/lines-artistic")))
    (system-mkdir (string-append pattern-path "/lines-artistic")))
  (when (not (url-exists? (string-append pattern-path "/lines-basic")))
    (system-mkdir (string-append pattern-path "/lines-basic")))
  (when (not (url-exists? (string-append pattern-path "/lines-default")))
    (system-mkdir (string-append pattern-path "/lines-default")))
  (when (not (url-exists? (string-append pattern-path "/dots-hatches")))
    (system-mkdir (string-append pattern-path "/dots-hatches")))
  ;;(generate-line-hatches)
  (generate-dots-hatches))
