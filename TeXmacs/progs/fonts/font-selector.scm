
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : font-selector.scm
;; DESCRIPTION : Widget for font selection
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fonts font-selector)
  (:use (kernel gui menu-widget)))

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

(define (build-font-table i1 i2)
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

(tm-define (insert-font-table hexa-start hexa-end)
  (:argument hexa-start "First unicode character in hexadecimal")
  (:argument hexa-end "Last unicode character in hexadecimal")
  (insert (build-font-table (hexadecimal->integer hexa-start)
                            (hexadecimal->integer hexa-end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state of font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-font-family "TeXmacs Computer Modern")
(tm-define selector-font-style "Regular")
(tm-define selector-font-size "10")

(tm-define (selector-get-font)
  (logical-font-patch
    (logical-font-public selector-font-family selector-font-style)
    (selected-properties)))

(define (selector-initialize-font)
  (let* ((fam (get-env "font"))
         (var (get-env "font-family"))
         (ser (get-env "font-series"))
         (sh  (get-env "font-shape"))
         (sz  (get-env "font-base-size"))
         (lf  (logical-font-private fam var ser sh))
         (fn  (logical-font-search lf #t)))
    ;;(display* "lf= " lf "\n")
    ;;(display* "fn= " fn "\n")
    (set! selector-font-family (car fn))
    (set! selector-font-style (cadr fn))
    (set! selector-font-size sz)
    (selector-initialize-search)))

(tm-define (make-multi-with . l)
  (with t (if (selection-active-any?) (selection-tree) "")
    (if (selection-active-any?) (clipboard-cut "null"))
    (insert-go-to `(with ,@l ,t) (cons (length l) (path-end t '())))))

(tm-define (selector-apply-font)
  (when (!= selector-font-style "Unknown")
    (with fn (selector-get-font)
      (with l '()
        (when (!= selector-font-size (get-env "font-base-size"))
          (set! l (cons* "font-base-size" selector-font-size l)))
        (when (!= (logical-font-shape fn) (get-env "font-shape"))
          (set! l (cons* "font-shape" (logical-font-shape fn) l)))
        (when (!= (logical-font-series fn) (get-env "font-series"))
          (set! l (cons* "font-series" (logical-font-series fn) l)))
        (when (!= (logical-font-variant fn) (get-env "font-family"))
          (set! l (cons* "font-family" (logical-font-variant fn) l)))
        (when (!= (logical-font-family fn) (get-env "font"))
          (set! l (cons* "font" (logical-font-family fn) l)))
        (apply make-multi-with l)))))

(define (selector-font-simulate-comment)
  (let* ((fn  (selector-get-font))
	 (fam (logical-font-family fn))
         (var (logical-font-variant fn))
         (ser (logical-font-series fn))
         (sh  (logical-font-shape fn))
         (lf  (logical-font-private fam var ser sh))
         (fn2 (logical-font-search lf #f)))
    ;;(display* "fn = " fn "\n")
    ;;(display* "lf = " lf "\n")
    ;;(display* "fn2= " fn2 "\n")
    (cond ((and (== selector-font-family (car fn2))
		(== selector-font-style (cadr fn2))) "")
	  ((null? (selected-properties))
	   (string-append "  (" selector-font-family " " selector-font-style
			  " -> " (car fn2) " " (cadr fn2) ")"))
	  (else
	   (string-append "  (closest match: " (car fn2) " " (cadr fn2) ")")))))

(define standard-selector-text
  `(document
     "abcdefghijklmnopqrstuvwxyz"
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "0123456789, +-*/^=<less><gtr>"
     "([{|}]) :;!? !@#$%&"
     "אחימץü"
     ,(string-append 
       "<alpha><beta><gamma><delta><epsilon> "
       "<leq><geq><leqslant><geqslant><prec><succ> "
       "<rightarrow><Rightarrow><mapsto> "
       "<times><cdot><oplus><otimes>")))

(define current-selector-text standard-selector-text)

(tm-define (set-selector-text)
  (set! current-selector-text
        (or (and (selection-active-any?)
                 (tree->stree (selection-tree)))
            (tree->stree (buffer-tree)))))

(tm-define (set-selector-table hexa-start hexa-end)
  (:argument hexa-start "First unicode character in hexadecimal")
  (:argument hexa-end "Last unicode character in hexadecimal")
  (set! current-selector-text
        (build-font-table (hexadecimal->integer hexa-start)
                          (hexadecimal->integer hexa-end))))

(define (selector-font-demo-text)
  (with fn (selector-get-font)
    ;;(display* "Font: " fn "\n")
    ;;(display* "Internal font: " (logical-font-family fn)
    ;;          ", " (logical-font-variant fn)
    ;;          ", " (logical-font-series fn)
    ;;          ", " (logical-font-shape fn)
    ;;          ", " selector-font-size "\n")
    `(document
       (with
         "font" ,(logical-font-family fn)
         "font-family" ,(logical-font-variant fn)
         "font-series" ,(logical-font-series fn)
         "font-shape" ,(logical-font-shape fn)
         "font-base-size" ,selector-font-size
         ,current-selector-text))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global state for font searching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define selector-search-weight "Any")
(tm-define selector-search-slant "Any")
(tm-define selector-search-stretch "Any")
(tm-define selector-search-serif "Any")
(tm-define selector-search-spacing "Any")
(tm-define selector-search-case "Any")
(tm-define selector-search-device "Any")
(tm-define selector-search-purpose "Any")

(define (selector-initialize-search)
  (set! selector-search-weight "Any")
  (set! selector-search-slant "Any")
  (set! selector-search-stretch "Any")
  (set! selector-search-serif "Any")
  (set! selector-search-spacing "Any")
  (set! selector-search-case "Any")
  (set! selector-search-device "Any")
  (set! selector-search-purpose "Any"))

(define (selected-properties)
  (with l (list selector-search-weight
                selector-search-slant
                selector-search-stretch
                selector-search-serif
                selector-search-spacing
                selector-search-case
                selector-search-device
                selector-search-purpose)
    (list-filter l (cut != <> "Any"))))

(tm-define-macro (selector-search-set! var val)
  `(begin
     (set! ,var ,val)
     (delayed
       (refresh-now "font-family-selector"))))

(tm-define (selected-families)
  (search-font-families (selected-properties)))

(tm-define (selected-styles family)
  (search-font-styles family (selected-properties)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Font selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (font-default-sizes)
  '("5" "6" "7" "8" "9" "10" "11" "12" "14" "16" "18" "20"
    "24" "28" "32" "36" "40" "48" "64" "72" "96"
    "128" "144" "192"))

(tm-widget (font-family-selector)
  (vertical
    (bold (text "Family"))
    ===
    (resize ("300px" "300px" "2000px") ("350px" "350px" "2000px")
      (scrollable
        (choice (set! selector-font-family answer)
                (selected-families)
                selector-font-family)))))

(tm-widget (font-style-selector)
  (vertical
    (bold (text "Style"))
    ===
    (resize ("200px" "200px" "400px") ("350px" "350px" "2000px")
      (scrollable
        (choice (set! selector-font-style answer)
                (selected-styles selector-font-family)
                selector-font-style)))))

(tm-widget (font-size-selector)
  (vertical
    (bold (text "Size"))
    ===
    (resize ("75px" "75px" "75px") ("350px" "350px" "2000px")
      (scrollable
        (choice (set! selector-font-size answer)
                (font-default-sizes)
                selector-font-size)))))

(tm-widget (font-sample-text)
  (texmacs-output
    `(with "bg-color" "white"
       ,(selector-font-demo-text))
    '(style "generic")))

(tm-widget (font-properties-selector)
  (vertical
    (horizontal
      (glue #f #f 0 0)
      (bold (text "Filter"))
      (glue #f #f 0 0))
    ===
    (aligned
      ;;(item (text "Base family:")
      ;;  (enum (set! selector-font-family answer)
      ;;        (font-database-families)
      ;;        selector-font-family "120px"))
      ;;(item (text "Base style:")
      ;;  (enum (set! selector-font-style answer)
      ;;        (font-database-styles selector-font-family)
      ;;        selector-font-style "120px"))
      ;;(item ====== ======)
      (item (text "Weight:")
        (enum (selector-search-set! selector-search-weight answer)
              '("Any" "Light" "Medium" "Bold" "Black")
              selector-search-weight "120px"))
      (item (text "Slant:")
        (enum (selector-search-set! selector-search-slant answer)
              '("Any" "Normal" "Italic" "Oblique")
              selector-search-slant "120px"))
      (item (text "Stretch:")
        (enum (selector-search-set! selector-search-weight answer)
              '("Any" "Condensed" "Unstretched" "Wide")
              selector-search-weight "120px"))
      (item (text "Case:")
        (enum (selector-search-set! selector-search-case answer)
              '("Any" "Mixed" "Small Capitals")
              selector-search-case "120px"))
      (item ====== ======)
      (item (text "Serif:")
        (enum (selector-search-set! selector-search-serif answer)
              '("Any" "Serif" "Sans Serif")
              selector-search-serif "120px"))
      (item (text "Spacing:")
        (enum (selector-search-set! selector-search-spacing answer)
              '("Any" "Proportional" "Monospaced")
              selector-search-spacing "120px"))
      (item (text "Device:")
        (enum (selector-search-set! selector-search-device answer)
              '("Any" "Print" "Typewriter" "Digital"
		"Pen" "Art Pen" "Chalk" "Marker")
              selector-search-device "120px")))
    (horizontal (glue #f #t 0 0))))

(tm-widget (font-selector-demo)
  (hlist
    (bold (text "Sample text"))
    (text (selector-font-simulate-comment))
    >>>)
  ===
  (if (== current-selector-text standard-selector-text)
      (resize ("300px" "300px" "2000px") ("200px" "200px" "200px")
        (scrollable
          (link font-sample-text))))
  (if (!= current-selector-text standard-selector-text)
      (resize ("300px" "300px" "2000px") ("500px" "500px" "500px")
        (scrollable
          (link font-sample-text)))))

(tm-widget (font-selector quit)
  (padded
    (horizontal
      (refresh font-family-selector)
      ///
      (refresh font-style-selector auto)
      ///
      (link font-size-selector)
      ///
      (link font-properties-selector))
    === === ===
    (refresh font-selector-demo auto)
    === ===
    (explicit-buttons
      (hlist >>> ("Ok" (begin (selector-apply-font) (quit)))))))

(tm-define (open-font-selector)
  (selector-initialize-font)
  (dialogue-window font-selector noop "Font selector"))
