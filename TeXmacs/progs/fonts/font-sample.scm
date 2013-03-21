
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
;; Inserting character tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-character-cells r c1 c2 c)
  (cond ((== c -1)
         (cons `(cell ,(integer->hexadecimal r))
               (build-character-cells r c1 c2 0)))
        ((>= c 16) (list))
        ((or (< c c1) (> c c2))
         (cons `(cell "") (build-character-cells r c1 c2 (+ c 1))))
        (else
          (let* ((i (+ (* 16 r) c))
                 (hex (integer->hexadecimal i))
                 (s (string-append "<#" hex ">")))
            (when (< i 128) (set! s (utf8->cork (cork->utf8 s))))
            (cons `(cell ,s) (build-character-cells r c1 c2 (+ c 1)))))))

(define (build-character-row r c1 c2)
  `(row ,@(build-character-cells r c1 c2 -1)))

(tm-define (build-character-table i1 i2)
  (let* ((r1 (quotient i1 16))
         (r2 (quotient i2 16))
         (c1 (remainder i1 16))
         (c2 (remainder i2 16))
         (hc (lambda (i) `(cell ,(integer->hexadecimal i))))
         (fr `(row (cell "") ,@(map hc (.. 0 16)))))
    (if (== r1 r2)
        `(block (tformat (table ,fr
                                ,(build-character-row r1 c1 c2))))
        `(block (tformat (table ,fr
                                ,(build-character-row r1 c1 15)
                                ,@(map (cut build-character-row <> 0 15)
                                       (.. (+ r1 1) r2))
                                ,(build-character-row r2 0 c2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutines for tables with various font samples or data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (new-local-fonts)
  (let* ((fams (font-database-delta-families))
         (xp (lambda (f) (map (lambda (s) (list f s))
                              (font-database-styles f)))))
    (append-map xp fams)))

(define (closest-font in)
  (let* ((fn  (logical-font-public (car in) (cadr in)))
         (fam (logical-font-family fn))
         (var (logical-font-variant fn))
         (ser (logical-font-series fn))
         (sh  (logical-font-shape fn))
         (lf  (logical-font-private fam var ser sh))
         (sfn (logical-font-search lf)))
    sfn))

(define (check-feature? fn feature)
  (set! fn (closest-font fn))
  (with lfn (logical-font-exact (car fn) (cadr fn))
    (in? feature lfn)))

(define (check-feature fn feature)
  (if (check-feature? fn feature)
      `(with "mode" "math" "<times>")
      ""))

(define (search-characteristic prefix l)
  (cond ((null? l) "")
        ((string-starts? (car l) prefix)
         (string-drop (car l) (string-length prefix)))
        (else (search-characteristic prefix (cdr l)))))

(define (get-characteristic fn which)
  (set! fn (closest-font fn))
  (with fl (font-database-characteristics (car fn) (cadr fn))
    (search-characteristic (string-append which "=") fl)))

(define (numeric-distance fn1 kind attr lim)
  (let* ((fn2 (cadr kind))
         (val1 (get-characteristic fn1 attr))
         (val2 (get-characteristic fn2 attr))
         (x1 (if (== val1 "") 1000000.0 (* 1.0 (string->number val1))))
         (x2 (if (== val2 "") 2000000.0 (* 1.0 (string->number val2))))
         (dx (abs (- x1 x2)))
         (r (min 1.0 (/ dx lim))))
    (number->string (inexact->exact (round (* 100 r))))))

(define (relative-distance fn1 kind attr base)
  (let* ((fn2 (cadr kind))
         (val1 (get-characteristic fn1 attr))
         (val2 (get-characteristic fn2 attr))
         (x1 (if (== val1 "") 1000.0 (+ 1.0 (abs (string->number val1)))))
         (x2 (if (== val2 "") 10000.0 (+ 1.0 (abs (string->number val2)))))
         (dx (abs (- (log x1) (log x2))))
         (r (/ dx (log base))))
    (number->string (inexact->exact (round (* 100 r))))))

(define (vector-distance fn1 kind attr scale)
  (let* ((fn2 (cadr kind))
         (val1 (get-characteristic fn1 attr))
         (val2 (get-characteristic fn2 attr))
         (r (trace-distance val1 val2 scale)))
    (number->string (inexact->exact (round (* 100 r))))))

(define (fn-distance fn1 kind)
  (let* ((fn2 (cadr kind))
         (r (font-distance fn1 fn2)))
    (number->string (inexact->exact (round (* 100 r))))))

(define (fn-distance* fn1 kind)
  (let* ((fn2 (cadr kind))
         (r (font-distance* fn1 fn2)))
    (number->string (inexact->exact (round (* 100 r))))))

(define (scaled-string fn1 kind)
  (let* ((fn2 (cadr kind))
         (s (caddr kind))
         (val1 (get-characteristic fn1 "ex"))
         (val2 (get-characteristic fn2 "ex")))
    (if (or (== val1 "") (== val2 "")) s
        (let* ((x1 (* 1.0 (string->number val1)))
               (x2 (* 1.0 (string->number val2)))
               (mag (/ x2 x1))
               (txt `(with "magnification" ,(number->string mag) ,s)))
          (build-with-font fn1 txt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting tables with various font samples or data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (unary? x fun)
  (and (list-2? x) (== (car x) fun)))

(define (binary? x fun)
  (and (list-3? x) (== (car x) fun)))

(define (build-font-header-cell kind)
  (cond ((string? kind) kind)
        ((== kind :name) "Name")
        ((== kind :thin) "th")
        ((== kind :light) "li")
        ((== kind :bold) "bf")
        ((== kind :black) "bl")
        ((== kind :oblique) "ob")
        ((== kind :italic) "it")
        ((== kind :smallcaps) "sc")
        ((== kind :condensed) "cd")
        ((== kind :wide) "wi")
        ((== kind :mono) "mo")
        ((== kind :sansserif) "ss")
        ((== kind :typewriter) "tt")
        ((== kind :pen) "pe")
        ((== kind :slant) "slant")
        ((== kind :ex) "ex")
        ((== kind :em) "em")
        ((== kind :lvw) "lvw")
        ((== kind :lhw) "lhw")
        ((== kind :uvw) "uvw")
        ((== kind :uhw) "uhw")
        ((== kind :fillp) "fillp")
        ((== kind :vcnt) "vcnt")
        ((== kind :lasprat) "lasprat")
        ((== kind :pasprat) "pasprat")
        ((== kind :loasc) "loasc")
        ((== kind :lodes) "lodes")
        ((== kind :dides) "dides")
        ((unary? kind :dist) "dist")
        ((unary? kind :dist*) "dist*")
        ((unary? kind :slant) "slant")
        ((unary? kind :ex) "ex")
        ((unary? kind :em) "em")
        ((unary? kind :lvw) "lvw")
        ((unary? kind :lhw) "lhw")
        ((unary? kind :uvw) "uvw")
        ((unary? kind :uhw) "uhw")
        ((unary? kind :fillp) "fillp")
        ((unary? kind :vcnt) "vcnt")
        ((unary? kind :lasprat) "lasprat")
        ((unary? kind :pasprat) "pasprat")
        ((unary? kind :loasc) "loasc")
        ((unary? kind :lodes) "lodes")
        ((unary? kind :dides) "dides")
        ((unary? kind :upw) "upw")
        ((unary? kind :uph) "uph")
        ((unary? kind :upc) "upc")
        ((unary? kind :low) "low")
        ((unary? kind :loh) "loh")
        ((unary? kind :loc) "loc")
        ((binary? kind :scale) (caddr kind))
        (else "")))

(define (build-font-header-row kinds)
  (with build-cell (lambda (kind) `(cell ,(build-font-header-cell kind)))
    `(row ,@(map build-cell kinds))))

(define (build-with-font fn body)
  (let* ((cfn (closest-font fn))
         (lfn (logical-font-public (car cfn) (cadr cfn))))
    `(with "font" ,(logical-font-family lfn)
           "font-family" ,(logical-font-variant lfn)
           "font-series" ,(logical-font-series lfn)
           "font-shape" ,(logical-font-shape lfn)
       ,body)))

(define (build-font-cell fn kind)
  (cond ((string? kind) (build-with-font fn kind))
        ((== kind :name) (string-append (car fn) " " (cadr fn)))
        ((== kind :thin) (check-feature fn "thin"))
        ((== kind :light) (check-feature fn "light"))
        ((== kind :bold) (check-feature fn "bold"))
        ((== kind :black) (check-feature fn "black"))
        ((== kind :oblique) (check-feature fn "oblique"))
        ((== kind :italic) (check-feature fn "italic"))
        ((== kind :smallcaps) (check-feature fn "smallcaps"))
        ((== kind :condensed) (check-feature fn "condensed"))
        ((== kind :wide) (check-feature fn "wide"))
        ((== kind :mono) (check-feature fn "mono"))
        ((== kind :sansserif) (check-feature fn "sansserif"))
        ((== kind :typewriter) (check-feature fn "typewriter"))
        ((== kind :pen) (check-feature fn "pen"))
        ((== kind :slant) (get-characteristic fn "slant"))
        ((== kind :ex) (get-characteristic fn "ex"))
        ((== kind :em) (get-characteristic fn "em"))
        ((== kind :lhw) (get-characteristic fn "lhw"))
        ((== kind :lvw) (get-characteristic fn "lvw"))
        ((== kind :uhw) (get-characteristic fn "uhw"))
        ((== kind :uvw) (get-characteristic fn "uvw"))
        ((== kind :fillp) (get-characteristic fn "fillp"))
        ((== kind :vcnt) (get-characteristic fn "vcnt"))
        ((== kind :lasprat) (get-characteristic fn "lasprat"))
        ((== kind :pasprat) (get-characteristic fn "pasprat"))
        ((== kind :loasc) (get-characteristic fn "loasc"))
        ((== kind :lodes) (get-characteristic fn "lodes"))
        ((== kind :dides) (get-characteristic fn "dides"))
        ((unary? kind :dist) (fn-distance fn kind))
        ((unary? kind :dist*) (fn-distance* fn kind))
        ((unary? kind :slant) (numeric-distance fn kind "slant" 33.3))
        ((unary? kind :ex) (relative-distance fn kind "ex" 1.5))
        ((unary? kind :em) (relative-distance fn kind "em" 1.5))
        ((unary? kind :lhw) (relative-distance fn kind "lhw" 2.0))
        ((unary? kind :lvw) (relative-distance fn kind "lvw" 2.0))
        ((unary? kind :uhw) (relative-distance fn kind "uhw" 2.0))
        ((unary? kind :uvw) (relative-distance fn kind "uvw" 2.0))
        ((unary? kind :fillp) (relative-distance fn kind "fillp" 1.25))
        ((unary? kind :vcnt) (relative-distance fn kind "vcnt" 1.25))
        ((unary? kind :lasprat) (relative-distance fn kind "lasprat" 1.33))
        ((unary? kind :pasprat) (relative-distance fn kind "pasprat" 1.33))
        ((unary? kind :loasc) (relative-distance fn kind "loasc" 1.5))
        ((unary? kind :lodes) (relative-distance fn kind "lodes" 1.5))
        ((unary? kind :dides) (relative-distance fn kind "dides" 1.5))
        ((unary? kind :upw) (vector-distance fn kind "upw" 0.33))
        ((unary? kind :uph) (vector-distance fn kind "uph" 0.33))
        ((unary? kind :upc) (vector-distance fn kind "upc" 0.33))
        ((unary? kind :low) (vector-distance fn kind "low" 0.33))
        ((unary? kind :loh) (vector-distance fn kind "loh" 0.33))
        ((unary? kind :loc) (vector-distance fn kind "loc" 0.33))
        ((binary? kind :scale) (scaled-string fn kind))
        (else "")))

(define (build-font-row fn kinds)
  (with build-cell (lambda (kind) `(cell ,(build-font-cell fn kind)))
    `(row ,@(map build-cell kinds))))

(tm-define (build-font-table fns kinds)
  `(block (tformat (table ,(build-font-header-row kinds)
                          ,@(map (cut build-font-row <> kinds) fns)))))

(tm-define (font-test)
  (with fns (new-local-fonts) ;;'(("Arial" "Italic") ("Fava" "Regular"))
    (with kinds (list :name :thin :light :bold :black
                      :oblique :italic :condensed :wide
                      :smallcaps :mono :sansserif :typewriter
                      "abc" "ABC" "123")
      (tm->tree (build-font-table fns kinds)))))

;;(tm-define (font-test)
;;  (with fns (new-local-fonts)
;;    (with kinds (list :name :thin :light :bold :black
;;                      "abc" "ABC" "123"
;;                      :fillp :vcnt :lasprat :pasprat :slant)
;;      (tm->tree (build-font-table fns kinds)))))

;; (tm-define (font-test)
;;   (with fns (new-local-fonts)
;;     (with kinds (list :name :condensed :wide
;;                       "abc" "ABC" "123"
;;                       :lasprat :pasprat :lhw :lvw :slant)
;;       (tm->tree (build-font-table fns kinds)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Find closest fonts sorted by distance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-all-fonts)
  (let* ((fams (font-database-families))
         (xp (lambda (f) (map (lambda (s) (list f s))
                              (font-database-styles f)))))
    (append-map xp fams)))

(define (font-distance fn1 fn2)
  (font-guessed-distance (car fn1) (cadr fn1) (car fn2) (cadr fn2)))

(define (font-distance* fn1 fn2)
  (let* ((c1 (font-database-characteristics (car fn1) (cadr fn1)))
         (c2 (font-database-characteristics (car fn2) (cadr fn2))))
    (characteristic-distance c1 c2)))

(tm-define (closest-fonts fn)
  (let* ((dist (lambda (fn2) (font-distance fn fn2)))
         (make (lambda (fn2) (list (dist fn2) fn2)))
         (l (map make (get-all-fonts)))
         (sl (sort l (lambda (x y) (<= (car x) (car y))))))
    sl))

(tm-define (show-closest-fonts fn)
  (with fns (sublist (map cadr (closest-fonts fn)) 0 25)
    (with kinds (list :name
                      (list :dist fn)
                      (list :dist* fn)
                      (list :slant fn)
                      (list :ex fn)
                      (list :em fn)
                      (list :lvw fn)
                      (list :lhw fn)
                      (list :fillp fn)
                      (list :vcnt fn)
                      (list :lasprat fn)
                      (list :pasprat fn)
                      (list :loasc fn)
                      (list :lodes fn)
                      (list :dides fn)
                      ;;(list :upw fn)
                      ;;(list :uph fn)
                      ;;(list :upc fn)
                      ;;(list :low fn)
                      ;;(list :loh fn)
                      ;;(list :loc fn)
                      (list :scale fn "abcdefghij")
                      (list :scale fn "ABCDEFGHIJ")
                      (list :scale fn "1234567890"))
      (tm->tree (build-font-table fns kinds)))))
