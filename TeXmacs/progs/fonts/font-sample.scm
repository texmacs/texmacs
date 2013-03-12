
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

(define (closest-font in flag?)
  (let* ((fn  (logical-font-public (car in) (cadr in)))
         (fam (logical-font-family fn))
         (var (logical-font-variant fn))
         (ser (logical-font-series fn))
         (sh  (logical-font-shape fn))
         (lf  (logical-font-private fam var ser sh))
         (sfn (logical-font-search lf flag?)))
    sfn))

(define (check-feature? fn feature)
  (set! fn (closest-font fn #f))
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
  (set! fn (closest-font fn #f))
  (with fl (font-database-characteristics (car fn) (cadr fn))
    (search-characteristic (string-append which "=") fl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Inserting tables with various font samples or data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
        ((== kind :fillp) "fillp")
        ((== kind :vcnt) "vcnt")
        ((== kind :slant) "slant")
        ((== kind :lasprat) "lasprat")
        ((== kind :pasprat) "pasprat")
        ((== kind :lhw) "lhw")
        ((== kind :lvw) "lvw")
        (else "")))

(define (build-font-header-row kinds)
  (with build-cell (lambda (kind) `(cell ,(build-font-header-cell kind)))
    `(row ,@(map build-cell kinds))))

(define (build-with-font fn body)
  (let* ((cfn (closest-font fn #f))
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
        ((== kind :fillp) (get-characteristic fn "fillp"))
        ((== kind :vcnt) (get-characteristic fn "vcnt"))
        ((== kind :slant) (get-characteristic fn "slant"))
        ((== kind :lasprat) (get-characteristic fn "lasprat"))
        ((== kind :pasprat) (get-characteristic fn "pasprat"))
        ((== kind :lhw) (get-characteristic fn "lhw"))
        ((== kind :lvw) (get-characteristic fn "lvw"))
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
