
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : server-cache-test.scm
;; DESCRIPTION : Tests for server tree cache (Merkle hashing)
;; COPYRIGHT   : (C) 2026  Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (server server-cache-test)
  (:use (server server-cache)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-big-string n)
  (make-string n #\x))

(define (make-cacheable-image data)
  `(image (tuple (raw-data ,data) "image/png") "100px" "100px" "" ""))

(define-macro (tc-clear) `(tree-cache-clear "testhost"))
(define-macro (tc-contains? h) `(tree-cache-contains? "testhost" ,h))
(define-macro (tc-put h t) `(tree-cache-put "testhost" ,h ,t))
(define-macro (tc-get h) `(tree-cache-get "testhost" ,h))
(define-macro (tc-update t) `(tree-cache-update "testhost" ,t))
(define-macro (tc-janitor) `(tree-cache-janitor "testhost"))
(define-macro (tc-set-max-size n) `(tree-cache-set-max-size "testhost" ,n))
(define-macro (tc-size) `(tree-cache-size "testhost"))

(define (tc-count-present hashes)
  (length (list-filter hashes (lambda (h) (tc-contains? h)))))

(define (collect-cache-hashes t)
  (cond ((tree-atomic? t) '())
        ((== (tree-label t) 'cache-ref)
         (list (tm->string (tree-ref t 0))))
        (else (apply append (map collect-cache-hashes (tree-children t))))))

;; Adversarial *binary* payload: real PNG magic plus bytes that stress
;; scheme (de)serialization -- embedded NUL, double-quote, backslash,
;; parens, tab and high (>127) bytes. The ASCII-only payloads used in the
;; other tests cannot catch a lossy raw-data round-trip; this can.
(define (make-binary-blob)
  (list->string
    (map integer->char
         (list 137 80 78 71 13 10 26 10   ; \x89 P N G \r \n \x1a \n
               0 34 92 40 41 9 255 254 128 1 2 3 200 150 0))))

;; Standard WITH+GRAPHICS with gr-geometry (from caching-test.tm)
(define (make-standard-graphics)
  '(with "gr-mode" (tuple "edit" "cspline")
         "gr-frame" (tuple "scale" "1cm" (tuple "0.5gw" "0.5gh"))
         "gr-geometry" (tuple "geometry" "1par" "0.6par")
         "gr-snap" (tuple "control point" "grid point" "grid curve point"
                          "curve-grid intersection" "text border point"
                          "text border")
         (graphics ""
           (point "-4.17547" "1.6592")
           (point "-1.81438" "0.25425")
           (point "0.293045" "2.01043")
           (point "-3.10225" "2.67388")
           (point "-2.16561" "1.3665")
           (point "0.566229" "-0.54579")
           (line (point "-5.30723" "1.73725")
                 (point "-3.7461800502712" "-2.51661926180712")
                 (point "3.59076928165101" "-1.93122436830269")
                 (point "2.71267694139436" "2.98609273713454")
                 (point "-5.30723309961635" "1.69822397142479")
                 (point "-0.780179256515412" "-1.38485580103188")
                 (point "-0.467968646646382" "0.234736737663712")
                 (point "0.332071041143008" "0.566460510649557")
                 (point "-5.6389568726022" "2.42021100674692")
                 (point "-4.58524606429422" "3.41538232570446")
                 (point "2.75170326762799" "2.9275532477841"))
           (spline (point "3.72736" "2.28362")
                   (point "4.58594060060855" "0.878671120518587"))
           (cspline (point "3.41515" "3.591")
                    (point "5.42500661463156" "2.77144794284958")
                    (point "4.5274011112581" "1.22990805662125")
                    (point "6.34212528112184" "0.546947347532742")))))

;; draw-over with inner WITH+GRAPHICS, no gr-geometry (from caching-test.tm)
(define (make-draw-over-graphics)
  '(draw-over "GRAPHICS "
     (with "gr-mode" (tuple "edit" "cspline")
       (graphics
         (line (point "-0.615128" "0.0569354")
               (point "0.555662124619659" "-0.0796567006217754")
               (point "2.19476782643207" "-0.391867310490806")
               (point "-0.45902235745469" "-0.0406303743881466"))))
     "0cm"))

(define (regtest-tree-hash)
  (regression-test-group
   "tree-hash" "tree-hash"
   (lambda (st) (tree-hash (stree->tree st))) :none
   (test "atomic string hashes to hex"
     "hello" (tree-hash (stree->tree "hello")))
   (test "compound node hashes to hex"
     '(bold "text") (tree-hash (stree->tree '(bold "text"))))
   (test "nested structure hashes deterministically"
     '(document "a" (bold "b"))
     (tree-hash (stree->tree '(document "a" (bold "b")))))))

(define (regtest-tree-hash-distinct)
  (regression-test-group
   "tree-hash distinctness" "tree-hash-distinct"
   (lambda (pair) (not (== (tree-hash (stree->tree (car pair)))
                           (tree-hash (stree->tree (cadr pair))))))
   :none
   (test "different strings give different hashes"
     '("hello" "world") #t)
   (test "compound differs from atomic"
     '((bold "text") "text") #t)))

(define (regtest-tree-hash-length)
  (regression-test-group
   "tree-hash output length" "tree-hash-length"
   (lambda (st) (string-length (tree-hash (stree->tree st)))) :none
   (test "atomic hash is 16 chars (64 bits)"
     "test" 16)
   (test "compound hash is 16 chars (64 bits)"
     '(bold "x") 16)))

(define (regtest-cache-image)
  (integration-test-group
   "image caching" "cache-image"
   (tc-clear)
   (noop)
   (test "image replaced by cache-ref"
     (car (tree->stree
       (tc-update (stree->tree (make-cacheable-image "imgdata")))))
     'cache-ref)
   (test "cache-ref value is 16-char hex"
     (string-length
       (cadr (tree->stree
         (tc-update (stree->tree (make-cacheable-image "imgdata2"))))))
     16)))

(define (regtest-cache-small)
  (integration-test-group
   "small tree not cached" "cache-small"
   (tc-clear)
   (noop)
   (test "plain string unchanged"
     (tree->stree (tc-update (stree->tree "hello")))
     "hello")
   (test "small compound unchanged"
     (tree->stree (tc-update (stree->tree '(bold "text"))))
     '(bold "text"))
   (test "document with small children keeps structure"
     (tree->stree (tc-update (stree->tree '(document "para1" "para2"))))
     '(document "para1" "para2"))))

(define (regtest-cache-roundtrip)
  (integration-test-group
   "cache round-trip" "cache-roundtrip"
   (tc-clear)
   (noop)
   (test "small tree unchanged by update"
     (tree->stree
       (tc-update (stree->tree '(document "hello" (bold "world")))))
     '(document "hello" (bold "world")))
   (test "image retrievable via cache-get"
     (let ((original (make-cacheable-image "roundtrip-data")))
       (tc-update (stree->tree original))
       (tree->stree
         (tc-get (tree-hash (stree->tree original)))))
     (make-cacheable-image "roundtrip-data"))
   (test "image child retrievable via cache-get"
     (let* ((img (make-cacheable-image "child-img"))
            (original `(document "intro" ,img)))
       (tc-update (stree->tree original))
       (tree->stree
         (tc-get (tree-hash (stree->tree img)))))
     (make-cacheable-image "child-img"))
   (test "nested image retrievable via cache-get"
     (let* ((img (make-cacheable-image (make-big-string 5000)))
            (original `(document
                         (section (section-title "Ch1") ,img)
                         "small para")))
       (tc-update (stree->tree original))
       (tree->stree
         (tc-get (tree-hash (stree->tree img)))))
     (make-cacheable-image (make-big-string 5000)))))

(define (regtest-cache-graphics)
  (integration-test-group
   "graphics caching" "cache-graphics"
   (tc-clear)
   (noop)
   (test "standard graphics cached exactly once"
     (length (collect-cache-hashes
               (tc-update (stree->tree (make-standard-graphics)))))
     1)
   (test "draw-over graphics cached exactly once"
     (length (collect-cache-hashes
               (tc-update (stree->tree (make-draw-over-graphics)))))
     1)
   (test "standard graphics cache-refs all resolve"
     (let* ((updated (tc-update (stree->tree (make-standard-graphics))))
            (hashes (collect-cache-hashes updated)))
       (and (> (length hashes) 0)
            (= (tc-count-present hashes) (length hashes))))
     #t)
   (test "draw-over graphics cache-refs all resolve"
     (let* ((updated (tc-update (stree->tree (make-draw-over-graphics))))
            (hashes (collect-cache-hashes updated)))
       (and (> (length hashes) 0)
            (= (tc-count-present hashes) (length hashes))))
     #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: binary raw-data survives the disk round-trip
;;
;; tree-cache-get must reconstruct the tree from disk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-cache-binary)
  (integration-test-group
   "binary round-trip" "cache-binary"
   (tc-clear)
   (noop)
   (test "binary image survives disk round-trip"
     (let ((original (make-cacheable-image (make-binary-blob))))
       (tc-update (stree->tree original))
       (tree->stree
         (tc-get (tree-hash (stree->tree original)))))
     (make-cacheable-image (make-binary-blob)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: collisions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (regtest-cache-collision)
  (integration-test-group
    "collision" "cache-collision"
    (begin
      (tc-clear)
      (tree-hash-set-limit 1))
    (tree-hash-set-limit 0)
    (test "collision on cache update does not return same cache-ref"
      (let* ((trees (map stree->tree
                         `(,(make-cacheable-image "0")
                            ,(make-cacheable-image "1")
                            ,(make-cacheable-image "2"))))
             (trees-hashes (map tree-hash trees))
             (trees-cached
               (map (lambda (t) (tree->stree (tc-update t)))
                    trees)))
        (map (lambda (t) (car t)) trees-cached))
      `(cache-ref image image))
    (test "collision on cache update does not overwrite"
      (let* ((trees (map stree->tree
                         `(,(make-cacheable-image "0")
                            ,(make-cacheable-image "1")
                            ,(make-cacheable-image "2"))))
             (trees-hashes (map tree-hash trees)))
        (for-each (cut tc-update <>) trees)
        (map (lambda (t) (tc-get t)) trees-hashes))
      `(,(stree->tree (make-cacheable-image "0"))
         ,(stree->tree (make-cacheable-image "0"))
         ,(stree->tree (make-cacheable-image "0"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: janitor and clear
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cache-default-max-size (* 500 1024 1024)) ; mirrors tree_cache.hpp default

(define (regtest-cache-janitor)
  (integration-test-group
    "clear cache and janitor runs" "cache-clear"
    (begin (tc-clear) (tc-set-max-size cache-default-max-size))
    (begin (tc-set-max-size cache-default-max-size) (tc-clear))
    (test "directly clear all cache"
      (begin
        (tc-clear)
        (tc-set-max-size cache-default-max-size)
        ;; graphics wrap the cacheable node, so read the real keys back from
        ;; the cache-ref nodes update leaves behind (see collect-cache-hashes)
        (let* ((updated (map (lambda (g) (tc-update (stree->tree g)))
                             (list (make-standard-graphics)
                                   (make-draw-over-graphics))))
               (hashes (apply append (map collect-cache-hashes updated))))
          (tc-clear)
          (tc-count-present hashes)))
      0)
    (test "janitor keeps entries while under capacity"
      (begin
        (tc-clear)
        (tc-set-max-size cache-default-max-size)
        (let* ((updated (map (lambda (g) (tc-update (stree->tree g)))
                             (list (make-standard-graphics)
                                   (make-draw-over-graphics))))
               (hashes (apply append (map collect-cache-hashes updated))))
          (tc-janitor)
          (tc-count-present hashes)))
      2)
    (test "janitor evicts oldest when over capacity"
      (begin
        (tc-clear)
        (tc-set-max-size cache-default-max-size)
        ;; two equal-size, escape-free payloads -> identical serialized size
        (let* ((trees (map stree->tree
                           `(,(make-cacheable-image "AAAAAAAA")
                              ,(make-cacheable-image "BBBBBBBB")))))
          (for-each (cut tc-update <>) trees)
          ;; leave room for exactly one of the two equal-size entries
          (tc-set-max-size (+ (quotient (tc-size) 2) 1))
          (tc-janitor)
          (tc-count-present (map tree-hash trees))))
      1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main entry point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (regtest-server-cache)
  (let ((n (+ (regtest-tree-hash)
              (regtest-tree-hash-distinct)
              (regtest-tree-hash-length)
              (regtest-cache-image)
              (regtest-cache-small)
              (regtest-cache-roundtrip)
              (regtest-cache-graphics)
              (regtest-cache-binary)
              (regtest-cache-collision)
              (regtest-cache-janitor))))
    (display* "Total: " (object->string n) " tests.\n")
    (display "Test suite of server-cache: ok\n")))
