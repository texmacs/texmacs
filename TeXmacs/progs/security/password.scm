
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : password.scm
;; DESCRIPTION : TeXmacs
;; COPYRIGHT   : (C) 2025 Robin Wils
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (security password))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Generation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lower-charset "abcdefghijklmnopqrstuvwxyz")
(define upper-charset "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
(define digits-charset "0123456789")
(define symbols-charset "!@#$%^&*()_+-=[]{}|;:,.?")
(define all-charset (string-append lower-charset upper-charset digits-charset symbols-charset))

(define (rnd n) (if (supports-gnutls?) (gnutls-random-number n) (random n)))

(define (string-shuffle! s)
  (let ((n (string-length s)))
    (let loop ((i (- n 1)))
      (if (<= i 0)
          s
          (let* ((j (random (+ i 1)))     ; j in [0..i]
                 (ci (string-ref s i))
                 (cj (string-ref s j)))
            (string-set! s i cj)
            (string-set! s j ci)
            (loop (- i 1)))))))

(define (random-char charset)
  (let* ((n (string-length charset))
         (i (rnd n)))
    (string-ref charset i)))

(define (generate-password-default n)
  "Generate password with at least 1 lower & upper char + digit + symbol"
  (let* ((pwd
           (string-append
             (string (random-char lower-charset))
             (string (random-char upper-charset))
             (string (random-char digits-charset))
             (string (random-char symbols-charset))
             (with s ""
                   (do ((i 0 (+ i 1)))
                     ((= i (- n 4)) s)
                     (set! s (string-append
                               s (string (random-char all-charset)))))))))
    (string-shuffle! pwd)
    pwd))

(tm-define (generate-password n) (generate-password-default n))
(tm-define (generate-salt n)     (generate-password-default n))
