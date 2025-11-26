
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

(define (list-shuffle l)
  "Shuffle list in place using Fisher-Yates algorithm"
  (let ((len (length l)))
    (do ((i 0 (+ i 1)))
      ((= i (- len 1)))
      (let* ((j (+ i (random (- len i))))  ; Pick random remaining element
             (tmp (list-ref l i)))
        (list-set! l i (list-ref l j))
        (list-set! l j tmp))))
  l)

(define (generate-char charset)
  (let* ((n (string-length charset))
         (i (random n)))
    (string-ref charset i)))

(define (generate-chars charset n)
  (list->string
    (map (lambda (_) (generate-char charset)) (iota n))))

(define (filter-invalid-chars s charset)
  (list->string (filter (lambda (c) (char-in-string? c charset))
                        (string->list s))))

(define (clean-data data)
        (filter-invalid-chars (string-trim-both data)
                              all-charset))

(define (generate-random-data method n)
  (cond ((== method `openssl)
         (with (ret out err)
               (evaluate-system
                 `("openssl" "rand" ,(object->string  n)) '() '() '(1 2))
               (if (== ret "0") out #f)))
        ((== method `gpg)
         (with (ret out err)
               (evaluate-system
                 `("gpg" "--gen-random" "2" ,(object->string n)) '() '() '(1 2))
               (if (== ret "0") out #f)))
        (else (generate-chars all-charset n))))

(define (generate-external method n)
  "Generate data using an external tool if available"
  (with data (generate-random-data method (* 8 n))
        (if data
          (with pwd (clean-data data)
                (string-take pwd (min n (string-length pwd))))
          #f)))

(define (generate-password-default n)
  "Generate password fallback in pure Scheme"
  (let* (
         ;; Ensure at least one of each required type
         (pwd (string-append
                (string (generate-char lower-charset))
                (string (generate-char upper-charset))
                (string (generate-char digits-charset))
                (string (generate-char symbols-charset))
                ;; Add n-4 more random chars to reach desired length
                (with s ""
                  (do ((i 0 (+ i 1)))
                      ((= i (- n 4)) s)
                    (set! s (string-append s (string (generate-char
                                                       all-charset))))))))
         ;; Convert to list, shuffle, and convert back
         (chars (list-shuffle (string->list pwd))))
    (list->string chars)))

(tm-define (generate-password n)
  "Generate secure password using available system tools"
  (or (generate-external 'openssl n)
      (generate-external 'gpg n)
      (generate-password-default n)
      (begin
        (display-err* "No suitable password generation tools found\n")
        #f)))

(tm-define (generate-salt n)
  "Generate salt using available system tools"
  (or (generate-external 'openssl n)
      (generate-external 'gpg n)
      (generate-random-data 'default n)
      (begin
        (display-err* "No suitable salt generation tools found\n")
        #f)))
