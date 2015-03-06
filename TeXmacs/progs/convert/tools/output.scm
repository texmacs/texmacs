
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : output.scm
;; DESCRIPTION : generation of indented and hyphenated output
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools output))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define output-accu '())
(define output-comment #f)
(define output-indentation 0)
(define output-count 0)
(define output-start-flag #t)
(define output-space-flag #f)
(define output-break-flag #t)
(define output-tail "")
(define output-exact #f)
(define output-line-length 79)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The output machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-set-exact x)
  (with old output-exact
    (set! output-exact x)
    old))

(tm-define (output-set-line-length x)
  (with old output-line-length
    (set! output-line-length x)
    old))

(tm-define (output-produce)
  (output-flush)
  (let ((r (apply string-append (reverse output-accu))))
    (set! output-accu '())
    (set! output-comment #f)
    (set! output-indentation 0)
    (set! output-count 0)
    (set! output-start-flag #t)
    (set! output-space-flag #f)
    (set! output-break-flag #t)
    (set! output-tail "")
;;    (display* "OUTPUT\n" r "\n")
    r))

(tm-define (get-output-start-flag)
  output-start-flag)

(tm-define (get-output-comment)
  output-comment)

(tm-define (set-output-comment comment)
  (set! output-comment comment))

(tm-define (get-output-indent)
  output-indentation)

(tm-define (set-output-indent indent)
  (set! output-indentation indent))

(tm-define (output-indent plus)
  (output-flush)
  (set! output-indentation (+ output-indentation plus)))

(define (output-return)
  (set! output-start-flag #t)
  (with indent (max 0 (min 40 output-indentation))
    (let ((s (if output-exact "" (make-string indent #\space)))
          (c (if output-comment "% " "")))
      (set! output-accu (cons (string-append "\n" s c) output-accu))
      (set! output-count indent))))

(define (output-raw s)
  (if (!= s "")
      (begin
	(set! output-start-flag #f)
	(set! output-break-flag #t)))
  (set! output-accu (cons s output-accu)))

(define (output-prepared s)
  (if (or (!= s "") output-space-flag)
      (begin
	(if output-space-flag (set! output-count (+ output-count 1)))
	(set! output-count (+ output-count (string-length s)))
	(if output-space-flag (set! output-break-flag #t))
	(if (or (< output-count output-line-length) output-start-flag
		(not output-break-flag) output-exact)
	    (begin
	      (if (and output-space-flag (not output-start-flag))
		  (output-raw " "))
	      (output-raw s))
	    (begin
	      (output-return)
	      (set! output-count (+ output-count (string-length s)))
	      (output-raw s)))
	(set! output-space-flag #f))))

(define (output-sub s i)
  (with pos (string-search-forwards " " i s)
    (if (>= pos i)
	(begin
	  (output-prepared (substring s i pos))
	  (set! output-space-flag #t)
	  (output-sub s (+ pos 1)))
	(set! output-tail (substring s i (string-length s))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-test-end? s)
  ;; to be used with extreme care
  (string-ends? output-tail s))

(tm-define (output-remove n)
  ;; to be used with extreme care
  (let ((k (string-length output-tail)))
    (if (>= k n) (set! output-tail (substring output-tail 0 (- k n))))))

(define (trim-right-spaces s)
  (if (string-ends? s " ")
      (trim-right-spaces (string-drop-right s 1))
      s))

(tm-define (output-remove-indentation)
  ;; to be used with extreme care
  (if (== output-tail "")
      (when (nnull? output-accu)
        (with s (trim-right-spaces (car output-accu))
          (cond ((== s "")
                 (set! output-accu (cdr output-accu))
                 (set! output-count (- 1 output-count))
                 (output-remove-indentation))
                ((string-ends? s "\n")
                 (set! output-accu (cons s (cdr output-accu)))
                 (set! output-count (- 1 output-count))))))
      (with s (trim-right-spaces output-tail)
        (cond ((== s "")
               (set! output-tail "")
               (output-remove-indentation))
              ((string-ends? s "\n")
               (set! output-tail s))))))

(tm-define (output-flush)
  (output-prepared output-tail)
  (set! output-tail ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (output-verb . ss)
  ;;(display-err* "Output verb " ss "\n")
  (if (!= output-tail "") (output-flush))
  (output-prepared (apply string-append ss)))

(tm-define (output-lf-verbatim . ss)
  ;;(display-err* "Output lf verbatim " ss "\n")
  (output-flush)
  (if (not output-start-flag) (output-raw "\n"))
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(tm-define (output-invariant . ss)
  ;;(display-err* "Output invariant " ss "\n")
  (output-remove-indentation)
  (apply output-lf-verbatim ss))

(tm-define (output-verbatim . ss)
  ;;(display-err* "Output verbatim " ss "\n")
  (output-flush)
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(tm-define (output-lf)
  ;;(display-err* "Output lf\n")
  (if (!= output-tail "") (output-flush))
  (output-return))

(tm-define (output-text . ss)
  ;;(display-err* "Output text " ss "\n")
  (let ((s (apply string-append (cons output-tail ss))))
    (output-sub s 0)))

(tm-define (output-marker s)
  ;;(display-err* "Output marker " s "\n")
  (if (!= output-tail "") (output-flush))
  (set! output-accu (cons s output-accu)))
