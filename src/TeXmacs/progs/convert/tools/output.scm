
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : output.scm
;; DESCRIPTION : generation of indented and hyphenated output
;; COPYRIGHT   : (C) 2002  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert tools output)
  (:export
    output-indent
    output-test-end? output-remove output-flush
    output-verb output-lf-verbatim output-verbatim output-lf output-text
    output-produce))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define output-accu '())
(define output-indentation 0)
(define output-count 0)
(define output-start-flag #t)
(define output-space-flag #f)
(define output-break-flag #t)
(define output-tail "")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The output machinery
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-produce)
  (output-flush)
  (let ((r (apply string-append (reverse output-accu))))
    (set! output-accu '())
    (set! output-indentation 0)
    (set! output-count 0)
    (set! output-start-flag #t)
    (set! output-space-flag #f)
    (set! output-break-flag #t)
    (set! output-tail "")
    r))

(define (output-indent plus)
  (output-flush)
  (set! output-indentation (max 0 (min 40 (+ output-indentation plus)))))

(define (output-return)
  (set! output-start-flag #t)
  (let ((s (make-string output-indentation #\space)))
    (set! output-accu (cons (string-append "\n" s) output-accu))
    (set! output-count output-indentation)))

(define (output-raw s)
  (if (not (== s ""))
      (begin
	(set! output-start-flag #f)
	(set! output-break-flag #t)))
  (set! output-accu (cons s output-accu)))

(define (output-prepared s)
  (if (or (not (== s "")) output-space-flag)
      (begin
	(if output-space-flag (set! output-count (+ output-count 1)))
	(set! output-count (+ output-count (string-length s)))
	(if output-space-flag (set! output-break-flag #t))
	(if (or (< output-count 79) output-start-flag (not output-break-flag))
	    (begin
	      (if (and output-space-flag (not output-start-flag))
		  (output-raw " "))
	      (output-raw s))
	    (begin
	      (output-return)
	      (set! output-count (+ output-count (string-length s)))
	      (output-raw s)))
	(set! output-space-flag #f))))

(define (output-sub s i j)
  (let ((pos (string-index (make-shared-substring s i j) #\space)))
    (if pos
	(begin
	  (output-prepared (substring s i (+ i pos)))
	  (set! output-space-flag #t)
	  (output-sub s (+ i pos 1) j))
	(set! output-tail (substring s i j)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-test-end? s)
  ; to be used with extreme care
  (string-ends? output-tail s))

(define (output-remove n)
  ; to be used with extreme care
  (let ((k (string-length output-tail)))
    (if (>= k n) (set! output-tail (substring output-tail 0 (- k n))))))

(define (output-flush)
  (output-prepared output-tail)
  (set! output-tail ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (output-verb . ss)
  ;(display-err* "Output verb " ss "\n")
  (if (not (== output-tail "")) (output-flush))
  (output-prepared (apply string-append ss)))

(define (output-lf-verbatim . ss)
  ;(display-err* "Output lf verbatim " ss "\n")
  (output-flush)
  (if (not output-start-flag) (output-raw "\n"))
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(define (output-verbatim . ss)
  ;(display-err* "Output verbatim " ss "\n")
  (output-flush)
  (output-raw (apply string-append ss))
  (set! output-break-flag #f))

(define (output-lf)
  ;(display-err* "Output lf\n")
  (if (not (== output-tail "")) (output-flush))
  (output-return))

(define (output-text . ss)
  ;(display-err* "Output text " ss "\n")
  (let ((s (apply string-append (cons output-tail ss))))
    (output-sub s 0 (string-length s))))
