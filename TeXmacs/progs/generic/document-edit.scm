
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : document-edit.scm
;; DESCRIPTION : setting global document properties
;; COPYRIGHT   : (C) 2001  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic document-edit)
  (:use (utils base environment)
        (utils library length)
        (utils library cursor)
        (generic generic-edit)
        (generic document-style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-property (project-attach master)
  (:argument master "file" "Master file"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Preamble mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (in-preamble?)
  (== (get-env "preamble") "true"))

(tm-define (toggle-preamble)
  (:synopsis "Toggle preamble mode.")
  (:check-mark "v" in-preamble?)
  (let ((new (if (string=? (get-env "preamble") "true") "false" "true")))
    (init-env "preamble" new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global environment variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default? . vals)
  (if (null? vals)
      #t
      (and (not (init-has? (car vals)))
           (apply test-default? (cdr vals)))))

(tm-define (init-default . args)
  (:check-mark "*" test-default?)
  (for-each init-default-one args))

(tm-define (get-init-env s)
  (with t (get-init-tree s)
    (cond ((tree-atomic? t) (tree->string t))
          ((and (tree-func? t 'macro 1) (tree-atomic? (tree-ref t 0)))
           (tree->string (tree-ref t 0)))
          (else #f))))

(tm-define (test-init? var val)
  (== (get-init-tree var) (string->tree val)))

(tm-property (init-env var val)
  (:check-mark "*" test-init?))

(tm-define (set-init-env s val)
  (with old (get-init-tree s)
    (if (and (tree-func? old 'macro 1) (not (tm-is? val 'macro)))
        (init-env-tree s `(macro ,val))
        (init-env-tree s val))))

(tm-define (init-interactive-env var)
  (:interactive #t)
  (interactive (lambda (s) (set-init-env var s))
    (list (or (logic-ref env-var-description% var) var) "string"
          (get-init-env var))))

(tm-define (test-init-true? var)
  (test-init? var "true"))

(tm-define (toggle-init-env var)
  (:check-mark "*" test-init-true?)
  (with new (if (== (get-init-env var) "true") "false" "true")
    (init-default var)
    (delayed
      (when (!= new (get-init-env var))
        (set-init-env var new)))))

(tm-define (init-multi l)
  (when (and (nnull? l) (nnull? (cdr l)))
    (init-env (car l) (cadr l))
    (init-multi (cddr l))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial environment management in specific buffers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (initial-set-tree u var val)
  (when (tm? val)
    (with-buffer u
      (init-env-tree var val))))

(tm-define (initial-set u var val)
  (when (string? val)
    (with-buffer u
      (init-env var val))))

(tm-define (initial-get-tree u var)
  (or (with-buffer u
        (get-init-tree var))
      (tree "")))

(tm-define (initial-get u var)
  (or (with-buffer u
        (get-init var))
      ""))

(tm-define (initial-defined? u var)
  (with-buffer u
    (style-has? var)))

(tm-define (initial-has? u var)
  (with-buffer u
    (init-has? var)))

(tm-define (initial-default u . vars)
  (with-buffer u
    (apply init-default vars)))


(tm-define (buffer-get-metadata u kind)
  (or (with-buffer u
        (get-metadata kind))
      ""))

(tm-define (buffer-has-biblio? u)
  (with-buffer u
    (with l (list-attachments)
      (nnull? (list-filter l (cut string-ends? <> "-bibliography"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text and paragraph properties
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (test-default-document-language?)
  (null? (list-intersection (get-style-list) supported-languages)))

(tm-define (set-default-document-language)
  (:check-mark "*" test-default-document-language?)
  (let* ((old (get-style-list))
         (new (list-difference old supported-languages)))
    (when (!= new old)
      (set-style-list new))))

(tm-define (get-document-language)
  (with l (list-intersection (get-style-list) supported-languages)
    (if (null? l) (get-init "language") (car l))))

(tm-define (test-document-language? s)
  (== s (get-document-language)))

(tm-define (set-document-language lan)
  (:check-mark "*" test-document-language?)
  (let* ((old (get-style-list))
         (rem (list-difference old supported-languages))
         (new (append rem (if (== lan "english") (list) (list lan)))))
    (when (!= new old)
      (set-style-list new))))

(define (search-env-var t which)
  (cond ((nlist? t) #f)
        ((null? t) #f)
        ((match? t '(associate "language" :%1)) (caddr t))
        (else (let ((val (search-env-var (car t) which)))
                (if val val (search-env-var (cdr t) which))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main page layout
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (test-default-page-medium?) (test-default? "page-medium"))
(tm-define (init-default-page-medium)
  (:check-mark "*" test-default-page-medium?)
  (init-default "page-medium")
  (notify-page-change))

(define (test-page-medium? s) (== (get-init "page-medium") s))
(tm-define (init-page-medium s)
  (:check-mark "*" test-page-medium?)
  (init-env "page-medium" s)
  (notify-page-change))

(define (test-default-page-type?)
  (test-default? "page-type" "page-width" "page-height"))
(tm-define (default-page-type)
  (:check-mark "*" test-default-page-type?)
  (init-default "page-type" "page-width" "page-height")
  (notify-page-change))

(define (test-page-type? s) (== (get-init "page-type") s))
(tm-define (init-page-type s)
  (:check-mark "*" test-page-type?)
  (init-env "page-type" s)
  (init-env "page-width" "auto")
  (init-env "page-height" "auto")
  (notify-page-change))

(tm-define (init-page-size w h)
  (:argument w "Page width")
  (:argument h "Page height")
  (init-env "page-type" "user")
  (init-env "page-width" w)
  (init-env "page-height" h)
  (notify-page-change))

(define (test-default-page-orientation?) (test-default? "page-orientation"))
(tm-define (init-default-page-orientation)
  (:check-mark "*" test-default-page-orientation?)
  (init-default "page-orientation")
  (notify-page-change))

(define (test-page-orientation? s) (string=? (get-env "page-orientation") s))
(tm-define (init-page-orientation s)
  (:check-mark "*" test-page-orientation?)
  (init-env "page-orientation" s)
  (notify-page-change))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wrapper for global page rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (panorama-packets)
  (let* ((nr (nr-pages))
         (ww (get-window-width))
         (wh (get-window-height))
         (pw (get-page-width #f))
         (ph (get-page-height #f))
         (best-n 0)
         (best-f 0))
    (for (n (.. 1 (+ nr 1)))
      (let* ((r (quotient (+ nr (- n 1)) n))
             (tw (* n pw))
             (th (* r ph))
             (aw (- ww (* n 5120)))
             (ah (- wh (* r 5120)))
             (fw (/ (* 1.0 aw) tw))
             (fh (/ (* 1.0 ah) th))
             (f (min fw fh)))
        (when (or (== n 1) (> f best-f))
          (set! best-n n)
          (set! best-f f))))
    (cond ((> best-n 0) best-n)
          ((> nr 10) 10)
          (else (inexact->exact (ceiling (sqrt (* 1.0 nr))))))))

(define (test-default-page-rendering?) (test-default? "page-medium"))
(tm-define (init-default-page-rendering)
  (:check-mark "*" test-default-page-rendering?)
  (init-default "page-medium")
  (init-default "page-border")
  (init-default "page-packet")
  (init-default "page-offset")
  (notify-page-change))

(tm-define (get-init-page-rendering)
  (cond ((== (get-init "page-border") "attached") "book")
        ((!= (get-init "page-packet") "1") "panorama")
        (else (get-init "page-medium"))))

(define (test-page-rendering? s) (== (get-init-page-rendering) s))
(tm-define (init-page-rendering s)
  (:check-mark "*" test-page-rendering?)
  (save-zoom (get-init-page-rendering))
  (cond ((== s "book")
         (init-env "page-medium" "paper")
         (init-env "page-border" "attached")
         (init-env "page-packet" "2")
         (init-env "page-offset" "1")
	 (notify-page-change)
	 (delayed (:idle 25) (restore-zoom s)))
        ((== s "panorama")
         (init-env "page-medium" "paper")
         (init-env "page-packet" (number->string (panorama-packets)))
         (init-default "page-border")
         (init-default "page-offset")
	 (notify-page-change)
	 (delayed (:idle 25) (fit-all-to-screen)))
        (else
          (init-env "page-medium" s)
          (init-default "page-border")
          (init-default "page-packet")
          (init-default "page-offset")
	  (notify-page-change)
	  (delayed (:idle 25) (restore-zoom s)))))

(tm-define (initial-get-page-rendering u)
  (with-buffer u
    (get-init-page-rendering)))

(tm-define (initial-set-page-rendering u s)
  (with-buffer u
    (init-page-rendering s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Further page layout settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (visible-header-and-footer?)
  (== (get-env "page-show-hf") "true"))

(tm-define (toggle-visible-header-and-footer)
  (:synopsis "Toggle visibility of headers and footers in 'page' paper mode.")
  (:check-mark "v" visible-header-and-footer?)
  (init-env "page-show-hf"
            (if (== (get-env "page-show-hf") "true") "false" "true")))

(define (page-width-margin?)
  (== (get-env "page-width-margin") "true"))

(tm-define (toggle-page-width-margin)
  (:synopsis "Toggle mode for determining margins from paragraph width.")
  (:check-mark "v" page-width-margin?)
  (init-env "page-width-margin" (if (page-width-margin?) "false" "true")))

(define (not-page-screen-margin?)
  (== (get-env "page-screen-margin") "false"))

(tm-define (toggle-page-screen-margin)
  (:synopsis "Toggle mode for using special margins for screen editing.")
  (:check-mark "v" not-page-screen-margin?)
  (init-env "page-screen-margin"
            (if (not-page-screen-margin?) "true" "false")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Document updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define doc-update-times 1)

(define (notify-doc-update-times var val)
  (with n (cond ((string? val) (or (string->number val) 1))
                ((number? val) val)
                (else 1))
    (set! doc-update-times (min (max 1 n) 5)))) ; Just in case

(define-preferences 
  ("document update times" "1" notify-doc-update-times))

(define (wait-update-current-buffer)
  (system-wait "Updating current buffer, " "please wait")
  (update-current-buffer))

(tm-define (update-document what)
  (for (.. 0 doc-update-times)       
    (delayed    ; allow typesetting/magic to happen before next update
      (:idle 1)
      (cursor-after
       (cond ((== what "all") 
              (generate-all-aux) (inclusions-gc) (wait-update-current-buffer))
             ((== what "bibliography")
              (generate-all-aux) (wait-update-current-buffer))
             ((== what "buffer") 
              (wait-update-current-buffer))
             (else (generate-aux what)))))))
