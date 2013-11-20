
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : debug-widgets.scm
;; DESCRIPTION : Debugging widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (debug debug-widgets))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Message selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (message-type m)
  (cond ((string-ends? m "-error") (string-drop-right m 6))
        ((string-ends? m "-warning") (string-drop-right m 8))
        ((string-ends? m "-bench") (string-drop-right m 6))
        ((string-starts? m "debug-") (string-drop m 6))
        (else m)))

(define (list-message-types kind)
  (let* ((l (tree-children (get-debug-messages)))
         (t (make-ahash-table)))
    (for (m l)
      (with s (tree->string (tree-ref m 0))
        (when (or (== kind "Debugging console")
                  (string-ends? s "-error")
                  (string-ends? s "-warning"))
          (ahash-set! t (message-type s) #t))))
    (sort (ahash-set->list t) string<=?)))

(define (message-among? m selected)
  (let* ((k (tm-ref m 0))
         (s (tm-ref m 1)))
    (and (tree-atomic? k)
         (tree-atomic? s)
         (in? (message-type (tree->string k)) selected))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Build a document with the list of all messages of a certain kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-message m)
  (let* ((k (tm->stree (tm-ref m 0)))
         (s (tm-ref m 1)))
    (cond ((string-ends? k "-error")
           `(with "color" "#c04040" (concat "Error: " ,s)))
          ((string-ends? k "-warning")
           `(with "color" "dark magenta" (concat "Warning: " ,s)))
          ((string-ends? k "-bench")
           `(with "color" "dark blue" ,s))
          (else s))))

(define (messages->document selected)
  (let* ((all-ms (tree-children (get-debug-messages)))
         (sel-ms (list-filter all-ms (cut message-among? <> selected))))
    `(document
       (with "language" "verbatim" "font-family" "tt" "par-par-sep" "0fn"
             (document
               ,@(map build-message sel-ms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main console widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define console-active? (make-ahash-table))
(tm-define console-categories (make-ahash-table))
(tm-define console-selected (make-ahash-table))

(tm-widget ((console-widget kind))
  (padded
    (horizontal
      (vertical
        (bold (text "Categories"))
        === ===
        (resize ("100px" "100px" "100px") ("300px" "600px" "1000px")
          (refreshable "console-widget-categories"
            (choices (begin
                       (ahash-set! console-selected kind answer)
                       (refresh-now "console-widget-messages"))
                     (ahash-ref console-categories kind)
                     (ahash-ref console-selected kind)))))
      ///
      (vertical
        (bold (text "Messages"))
        === ===
        (resize ("500px" "800px" "1200px" "left")
            ("300px" "600px" "1000px" "bottom")
          (refreshable "console-widget-messages"
            (texmacs-output
              (messages->document (ahash-ref console-selected kind))
              '(style "generic"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic updates of consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define console-updating? #f)
(tm-define console-errors? #f)

(tm-define (update-consoles)
  (for (kind (ahash-set->list console-active?))
    (let* ((old (ahash-ref console-categories kind))
           (new (list-message-types kind))
           (delta (list-difference new old)))
      (ahash-set! console-categories kind new)
      (ahash-set! console-selected kind
                  (list-union (ahash-ref console-selected kind) delta))))
  (when (nnull? (ahash-set->list console-active?))
    (refresh-now "console-widget-categories")
    (refresh-now "console-widget-messages"))
  (when (and console-errors?
             (not (ahash-ref console-active? "Error messages")))
    (delayed
      (:idle 1)
      (open-error-messages)))
  (set! console-updating? #f)
  (set! console-errors? #f))

(tm-define (notify-debug-message channel)
  (when (or (string-ends? channel "-error")
            (string-ends? channel "-warning"))
    (set! console-errors? #t))
  (when (not console-updating?)
    (set! console-updating? #t)
    (delayed
      (:idle 1)
      (update-consoles))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (open-console kind)
  (when (not (ahash-ref console-active? kind))
    (ahash-set! console-active? kind #t)
    (ahash-set! console-categories kind (list-message-types kind))
    (ahash-set! console-selected kind (ahash-ref console-categories kind))
    (top-window (console-widget kind) kind
                (lambda x (ahash-remove! console-active? kind)))))

(tm-define (open-debug-console)
  (open-console "Debugging console"))

(tm-define (open-error-messages)
  (open-console "Error messages"))
