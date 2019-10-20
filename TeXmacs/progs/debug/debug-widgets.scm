
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
;; Preferences
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (refresh-console . x)
  (refresh-now "console-widget-categories")
  (refresh-now "console-widget-messages"))

(define-preferences
  ("console details" "normal" refresh-console)
  ("console size" "100" refresh-console))

(define (encode-size sz)
  (cond ((== sz "All") "1000000")
        ((string-starts? sz "Last ") (string-drop sz 5))
        (else "100")))

(define (decode-size sz)
  (cond ((not (string->number sz)) "Last 100")
        ((> (string->number sz) 10000) "All")
        (else (string-append "Last " sz))))

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
  (let* ((l (tree-children (get-debug-messages kind 100)))
         (t (make-ahash-table)))
    (for (m l)
      (ahash-set! t (message-type (tree->string (tree-ref m 0))) #t))
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
         (s (utf8->cork (tm->stree (tm-ref m 1))))
         (t (tm->stree (tm-ref m 2))))
    (cond ((and (!= t "") (== (get-preference "console details") "detailed"))
           `(document ,(build-message `(tuple ,(tm-ref m 0) ,(tm-ref m 1) ""))
                      (indent (small ,t))))
          ((string-ends? k "-error")
           `(with "color" "#e02020" (concat "Error: " ,s)))
          ((string-ends? k "-warning")
           `(with "color" "dark magenta" (concat "Warning: " ,s)))
          ((string-ends? k "-bench")
           `(with "color" "dark blue" ,s))
          (else s))))

(define (messages->document kind selected)
  (let* ((n (or (string->number (get-preference "console size")) 100))
         (all-ms (tree-children (get-debug-messages kind n)))
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
              (messages->document kind (ahash-ref console-selected kind))
              '(style "generic"))))))
    ======
    (explicit-buttons
      (hlist
        (enum (set-preference "console details" (locase-all answer))
              '("Normal" "Detailed")
              (upcase-first (get-preference "console details")) "80px")
        // //
        (enum (set-preference "console size" (encode-size answer))
              '("Last 25" "Last 100" "Last 250" "Last 1000" "All")
              (decode-size (get-preference "console size")) "80px")
        >>>
        (=> "Preferences"
            ("Automatically open this console on errors"
             (toggle-preference "open console on errors"))
            ("Automatically open this console on warnings"
             (toggle-preference "open console on warnings")))
        // //
        ("Clear" (clear-debug-messages) (refresh-console))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatic updates of consoles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define console-updating? #f)
(tm-define console-errors? #f)
(tm-define console-warnings? #f)

(tm-define (update-consoles)
  (for (kind (ahash-set->list console-active?))
    (let* ((old (ahash-ref console-categories kind))
           (new (list-message-types kind))
           (delta (list-difference new old)))
      (ahash-set! console-categories kind new)
      (ahash-set! console-selected kind
                  (list-union (ahash-ref console-selected kind) delta))))
  (when (nnull? (ahash-set->list console-active?))
    (refresh-console))
  (when (and (or (and console-errors?
                      (get-boolean-preference "open console on errors"))
                 (and console-warnings?
                      (get-boolean-preference "open console on warnings")))
             (not (ahash-ref console-active? "Error messages")))
    (delayed
      (:idle 1)
      (open-error-messages)))
  (set! console-updating? #f)
  (set! console-errors? #f)
  (set! console-warnings? #f))

(tm-define (notify-debug-message channel)
  (when (string-ends? channel "-error")
    (set! console-errors? #t))
  (when (string-ends? channel "-warning")
    (set! console-warnings? #t))
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
