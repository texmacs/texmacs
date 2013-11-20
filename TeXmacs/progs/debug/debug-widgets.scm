
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

(define (list-message-types select?)
  (let* ((l (tree-children (get-debug-messages)))
         (t (make-ahash-table)))
    (for (m l)
      (when (select? m)
        (ahash-set! t (message-type (tree->stree (tree-ref m 0))) #t)))
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

(tm-widget ((console-widget select?) quit)
  (with types (list-message-types select?)
    (with selected types
      (padded
        (horizontal
          (vertical
            (bold (text "Categories"))
            ===
            (resize ("100px" "100px" "100px") ("300px" "600px" "1000px")
              (refreshable "console-widget-types"
                (choices (begin
                           (set! selected answer)
                           (refresh-now "console-widget-messages"))
                         types selected))))
          ///
          (vertical
            (bold (text "Messages"))
            ===
            (resize ("500px" "800px" "1200px" "left")
                ("300px" "600px" "1000px" "bottom")
              (refreshable "console-widget-messages"
                (texmacs-output
                  (messages->document selected)
                  '(style "generic"))))))
        (glue #t #f 0 0)
        ======
        (hlist
          >>
          (explicit-buttons
            ("Done" (quit))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-debug-console)
  (:interactive #t)
  (let* ((kinds (list "debug-automatic" "debug-boot"
                      "debug-io" "debug-std"))
         (selected kinds))
    (dialogue-window (console-widget (lambda (x) #t))
		     (lambda x (noop))
		     "Debugging console")))
