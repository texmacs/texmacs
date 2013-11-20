
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
;; Build a document with the list of all messages of a certain kind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (message-among? m selected)
  (let* ((k (tm-ref m 0))
         (s (tm-ref m 1)))
    (and (tree-atomic? k)
         (tree-atomic? s))))

(tm-define (build-message m)
  (let* ((k (tm->stree (tm-ref m 0)))
         (s (tm-ref m 1)))
    (cond ((string-ends? k "-error")
           `(with "color" "#c04040" (concat "Error: " ,s)))
          ((string-ends? k "-warning")
           `(with "color" "dark magenta" (concat "Warning: " ,s)))
          ((string-ends? k "-bench")
           `(with "color" "dark blue" ,s))
          (else s))))

(tm-define (messages->document selected)
  (let* ((all-ms (tree-children (get-debug-messages)))
         (sel-ms (list-filter all-ms (cut message-among? <> selected))))
    `(document
       (with "language" "verbatim" "font-family" "tt" "par-par-sep" "0fn"
             (document
               ,@(map build-message sel-ms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main console widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((console-widget kinds selected) quit)
  (padded
    (resize ("500px" "800px" "1200px" "left")
            ("300px" "600px" "1000px" "bottom")
      (refreshable "console-widget"
        (texmacs-output
          (messages->document selected)
          '(style "generic"))))
    (glue #t #f 0 0)
    ======
    (hlist
      >>
      (explicit-buttons
        ("Done" (quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-debug-console)
  (:interactive #t)
  (let* ((kinds (list "debug-automatic" "debug-boot"
                      "debug-io" "debug-std"))
         (selected kinds))
    (dialogue-window (console-widget kinds selected)
		     (lambda x (noop))
		     "Debugging console")))
