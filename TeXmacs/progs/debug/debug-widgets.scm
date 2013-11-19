
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

(tm-define (message-among? m selected) #t)

(tm-define (build-message m)
  (with s (tm-ref (tree->stree m) 1)
    (if (string? s) s "invalid message")))

(tm-define (messages->document selected)
  (let* ((all-ms (tree-children (get-debug-messages)))
         (sel-ms (list-filter all-ms (cut message-among? <> selected))))
    `(document ,@(map build-message sel-ms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main console widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((console-widget kinds selected) quit)
  (padded
    (resize "500px" "300px"
      (refreshable "console-widget"
        (texmacs-output
          (messages->document selected)
          '(style "generic"))))
    (glue #t #f 0 0))
  ======
  (hlist
    >>
    (explicit-buttons
      ("Done" (quit)))))

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
