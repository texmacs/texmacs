
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : debug-menu.scm
;; DESCRIPTION : the debug menu
;; COPYRIGHT   : (C) 1999  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (debug debug-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-memory-information t)
  (let* ((s (tree->stree t))
         (a `(concat ,s " [" ,(number->string (texmacs-memory)) " bytes]")))
    (stree->tree a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guile
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (debug-backtrace-errors?) (in? 'backtrace (debug-options)))
(tm-define (debug-toggle-backtrace-errors)
  (:synopsis "Toggle scheme backtracing of errors.")
  (:check-mark "v" debug-backtrace-errors?)
  (if (debug-backtrace-errors?)
      (debug-disable 'backtrace 'debug)
      (debug-enable 'backtrace 'debug)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General debugging options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (debug-toggle s)
  (:check-mark "v" debug-get)
  (debug-set s (not (debug-get s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind provoke-error-menu
  (xxx))

(menu-bind debug-menu
  (-> "Guile"
      ("Backtrace errors" (debug-toggle-backtrace-errors)))
  (-> "Execute"
      ("Execute system command" (interactive system))
      ("Evaluate scheme expression" (interactive footer-eval)))
  (-> "Consoles"
      ("Debugging console" (open-debug-console))
      ("Error messages" (open-error-messages))
      ---
      (group "Automatic")
      ("Open on errors" (toggle-preference "open console on errors"))
      ("Open on warnings" (toggle-preference "open console on warnings")))
  (-> "Status"
      ("Tree" (show-tree))
      ("Path" (show-path))
      ("Cursors" (show-cursor))
      ("Selection" (show-selection))
      ("Focus" (display* "focus: " (get-focus-path) "\n"))
      ("Environment" (show-env))
      ("History" (show-history))
      ("Memory usage" (show-meminfo)))
  (-> "Timings"
      ("All" (bench-print-all)))
  (-> "Memory"
      ("Memory usage" (show-meminfo))
      ("Collect garbage" (gc))
      ---
      (group "Permanent")
      ("Show memory usage" (set! footer-hook show-memory-information))
      ("Garbage collection" (delayed (:idle 1000) (gc))))
  (when (debug-get "correct")
    (-> "Mathematics"
        ("Error status report" (math-status-print))
        ("Reset error counters" (math-status-reset))))
  (-> "Miscellaneous"
      ("Test routine" (edit-test))
      ("Provoke scheme error" (oops))
      ("Provoke C++ error" (cpp-error))
      (-> "Provoke menu error"
          (link provoke-error-menu)))
  ---
  ("auto" (debug-toggle "auto"))
  ("verbose" (debug-toggle "verbose"))
  ("events" (debug-toggle "events"))
  ("std" (debug-toggle "std"))
  ("io" (debug-toggle "io"))
  ("bench" (debug-toggle "bench"))
  ("history" (debug-toggle "history"))
  ("qt" (debug-toggle "qt"))
  ("qt-widgets" (debug-toggle "qt-widgets"))
  ("keyboard" (debug-toggle "keyboard"))
  ("packrat" (debug-toggle "packrat"))
  ("flatten" (debug-toggle "flatten"))
  ("parser" (debug-toggle "parser"))
  ("correct" (debug-toggle "correct"))
  ("convert" (debug-toggle "convert")))
