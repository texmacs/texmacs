
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

(texmacs-module (texmacs menus debug-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-memory-information s)
  (string-append s "#[" (number->string (texmacs-memory)) "#bytes]"))

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

(menu-bind debug-menu
  (-> "Guile"
      ("Backtrace errors" (debug-toggle-backtrace-errors)))
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
      ("Provoke scheme error" (oops))
      ("Provoke C++ error" (cpp-error))
      ("Test routine" (edit-test)))
  (-> "Experimental"
      ("Fast environments" (toggle-preference "fast environments"))
      ("Alpha transparency" (toggle-preference "experimental alpha")))
  ---
  ("auto" (debug-toggle "auto"))
  ("verbose" (debug-toggle "verbose"))
  ("events" (debug-toggle "events"))
  ("std" (debug-toggle "std"))
  ("io" (debug-toggle "io"))
  ("bench" (debug-toggle "bench"))
  ("history" (debug-toggle "history"))
  ("qt" (debug-toggle "qt"))
  ("keyboard" (debug-toggle "keyboard"))
  ("packrat" (debug-toggle "packrat"))
  ("flatten" (debug-toggle "flatten"))
  ("correct" (debug-toggle "correct")))
