
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : print-widgets.scm
;; DESCRIPTION : the print widgets
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (texmacs menus print-widgets)
  (:use (texmacs texmacs tm-print)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Page setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (printing-command-list)
  (with l (cons (get-default-printing-command)
                (list "lpr" "lp" "pdq" ""))
    (list-remove-duplicates l)))

(tm-widget (page-setup-widget)
  (centered
    (aligned
      (item (text "Preview command:")
        (enum (set-pretty-preference "preview command" answer)
              '("default" "ggv" "ghostview" "gv" "kghostview" "open" "")
              (get-pretty-preference "preview command")
              "18em"))
      (item (text "Printing command:")
        (enum (set-pretty-preference "printing command" answer)
              (printing-command-list)
              (get-pretty-preference "printing command")
              "18em"))
      (item (text "Paper type:")
        (enum (set-pretty-preference "paper type" answer)
              '("default" "A3" "A4" "A5" "B4" "B5" "B6"
                "Letter" "Legal" "Executive" "")
              (get-pretty-preference "paper type")
              "18em"))
      (item (text "Printer dpi:")
        (enum (set-pretty-preference "printer dpi" answer)
              '("150" "200" "300" "400" "600" "800" "1200" "")
              (get-pretty-preference "printer dpi")
              "18em")))))

(tm-define (open-page-setup)
  (:interactive #t)
  (top-window page-setup-widget "Page setup"))
