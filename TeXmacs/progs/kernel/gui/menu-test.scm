
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : menu-test.scm
;; DESCRIPTION : some test widgets
;; COPYRIGHT   : (C) 2012  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui menu-test)
  (:use (kernel gui menu-widget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some test widgets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (widget1)
  (centered
    (aligned
      (item (text "First:")
        (toggle (display* "First " answer "\n") #f))
      (item (text "Second:")
        (toggle (display* "Second " answer "\n") #f)))))

(tm-widget (widget2)
  (tabs
    (tab (text "General")
      (centered
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f)))))
    (tab (text "Extra")
      (centered
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f))))
      (bottom-buttons
        ("Cancel" (display "Cancel\n")) >> ("Ok" (display "Ok\n"))))
    (tab (text "Settings")
      (centered
        (aligned
          (item (text "First:")
            (enum (display* "First " answer "\n")
                  '("gnu" "gnat" "zebra")
                  "zebra" "10em"))
          (item (text "Second:")
            (enum (display* "Second " answer "\n")
                  '("fun" "foo" "bar")
                  "fun" "10em"))))
      (bottom-buttons
        >> ("Ok" (display "Ok\n"))))))

(tm-widget (widget3)
  (centered
    (resize "200px" "100px"
      (scrollable
        (aligned
          (item (text "First:")
            (toggle (display* "First " answer "\n") #f))
          (item (text "Second:")
            (toggle (display* "Second " answer "\n") #f))
          (item (text "Third:")
            (toggle (display* "Third " answer "\n") #f))
          (item (text "Fourth:")
            (toggle (display* "Fourth " answer "\n") #f))
          (item (text "Fifth:")
            (toggle (display* "Fifth " answer "\n") #f))
          (item (text "Sixth:")
            (toggle (display* "Sixth " answer "\n") #f))
          (item (text "Seventh:")
            (toggle (display* "Seventh " answer "\n") #f))
          (item (text "Eighth:")
            (toggle (display* "Eighth " answer "\n") #f)))))))

(tm-widget (widget4)
  (centered
    (resize "200px" "50px"
      (scrollable
        (choice (display* answer "\n")
                '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                "Third")))
    ======
    (resize "200px" "150px"
      (choices (display* answer "\n")
               '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
               '("Third" "Fifth")))))

(tm-widget (widget5)
  ===
  (hlist
    //
    (hsplit
      (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
        (scrollable
          (choice (display* answer "\n")
                  '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                  "Third")))
      (resize ("100px" "200px" "400px") ("50px" "100px" "150px")
        (scrollable
          (choices (display* answer "\n")
                   '("First" "Second" "Third" "Fourth" "Fifth" "Sixth")
                   '("Third" "Fifth")))))
    //)
  ===)

(tm-widget (widget6)
  (centered
    (resize "500px" "50px"
      (texmacs-output
        '(document (theorem (document "This is true.")))))
    ======
    (resize "500px" "300px"
      (texmacs-input
        '(with "bg-color" "#fcfcf8"
           (document (proof (document "Trivial."
                                      "But you may add more details."))))
        (noop) #f))))

(tm-widget (widget7)
  (padded
    ;;(ink (display* answer "\n"))))
    (ink (noop))))

(tm-define widget8-list '("First" "Second"))

(menu-bind widget8-sub
  (for (x widget8-list)
    ((eval x) (display* x "\n"))))

(tm-widget (widget8)
  (padded
    (with l '("First" "Second")
      (input (if answer (set! widget8-list (cons answer widget8-list)))
             "string" '() "1w")
      ===
      (refresh widget8-sub))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some test forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (form1 cmd)
  (form "Test"
    (centered
      (aligned
        (item (text "First:")
          (form-input "First" "string" '("gnu") "1w"))
        (item (text "Second:")
          (form-input "Second" "string" '("gnat") "1w"))))
    (bottom-buttons
      ("Cancel" (cmd "Cancel")) >>
      ("Ok"
       (display* (form-fields) " -> " (form-values) "\n")
       (cmd "Ok")))))

(tm-widget (form2 cmd)
  (centered
    (aligned
      (item (text "First:")
        (toggle (display* "First " answer "\n") #f))
      (item (text "Second:")
        (toggle (display* "Second " answer "\n") #f))))
  (bottom-buttons >> ("Ok" (cmd "Ok"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Routines for showing widgets and forms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (show w)
  ;; Example: execute (show widget1) in a Scheme session
  (top-window w "Simple widget"))

(tm-define (show-form w)
  ;; Example: execute (show-form form1) in a Scheme session
  (dialogue-window w (lambda (x) (display* x "\n")) "Simple form"))
