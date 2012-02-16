
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-test.scm
;; DESCRIPTION : Several test routines for widgets
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-test))

(tm-define (open-test-widget)
  ;; NOTE: close with Done in order to test other widgets
  (widget-popup "Test" '(widget-4)))

(tm-build-widget (widget-1)
  "Hallo, hier komt een lange regel met tekst."
  (concat "Links" >>> "Rechts"))

(tm-build-widget (widget-2)
  (concat "a+" (frac "1" "2")))

(tm-build-widget (widget-3 given)
  (center (concat (strong "Different types of data") (vspace "0.5em")))
  (let ((hidden "Secret text"))
    (internal "internal" (+ 1 1))
    (input "visible" "")
    ===
    (with (:button-shape "invisible")
      (bar
	(button "Parameter" (display* ,given "\n"))
	(button "Hidden" (display* hidden "\n"))
	(button "Internal" (display* (internal-ref "internal") "\n"))
	(button "Visible" (display* (widget-ref "visible") "\n"))
	>>>
	(button "Done" (dismiss))))))

(tm-build-widget (widget-4)
  (short-bar
    (toggle "pressed?" #f)
    (toggle (:box-shape "square") "second?" #f)
    (toggle-button "third?" #f "Toggle"))
  (hidden-input "color" "red"
    (short-bar
      "Green" (radio (:box-color "pastel green") "color" "green")
      "Red" (radio (:box-color "pastel red") "color" "red")
      "Blue" (radio (:box-color "pastel blue") "color" "blue")))
  (short-bar
    (button "Toggles"
      (display* (widget-ref "pressed?") ", "
		(widget-ref "second?") ", "
		(widget-ref "third?") "\n"))
    (button "Radio buttons"
      (display* (widget-ref "color") "\n"))
    (button "Done" (dismiss))))

(tm-build-widget (widget-5)
  (association-tile
    (:font-shape "italic")
    ("From:" (input "First" ""))
    ("To:" (input "Second" "")))
  ===
  (bar
    (button "Copy" (widget-set! "Second" (widget-ref "First")))
    (button "Clear" (widget-set! "Second" ""))
    >>>
    (button "Done" (dismiss))))

(tm-build-widget (widget-6)
  (let ((nr 0)
	(busy #f)
	(sync (lambda () (widget-set! "Output" (tree (number->string nr)))))
	(inc (lambda () (set! nr (+ nr 1)) (sync))))
    (input "Output" "0")
    ===
    (bar
      (button "Start"
	(when (not busy)
	  (set! busy #t)
	  (widget-delayed
	    (:while busy)
	    (:every 1000)
	    (inc))))
      (button "End"
	(set! busy #f))
      (button "Reset"
	(set! nr 0)
	(sync))
      (button "Increase"
	(inc))
      >>>
      (button "Done"
	(dismiss)))))

(tm-build-widget (widget-7)
  (hidden-input "page" "1"
    (header-bar
      (radio-button "page" "1" "First")
      (radio-button "page" "2" "Second")
      (radio-button "page" "3" "Third"))
    (pagelet "page" "1"
      "Hallo"
      "Hop")
    (pagelet "page" "2"
      "Blah"
      "Boem")
    (pagelet "page" "3"
      "Einde verhaal")))

(tm-build-widget (widget-8)
  "Input:"
  (math (block-input "input" ""))
  ===
  "Output:"
  (math (block-input "output" ""))
  ===
  (bar
    (button "Differentiate"
      (widget->script "tmin" "input")
      (script->widget "output" "diff(tmin,x)"))
    (button "Integrate"
      (widget->script "tmin" "input")
      (script->widget "output" "integrate(tmin,x)"))
    (button "Taylor series"
      (widget->script "tmin" "input")
      (script->widget "output" "taylor(tmin,x,0,25)"))
    >>>
    (button "Done" (dismiss))))

(tm-build-widget (widget-9)
  (form ("test" "first" "second")
    (suggestions "first" '("hallo" "hop"))
    (association-tile
      ("First input:" (input "first" :auto))
      ("Second input:" (input "second" :auto)))
    ===
    (bar
      (form-previous)
      (form-next)
      >>>
      (form-cancel)
      (form-done "Ok" (lambda (x y) (display* x ", " y "\n"))))))

(tm-build-widget (widget-10)
  (canvas-input "canvas" "-6em" "6em" "")
  ===
  (bar
    (button "Display" (display* (widget-ref "canvas") "\n"))
    >>>
    (button "Done" (dismiss))))

(tm-build-widget (print-widget)
  (center
    (short-tile
      (:cell-halign * 1 "r")
      (:cell-lsep * -1 "2em")
      ("Printer:"
       (short-input (:short-width "10em") "printer" "default")
       (button (small "Configure")
	  (:button-shape "invisible")
	  (with name (tree->string (widget-ref "printer"))
	    (widget-popup "Printer configuration"
	      `(configure-printer-widget ,name)))))
      ("Options:"
       (short-input (:short-width "10em") "options" "default")
       (button (small "Configure")
	  (with type (tree->string (widget-ref "printer"))
	    (widget-popup "Printer options configuration"
	      `(configure-printer-options-widget ,type)))))))
  ---
  (center
    (short-tile
      (:cell-halign * 1 "r")
      ("Pages:" (concat (short-input (:short-width "2em") "first" "1") " -- "
			(short-input (:short-width "2em") "last" "1")))
      ("Copies:" (short-input (:short-width "2em") "number" "1"))))
  ---
  (bar
    (button "Preview" (noop))
    (button "Export" (noop))
    >>>
    (button "Cancel" (dismiss))
    (button "Print" (noop))))

(tm-build-widget (configure-printer-widget which)
  (let ((name ,which)
	(cmd (if (== name "default") "lpr" (string-append "lpr -P" name))))
    (form ("configure-printer" "name" "command" "paper" "dpi")
      (suggestions "name" (list name))
      (suggestions "command" (list cmd))
      (suggestions "paper" (list "a4"))
      (suggestions "dpi" (list "600"))
      (association-tile
	("Printer name:" (input "name" :auto))
	("Printing command:" (input "command" :auto))
	("Paper format:" (input "paper" :auto))
	("Dots per inch:" (input "dpi" :auto)))
      ===
      (bar
	(form-previous)
	(form-next)
	>>>
	(form-cancel)
	(form-done "Save" ignore)))))

(tm-build-widget (configure-printer-options-widget which)
  (let ((type ,which))
    (form ("configure-printer-options" "type" "parity" "reduce")
      (suggestions "type" (list type))
      (suggestions "parity" (list "all"))
      (suggestions "reduce" (list "1"))
      (suggestions "booklet" (list "false"))
      (association-tile
	("Printer options type:" (input "type" :auto))
	("Filter pages:" (input "parity" :auto))
	("Document pages per page:" (input "reduce" :auto))
	("Reorder pages as booklet:" (toggle "booklet" :auto)))
      ===
      (bar
	(form-previous)
	(form-next)
	>>>
	(form-cancel)
	(form-done "Save" ignore)))))
