
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : gui-test.scm
;; DESCRIPTION : Several test routines for widgets
;; COPYRIGHT   : (C) 2007  Joris van der Hoeven
;;
;; This software falls under the GNU general public license and comes WITHOUT
;; ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for details.
;; If you don't have this file, write to the Free Software Foundation, Inc.,
;; 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (kernel gui gui-test))

(tm-build-widget (widget-1)
  "Hallo, hier komt een lange regel met tekst."
  (concat "Links" >>> "Rechts"))

(tm-build-widget (widget-2)
  (concat "a+" (frac "1" "2")))

(tm-build-widget (widget-3 given)
  (center "Different types of data")
  ===
  (let ((hidden "Secret text"))
    (internal "internal" (+ 1 1))
    (input "visible" "")
    ===
    (with "button-shape" "invisible"
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
  (raster
    (:cell-halign * 1 "r")
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
    (raster
      (:cell-halign * 1 "r")
      ("First input:" (input "first" :auto))
      ("Second input:" (input "second" :auto)))
    ===
    (bar
      (form-previous)
      (form-next)
      >>>
      (form-cancel)
      (form-done "Ok" (lambda (x y) (display* x ", " y "\n"))))))
