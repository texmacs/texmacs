
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scripts-plot.scm
;; DESCRIPTION : routines for on-the-fly evaluation of graphical scripts
;; COPYRIGHT   : (C) 2019  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic scripts-plot)
  (:use (dynamic scripts-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (plot-ready? gt)
  (and (tree? gt) (tree-is? gt 'graphics)))

(define (plot-script? t . opt-name)
  (and (tm-is? t 'plot-script-output)
       (== (tm-arity t) 5)
       (tm-atomic? (tm-ref t 0))
       (tm-atomic? (tm-ref t 1))
       (tm-atomic? (tm-ref t 2))
       (or (null? opt-name)
           (tm-equal? (tm-ref t 2) (car opt-name)))))

(define (plot-search gt name)
  (and (plot-ready? gt)
       (with l (list-filter (tree-children gt) (cut plot-script? <> name))
         (and (nnull? l) (car l)))))

(define (plot-get u)
  (with buf (buffer-get-body u)
    (and (tm-func? buf 'document 1)
         (tm-func? (tm-ref buf 0) 'plot-script-input 5)
         (tm-atomic? (tm-ref buf 0 2))
         (tm-ref buf 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing routines for plot scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (get-plot-names gt)
  (if (plot-ready? gt)
      (with l (list-filter (tree-children gt) plot-script?)
        (map (lambda (t) (tm->string (tm-ref t 2))) l))
      (list)))

(define (first-plot-name l k)
  (with name (string-append "object-" (number->string k))
    (if (nin? name l) name
        (first-plot-name l (+ k 1)))))

(tm-define (new-plot-name gt)
  (first-plot-name (get-plot-names gt) 1))

(tm-define (build-plot-document lan ses gt name)
  (with p (plot-search gt name)
    (if p
        `(document
           (plot-script-input ,@(tm-children p)))
        `(document
           (plot-script-input ,lan ,ses ,name (document "") "")))))

(tm-define (set-plot-name u lan ses gt new-name . opts)
  (and-with new-plot (plot-get u)
    (let* ((old-name (tm->string (tm-ref new-plot 2)))
           (old-plot (plot-search gt old-name)))
      (cond ((and (plot-search gt new-name) (!= new-name old-name))
             (tree-set (buffer-get-body u)
                       (build-plot-document lan ses gt new-name)))
            ((in? :new opts)
             (tree-set new-plot 2 new-name)
             (tree-set (buffer-get-body u)
                       (build-plot-document lan ses gt new-name)))
            ((== new-name old-name) (noop))
            ((and old-plot
                  (tm-equal? (tm-ref new-plot 3)
                             (tm-ref old-plot 3)))
             (tree-set old-plot 2 new-name)
             (tree-set new-plot 2 new-name))
            (else
             (tree-set new-plot 2 new-name))))))

(tm-define (plot-insert u gt name)
  (and-let* ((p (and (plot-ready? gt) (plot-get u)))
             (new (tree-copy
                   (tm->tree
                    `(plot-script-output ,@(tm-children p))))))
    (with old (plot-search gt name)
      (if old
          (tree-set (tm-ref old 3) (tm-ref new 3))
          (tree-insert gt (max (- (tree-arity gt) 1) 0) (list new))))))

(tm-define (plot-refresh gt name)
  (and-let* ((p (plot-search gt name))
             (lan (tm->string (tm-ref p 0)))
             (ses (tm->string (tm-ref p 1)))
             (in (tm-ref p 3))
             (out (tm-ref p 4)))
    (script-eval-at out lan ses in :simplify-output :replace)))

(tm-define (plot-delete u gt)
  (when (plot-get u)
    (let* ((new-plot (plot-get u))
           (name (tm->string (tm-ref new-plot 2)))
           (old-plot (and (plot-ready? gt) (plot-search gt name))))
      (when old-plot
        (tree-remove gt (tree-index old-plot) 1))
      (tree-set new-plot 3 `(document "")))))

(tm-define (plot-apply u gt name)
  (plot-insert u gt name)
  (plot-refresh gt name)
  (delayed
    (:idle 1000)
    (when (and (tree? gt) (tree->path gt))
      (players-set-elapsed gt 0.0)
      (update-players (tree->path gt) #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Example plots
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define sine
  (list "($graph2d -10 10 200 sin)"))

(define sine-cosine
  (list "($graphical"
        "  ($pen-color \"dark blue\""
        "    ($graph2d -10 10 200 sin))"
        "  ($pen-color \"dark green\""
        "    ($graph2d -10 10 200 cos)))"))

(define lissajous
  (list "($fill-color \"pastel yellow\""
        "  ($curve2d -3.14159 3.14159 500"
        "    (lambda (t) (* 4 (sin (* 6 t))))"
        "    (lambda (t) (* 3 (cos (* 10 t))))))"))

(define animation-1
  (list "($animation \"0.2s\""
        "  ($for (t (.. 1 40))"
        "    ($graph2d -10 10 (* 20 t)"
        "      (lambda (x)"
        "        (/ (sin (* 0.2 x t)) (+ (* x x) 1))))))"))

(define animation-2
  (list "($animation \"0.2s\""
        "  ($for (k (.. 0 101))"
        "    ($fill-color \"pastel yellow\""
        "      ($curve2d -3.14159 3.14159 400"
        "        (lambda (t)"
        "          (* 4 (sin (+ (* 6 t) (* k 0.0314159)))))"
        "        (lambda (t)"
        "          (* 3 (cos (* 10 t))))))))"))

(tm-define (plot-select-example u gt name)
  (with example (cond ((== name "sine") sine)
                      ((== name "sine-cosine") sine-cosine)
                      ((== name "lissajous") lissajous)
                      ((== name "animation-1") animation-1)
                      ((== name "animation-2") animation-2))
    (and-with new-plot (plot-get u)
      (tree-set new-plot 0 "scheme")
      (tree-set new-plot 1 "default")
      (tree-set new-plot 2 name)
      (tree-set new-plot 3 `(document ,@example))
      (plot-apply u gt name))))

(define-macro (plot-example u gt name)
  `(begin
     (set! name ,name)
     (plot-select-example ,u ,gt ,name)
     (refresh-now "plot-name")
     (refresh-now "plots-list")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The main editor widget
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((plots-editor u packs lan ses gt name) quit)
  (padded
    (horizontal
      (vertical
        (refreshable "plots-list"
          (resize ("100px" "100px" "100px") ("400px" "400px" "400px")
            (choice (when (string? answer)
                      (set! name answer)
                      (set-plot-name u lan ses gt name)
                      (refresh-now "plot-name"))
                    (get-plot-names gt) name)))
        (glue #f #t 0 0))
      ///
      (vertical
        (refreshable "plot-name"
          (horizontal
            (text "Name:")
            ///
            (input (when (string? answer)
                     (set! name answer)
                     (set-plot-name u lan ses gt name)
                     (refresh-now "plots-list"))
                   "string" (list name) "15em")
            ///
            ((icon "tm_add.xpm")
             (set! name (new-plot-name gt))
             (set-plot-name u lan ses gt name :new)
             (refresh-now "plot-name")
             (refresh-now "plots-list"))
            //
            ((icon "tm_remove.xpm")
             (plot-delete u gt)
             (refresh-now "plot-name")
             (refresh-now "plots-list"))))
        ======
        (resize "800px" "365px"
          (texmacs-input (build-plot-document lan ses gt name)
                         `(style (tuple ,@packs)) u))
        (glue #f #t 0 0)))
    ======
    (hlist
      (explicit-buttons
        (=> "Examples"
            ("Sine" (plot-example u gt "sine"))
            ("Sine and cosine" (plot-example u gt "sine-cosine"))
            ("Lissajous" (plot-example u gt "lissajous"))
            ("Animation 1" (plot-example u gt "animation-1"))
            ("Animation 2" (plot-example u gt "animation-2")))
        >>
        ("Apply" (begin
                   (plot-apply u gt name)
                   (refresh-now "plots-list")))
        // //
        ("Ok" (plot-apply u gt name) (quit))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (open-plots-editor lan ses name)
  (:interactive #t)
  (let* ((b (current-buffer-url))
	 (u (string->url "tmfs://aux/plot-source"))
         (packs (get-style-list))
         (gt (tree-innermost 'graphics #t))
         (wt (if (and gt (tree-is? (tree-up gt) 'with)) (tree-up gt) gt)))
    (when (== name "") (set! name (new-plot-name gt)))
    (when (tree? gt) (tree-go-to wt :end))
    (delayed
      (:idle 1)
      (dialogue-window (plots-editor u packs lan ses gt name)
                       (lambda x
                         (when (tree->path gt)
                           (delayed
                             (:idle 1)
                             (tree-go-to gt :last :end))))
                       "Plots editor" u)
      (buffer-set-master u b))))
