
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : format-widgets.scm
;; DESCRIPTION : Widgets for text, paragraph and page properties
;; COPYRIGHT   : (C) 2013  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic pattern-selector)
  (:use (generic format-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector / accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define global-picture? #f)
(define global-pattern-color `(pattern "neutral-pattern.png" "1cm" "100@"))

(define (set-color col)
  (set! global-pattern-color col)
  (refresh-now "pattern-sample")
  (refresh-now "pattern-options"))

(define (get-color)
  global-pattern-color)

(define (set-name name)
  (with p (url->unix "$TEXMACS_PATH/misc/patterns")
    (when (string-starts? name p)
      (set! name (url->unix (url-delta (url-append (unix->url p) "dummy")
                                       (unix->url name)))))
    (with col (get-color)
      (set-color `(pattern ,name ,@(cddr col))))))

(define (get-name)
  (cadr (get-color)))

(define (set-width w)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,w ,@(cdddr col)))))

(define (get-width)
  (caddr (get-color)))

(define (set-height h)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,(caddr col) ,h ,@(cddddr col)))))

(define (get-height)
  (cadddr (get-color)))

(define (set-size s)
  (cond ((== s "Fit") (set-width "100%") (set-height "100%"))
        ((== s "Fit to width") (set-width "100%") (set-height "100@"))
        ((== s "Fit to height") (set-width "100@") (set-height "100%"))))

(define (get-size)
  (cond ((and (== (get-width) "100%") (== (get-height) "100%")) "Fit")
        ((== (get-width) "100%") "Fit to width")
        ((== (get-height) "100%") "Fit to height")
        (else "Fit")))

(define (reset-effect eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) eff)
        ((== (car eff) kind) (reset-effect (cadr eff) kind))
        (else (cons* (car eff) (reset-effect (cadr eff) kind) (cddr eff)))))

(define (assign-effect eff kind args)
  (cons* kind eff args))

(define (set-effect* tail apply? kind args)
  (if (null? tail) (set! tail (list "0")))
  (with res (reset-effect (car tail) kind)
    (with new (if apply? (assign-effect res kind args) res)
      (if (== new "0") (list) (list new)))))

(define (set-effect kind apply? . args)
  (with col (get-color)
    (set-color `(pattern ,(cadr col) ,(caddr col) ,(cadddr col)
                         ,@(set-effect* (cddddr col) apply? kind args)))))

(define (get-effect* eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) #f)
        ((== (car eff) kind) (cddr eff))
        (else (get-effect* (cadr eff) kind))))

(define (get-effect kind)
  (with col (get-color)
    (with eff (and (nnull? (cddddr col)) (car (cddddr col)))
      (get-effect* eff kind))))

(define (set-recolor recol)
  (set-effect 'eff-recolor (nnot recol) recol))

(define (get-recolor)
  (and-with opts (get-effect 'eff-recolor)
    (car opts)))

(define (set-skin skin)
  (when (string? skin)
    (with (r g b a) (named-color->rgba skin)
      (when (== a 255) (set! a 64))
      (set! skin (rgba->named-color (list r g b a)))))
  (set-effect 'eff-skin (nnot skin) skin))

(define (inc-skin)
  (and-with skin (get-skin)
    (with (r g b a) (named-color->rgba skin)
      (set! a (min 215 (inexact->exact (round (* (sqrt (sqrt 2)) a)))))
      (set-skin (rgba->named-color (list r g b a))))))

(define (dec-skin)
  (and-with skin (get-skin)
    (with (r g b a) (named-color->rgba skin)
      (set! a (max 4 (inexact->exact (round (* (sqrt (sqrt 0.5)) a)))))
      (set-skin (rgba->named-color (list r g b a))))))

(define (get-skin)
  (and-with opts (get-effect 'eff-skin)
    (car opts)))

(define (set-blur r)
  (set-effect 'eff-blur (nnot r) `(eff-gaussian ,r)))

(define (get-blur)
  (and-with opts (get-effect 'eff-blur)
    (and (tm-func? (car opts) 'eff-gaussian)
         (tm-ref (car opts) 0))))

(define (normalize-color col)
  (if (tm-func? col 'pattern)
      (apply tm-pattern (cdr col))
      col))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (pattern-name-selector)
  (let* ((name (unix->url (get-name)))
         (base "$TEXMACS_PATH/misc/patterns/neutral-pattern.png")
         (curr (url-relative base name))
         (setter (lambda (c)
                   (when (and (pair? c) (url? (car c)))
                     (set-name (url->unix (car c)))))))
    (hlist
      (enum (set-name (url->unix answer))
            (list (url->system name) "")
            (url->system name) "15em")
      // // //
      ((icon "tm_find.xpm")
       (cond ((not global-picture?)
              (choose-file setter "Background pattern" "image" "" curr))
             ((url-rooted? (unix->url (get-name)))
              (choose-file setter "Background picture" "image" "" curr))
             (else
              (choose-file setter "Background picture" "image"))))
      >>)))

(tm-widget (pattern-recolor-options)
  (with recol (get-recolor)
    (hlist
      (when recol
        (enum (set-recolor answer)
              (list (or recol "")
		    "black" "white" "grey" "red" "green" "blue"
		    "yellow" "cyan" "magenta" "orange" "brown" "")
              (or recol "") "15em"))
      // // //
      (toggle (set-recolor (and answer "black"))
              (nnot (get-recolor)))
      // // //
      ((icon "tm_color.xpm")
       (interactive-color set-recolor (list (or recol ""))))
      >>)))

(tm-widget (pattern-skin-options)
  (with skin (get-skin)
    (hlist
      (when skin
        (enum (set-skin answer)
              (list (or skin "")
		    "black" "white" "grey" "red" "green" "blue"
		    "yellow" "cyan" "magenta" "orange" "brown" "")
              (or skin "") "15em"))
      // // //
      (toggle (set-skin (and answer "black"))
              (nnot (get-skin)))
      // // //
      ((icon "tm_color.xpm")
       (interactive-color set-skin (list (or skin ""))))
      // //
      (when skin
	((icon "tm_remove.xpm") (dec-skin))
	((icon "tm_add.xpm") (inc-skin)))
      >>)))

(tm-widget (pattern-blur-options)
  (with blur (get-blur)
    (hlist
      (when blur
        (enum (set-blur answer)
              (list (or blur "") "0.2pt" "0.5pt" "1pt" "2pt" "5px" "")
              (or blur "") "15em"))
      // // //
      (toggle (set-blur (and answer "1pt"))
              (nnot (get-blur)))
      >>)))

(tm-widget ((pattern-selector u) cmd)
  (padded
    (hlist
      (vlist
        (refreshable "pattern-sample"
          (resize "600px" "450px"
            (texmacs-output `(document
                               (block
                                (tformat
                                 (cwith "1" "1" "1" "1" "cell-width" "590px")
                                 (cwith "1" "1" "1" "1" "cell-height" "440px")
                                 (cwith "1" "1" "1" "1" "cell-vmode" "exact")
                                 (cwith "1" "1" "1" "1" "cell-background"
                                        ,(get-color))
                                 (table (row (cell ""))))))
                            `(style (tuple "generic"))))))
      // // //
      (explicit-buttons
        (vlist
          (refreshable "pattern-options"
            (assuming (not global-picture?)
              (aligned
                (item (text "Name:")
                  (link pattern-name-selector))
                (item (text "Width:")
                  (hlist
                    (enum (set-width answer)
                          (list (get-width) "100%" "100@" "1cm" "")
                          (get-width) "15em") >>))
                (item (text "Height:")
                  (hlist
                    (enum (set-height answer)
                          (list (get-height) "100%" "100@" "1cm" "")
                          (get-height) "15em") >>))
                (item (text "Recolor:")
                  (link pattern-recolor-options))
                (item (text "Skin:")
                  (link pattern-skin-options))
                ;; TODO: the blur effect changes the image size;
                ;; one needs a blur that wraps around torically
                ;;(item (text "Blur:")
                ;;  (link pattern-blur-options))
                ))
            (assuming global-picture?
              (aligned
                (item (text "Name:")
                  (link pattern-name-selector))
                (item (text "Size:")
                  (hlist
                    (enum (set-size answer)
                          (list "Fit" "Fit to width" "Fit to height")
                          (get-size) "15em") >>))
                (item (text "Recolor:")
                  (link pattern-recolor-options))
                (item (text "Skin:")
                  (link pattern-skin-options))
                )))
          ======
          (glue #f #t 0 0))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Ok" (with col (normalize-color (get-color))
                (if global-picture?
                    (insert-preferred-list "my pictures" col 16)
                    (insert-preferred-list "my patterns" col 16))
                (cmd col)))))))

(tm-define (open-pattern-selector cmd w)
  (:interactive #t)
  (when (or global-picture? (== (get-name) "neutral-pattern.png"))
    (set! global-picture? #f)
    (set! global-pattern-color `(pattern "neutral-pattern.png" ,w "100@")))
  (with u (current-buffer)
    (dialogue-window (pattern-selector u) cmd "Pattern selector")))

(tm-define (open-background-picture-selector cmd . opt-old)
  (:interactive #t)
  (when (or (not global-picture?) (== (get-name) "neutral-pattern.png"))
    (set! global-picture? #t)
    (set! global-pattern-color `(pattern "neutral-pattern.png" "100%" "100%")))
  (when (nnull? opt-old)
    (set! global-pattern-color (car opt-old)))
  (with u (current-buffer)
    (dialogue-window (pattern-selector u) cmd "Background picture selector")))
