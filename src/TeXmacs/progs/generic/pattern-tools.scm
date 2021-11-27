
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : pattern-tools.scm
;; DESCRIPTION : Widget for pattern selection
;; COPYRIGHT   : (C) 2016-2021  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; See menu-define.scm for the grammar of menus
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (generic pattern-tools)
  (:use (generic pattern-selector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Name conversions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (encode-pattern-name key u)
  (let* ((name (if (string? u) u (url->unix u)))
         (t (url->unix (url-tail u)))
         (p (url->unix "$TEXMACS_PATH/misc/patterns"))
         (a (url->unix "$TEXMACS_PATH/misc"))
         (p* (url-append (unix->url p) "dummy"))
         (a* (url-append (unix->url a) "dummy")))
    (cond ((string-starts? name p)
           (url->unix (url-delta p* (unix->url name))))
          ((and (string-starts? name a)
                (string-starts? t "thumbnail-"))
           (let* ((t* (string-drop t 10))
                  (d (url-delta a* (unix->url name)))
                  (d* (url-relative d (unix->url t*))))
             (url->unix (url-append "tmfs://artwork" d*))))
          (else u))))

(define (decode-pattern-name key s)
  (let* ((name (unix->url s))
         (base1 "$TEXMACS_PATH/misc/patterns/neutral-pattern.png")
         (base2 "$TEXMACS_PATH/misc/pictures/gradients/vertical-white-black.png")
         (base (if (gradient? key) base2 base1))
         (artw "$TEXMACS_PATH/misc/dummy"))
    (cond ((not (url-rooted? name))
           (url-relative base name))
          ((and (string? s) (string-starts? s "tmfs://artwork/"))
           (let* ((u (url->unix (string-drop s 15)))
                  (dir (url-head u))
                  (tn (string-append "thumbnail-" (url->unix (url-tail u))))
                  (file (url-append dir (unix->url tn))))
             (url-relative artw file)))
          (else name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector / accessors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pattern? key) (func? key :pattern))
(define (picture? key) (func? key :picture))
(define (gradient? key) (func? key :gradient))

(define global-pattern-color (make-ahash-table))

(define (set-color key col)
  (ahash-set! global-pattern-color key col)
  (refresh-now "pattern-sample")
  (refresh-now "pattern-options")
  (update-menus))

(define (get-color key)
  (or (ahash-ref global-pattern-color key)
      (cond ((pattern? key)
             `(pattern "neutral-pattern.png" "1cm" "100@"))
            ((gradient? key)
             `(pattern "vertical-white-black.png" "100%" "100%"))
            ((picture? key)
             `(pattern "neutral-pattern.png" "100%" "100%"))
            (else
             `(pattern "neutral-pattern.png" "1cm" "100@")))))

(define (set-name key name)
  (with enc (encode-pattern-name key name)
    (with col (get-color key)
      (set-color key `(pattern ,enc ,@(cddr col))))))

(define (get-name key)
  (cadr (get-color key)))

(define (set-width key w)
  (with col (get-color key)
    (set-color key `(pattern ,(cadr col) ,w ,@(cdddr col)))))

(define (get-width key)
  (caddr (get-color key)))

(define (set-height key h)
  (with col (get-color key)
    (set-color key `(pattern ,(cadr col) ,(caddr col) ,h ,@(cddddr col)))))

(define (get-height key)
  (cadddr (get-color key)))

(define (set-size key s)
  (cond ((== s "Fit")
         (set-width key "100%") (set-height key "100%"))
        ((== s "Fit to width")
         (set-width key "100%") (set-height key "100@"))
        ((== s "Fit to height")
         (set-width key "100@") (set-height key "100%"))))

(define (get-size key)
  (cond ((and (== (get-width key) "100%")
              (== (get-height key) "100%")) "Fit")
        ((== (get-width key) "100%") "Fit to width")
        ((== (get-height key) "100%") "Fit to height")
        (else "Fit")))

(define (reset-effect key eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) eff)
        ((== (car eff) kind) (reset-effect key (cadr eff) kind))
        (else
          (cons* (car eff) (reset-effect key (cadr eff) kind) (cddr eff)))))

(define (assign-effect key eff kind args)
  (cons* kind eff args))

(define (set-effect* key tail apply? kind args)
  (if (null? tail) (set! tail (list "0")))
  (with res (reset-effect key (car tail) kind)
    (with new (if apply? (assign-effect key res kind args) res)
      (if (== new "0") (list) (list new)))))

(define (set-effect key kind apply? . args)
  (with col (get-color key)
    (set-color key `(pattern ,(cadr col) ,(caddr col) ,(cadddr col)
                         ,@(set-effect* key (cddddr col) apply? kind args)))))

(define (get-effect* key eff kind)
  (cond ((or (npair? eff) (npair? (cdr eff))) #f)
        ((== (car eff) kind) (cddr eff))
        (else (get-effect* key (cadr eff) kind))))

(define (get-effect key kind)
  (with col (get-color key)
    (with eff (and (nnull? (cddddr col)) (car (cddddr col)))
      (get-effect* key eff kind))))

(define (set-recolor key recol)
  (set-effect key 'eff-recolor (nnot recol) recol))

(define (get-recolor key)
  (and-with opts (get-effect key 'eff-recolor)
    (car opts)))

(define (set-skin key skin)
  (when (string? skin)
    (with (r g b a) (named-color->rgba skin)
      (when (== a 255) (set! a 64))
      (set! skin (rgba->named-color (list r g b a)))))
  (set-effect key 'eff-skin (nnot skin) skin))

(define (inc-skin key)
  (and-with skin (get-skin key)
    (with (r g b a) (named-color->rgba skin)
      (set! a (min 215 (inexact->exact (round (* (sqrt (sqrt 2)) a)))))
      (set-skin key (rgba->named-color (list r g b a))))))

(define (dec-skin key)
  (and-with skin (get-skin key)
    (with (r g b a) (named-color->rgba skin)
      (set! a (max 4 (inexact->exact (round (* (sqrt (sqrt 0.5)) a)))))
      (set-skin key (rgba->named-color (list r g b a))))))

(define (get-skin key)
  (and-with opts (get-effect key 'eff-skin)
    (car opts)))

(define (set-blur key r)
  (set-effect key 'eff-blur (nnot r) `(eff-gaussian ,r)))

(define (get-blur key)
  (and-with opts (get-effect key 'eff-blur)
    (and (tm-func? (car opts) 'eff-gaussian)
         (tm-ref (car opts) 0))))

(define (set-gradient-foreground key fg)
  (with opts (or (get-effect key 'eff-gradient) (list "black" "white"))
    (set-effect key 'eff-gradient #t fg (cadr opts))))

(define (get-gradient-foreground key)
  (and-with opts (get-effect key 'eff-gradient)
    (car opts)))

(define (set-gradient-background key bg)
  (with opts (or (get-effect key 'eff-gradient) (list "black" "white"))
    (set-effect key 'eff-gradient #t (car opts) bg)))

(define (get-gradient-background key)
  (and-with opts (get-effect key 'eff-gradient)
    (cadr opts)))

(define (normalize-color col)
  (if (tm-func? col 'pattern)
      (apply tm-pattern (cdr col))
      col))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern selector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (pattern-name-selector key)
  (let* ((name (unix->url (get-name key)))
         (curr (decode-pattern-name key (get-name key)))
         (setter (lambda (c)
                   (when (and (pair? c) (url? (car c)))
                     (set-name key (url->unix (car c)))))))
    (hlist
      (enum (set-name key (url->unix answer))
            (list (url->system name) "")
            (url->system name) "15em")
      // // //
      ((icon "tm_find.xpm")
       (cond ((pattern? key)
              (choose-file setter "Background pattern" "image" "" curr))
             ((gradient? key)
              (choose-file setter "Background gradient" "image" "" curr))
             ((url-rooted? (unix->url (get-name key)))
              (choose-file setter "Background picture" "image" "" curr))
             (else
              (choose-file setter "Background picture" "image"))))
      >>)))

(tm-widget (pattern-recolor-options key)
  (hlist
    (when (get-recolor key)
      (enum (set-recolor key answer)
            (list (or (get-recolor key) "")
                  "black" "white" "grey" "red" "green" "blue"
                  "yellow" "cyan" "magenta" "orange" "brown" "")
            (or (get-recolor key) "") "15em"))
    // // //
    (toggle (set-recolor key (and answer "black"))
            (nnot (get-recolor key)))
    // // //
    ((icon "tm_color.xpm")
     (interactive-color (cut set-recolor key <>)
                        (list (or (get-recolor key) ""))))
    >>))

(tm-widget (pattern-skin-options key)
  (hlist
    (when (get-skin key)
      (enum (set-skin key answer)
            (list (or (get-skin key) "")
                  "black" "white" "grey" "red" "green" "blue"
                  "yellow" "cyan" "magenta" "orange" "brown" "")
            (or (get-skin key) "") "15em"))
    // // //
    (toggle (set-skin key (and answer "black"))
            (nnot (get-skin key)))
    // // //
    ((icon "tm_color.xpm")
     (interactive-color (cut set-skin key <>)
                        (list (or (get-skin key) ""))))
    // //
    (when (get-skin key)
      ((icon "tm_remove.xpm") (dec-skin key))
      ((icon "tm_add.xpm") (inc-skin key)))
    >>))

(tm-widget (pattern-blur-options key)
  (hlist
    (when (get-blur key)
      (enum (set-blur key answer)
            (list (or (get-blur key) "")
                  "0.2pt" "0.5pt" "1pt" "2pt" "5px" "")
            (or (get-blur key) "") "15em"))
    // // //
    (toggle (set-blur key (and answer "1pt"))
            (nnot (get-blur key)))
    >>))

(tm-widget (pattern-background-options key)
  (hlist
    (enum (set-gradient-background key answer)
          (list (or (get-gradient-background key) "")
                "black" "white" "grey" "red" "green" "blue"
                "yellow" "cyan" "magenta" "orange" "brown" "")
          (or (get-gradient-background key) "white") "15em")
    // // //
    ((icon "tm_color.xpm")
     (interactive-color (cut set-gradient-background key <>)
                        (list (or (get-gradient-background key) "white"))))
    >>))

(tm-widget (pattern-foreground-options key)
  (hlist
    (enum (set-gradient-foreground key answer)
          (list (or (get-gradient-foreground key) "")
                "black" "white" "grey" "red" "green" "blue"
                "yellow" "cyan" "magenta" "orange" "brown" "")
          (or (get-gradient-foreground key) "black") "15em")
    // // //
    ((icon "tm_color.xpm")
     (interactive-color (cut set-gradient-foreground key <>)
                        (list (or (get-gradient-foreground key) "black"))))
    >>))

(tm-widget (pattern-sample key u)
  (refreshable "pattern-sample"
    (resize "400px" "250px"
      (texmacs-output
       `(document
          (block
           (tformat
            (cwith "1" "1" "1" "1" "cell-width" "396guipx")
            (cwith "1" "1" "1" "1" "cell-height" "246guipx")
            (cwith "1" "1" "1" "1" "cell-vmode" "exact")
            (cwith "1" "1" "1" "1" "cell-background"
                   ,(get-color key))
            (table (row (cell ""))))))
       `(style (tuple "generic"))))))

(tm-widget (pattern-options key u)
  (refreshable "pattern-options"
    (assuming (pattern? key)
      (aligned
        (item (text "Name:")
          (dynamic (pattern-name-selector key)))
        (item (text "Width:")
          (hlist
            (enum (set-width key answer)
                  (list (get-width key) "100%" "100@" "1cm" "")
                  (get-width key) "15em") >>))
        (item (text "Height:")
          (hlist
            (enum (set-height key answer)
                  (list (get-height key) "100%" "100@" "1cm" "")
                  (get-height key) "15em") >>))
        (item (text "Recolor:")
          (dynamic (pattern-recolor-options key)))
        (item (text "Skin:")
          (dynamic (pattern-skin-options key)))
        ;; TODO: the blur effect changes the image size;
        ;; one needs a blur that wraps around torically
        ;;(item (text "Blur:")
        ;;  (dynamic (pattern-blur-options key)))
        ))
    (assuming (gradient? key)
      (aligned
        (item (text "Name:")
          (dynamic (pattern-name-selector key)))
        (item (text "Width:")
          (hlist
            (enum (set-width key answer)
                  (list (get-width key) "100%" "100@" "1cm" "")
                  (get-width key) "15em") >>))
        (item (text "Height:")
          (hlist
            (enum (set-height key answer)
                  (list (get-height key) "100%" "100@" "1cm" "")
                  (get-height key) "15em") >>))
        (item (text "Foreground:")
          (dynamic (pattern-foreground-options key)))
        (item (text "Background:")
          (dynamic (pattern-background-options key)))
        ))
    (assuming (picture? key)
      (aligned
        (item (text "Name:")
          (dynamic (pattern-name-selector key)))
        (item (text "Size:")
          (hlist
            (enum (set-size key answer)
                  (list "Fit" "Fit to width" "Fit to height")
                  (get-size key) "15em") >>))
        (item (text "Recolor:")
          (dynamic (pattern-recolor-options key)))
        (item (text "Skin:")
          (dynamic (pattern-skin-options key)))
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for stand-alone windows
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget ((pattern-selector key u) cmd)
  (padded
    (hlist
      (vlist
        (dynamic (pattern-sample key u)))
      // // //
      (explicit-buttons
        (vlist
          (dynamic (pattern-options key u))
          ======
          (glue #f #t 0 0))))
    ======
    (explicit-buttons
      (hlist
        >>>
        ("Ok" (with col (normalize-color (get-color key))
                (if (picture? key)
                    (insert-preferred-list "my pictures" col 16)
                    (insert-preferred-list "my patterns" col 16))
                (cmd col)))))))

(tm-define (open-pattern-selector cmd w)
  (:interactive #t)
  (with key (list :pattern)
    (with u (current-buffer)
      (dialogue-window (pattern-selector key u) cmd "Pattern selector"))))

(tm-define (open-gradient-selector cmd . opt-old)
  (:interactive #t)
  (with key (list :gradient)
    (when (nnull? opt-old)
      (ahash-set! global-pattern-color key (car opt-old)))
    (with u (current-buffer)
      (dialogue-window (pattern-selector key u) cmd "Gradient selector"))))

(tm-define (open-background-picture-selector cmd . opt-old)
  (:interactive #t)
  (with key (list :picture)
    (when (nnull? opt-old)
      (ahash-set! global-pattern-color key (car opt-old)))
    (with u (current-buffer)
      (dialogue-window (pattern-selector key u) cmd
                       "Background picture selector"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface for side-tools
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-widget (pattern-selector-tool key u)
  (dynamic (pattern-sample key u))
  ======
  (dynamic (pattern-options key u))
  ;;======
  ;;(hlist
  ;;  >>>
  ;;  ("Ok" (with col (normalize-color (get-color key))
  ;;          (if (picture? key)
  ;;              (insert-preferred-list "my pictures" col 16)
  ;;              (insert-preferred-list "my patterns" col 16))
  ;;          (cmd col))))
  )

(tm-tool (pattern-tool win name)
  (:name (cadr tool))
  (let* ((key (list :pattern name win))
         (u (window->buffer win)))
    (dynamic (pattern-selector-tool key u))))

(tm-tool (gradient-tool win name)
  (:name (cadr tool))
  (let* ((key (list :gradient name win))
         (u (window->buffer win)))
    (dynamic (pattern-selector-tool key u))))

(tm-tool (picture-tool win name)
  (:name (cadr tool))
  (let* ((key (list :picture name win))
         (u (window->buffer win)))
    (dynamic (pattern-selector-tool key u))))

(display* "Alternative pattern selector\n")
