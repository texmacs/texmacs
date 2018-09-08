
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : animate-menu.scm
;; DESCRIPTION : menus for editing animations
;; COPYRIGHT   : (C) 2016  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (dynamic animate-menu)
  (:use (dynamic animate-edit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Insert animations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind insert-animation-menu
  (when (or (selection-active-small?)
            (and (selection-active-any?)
                 (tm-func? (selection-tree) 'gr-screen)))
    ("Animate" (interactive animate-selection))
    ---)
  (when (not (selection-active-non-small?))
    ("Fixed" (interactive make-anim-constant))
    ("Compose" (make 'anim-compose))
    ("Repeat" (make 'anim-repeat))
    ("Retime" (retime-selection "normal")))
  ---
  (-> "Appear"
      ("Translate" (make-anim 'translate-in))
      ("Progressive" (make-anim 'progressive-in))
      ("Fade" (make-anim 'fade-in))
      ("Zoom" (make-anim 'zoom-in)))
  (-> "Vanish"
      ("Translate" (make-anim 'translate-out))
      ("Progressive" (make-anim 'progressive-out))
      ("Fade" (make-anim 'fade-out))
      ("Zoom" (make-anim 'zoom-out)))
  (-> "Alter"
      ("Translate" (make-anim 'translate-smooth))
      ("Progressive" (make-anim 'progressive-smooth))
      ("Fade" (make-anim 'fade-smooth))
      ("Zoom" (make-anim 'zoom-smooth)))
  (assuming (== (get-preference "bitmap effects") "on")
    (-> "Emphasize"
        ("Shadowed" (make-anim 'shadowed-smooth))
        ("Emboss" (make-anim 'emboss-smooth))
        ("Outlined emboss" (make-anim 'outlined-emboss-smooth))))
  ---
  ("Animation" (choose-file make-animation "Load file" "animation"))
  ("Sound" (choose-file make-sound "Load file" "sound")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (focus-tree-modified t)
  (:require (anim-get-accelerate t))
  (reset-players (anim-get-accelerate t)))

(tm-menu (focus-toggle-menu t)
  (:require (anim-get-accelerate t)))

(tm-menu (focus-toggle-icons t)
  (:require (anim-get-accelerate t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Time bending
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (accelerate-icon type)
  (cond ((== type "fade-in") "tm_anim_fade_in.xpm")
        ((== type "fade-out") "tm_anim_fade_out.xpm")
        ((== type "faded") "tm_anim_faded.xpm")
        ((== type "bump") "tm_anim_bump.xpm")
        ((== type "reverse") "tm_anim_reverse.xpm")
        ((== type "reverse-fade-in") "tm_anim_reverse_fade_in.xpm")
        ((== type "reverse-fade-out") "tm_anim_reverse_fade_out.xpm")
        ((== type "reverse-faded") "tm_anim_reverse_faded.xpm")
        ((== type "reverse-bump") "tm_anim_reverse_bump.xpm")
        (else "tm_anim_normal.xpm")))

(tm-menu (anim-fixed-menu t)
  (with setter (lambda (portion)
                 (accelerate-set-type* t `(tuple "fixed" ,portion)))
    ("Start" (setter "0"))
    ("10%" (setter "0.1"))
    ("20%" (setter "0.2"))
    ("30%" (setter "0.3"))
    ("40%" (setter "0.4"))
    ("50%" (setter "0.5"))
    ("60%" (setter "0.6"))
    ("70%" (setter "0.7"))
    ("80%" (setter "0.8"))
    ("90%" (setter "0.9"))
    ("End" (setter "1"))
    ---
    ("Other" (interactive setter (list "Portion" "0")))))

(tm-menu (anim-acceleration-menu t)
  ("Normal" (accelerate-set-type* t "normal"))
  ("Smooth start" (accelerate-set-type* t "fade-in"))
  ("Smooth end" (accelerate-set-type* t "fade-out"))
  ("Smooth extremities" (accelerate-set-type* t "faded"))
  ("Bump" (accelerate-set-type* t "bump"))
  ---
  ;;(-> "Fixed" (dynamic (anim-fixed-menu t)))
  ("Reverse" (accelerate-toggle-reverse? t)))

(tm-menu (focus-animate-menu t)
  (:require (anim-get-accelerate t))
  (with type (accelerate-get-type t)
    (-> "Time evolution"
        (dynamic (anim-acceleration-menu t)))))

(tm-menu (focus-animate-icons t)
  (:require (anim-get-accelerate t))
  (with type (accelerate-get-type t)
    (=> (balloon (icon (eval (accelerate-icon type)))
                 "Time evolution")
        (dynamic (anim-acceleration-menu t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction menu for translations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (translate-icon x y)
  (cond ((and (= x -1) (= y  0)) "tm_right.xpm")
        ((and (= x  1) (= y  0)) "tm_left.xpm")
        ((and (= x  0) (= y -1)) "tm_up.xpm")
        ((and (= x  0) (= y  1)) "tm_down.xpm")
        ((and (= x -1) (= y -1)) "tm_right_up.xpm")
        ((and (= x -1) (= y  1)) "tm_right_down.xpm")
        ((and (= x  1) (= y -1)) "tm_left_up.xpm")
        ((and (= x  1) (= y  1)) "tm_left_down.xpm")
        (else "tm_customized.xpm")))

(define (translate-test? t x y inv?)
  (set! inv? (inside? 'translate-out))
  (let* ((xx (get-env (if inv? "translate-end-x" "translate-start-x")))
         (yy (get-env (if inv? "translate-end-y" "translate-start-y"))))
    (and (= (if inv? (- x) x) (string->number xx))
         (= (if inv? (- y) y) (string->number yy)))))
  
(tm-define (translate-set t x y inv?)
  (:check-mark "*" translate-test?)
  (let* ((xx (number->string (if inv? (- x) x)))
         (yy (number->string (if inv? (- y) y)))
         (xn (if inv? "translate-end-x" "translate-start-x"))
         (yn (if inv? "translate-end-y" "translate-start-y")))
    (tree-with-set t xn xx yn yy)))

(tm-menu (anim-translation-menu t inv?)
  ("Right" (translate-set t -1 0 inv?))
  ("Left" (translate-set t 1 0 inv?))
  ("Up" (translate-set t 0 -1 inv?))
  ("Down" (translate-set t 0 1 inv?))
  ---
  ("Right up" (translate-set t -1 -1 inv?))
  ("Right down" (translate-set t -1 1 inv?))
  ("Left up" (translate-set t 1 -1 inv?))
  ("Left down" (translate-set t 1 1 inv?)))

(tm-menu (focus-misc-icons t)
  (:require (tree-in? t '(translate-in)))
  (let* ((x (string->number (get-env "translate-start-x")))
         (y (string->number (get-env "translate-start-y"))))
    (=> (balloon (icon (eval (translate-icon x y)))
                 "Direction of translation")
        (dynamic (anim-translation-menu t #f)))))

(tm-menu (focus-misc-icons t)
  (:require (tree-in? t '(translate-out)))
  (let* ((x (string->number (get-env "translate-end-x")))
         (y (string->number (get-env "translate-end-y"))))
    (=> (balloon (icon (eval (translate-icon (- x) (- y))))
                 "Direction of translation")
        (dynamic (anim-translation-menu t #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direction menu for progressive in/out
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (progressive-icon l b r t inv?)
  (cond ((and (= l 0.5) (= r 0.5) (= b 0.5) (= t 0.5))
         (if inv? "tm_customized.xpm" "tm_outwards.xpm"))
        ((and (= l 0) (= r 0) (= b 0) (= t 0)) "tm_right_up.xpm")
        ((and (= l 0) (= r 0) (= b 1) (= t 1)) "tm_right_down.xpm")
        ((and (= l 1) (= r 1) (= b 0) (= t 0)) "tm_left_up.xpm")
        ((and (= l 1) (= r 1) (= b 1) (= t 1)) "tm_left_down.xpm")
        ((and (= l 0) (= r 0) (= b 0) (= t 1)) "tm_right.xpm")
        ((and (= l 1) (= r 1) (= b 0) (= t 1)) "tm_left.xpm")
        ((and (= b 0) (= t 0) (= l 0) (= r 1)) "tm_up.xpm")
        ((and (= b 1) (= t 1) (= l 0) (= r 1)) "tm_down.xpm")
        (else "tm_customized.xpm")))

(define (progressive-test? ft l b r t inv?)
  (set! inv? (inside? 'progressive-out))
  (let* ((ll (get-env (if inv? "progressive-end-l" "progressive-start-l")))
         (bb (get-env (if inv? "progressive-end-b" "progressive-start-b")))
         (rr (get-env (if inv? "progressive-end-r" "progressive-start-r")))
         (tt (get-env (if inv? "progressive-end-t" "progressive-start-t"))))
    (and (= (if inv? (min (- 1 l) (- 1 r)) l) (string->number ll))
         (= (if inv? (min (- 1 b) (- 1 t)) b) (string->number bb))
         (= (if inv? (max (- 1 l) (- 1 r)) r) (string->number rr))
         (= (if inv? (max (- 1 b) (- 1 t)) t) (string->number tt)))))
  
(tm-define (progressive-set ft l b r t inv?)
  (:check-mark "*" progressive-test?)
  (let* ((ll (number->string (if inv? (min (- 1 l) (- 1 r)) l)))
         (bb (number->string (if inv? (min (- 1 b) (- 1 t)) b)))
         (rr (number->string (if inv? (max (- 1 l) (- 1 r)) r)))
         (tt (number->string (if inv? (max (- 1 b) (- 1 t)) t)))
         (ln (if inv? "progressive-end-l" "progressive-start-l"))
         (bn (if inv? "progressive-end-b" "progressive-start-b"))
         (rn (if inv? "progressive-end-r" "progressive-start-r"))
         (tn (if inv? "progressive-end-t" "progressive-start-t")))
    (tree-with-set ft ln ll bn bb rn rr tn tt)))

(tm-menu (anim-progressive-menu t inv?)
  (assuming (not inv?)
    ("Outwards" (progressive-set t 0.5 0.5 0.5 0.5 #f))
    ---)
  (assuming inv?
    ("Inwards" (progressive-set t 0.5 0.5 0.5 0.5 #t))
    ---)
  ("Right" (progressive-set t 0 0 0 1 inv?))
  ("Left" (progressive-set t 1 0 1 1 inv?))
  ("Up" (progressive-set t 0 0 1 0 inv?))
  ("Down" (progressive-set t 0 1 1 1 inv?))
  ---
  ("Right up" (progressive-set t 0 0 0 0 inv?))
  ("Right down" (progressive-set t 0 1 0 1 inv?))
  ("Left up" (progressive-set t 1 0 1 0 inv?))
  ("Left down" (progressive-set t 1 1 1 1 inv?)))

(tm-menu (focus-misc-icons ft)
  (:require (tree-in? ft '(progressive-in)))
  (let* ((l (string->number (get-env "progressive-start-l")))
         (b (string->number (get-env "progressive-start-b")))
         (r (string->number (get-env "progressive-start-r")))
         (t (string->number (get-env "progressive-start-t"))))
    (=> (balloon (icon (eval (progressive-icon l b r t #f)))
                 "Direction of progression")
        (dynamic (anim-progressive-menu ft #f)))))

(tm-menu (focus-misc-icons ft)
  (:require (tree-in? ft '(progressive-out)))
  (let* ((l (string->number (get-env "progressive-end-l")))
         (b (string->number (get-env "progressive-end-b")))
         (r (string->number (get-env "progressive-end-r")))
         (t (string->number (get-env "progressive-end-t")))
         (cl (min (- 1 l) (- 1 r)))
         (cr (max (- 1 l) (- 1 r)))
         (cb (min (- 1 b) (- 1 t)))
         (ct (max (- 1 b) (- 1 t))))
    (=> (balloon (icon (eval (progressive-icon cl cb cr ct #t)))
                 "Direction of progression")
        (dynamic (anim-progressive-menu ft #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Customized focus icons
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (focus-toggle-menu t)
  (:require (tree-is? t 'anim-edit))
  ("Play animation" (current-anim-commit)))
  
(tm-menu (focus-toggle-menu t)
  (:require (tree-in? t '(anim-static anim-dynamic)))
  ("Edit animation" (current-anim-checkout)))

(tm-menu (focus-toggle-menu t)
  (:require (tree-func? t 'gr-screen 1)
            (user-anim-context? (tree-ref t 0)))
  (dynamic (focus-toggle-menu (tree-ref t 0))))

(tm-menu (focus-hidden-menu t)
  (:require (user-anim-context? t)))

(tm-menu (focus-toggle-icons t)
  (:require (tree-is? t 'anim-edit))
  ((balloon (icon "tm_search_next.xpm") "Play animation")
   (current-anim-commit)))
  
(tm-menu (focus-toggle-icons t)
  (:require (tree-in? t '(anim-static anim-dynamic)))
  ((balloon (icon "tm_show_hidden.xpm") "Edit animation")
   (current-anim-checkout)))

(tm-menu (focus-toggle-icons t)
  (:require (tree-func? t 'gr-screen 1)
            (user-anim-context? (tree-ref t 0)))
  (dynamic (focus-toggle-icons (tree-ref t 0))))

(tm-menu (focus-hidden-icons t)
  (:require (tree-is? t 'anim-edit)))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(anim-static anim-dynamic)))
  (assuming (not (test-bottom-bar? "animate"))
    //
    (dynamic (anim-duration-field "Duration" t 1))
    (dynamic (anim-step-field "Step" t 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Timing parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (anim-time-bar t)
  (let* ((n 50)
         (e (/ 1.0 n))
         (h (/ e 2.0))
         (c (or (anim-portion t) 0.0))
         (a (anim-control-times t)))
    (minibar
      (text "[")
      (for (i (... 0 n))
        (let* ((x (exact->inexact (* e i)))
               (now? (< (abs (- x c)) h))
               (sym (if now? "*" (if (in? x a) "+" "-"))))
          ((eval sym) (current-anim-set-portion x))))
      (text "]"))))

(tm-menu (anim-input-field name t i setter)
  (with in (tree->string (tree-ref t i))
    (mini #t
      (text (eval (string-append name ":")))
      (input (setter answer) "string" (list in) "5em")
      //)))

(tm-menu (anim-duration-field name t i)
  (dynamic (anim-input-field name t i current-anim-set-duration)))

(tm-menu (anim-step-field name t i)
  (dynamic (anim-input-field name t i current-anim-set-step)))

(tm-menu (anim-now-field name t i)
  (dynamic (anim-input-field name t i current-anim-set-now)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Animation toolbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (animate-toolbar)
  (with t (tree-innermost user-anim-context? #t)
    (hlist
      (assuming (not t)
        ((balloon (icon "tm_search_next.xpm") "Play all animations")
         (reset-players (buffer-tree)))
        // // //
        (text "No animation"))
      (assuming (tree-in? t '(anim-static anim-dynamic))
        ((balloon (icon "tm_search_next.xpm") "Play animation")
         (current-anim-play))
        ((balloon (icon "tm_show_hidden.xpm") "Edit animation")
         (current-anim-checkout)
         (notify-change 256))
        ((balloon (icon "tm_focus_delete.xpm") "Remove animation")
         (current-anim-remove))
        // // //
        (dynamic (anim-duration-field "Duration" t 1))
        (dynamic (anim-step-field "Step" t 2)))
      (assuming (tree-in? t '(anim-edit))
        ((balloon (icon "tm_search_next.xpm") "Play animation")
         (current-anim-commit)
         (notify-change 256))
        (assuming (anim-can-remove-frame? t)
          ((balloon (icon "tm_remove.xpm") "Remove this frame")
           (anim-current-remove-frame)))
        ((balloon (icon "tm_focus_delete.xpm") "Remove animation")
         (current-anim-remove))
        // // //
        (dynamic (anim-time-bar t))
        //
        (dynamic (anim-now-field "Now" t 4))
        (assuming (current-anim-can-copy?)
          // // //
          ((balloon (icon "tm_copy_before.xpm") "Copy to all frames before")
           (current-anim-copy-before))
          ((balloon (icon "tm_copy_both.xpm") "Copy to all other frames")
           (current-anim-copy-all))
          ((balloon (icon "tm_copy_after.xpm") "Copy to all frames after")
           (current-anim-copy-after))
          // // //
          ((balloon (icon "tm_delete_before.xpm")
                    "Remove from previous frames")
           (current-anim-delete-before))
          ((balloon (icon "tm_delete_after.xpm")
                    "Remove from subsequent frames")
           (current-anim-delete-after)))
        (assuming (and (not (current-anim-can-copy?))
                       (anim-show-new-mode?))
          // // //
          ((check (balloon (icon "tm_copy_before.xpm")
                           "Copy new objects to previous frames")
                  "v" (anim-test-new-mode? "before"))
           (anim-set-new-mode "before"))
          ((check (balloon (icon "tm_copy_both.xpm")
                           "Copy new objects to all frames")
                  "v" (anim-test-new-mode? "all"))
           (anim-set-new-mode "all"))
          ((check (balloon (icon "tm_copy_after.xpm")
                           "Copy new objects to subsequent frames")
                  "v" (anim-test-new-mode? "after"))
           (anim-set-new-mode "after"))))
      >>> >>> >>>
      ((balloon (icon "tm_close_tool.xpm") "Close animation tool")
       (set-bottom-bar "animate" #f)))))
