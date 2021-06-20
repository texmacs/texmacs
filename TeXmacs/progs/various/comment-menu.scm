
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : comment-menu.scm
;; DESCRIPTION : menus for commenting a text
;; COPYRIGHT   : (C) 2020  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (various comment-menu)
  (:use (various comment-edit)
        (various comment-widgets)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main 'Comment' menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-bind comment-menu
  (:require (in-comment?))
  ("New folded comment" (make-folded-comment "comment"))
  ("New unfolded comment" (make-unfolded-comment "comment"))
  ---
  ((shortcut "Edit comment" "C-return") (open-comment-editor))
  ("Edit comments" (open-comments-editor))
  ---
  ("First comment" (go-to-comment :first))
  ("Previous comment" (go-to-comment :previous))
  ("Next comment" (go-to-comment :next))
  ("Last comment" (go-to-comment :last))
  ---
  ("Fold comments" (operate-on-comments :fold))
  ("Unfold comments" (operate-on-comments :unfold))
  ("Remove comments" (operate-on-comments :cut))
  (with tl (comment-type-list :all)
    (assuming (> (length tl) 1)
      ---
      (for (tp tl)
        ((check (eval (upcase-first tp)) "v" (comment-test-type? tp))
         (comment-toggle-type tp)))))
  (with bl (comment-by-list :all)
    (assuming (> (length bl) 1)
      ---
      (for (by bl)
        ((check (eval by) "v" (comment-test-by? by))
         (comment-toggle-by by))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Selecting the colors of different types of comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-menu (comment-color-menu type by val)
  (with setter (lambda (val) (when val (set-comment-color type by val)))
    ((check "Default" "v" (default-comment-color? type by))
     (reset-comment-color type by))
    ---
    (pick-color (setter answer))
    ---
    ("Palette" (interactive-color setter (list val)))
    ("Other" (interactive setter
               (list "Color" "color" val)))))

(tm-menu (focus-extra-menu t)
  (:require (any-comment-context? t))
  (let* ((type (comment-type t))
         (by (comment-by t))
         (val (get-comment-color type by)))
    (-> "Set color"
        (dynamic (comment-color-menu type by val)))))

(tm-menu (focus-extra-icons t)
  (:require (any-comment-context? t))
  (let* ((type (comment-type t))
         (by (comment-by t))
         (val (get-comment-color type by))
         (setter (lambda (val) (when val (set-comment-color type by val)))))
    //
    (mini #t (text "Color:"))
    (=> (color val #f #f 24 16) 
        (dynamic (comment-color-menu type by val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard shortcuts for comments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-comment?)
  ("std :" (make-folded-comment "comment"))
  ("std [" (go-to-comment :previous))
  ("std ]" (go-to-comment :next))
  ("std {" (go-to-comment :first))
  ("std }" (go-to-comment :last)))

(kbd-map
  (:mode in-comment?)
  (:profile no-macos)
  ("std ;" (make-unfolded-comment "comment")))

(kbd-map
  (:mode in-comment?)
  (:profile macos)
  ("std /" (make-unfolded-comment "comment")))
